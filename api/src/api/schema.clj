(ns api.schema
  "Contains custom resolvers and a function to provide the full schema."
  (:require [api.db :as db]
            [clojure.java.io :as io]
            [com.walmartlabs.lacinia.util :as util]
            [com.walmartlabs.lacinia.schema :as schema]
            [com.walmartlabs.lacinia.resolve :as resolve]
            [clojure.core.async :refer [thread]]
            [clojure.edn :as edn]
            [api.util :refer [date-to-y-m-d-map, value-map, date-from-y-m-d-map]])
  (:use [slingshot.slingshot :only [throw+, try+]]))


(defn unnest-tagged-unions-on-input-object
  "Recursively converts all the tagged unions in the input object to have data unnested.
  Will look for the value ascociated with the`:type_name` key and unnest the data stored
  with that as its key, if no :type_name key is present or it isn't a map, returns
  the input-object unchanged. Does this recursively for all nested keys if it is a map."
  [input-object]
  (let [type_name (and (map? input-object) (:type_name input-object))]
    (if (and (map? input-object) (not (nil? type_name)))
      (as-> (input-object (keyword type_name)) rtrn
        (if (nil? rtrn) (throw+ {:type ::invalid_tagged_union_null :tag_name type_name}) rtrn)
        (value-map rtrn unnest-tagged-unions-on-input-object)
        (assoc rtrn :type_name type_name))
      input-object)))

(defn tag-type
  "Tag a map using `schema/tag-with-type` and the map's `type_name`."
  [map-with-type]
  (schema/tag-with-type map-with-type (keyword (:type_name map-with-type))))

(defn create-async-resolver
  "Spawn a thread to run the resolver and immediately return a lacinia `ResolverResultPromise`."
  [resolver]
  (fn [context args value]
    (let [result (resolve/resolve-promise)]
      (thread
       (try+
         (resolve/deliver! result (resolver context args value))
         (catch [:type ::invalid_tagged_union_null] {:keys [tag_name]}
           (resolve/deliver! result nil
                             {:message (str
                                        "Invalid tagged union, tag name was "
                                        tag_name
                                        " but value associated with that key was null.")

                              :status 400}))
         (catch Throwable t
           (resolve/deliver! result nil
                             {:message (str "Exception: " (.getMessage t))
                              :status 400}))

         (catch Object t
           (resolve/deliver! result nil
                             {:message (str "Exception: " t)
                              :status 400}))))

      result)))

(defn create-tag-type-resolver
  "Create a resolver which returns the `field-name` of `value` tagged with `tag-type`."
  [field-name]
  (fn [context args value] (tag-type (value field-name))))

(defn create-date-to-y-m-d-resolver
  "Creates a resolver that converts a joda date-time to a y-m-d map."
  [field-name]
  (fn [context args value] (date-to-y-m-d-map (value field-name))))

(defn create-nilable-date-to-y-m-d-resolver
  "Creates a resolver that converts a possibly `nil` joda date-time to a y-m-d map."
  [field-name]
  (fn [context args value]
    (let [datetime (value field-name)]
      (if (nil? datetime)
        nil
        (date-to-y-m-d-map datetime)))))

(defn resolve-get-habits
  "@refer `db/get-habits`."
  [context args value]
  (map tag-type (db/get-habits args)))

(defn resolve-mutation-add-habit
  "@refer `db/add-habit`."
  [context {:keys [create_habit_data frequency_start_date] :as all} value]
  (as-> all $
        (assoc (dissoc $ :create_habit_data) :habit (unnest-tagged-unions-on-input-object create_habit_data))
        (assoc (dissoc $ :frequency_start_date) :frequency-start-datetime (date-from-y-m-d-map frequency_start_date))
        (tag-type (db/add-habit $))))

(defn resolve-mutation-set-habit-data
  "@refer `db/set-habit-data`."
  [context {:keys [date] :as all} value]
  (db/set-habit-data (assoc (dissoc all :date) :date-time (date-from-y-m-d-map date))))

(defn resolve-mutation-set-habit-day-note
  "@refer `db/set-habit-day-note`."
  [context {:keys [date] :as all} value]
  (db/set-habit-day-note (assoc (dissoc all :date) :date-time (date-from-y-m-d-map date))))

(defn resolve-get-habit-data
  "@refer `db/get-habit-data`."
  [context args value]
  (db/get-habit-data args))

(defn resolve-get-habit-day-notes
  "@refer `db/get-habit-day-notes`."
  [context args value]
  (db/get-habit-day-notes args))

(defn resolve-mutation-delete-habit
  "@refer `db/delete-habit`."
  [context args value]
  (db/delete-habit args))

(defn resolve-query-get-frequency-stats
  "@refer `db/get-frequency-stats`."
  [context {:keys [current_client_date] :as all} value]
  (map tag-type (db/get-frequency-stats (assoc all :current_client_date (date-from-y-m-d-map current_client_date)))))

(defn resolve-mutation-edit-habit-suspensions
  "@refer `db/edit-habit-suspensions`."
  [context {:keys [new_suspensions] :as all} value]
  (let [convert-suspended-interval-y-m-d-maps-to-dates (fn [suspended-interval]
                                                         (let [{start-date-ymd :start_date
                                                                end-date-ymd :end_date} suspended-interval]
                                                           (assoc suspended-interval
                                                                  :start_date (date-from-y-m-d-map start-date-ymd)
                                                                  :end_date (if (nil? end-date-ymd)
                                                                              nil
                                                                              (date-from-y-m-d-map end-date-ymd)))))
        new_suspensions (map convert-suspended-interval-y-m-d-maps-to-dates new_suspensions)]
    (tag-type (db/edit-habit-suspensions (assoc all :new_suspensions new_suspensions)))))

(defn resolve-mutation-edit-habit-goal-frequencies
  "@refer `db/edit-habit-goal-frequencies`."
  [context {:keys [new_frequencies] :as all} value]
  (let [new_frequencies (map #(update % :new_frequency unnest-tagged-unions-on-input-object)
                             new_frequencies),
        convert-frequency-y-m-d-maps-to-dates (fn [frequency-change-record]
                                                (let [{start-date-ymd :start_date
                                                       end-date-ymd :end_date} frequency-change-record]
                                                  (assoc frequency-change-record
                                                         :start_date (date-from-y-m-d-map start-date-ymd)
                                                         :end_date (if (nil? end-date-ymd)
                                                                     nil
                                                                     (date-from-y-m-d-map end-date-ymd))))),
        new_frequencies (map convert-frequency-y-m-d-maps-to-dates new_frequencies)]
    (tag-type (db/edit-habit-goal-frequencies (assoc all :new_frequencies new_frequencies)))))

(defn resolver-map
  []
  {:query/get-habits (create-async-resolver resolve-get-habits)
   :query/tag-type-for-new-frequency (create-tag-type-resolver :new_frequency)
   :query/resolve-mutation-add-habit (create-async-resolver resolve-mutation-add-habit)
   :query/resolve-mutation-set-habit-data (create-async-resolver resolve-mutation-set-habit-data)
   :query/resolve-mutation-set-habit-day-note (create-async-resolver resolve-mutation-set-habit-day-note)
   :query/get-habit-data (create-async-resolver resolve-get-habit-data)
   :query/date-to-y-m-d-format (create-date-to-y-m-d-resolver :date)
   :query/habit-day-note-date-to-y-m-d-format (create-date-to-y-m-d-resolver :date)
   :query/suspended-start-date-to-y-m-d-format (create-date-to-y-m-d-resolver :start_date)
   :query/suspended-end-date-to-y-m-d-format (create-nilable-date-to-y-m-d-resolver :end_date)
   :query/start-date-to-y-m-d-format (create-date-to-y-m-d-resolver :start_date)
   :query/end-date-to-y-m-d-format (create-nilable-date-to-y-m-d-resolver :end_date)
   :query/resolve-mutation-delete-habit (create-async-resolver resolve-mutation-delete-habit)
   :query/get-frequency-stats (create-async-resolver resolve-query-get-frequency-stats)
   :query/resolve-mutation-edit-habit-suspensions (create-async-resolver resolve-mutation-edit-habit-suspensions)
   :query/resolve-mutation-edit-habit-goal-frequencies (create-async-resolver resolve-mutation-edit-habit-goal-frequencies)
   :query/get-habit-day-notes (create-async-resolver resolve-get-habit-day-notes)})

(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))

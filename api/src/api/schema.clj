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

(defn resolve-get-habits
  "@refer `db/get-habits`."
  [context args value]
  (map tag-type (db/get-habits args)))

(defn resolve-mutation-add-habit
  "@refer `db/add-habit`."
  [context {:keys [create_habit_data creation_date] :as all} value]
  (as-> all $
        (assoc (dissoc $ :create_habit_data) :habit (unnest-tagged-unions-on-input-object create_habit_data))
        (assoc (dissoc $ :creation_date) :creation-date-time (date-from-y-m-d-map creation_date))
        (tag-type (db/add-habit $))))

(defn resolve-mutation-set-habit-data
  "@refer `db/set-habit-data`."
  [context {:keys [date] :as all} value]
  (db/set-habit-data (assoc (dissoc all :date) :date-time (date-from-y-m-d-map date))))

(defn resolve-get-habit-data
  "@refer `db/get-habit-data`."
  [context args value]
  (db/get-habit-data args))

(defn resolve-mutation-delete-habit
  "@refer `db/delete-habit`."
  [context args value]
  (db/delete-habit args))

(defn resolve-mutation-toggle-suspended-habit
  "@refer `db/toggle-suspended-habit`."
  [context {:keys [toggle_date] :as all} value]
  (db/toggle-suspended-habit (assoc (dissoc all :toggle_date) :toggle-date-time (date-from-y-m-d-map toggle_date))))

(defn resolve-query-get-frequency-stats
  "@refer `db/get-frequency-stats`."
  [context {:keys [current_client_date] :as all} value]
  (map tag-type (db/get-frequency-stats (assoc all :current_client_date (date-from-y-m-d-map current_client_date)))))

(defn resolver-map
  []
  {:query/get-habits (create-async-resolver resolve-get-habits)
   :query/tag-type-for-new-frequency (create-tag-type-resolver :new_frequency)
   :query/resolve-mutation-add-habit (create-async-resolver resolve-mutation-add-habit)
   :query/resolve-mutation-set-habit-data (create-async-resolver resolve-mutation-set-habit-data)
   :query/get-habit-data (create-async-resolver resolve-get-habit-data)
   :query/date-to-y-m-d-format (create-date-to-y-m-d-resolver :date)
   :query/toggle-date-to-y-m-d-format (create-date-to-y-m-d-resolver :toggle_date)
   :query/start-date-to-y-m-d-format (create-date-to-y-m-d-resolver :start_date)
   :query/end-date-to-y-m-d-format (create-date-to-y-m-d-resolver :end_date)
   :query/resolve-mutation-delete-habit (create-async-resolver resolve-mutation-delete-habit)
   :query/resolve-mutation-toggle-suspended-habit (create-async-resolver resolve-mutation-toggle-suspended-habit)
   :query/get-frequency-stats (create-async-resolver resolve-query-get-frequency-stats)})

(defn load-schema
  []
  (-> (io/resource "schema.edn")
      slurp
      edn/read-string
      (util/attach-resolvers (resolver-map))
      schema/compile))

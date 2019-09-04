(ns api.db
  "Interaction with the database happens in this namespace"
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [monger.joda-time]
            [api.util :refer :all]
            [api.freq-stats-util :refer [get-freq-stats-for-habit]]
            [api.goal-intervals-util :refer [get-habit-goal-interval-list]]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId
           org.joda.time.DateTimeZone
           org.jasypt.util.password.StrongPasswordEncryptor))

(DateTimeZone/setDefault DateTimeZone/UTC)

(def collection-names
  "The names of all the collections in the database."
  {:habits "habits"
   :habit_data "habit_data"
   :habit_day_notes "habit_day_notes"
   :users "users"})

(defonce connection (mg/connect))

(defonce habby_db (mg/get-db connection "habby"))

(defn add-habit
  "Add a habit to the db for a user, and returns that habit including the ID.
  Will create an ID if the habit passed doesn't have an ID.
  Converts the habit's `target_frequency` or `threshold_frequency` into an array of `frequency_change_record`s.
  Initializes the habit's `suspensions` array to empty."
  [{:keys [db habit frequency-start-datetime] :or {db habby_db}}]
  (as-> habit final_habit
        (if (contains? final_habit :_id) final_habit (assoc final_habit :_id (ObjectId.)))
        (if (contains? final_habit :initial_target_frequency)
          (assoc (dissoc final_habit :initial_target_frequency)
                 :target_frequencies [{:start_date frequency-start-datetime,
                                       :end_date nil,
                                       :new_frequency (:initial_target_frequency final_habit)}])
          final_habit)
        (if (contains? final_habit :initial_threshold_frequency)
          (assoc (dissoc final_habit :initial_threshold_frequency)
                 :threshold_frequencies [{:start_date frequency-start-datetime,
                                          :end_date nil,
                                          :new_frequency (:initial_threshold_frequency final_habit)}])
          final_habit)
        (assoc final_habit :suspensions [])
        (update final_habit :user_id #(ObjectId. %))
        (mc/insert-and-return db (:habits collection-names) final_habit)
        (if (contains? final_habit :threshold_frequencies)
          ; Bad habit
          (do
            ; Set habit data of 0 to initialize the bad habit
            (mc/find-and-modify db
                                (:habit_data collection-names)
                                {:date frequency-start-datetime,
                                 :habit_id (:_id final_habit),
                                 :user_id (:user_id final_habit)}
                                {$set {:amount 0}
                                 $setOnInsert {:_id (ObjectId.),
                                               :user_id (:user_id final_habit),
                                               :habit_id (:_id final_habit),
                                               :date frequency-start-datetime}}
                                {:upsert true, :return-new true})
            ; Return the inserted bad habit
            final_habit)
          ; Good habit
          final_habit)))

(defn add-user
  "Attempts to add a user to the database, encrypting the inputted password along the way.
  Returns `nil` if username or email address is already taken, else returns the new user's safe fields.
  `new_email_address` could be `nil`."
  [{:keys [db new_username new_display_name new_email_address new_password] :or {db habby_db}}]
  (if (and (empty? (mc/find-maps db
                                 (:users collection-names)
                                 {:username new_username}))
           (or (nil? new_email_address)
               (empty? (mc/find-maps db
                                   (:users collection-names)
                                   {:email_address new_email_address}))))
    (let [new-encrypted-password (.encryptPassword (StrongPasswordEncryptor.) new_password),
          new-user {:_id (ObjectId.),
                    :username new_username,
                    :display_name new_display_name,
                    :email_address new_email_address,
                    :encrypted_password new-encrypted-password}]
      (dissoc (mc/insert-and-return db (:users collection-names) new-user)
              :encrypted_password))))

(defn edit-habit-suspensions
  "Changes a user's habit's `:suspensions` field."
  [{:keys [db user_id habit_id new_suspensions] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habits collection-names)
                      {:_id (ObjectId. habit_id) :user_id (ObjectId. user_id)}
                      {$set {:suspensions new_suspensions}}
                      {:return-new true}))


(defn edit-habit-goal-frequencies
  "Changes a user's habit's `:target_frequencies` or `:threshold_frequencies` field."
  [{:keys [db user_id habit_id new_frequencies habit_type] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habits collection-names)
                      {:_id (ObjectId. habit_id) :user_id (ObjectId. user_id)}
                      {$set {(if (= habit_type "good_habit")
                               :target_frequencies
                               :threshold_frequencies)
                             new_frequencies}}
                      {:return-new true}))

(defn edit-habit-info
  "Changes a user's habit's info fields.
  `new_info` contains a `time_of_day` field but this should only be mutated for good habits."
  [{:keys [db user_id habit_id habit_type new_info] :or {db habby_db}}]
  (let [modifiers (if (= habit_type "good_habit")
                    new_info
                    (dissoc new_info :time_of_day))]
    (mc/find-and-modify db
                        (:habits collection-names)
                        {:_id (ObjectId. habit_id) :user_id (ObjectId. user_id)}
                        {$set modifiers}
                        {:return-new true})))

(defn delete-habit
  "Deletes a habit from the database, returns true if the habit was deleted."
  [{:keys [db habit_id] :or {db habby_db}}]
  (= 1 (.getN (mc/remove-by-id db (:habits collection-names) (ObjectId. habit_id)))))

(defn get-habits
  "Retrieves all the user's habits from the db as clojure maps."
  [{:keys [db user_id habit_ids] :or {db habby_db}}]
  (as-> {:user_id (ObjectId. user_id)} find-query-filter
        (if (nil? habit_ids) find-query-filter (assoc find-query-filter :_id {$in (map #(ObjectId. %) habit_ids)}))
        (mc/find-maps db
                      (:habits collection-names)
                      find-query-filter)))

(defn login-user
  "Returns a `:user_safe_fields` map of the authenticated user if authentication succeeded, else `nil`."
  [{:keys [db user_name_input user_password_input] :or {db habby_db}}]
  (as-> (mc/find-maps db (:users collection-names) {:username user_name_input}) matching-users
        (filter #(.checkPassword (StrongPasswordEncryptor.) user_password_input (:encrypted_password %))
                matching-users)
        (if-not (nil? (first matching-users))
          (dissoc (first matching-users)
                  :encrypted_password))))

(defn get-habit-data
  "Gets a user's habit data from the db, optionally after/before a specific date or for specific habits."
  [{:keys [db user_id after_date before_date habit_ids] :or {db habby_db}}]
  (as-> {:user_id (ObjectId. user_id)} find-query-filter
        (if (nil? habit_ids) find-query-filter (assoc find-query-filter :habit_id {$in (map #(ObjectId. %) habit_ids)}))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (if (nil? before_date)
          find-query-filter
          (assoc find-query-filter
                 ; Require dates be earlier than midnight the day after `before_date`, so that times don't interfere
                 :date {$lt (t/plus (date-from-y-m-d-map before_date) (t/days 1))}))
        (mc/find-maps db (:habit_data collection-names) find-query-filter)))

(defn get-habit-day-notes
  "Gets a user's habit day notes from the db, optionally after/before a specific date or for specific habits."
  [{:keys [db user_id after_date before_date habit_ids] :or {db habby_db}}]
  (as-> {:user_id (ObjectId. user_id)} find-query-filter
        (if (nil? habit_ids) find-query-filter (assoc find-query-filter :habit_id {$in (map #(ObjectId. %) habit_ids)}))
        (if (nil? after_date) find-query-filter (assoc find-query-filter :date {$gte (date-from-y-m-d-map after_date)}))
        (if (nil? before_date)
          find-query-filter
          (assoc find-query-filter
                 ; Require dates be earlier than midnight the day after `before_date`, so that times don't interfere
                 :date {$lt (t/plus (date-from-y-m-d-map before_date) (t/days 1))}))
        (mc/find-maps db (:habit_day_notes collection-names) find-query-filter)))

(defn set-habit-data
  "Set the `amount` for a user's habit on a specfic day."
  [{:keys [db user_id habit_id amount date-time] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habit_data collection-names)
                      {:date date-time, :habit_id (ObjectId. habit_id), :user_id (ObjectId. user_id)}
                      {$set {:amount amount}
                       $setOnInsert {:_id (ObjectId.),
                                     :user_id (ObjectId. user_id),
                                     :habit_id (ObjectId. habit_id),
                                     :date date-time}}
                      {:upsert true, :return-new true}))

(defn set-habit-day-note
  "Add a `note` for a habit on a given day."
  [{:keys [db user_id habit_id note date-time] :or {db habby_db}}]
  (mc/find-and-modify db
                      (:habit_day_notes collection-names)
                      {:date date-time, :habit_id (ObjectId. habit_id), :user_id (ObjectId. user_id)}
                      {$set {:note note}
                       $setOnInsert {:_id (ObjectId.),
                                     :user_id (ObjectId. user_id),
                                     :habit_id (ObjectId. habit_id),
                                     :date date-time}}
                      {:upsert true, :return-new true}))

(defn get-habit-goal-interval-lists
  "Returns a list of `habit_goal_interval_list`s for the user's specified habits, one `habit_goal_interval_list` for each.
  Retrieves habits and habit data from database `db`.
  Only returns `habit_goal_interval`s that started after `start-date-time`, and cuts them off at `end-date-time`."
  [{:keys [db user_id habit_ids start-date-time end-date-time], :or {db habby_db}}]
  (let [all-habits (get-habits {:db db,
                                :user_id user_id,
                                :habit_ids habit_ids}),
        start-date (if (nil? start-date-time) nil (date-to-y-m-d-map start-date-time))
        all-relevant-habits-data (get-habit-data {:db db,
                                                  :user_id user_id,
                                                  :after_date start-date,
                                                  :before_date (date-to-y-m-d-map end-date-time),
                                                  :habit_ids habit_ids})]
    (map #(get-habit-goal-interval-list %
                                        all-relevant-habits-data
                                        start-date-time
                                        end-date-time)
         all-habits)))

(defn get-frequency-stats
  "Returns performance statistics for a user's habits, or optionally only for specific habits of theirs.
  Retrieves habits and habit data from database `db`.
  Analyzes user performance based on habit data from `current-client-date-time` or earlier.
  Generates a list of `habit_frequency_stats` maps, one for each ID in `habit_ids`."
  [{:keys [db user_id habit_ids current-client-date-time] :or {db habby_db, current-client-date-time (t/today-at 0 0)}}]
  (let [all-habits (get-habits {:db db,
                                :user_id user_id,
                                :habit_ids habit_ids})
        all-habit-data-until-current-date (get-habit-data {:db db,
                                                           :user_id user_id,
                                                           :before_date (date-to-y-m-d-map current-client-date-time),
                                                           :habit_ids habit_ids})]
    (map #(get-freq-stats-for-habit %
                                    all-habit-data-until-current-date
                                    current-client-date-time)
         all-habits)))

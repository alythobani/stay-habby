(ns api.freq-stats-util
  "A namespace for holding utilities related to calculation of performance statistics."
  (:require [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime, get-consecutive-datetimes,
                                 day-of-week-keyword, days-spanned-between-datetimes, earliest-datetime]]
            [api.habit-util :refer [get-frequencies]]
            [clj-time.core :as t]))

(def default-frequency-stats {:total_fragments 0, :successful_fragments 0, :total_done 0,
                              :current_fragment_streak 0, :best_fragment_streak 0,
                              :current_fragment_total 0, :current_fragment_goal 0, :current_fragment_days_left 0,
                              :habit_has_started false, :currently_suspended false})

(defn get-habit-goal-fragment-length
  "Returns the number of days within each habit goal fragment, given a goal frequency `freq`.
  A habit goal fragment is a fragment of time that we will treat as either a success or a failure for a habit."
  [freq]
  (condp = (:type_name freq)
         "specific_day_of_week_frequency" 1,
         "total_week_frequency" 7,
         "every_x_days_frequency" (:days freq)))

(defn get-habit-goal-amount-for-datetime
  "Returns the target/threshold amount for a particular DateTime, given the goal frequency."
  [dt freq]
  (condp = (:type_name freq)
         "specific_day_of_week_frequency" (-> dt
                                              day-of-week-keyword
                                              freq),
         "total_week_frequency" (:week freq),
         "every_x_days_frequency" (:times freq)))

(defn partition-datetimes-based-on-fragment-length
  "Returns a partition of consecutive DateTimes corresponding to habit goal fragments.
  Boundaries are inclusive, with respect to `from-date` and `until-date`."
  [fragment-length from-date until-date]
  (let [datetimes (get-consecutive-datetimes from-date until-date)]
    (partition-all fragment-length datetimes)))

(defn count-fragment-as-suspended?
  "Returns true iff we should count a fragment as suspended based on `suspended-interval`.
  Treat it as suspended if there is any overlap."
  [fragment-start-dt fragment-end-dt suspended-interval]
  (and (date-leq? (:start_date suspended-interval)
                  fragment-end-dt)
       (or (nil? (:end_date suspended-interval))
           (date-geq? (:end_date suspended-interval)
                      fragment-start-dt))))

(defn create-habit-goal-fragment
  "Constructs a habit goal fragment corresponding to a list of consecutive DateTimes and a goal frequency `freq`.
  `datetimes` should be nonempty, be sorted increasingly by date, and correspond to a habit goal fragment.
  `:valid`: true iff the fragment has elapsed its expected fragment length (i.e. was not cut off by the
            current date or the next goal)
  `:goal-amount`: the goal amount for the fragment
  `:expected-fragment-length`: the number of days in each full fragment for `freq`
  Initializes `:total-done` to 0 and `:successful` to false. (To be evaluated later.)"
  [datetimes freq suspended-intervals]
  (let [fragment-start-dt (first datetimes),
        fragment-end-dt (last datetimes),
        expected-fragment-length (get-habit-goal-fragment-length freq),
        actual-fragment-length (days-spanned-between-datetimes (first datetimes) (last datetimes))]
    {:start-date fragment-start-dt,
     :end-date fragment-end-dt,
     :total-done 0,
     :successful false,
     :suspended (some #(count-fragment-as-suspended? fragment-start-dt fragment-end-dt %) suspended-intervals),
     :valid (= expected-fragment-length actual-fragment-length),
     :goal-amount (get-habit-goal-amount-for-datetime fragment-start-dt freq),
     :expected-fragment-length expected-fragment-length,
     :actual-fragment-length actual-fragment-length}))

(defn during-habit-goal-fragment?
  "Returns true iff `datetime` occurs during `habit-goal-fragment`."
  [datetime habit-goal-fragment]
  (and (date-geq? datetime (:start-date habit-goal-fragment))
       (date-leq? datetime (:end-date habit-goal-fragment))))

(defn get-habit-data-during-fragment
  "Finds all `habit_day_record`s in `habit-data` that occur during `habit-goal-fragment`."
  [habit-data habit-goal-fragment]
  (filter #(during-habit-goal-fragment? (:date %) habit-goal-fragment) habit-data))

(defn evaluate-habit-goal-fragment-total-done
  "Updates the `:total-done` field of `habit-goal-fragment` based on a list of `habit_day_record`s."
  [habit-goal-fragment habit-data-during-fragment]
  (reduce #(update %1 :total-done + (:amount %2))
          habit-goal-fragment
          habit-data-during-fragment))

(defn evaluate-habit-goal-fragment-successful
  "Evaluates the `:successful` field of `habit-goal-fragment` based on its `:total-done` field, the type of habit, and the habit's goal."
  [habit-goal-fragment habit-type freq]
  (assoc habit-goal-fragment
         :successful ((if (= habit-type "good_habit") >= <=)
                      (:total-done habit-goal-fragment)
                      (:goal-amount habit-goal-fragment))))

(defn evaluate-habit-goal-fragment
  "Evaluates `:total-done` and `:successful` fields of a habit goal fragment."
  [habit-goal-fragment habit-data habit-type freq]
  (let [habit-data-during-fragment (get-habit-data-during-fragment habit-data habit-goal-fragment)]
    (-> habit-goal-fragment
        (evaluate-habit-goal-fragment-total-done habit-data-during-fragment)
        (evaluate-habit-goal-fragment-successful habit-type freq))))

(defn create-habit-goal-fragments-for-an-fcr
  "Initializes habit goal fragments for a habit based on a `frequency_change_record` (`fcr`).
  Partitions the datetimes spanned by `fcr` based on the goal length.
  Then converts each sequence of datetimes in the partition into a habit goal fragment."
  [current-date fcr suspended-intervals]
  (let [freq (:new_frequency fcr),
        fragment-length (get-habit-goal-fragment-length freq),
        goal-end-date (if (nil? (:end_date fcr))
                        ; Neverending (i.e. current) goal. Cut off last fragment at today.
                        current-date
                        ; This goal ends. Cut off last fragment at the end date, or today if the end_date falls after today.
                        (earliest-datetime (:end_date fcr) current-date)),
        partitioned-datetimes (partition-datetimes-based-on-fragment-length fragment-length
                                                                            (:start_date fcr)
                                                                            goal-end-date)]
    (map #(create-habit-goal-fragment % freq suspended-intervals) partitioned-datetimes)))

(defn create-habit-goal-fragments
  "Bring together all fragments generated for each of the habit's goals, into one array of fragments."
  [current-date freq-change-records suspended-intervals]
  (apply concat
         (map #(create-habit-goal-fragments-for-an-fcr current-date % suspended-intervals) freq-change-records)))

(defn get-habit-goal-fragments
  "Creates and evaluates habit goal fragments for a habit based on data from `current-date` or earlier.
  Returns `nil` if `sorted-habit-data` is empty, i.e. the habit has no relevant data."
  [sorted-habit-data current-date habit-type freq-change-records suspended-intervals]
  (if-not (empty? sorted-habit-data)
    (let [habit-goal-fragments (create-habit-goal-fragments current-date
                                                            freq-change-records
                                                            suspended-intervals)]
      (map #(evaluate-habit-goal-fragment % sorted-habit-data habit-type freq) habit-goal-fragments))))

(defn update-freq-stats-with-past-fragment
  "Updates fields of a `habit_frequency_stats` based on a past habit goal fragment."
  [habit-frequency-stats habit-goal-fragment]
  (let [successful (:successful habit-goal-fragment)]
    (if (or (:suspended habit-goal-fragment)
            (not (:valid habit-goal-fragment)))
      ; Don't count towards stats, other than `:total_done`
      (update habit-frequency-stats :total_done + (:total-done habit-goal-fragment))
      ; Can count normally towards stats
      (as-> habit-frequency-stats $
            (update $ :total_fragments inc)
            (update $ :successful_fragments (if successful inc identity))
            (update $ :total_done + (:total-done habit-goal-fragment))
            (update $ :current_fragment_streak (if successful inc (constantly 0)))
            (assoc $ :best_fragment_streak (max (:current_fragment_streak $) (:best_fragment_streak $)))))))

(defn update-freq-stats-with-current-fragment
  "Updates fields of a `habit_frequency_stats` based on the current habit goal fragment and its goal `freq`.
  Computes `:current_fragment_days_left` based on the fragment length defined by `freq` minus the span
  of `current-fragment`, whose `:end-date` field was cut short at the current date during construction.
  Only ever treats the current fragment as successful for good habits, and only ever treats it as failed for bad
  habits; we don't punish unfinished good habits or reward unfinished bad habits."
  [freq-stats current-fragment habit-type]
  (let [treat-as-successful (and (= habit-type "good_habit") (:successful current-fragment))
        treat-as-failed (and (= habit-type "bad_habit") (not (:successful current-fragment)))]
    (as-> freq-stats $
          (update $ :total_done + (:total-done current-fragment))
          (assoc $ :current_fragment_total (:total-done current-fragment))
          (assoc $ :current_fragment_goal (:goal-amount current-fragment))
          (assoc $ :current_fragment_days_left (- (:expected-fragment-length current-fragment)
                                                  (:actual-fragment-length current-fragment)))
          (if (:suspended current-fragment)
            $
            (as-> $ $
                  (update $ :total_fragments (if (or treat-as-successful treat-as-failed) inc identity))
                  (update $ :successful_fragments (if treat-as-successful inc identity))
                  (update $ :current_fragment_streak (if treat-as-successful
                                                       inc
                                                       (if treat-as-failed (constantly 0) identity)))
                  (assoc $ :best_fragment_streak (max (:current_fragment_streak $) (:best_fragment_streak $))))))))

(defn update-freq-stats-with-habit-goal-fragments
  "Updates a `habit_frequency_stats` based on a non-empty ordered-by-date list of habit goal fragments."
  [freq-stats habit-goal-fragments habit]
  (let [past-fragments (butlast habit-goal-fragments),
        current-fragment (last habit-goal-fragments),
        updated-with-past-fragments-freq-stats (reduce update-freq-stats-with-past-fragment freq-stats past-fragments)]
    (update-freq-stats-with-current-fragment updated-with-past-fragments-freq-stats current-fragment (:type_name habit))))

(defn datetime-falls-within-suspended-interval?
  "Returns true iff `datetime` occurs during `suspended-interval`."
  [datetime suspended-interval]
  (and (date-geq? datetime (:start_date suspended-interval))
       (or (nil? (:end_date suspended-interval))
           (date-leq? datetime (:end_date suspended-interval)))))

(defn get-freq-stats-for-habit
  "Computes a `habit_frequency_stats` for `habit` based on habit data from `current-date` or earlier.
  `all-habit-data-until-current-date` may include data from other habits.
  Uses the habit's `:suspensions` array to determine when to exclude dates from performance calculation."
  [habit all-habit-data-until-current-date current-date]
  (let [sorted-habit-data (->> all-habit-data-until-current-date
                               (filter #(= (:habit_id %) (:_id habit)))
                               (sort-by :date)),
        suspensions (:suspensions habit),
        freq-change-records (get-frequencies habit),
        ; Exclude goals started after today
        relevant-freq-change-records (filter #(date-leq? (:start_date %) current-date)
                                             freq-change-records),
        habit-goal-fragments (get-habit-goal-fragments sorted-habit-data
                                                       current-date
                                                       (:type_name habit)
                                                       relevant-freq-change-records
                                                       suspensions)]
    (as-> (assoc default-frequency-stats :habit_id (:_id habit)) freq-stats
          (if (seq suspensions)
            ; The habit has been suspended before, check if it's currently suspended
            (assoc freq-stats
                   :currently_suspended (some #(datetime-falls-within-suspended-interval? current-date %) suspensions))
            ; The habit has never been suspended before, so it's not currently suspended
            freq-stats)
          (if (nil? habit-goal-fragments)
            freq-stats
            (-> (update-freq-stats-with-habit-goal-fragments freq-stats
                                                             habit-goal-fragments
                                                             habit)
                (assoc :habit_has_started true))))))

(ns api.goal-intervals-util
  "A namespace for holding utilities related to generating `habit_goal_interval_list`s."
  (:require [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime, get-consecutive-datetimes,
                                 day-of-week-keyword, days-spanned-between-datetimes, earliest-datetime, latest-datetime]]
            [api.habit-util :refer [get-frequencies]]
            [api.freq-stats-util :refer [partition-datetimes-based-on-fragment-length, get-habit-goal-fragment-length,
                                         evaluate-habit-goal-fragment, create-habit-goal-fragment]]
            [clj-time.core :as t]))

(defn create-habit-goal-intervals-for-an-fcr
  "Initializes `habit_goal_interval`s for a habit based on a `frequency_change_record` (`fcr`).
  Partitions the datetimes spanned by `fcr` based on the goal length.
  Then converts each sequence of datetimes in the partition into a `habit_goal_interval`."
  [start-date-time end-date-time fcr suspended-intervals]
  (let [freq (:new_frequency fcr),
        fragment-length (get-habit-goal-fragment-length freq),
        intervals-start-date (latest-datetime start-date-time (:start_date fcr)),
        intervals-end-date (if (nil? (:end_date fcr))
                            ; Neverending (i.e. current) goal. Cut off last interval at `end-date-time`.
                            end-date-time
                            ; This goal ends. Cut off last interval then, or at `end-date-time` if that comes earlier.
                            (earliest-datetime (:end_date fcr) end-date-time)),
        partitioned-datetimes (partition-datetimes-based-on-fragment-length fragment-length
                                                                            intervals-start-date
                                                                            intervals-end-date)]
    (map #(create-habit-goal-fragment % freq suspended-intervals) partitioned-datetimes)))

(defn create-habit-goal-intervals
  "Bring together all `habit_goal_interval`s generated for each of the habit's goals, into one array of intervals."
  [start-date-time end-date-time freq-change-records suspended-intervals]
  (apply concat
         (map #(create-habit-goal-intervals-for-an-fcr start-date-time end-date-time % suspended-intervals)
              freq-change-records)))

(defn get-habit-goal-intervals
  "Creates and evaluates `habit_goal_interval`s for a habit, ranging from `start-date-time` to `end-date-time`, or
  smaller than that range if goals only cover part of the range."
  [sorted-habit-data start-date-time end-date-time habit-type freq-change-records suspended-intervals]
  (let [habit-goal-intervals (create-habit-goal-intervals start-date-time
                                                          end-date-time
                                                          freq-change-records
                                                          suspended-intervals)]
    (map #(evaluate-habit-goal-fragment % sorted-habit-data habit-type) habit-goal-intervals)))

(defn get-evaluated-habit-goal-intervals
  "Gets evaluated `habit_goal_interval`s for `habit`."
  [habit all-relevant-habits-data start-date-time end-date-time]
  (let [sorted-habit-data (->> all-relevant-habits-data
                               (filter #(= (:habit_id %) (:_id habit)))
                               (sort-by :date)),
        suspensions (:suspensions habit),
        freq-change-records (get-frequencies habit),
        ; Exclude goals started after `end-date-time` or ended before `start-date-time`
        relevant-freq-change-records (filter #(and (date-leq? (:start_date %) end-date-time)
                                                   (or (nil? (:end_date %))
                                                       (date-geq? (:end_date %) start-date-time)))
                                             freq-change-records)]
    (get-habit-goal-intervals sorted-habit-data
                              start-date-time
                              end-date-time
                              (:type_name habit)
                              relevant-freq-change-records
                              suspensions)))

(defn get-habit-goal-interval-list
  "Computes a `habit_goal_interval_list` for `habit`"
  [habit all-relevant-habits-data start-date-time end-date-time]
  (let [evaluated-habit-goal-intervals (get-evaluated-habit-goal-intervals habit
                                                                           all-relevant-habits-data
                                                                           start-date-time
                                                                           end-date-time),
        intervals-without-unnecessary-fields (map #(dissoc % :expected-fragment-length :actual-fragment-length)
                                                  evaluated-habit-goal-intervals)]
    {:habit_id (:_id habit)
     :goal_intervals intervals-without-unnecessary-fields}))

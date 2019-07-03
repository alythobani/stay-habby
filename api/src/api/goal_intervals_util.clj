(ns api.goal-intervals-util
  "A namespace for holding utilities related to generating `habit_goal_interval_list`s."
  (:require [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime, get-consecutive-datetimes,
                                 day-of-week-keyword, days-spanned-between-datetimes, earliest-datetime, latest-datetime]]
            [api.habit-util :refer [get-frequencies]]
            [api.freq-stats-util :refer [partition-datetimes-based-on-fragment-length, get-habit-goal-fragment-length,
                                         evaluate-habit-goal-fragment, create-habit-goal-fragments]]
            [clj-time.core :as t]))

(defn get-habit-goal-intervals
  "Creates and evaluates `habit_goal_interval`s for a habit, ranging from `start-date-time` to `end-date-time`, or
  smaller than that range if goals only cover part of the range."
  [sorted-habit-data start-date-time end-date-time habit-type freq-change-records suspended-intervals]
  (let [habit-goal-intervals (create-habit-goal-fragments start-date-time
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
                                                   (or (or (nil? (:end_date %)) (nil? start-date-time))
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

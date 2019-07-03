(ns api.goal-intervals-util-test
  (:require [clojure.test :refer :all]
            [api.db :refer [get-frequency-stats, get-habit-data, get-habits]]
            [api.freq-stats-util :refer [default-frequency-stats, datetime-falls-within-suspended-interval?,
                                         update-freq-stats-with-habit-goal-fragments]]
            [api.goal-intervals-util :refer [get-evaluated-habit-goal-intervals]]
            [api.util :refer [date-to-y-m-d-map]]
            [clj-time.core :as t]))

; Useful variables
(def today (t/today-at 0 0))
(def june-2017 (t/date-time 2017 6))

(defn get-freq-stats-based-on-intervals
  "Computes a `habit_frequency_stats` for `habit` based on `intervals`."
  [habit intervals current-date]
  (let [suspensions (:suspensions habit)]
    (as-> (assoc default-frequency-stats :habit_id (:_id habit)) freq-stats
          (if (seq suspensions)
            ; The habit has been suspended before, check if it's currently suspended
            (assoc freq-stats
                   :currently_suspended (some? (some #(datetime-falls-within-suspended-interval? current-date %) suspensions)))
            ; The habit has never been suspended before, so it's not currently suspended
            freq-stats)
          (-> (update-freq-stats-with-habit-goal-fragments freq-stats
                                                           intervals
                                                           habit)
              (assoc :habit_has_started true)))))

(deftest goal-intervals-test
  (let [all-habits (get-habits {}),
        all-frequency-stats (get-frequency-stats {})
        all-relevant-habits-data (get-habit-data {:after_date (date-to-y-m-d-map june-2017),
                                                  :before_date (date-to-y-m-d-map today)}),
        all-evaluated-intervals (map #(get-evaluated-habit-goal-intervals % all-relevant-habits-data june-2017 today)
                                     all-habits),
        all-freq-stats-based-on-intervals (map #(get-freq-stats-based-on-intervals %
                                                                                   (get-evaluated-habit-goal-intervals %
                                                                                                                       all-relevant-habits-data
                                                                                                                       june-2017
                                                                                                                       today)
                                                                                   today)
                                               all-habits)]
    (testing "Check that freq stats are same whether using goal-intervals-util or not"
      (is (= all-frequency-stats all-freq-stats-based-on-intervals)))))

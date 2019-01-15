(ns api.freq-stats-util
  "A namespace for holding utilities related to calculation of performance statistics."
  (:require [api.dt-util :refer [date-geq?, date-leq?, first-monday-before-datetime, get-consecutive-datetimes,
                                 day-of-week-keyword, days-spanned-between-datetimes]]
            [api.habit-util :refer [get-first-frequency]]
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

(defn get-habit-start-date
  "Returns the `DateTime` on which we consider a habit to have started, given its data and goal.
  If `freq` is based on the calendar week (Monday to Sunday), returns the Monday of the first `habit_day_record`.
  Otherwise just returns the date of the first `habit_day_record`.
  Assumes `sorted-habit-data` is sorted increasingly by date and non-empty."
  [sorted-habit-data freq]
  (let [date-of-first-habit-day-record (:date (first sorted-habit-data))]
    (if (= (:type_name freq) "total_week_frequency")
      (first-monday-before-datetime date-of-first-habit-day-record)
      date-of-first-habit-day-record)))

(defn partition-datetimes-based-on-habit-goal
  "Returns a partition of consecutive DateTimes corresponding to habit goal fragments.
  Boundaries are inclusive, with respect to `from-date` and `until-date`."
  [freq from-date until-date]
  (let [datetimes (get-consecutive-datetimes from-date until-date),
        fragment-length (get-habit-goal-fragment-length freq)]
    (partition-all fragment-length datetimes)))

(defn create-habit-goal-fragment
  "Constructs a habit goal fragment from a list of consecutive DateTimes.
  `datetimes` should be nonempty, be sorted increasingly by date, and correspond to a habit goal fragment.
  Initializes `:total-done` to 0, `:successful` to false, and `:suspended` to false."
  [datetimes]
  {:start-date (first datetimes),
   :end-date (last datetimes),
   :total-done 0,
   :successful false,
   :suspended false})

(defn span-of-habit-goal-fragment
  "Gets the number of days spanned by `habit-goal-fragment`."
  [habit-goal-fragment]
  (days-spanned-between-datetimes (:start-date habit-goal-fragment) (:end-date habit-goal-fragment)))

(defn count-fragment-as-suspended?
  "Returns true iff we should count `habit-goal-fragment` as suspended based on `suspended-interval`.
  Condition 1: the fragment is suspended only if the habit was suspended before (or day of) the fragment started. We shouldn't let the
  user suspend a habit post-start-of-fragment in order to avoid poor performance stats.
  Condition 2: the fragment is suspended only if the habit was resumed after the end of the fragment. If the user wants to
  resume a habit in the middle of a fragment, we should let them count the fragment towards their statistics."
  [habit-goal-fragment suspended-interval]
  (if (nil? (:resume-date suspended-interval))
    ; Never-ending suspended interval
    (date-leq? (:suspend-date suspended-interval) (:start-date habit-goal-fragment))
    (and (date-leq? (:suspend-date suspended-interval)
                    (:start-date habit-goal-fragment))
         (date-geq? (:resume-date suspended-interval)
                    (t/plus (:end-date habit-goal-fragment) (t/days 1))))))

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
  (let [goal-amount (get-habit-goal-amount-for-datetime (:start-date habit-goal-fragment) freq)]
    (assoc habit-goal-fragment
           :successful ((if (= habit-type "good_habit") >= <=)
                        (:total-done habit-goal-fragment)
                        goal-amount))))

(defn evaluate-habit-goal-fragment-suspended
  "Evaluates the `:suspended` field of `habit-goal-fragment` based on `suspended-intervals`."
  [habit-goal-fragment suspended-intervals]
  (assoc habit-goal-fragment
         :suspended (if (some #(count-fragment-as-suspended? habit-goal-fragment %) suspended-intervals)
                      true false)))

(defn evaluate-habit-goal-fragment
  "Evaluates `:total-done`, `:successful`, and `:suspended` fields of a habit goal fragment."
  [habit-goal-fragment habit-data habit-type freq suspended-intervals]
  (let [habit-data-during-fragment (get-habit-data-during-fragment habit-data habit-goal-fragment)]
    (-> habit-goal-fragment
        (evaluate-habit-goal-fragment-total-done habit-data-during-fragment)
        (evaluate-habit-goal-fragment-successful habit-type freq)
        (evaluate-habit-goal-fragment-suspended suspended-intervals))))

(defn get-habit-goal-fragments
  "Creates and evaluates habit goal fragments for a habit based on data from `current-date` or earlier.
  Returns `nil` if `sorted-habit-data` is empty, i.e. the habit has no relevant data."
  [sorted-habit-data current-date habit-type freq suspended-intervals]
  (if-not (empty? sorted-habit-data)
    (let [habit-start-date (get-habit-start-date sorted-habit-data freq)
          partitioned-datetimes (partition-datetimes-based-on-habit-goal freq habit-start-date current-date)
          habit-goal-fragments (map #(create-habit-goal-fragment %) partitioned-datetimes)]
      (map #(evaluate-habit-goal-fragment % sorted-habit-data habit-type freq suspended-intervals) habit-goal-fragments))))

(defn update-freq-stats-with-past-fragment
  "Updates fields of a `habit_frequency_stats` based on a past habit goal fragment."
  [habit-frequency-stats habit-goal-fragment]
  (let [successful (:successful habit-goal-fragment)]
    (if (:suspended habit-goal-fragment)
      (update habit-frequency-stats :total_done + (:total-done habit-goal-fragment))
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
  [freq-stats current-fragment freq habit-type]
  (let [treat-as-successful (and (= habit-type "good_habit") (:successful current-fragment))
        treat-as-failed (and (= habit-type "bad_habit") (not (:successful current-fragment)))]
    (as-> freq-stats $
          (update $ :total_done + (:total-done current-fragment))
          (assoc $ :current_fragment_total (:total-done current-fragment))
          (assoc $ :current_fragment_goal (get-habit-goal-amount-for-datetime (:start-date current-fragment) freq))
          (assoc $ :current_fragment_days_left (- (get-habit-goal-fragment-length freq)
                                                  (span-of-habit-goal-fragment current-fragment)))
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
  "Updates a `habit_frequency_stats` based on a non-empty list of habit goal fragments with goal `freq`.
  Only look at fragments when the habit wasn't suspended."
  [freq-stats habit-goal-fragments habit freq]
  (let [past-fragments (butlast habit-goal-fragments),
        current-fragment (last habit-goal-fragments),
        updated-with-past-fragments-freq-stats (reduce update-freq-stats-with-past-fragment freq-stats past-fragments)]
    (update-freq-stats-with-current-fragment updated-with-past-fragments-freq-stats current-fragment freq (:type_name habit))))

(defn get-suspended-intervals
  "Takes in a list of `suspended_toggle_event`s and returns a list of maps representing intervals during which the habit was suspended."
  [sorted-suspended-toggle-events]
  (let [acc (reduce (fn [acc suspended-toggle-event]
                      (if (:suspended suspended-toggle-event)
                        ; Suspend
                        (if (:unresumed-suspend-date acc)
                          ; Habit was already suspended earlier. Use the existing unresumed-suspend-date, ignore this one.
                          acc
                          ; Habit was not already suspended. Now it is and we can wait for a Resume.
                          (assoc acc :unresumed-suspend-date (:toggle_date suspended-toggle-event)))
                        ; Resume
                        (if (:unresumed-suspend-date acc)
                          ; We can add a new suspend interval, and then clear :unresumed-suspend-date because it's done its work
                          (assoc acc
                                 :suspended-intervals (conj (:suspended-intervals acc)
                                                            {:suspend-date (:unresumed-suspend-date acc),
                                                             :resume-date (:toggle_date suspended-toggle-event)})
                                 :unresumed-suspend-date nil)
                          ; This Resume shouldn't affect any performance statistics.
                          acc)))
                    {:suspended-intervals [], :unresumed-suspend-date nil}
                    sorted-suspended-toggle-events)]
    (if (:unresumed-suspend-date acc)
      ; The last Suspend was never dealt with, we still need to add a never-ending suspended interval
      (conj (:suspended-intervals acc)
            {:suspend-date (:unresumed-suspend-date acc),
             :resume-date nil})
      (:suspended-intervals acc))))

(defn get-freq-stats-for-habit
  "Computes a `habit_frequency_stats` for `habit` based on habit data from `current-date` or earlier.
  `all-habit-data-until-current-date` may include data from other habits.
  Uses `all-suspended-toggle-events-until-current-date` to determine when to exclude dates from performance calculation."
  [habit all-habit-data-until-current-date all-suspended-toggle-events-until-current-date current-date]
  (let [sorted-habit-data (->> all-habit-data-until-current-date
                               (filter #(= (:habit_id %) (:_id habit)))
                               (sort-by :date)),
        sorted-suspended-toggle-events (->> all-suspended-toggle-events-until-current-date
                                            (filter #(= (:habit_id %) (:_id habit)))
                                            (sort-by :toggle_date)),
        suspended-intervals (get-suspended-intervals sorted-suspended-toggle-events),
        first-freq (get-first-frequency habit),
        habit-goal-fragments (get-habit-goal-fragments sorted-habit-data current-date (:type_name habit) first-freq suspended-intervals)]
    (as-> (assoc default-frequency-stats :habit_id (:_id habit)) freq-stats
          (if (seq suspended-intervals)
            (assoc freq-stats :currently_suspended (nil? (:resume-date (last suspended-intervals))))
            freq-stats)
          (if (nil? habit-goal-fragments)
            freq-stats
            (-> (update-freq-stats-with-habit-goal-fragments freq-stats habit-goal-fragments habit first-freq)
                (assoc :habit_has_started true))))))

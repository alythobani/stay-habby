(ns api.db-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [api.db :refer :all]
            [api.util :refer [date-to-y-m-d-map]]
            [api.freq-stats-util :refer [default-frequency-stats, get-freq-stats-based-on-intervals]]
            [api.goal-intervals-util :refer [get-evaluated-habit-goal-intervals]]
            [monger.core :as mg]
            [monger.db :as mdb]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId))



; Useful variables
(def test_conn (mg/connect))
(def test_db (mg/get-db test_conn "test_db"))
(def test_user_id_str (str (ObjectId.)))
(def default_habit {:user_id test_user_id_str
                    :name "test habit" :description "test description" :unit_name_singular "test unit"
                    :unit_name_plural "test units" :time_of_day :ANYTIME})
(def today (t/today-at 0 0))
(def tomorrow (t/plus today (t/days 1)))
(def day-after-tomorrow (t/plus tomorrow (t/days 1)))
(def next-week (t/plus today (t/days 7)))
(def two-weeks-from-today (t/plus today (t/days 14)))
(def five-days-from-today (t/plus today (t/days 5)))
(def ten-days-from-today (t/plus today (t/days 10)))



; Useful functions

(defn add-habit-to-test-db
  "Add a habit to the test database, set the first goal to start today"
  [habit]
  (add-habit {:db test_db :habit habit :frequency-start-datetime today}))

(defn does-add-habit-inputted-habit-match-db-habit
  "Compares a habit map inputted into `add-habit` with a habit map retrieved from the db.
  Returns true iff all fields in `inputted-habit` must have the same value in `db-habit`, with the
  exception of Keyword values in `inputted-habit` since they are converted by Monger into Strings
  when the habit is added to the database, and the exception of `:initial_target_frequency` and
  `:initial_threshold_frequency` fields that are used to generate the `:target_frequencies` and
  `:threshold_frequencies` fields of `db-habit`, and the exception of db ObjectIds.
  Note that this function doesn't care if `db-habit` has a field that `inputted-habit` doesn't have.
  Assumes the habit was added today."
  [inputted-habit db-habit]
  (every? (fn [key]
           (let [clj-val (key inputted-habit)
                 db-val (key db-habit)]
             (condp = key
                    :initial_target_frequency (= (:target_frequencies db-habit)
                                                 [{:start_date today
                                                   :end_date nil
                                                   :new_frequency clj-val}])
                    :initial_threshold_frequency (= (:threshold_frequencies db-habit)
                                                    [{:start_date today
                                                      :end_date nil
                                                      :new_frequency clj-val}])
                    (if (= (type clj-val) clojure.lang.Keyword)
                      ; If `clj-val` is a keyword
                      (= clj-val (keyword db-val))
                      (if (= (type db-val) org.bson.types.ObjectId)
                        ; If `db-val` is an `ObjectId`
                        (= (ObjectId. clj-val) db-val)
                        ; Anything else
                        (= clj-val db-val))))))
          (keys inputted-habit)))

(defn supermap?
  "Returns true iff all fields of `map2` have the same value in `map1`.
  I.e., `map1` is a supermap of `map2`. Patent pending."
  [map1 map2]
  (nil? (first (diff map2 map1))))

(defn get-all-testdb-habits
  "Gets all the test user's habits from the test db."
  []
  (get-habits {:db test_db :user_id test_user_id_str}))

(defn set-testdb-habit-data
  "Sets a habit day record for the test user on the test db."
  [{:keys [habit_id amount date-time]}]
  (set-habit-data {:db test_db :user_id test_user_id_str :habit_id habit_id :amount amount :date-time date-time}))

(defn get-testdb-frequency-stats
  "Gets frequency stats for the test user from the test db."
  [{:keys [habit_ids current-client-date-time]}]
  (as-> {:db test_db :user_id test_user_id_str :habit_ids habit_ids} args
        (if (nil? current-client-date-time) args (assoc args :current-client-date-time current-client-date-time))
        (get-frequency-stats args)))

(defn edit-testdb-habit-suspensions
  "Edits the habit suspensions of the test user's habit on the test db."
  [{:keys [habit_id new_suspensions]}]
  (edit-habit-suspensions {:db test_db
                           :user_id test_user_id_str
                           :habit_id habit_id
                           :new_suspensions new_suspensions}))



; Tests

(deftest add-habit-test
  (testing "No habits"
    (is (= 0 (count (get-all-testdb-habits)))))
  (let [habit_1 (assoc default_habit
                       :type_name "good_habit"
                       :initial_target_frequency {:type_name "total_week_frequency"
                                                  :week 6})
        _ (add-habit-to-test-db habit_1)
        all_habits (get-all-testdb-habits)]
    (testing "One habit"
      (is (= 1 (count all_habits)) "There should be one habit in the db")
      (is (some #(does-add-habit-inputted-habit-match-db-habit habit_1 %) all_habits) "Habit 1 not added properly")
      (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set"))
    (let [habit_2 (assoc default_habit
                         :type_name "bad_habit"
                         :initial_threshold_frequency {:type_name "every_x_days_frequency"
                                                       :days 4
                                                       :times 3})
          _ (add-habit-to-test-db habit_2)
          all_habits (get-all-testdb-habits)]
      (testing "Two habits"
        (is (= 2 (count all_habits)) "There should be two habits in the db")
        (is (some #(does-add-habit-inputted-habit-match-db-habit habit_1 %) all_habits) "Habit 1 not added properly")
        (is (some #(does-add-habit-inputted-habit-match-db-habit habit_2 %) all_habits) "Habit 2 not added properly")
        (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set")))))

(deftest get-frequency-stats-test
  (testing "No habits added yet"
    (is (= 0 (count (get-all-testdb-habits)))))
  (testing "Good habit, specific day of week frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :initial_target_frequency {:type_name "specific_day_of_week_frequency"
                                                  :monday 2 :tuesday 2 :wednesday 2 :thursday 2
                                                  :friday 2 :saturday 2 :sunday 2})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with no habit data"
        (is (= 1 (count (get-all-testdb-habits))) "There should only be one habit so far")
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-testdb-frequency-stats {:habit_ids [habit_id_str]})))
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-testdb-frequency-stats {})) "`habit_ids` should be an optional param"))
      (testing "with a successful habit record today"
        (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 4 :date-time today})
              stats-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                       :current-client-date-time today})]
          (is (= stats-today [{:habit_id habit_id
                               :total_fragments 1 :successful_fragments 1 :total_done 4
                               :current_fragment_streak 1 :best_fragment_streak 1
                               :current_fragment_total 4 :current_fragment_goal 2 :current_fragment_days_left 0
                               :habit_has_started true, :currently_suspended false}])))
        (testing "and a failure habit record tomorrow"
          (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 1 :date-time tomorrow}),
                stats-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                       :current-client-date-time tomorrow}),
                stats-day-after-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                 :current-client-date-time day-after-tomorrow})]
            (is (= stats-tom [{:habit_id habit_id
                               :total_fragments 1 :successful_fragments 1 :total_done 5
                               :current_fragment_streak 1 :best_fragment_streak 1
                               :current_fragment_total 1 :current_fragment_goal 2 :current_fragment_days_left 0
                               :habit_has_started true, :currently_suspended false}]))
            (is (= stats-day-after-tom [{:habit_id habit_id
                                         :total_fragments 2 :successful_fragments 1 :total_done 5
                                         :current_fragment_streak 0 :best_fragment_streak 1
                                         :current_fragment_total 0 :current_fragment_goal 2 :current_fragment_days_left 0
                                         :habit_has_started true, :currently_suspended false}])))
          (testing "and a successful habit record at 11pm the day after tomorrow"
            (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 3 :date-time (t/plus day-after-tomorrow (t/hours 23))})
                  stats-day-after-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                   :current-client-date-time day-after-tomorrow})]
              (is (= stats-day-after-tom [{:habit_id habit_id
                                           :total_fragments 3 :successful_fragments 2 :total_done 8
                                           :current_fragment_streak 1 :best_fragment_streak 1
                                           :current_fragment_total 3 :current_fragment_goal 2 :current_fragment_days_left 0
                                           :habit_has_started true, :currently_suspended false}])))
            (testing "and tomorrow the user suspended the habit"
              (let [_ (edit-testdb-habit-suspensions {:habit_id habit_id_str
                                                      :new_suspensions [{:start_date tomorrow, :end_date nil}]}),
                    stats-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                           :current-client-date-time tomorrow}),
                    stats-day-after-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                     :current-client-date-time day-after-tomorrow})]
                (is (= stats-tom [{:habit_id habit_id
                                   :total_fragments 1 :successful_fragments 1 :total_done 5
                                   :current_fragment_streak 1 :best_fragment_streak 1
                                   :current_fragment_total 1 :current_fragment_goal 2 :current_fragment_days_left 0
                                   :habit_has_started true, :currently_suspended true}]))
                (is (= stats-day-after-tom [{:habit_id habit_id
                                             :total_fragments 1 :successful_fragments 1 :total_done 8
                                             :current_fragment_streak 1 :best_fragment_streak 1
                                             :current_fragment_total 3 :current_fragment_goal 2 :current_fragment_days_left 0
                                             :habit_has_started true, :currently_suspended true}])))
              (testing "and then the user resumed the habit the day after tomorrow"
                (let [_ (edit-testdb-habit-suspensions {:habit_id habit_id_str
                                                        :new_suspensions [{:start_date tomorrow, :end_date tomorrow}]}),
                      stats-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                             :current-client-date-time tomorrow}),
                      stats-day-after-tom (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                       :current-client-date-time day-after-tomorrow})]
                  (is (= stats-tom [{:habit_id habit_id
                                     :total_fragments 1 :successful_fragments 1 :total_done 5
                                     :current_fragment_streak 1 :best_fragment_streak 1
                                     :current_fragment_total 1 :current_fragment_goal 2 :current_fragment_days_left 0
                                     :habit_has_started true, :currently_suspended true}]))
                  (is (= stats-day-after-tom [{:habit_id habit_id
                                               :total_fragments 2 :successful_fragments 2 :total_done 8
                                               :current_fragment_streak 2 :best_fragment_streak 2
                                               :current_fragment_total 3 :current_fragment_goal 2 :current_fragment_days_left 0
                                               :habit_has_started true, :currently_suspended false}]))))))))))
  (testing "Good habit, total week frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :initial_target_frequency {:type_name "total_week_frequency"
                                                  :week 5})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with this week as a failure"
        (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 3 :date-time today})
              stats-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                       :current-client-date-time today})
              stats-next-week (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                           ; 7 days from now is in the next fragment
                                                           :current-client-date-time next-week})]
          (is (supermap? stats-next-week [{:habit_id habit_id
                                           :total_fragments 1 :successful_fragments 0 :total_done 3
                                           :current_fragment_streak 0 :best_fragment_streak 0
                                           :current_fragment_total 0 :current_fragment_goal 5
                                           :habit_has_started true, :currently_suspended false}]))
          (is (supermap? stats-today [{:habit_id habit_id
                                       :total_fragments 0 :successful_fragments 0 :total_done 3
                                       :current_fragment_streak 0 :best_fragment_streak 0
                                       :current_fragment_total 3 :current_fragment_goal 5
                                       :habit_has_started true, :currently_suspended false}])))
        (testing "and next week as a success"
          (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 5 :date-time next-week})
                stats-next-week (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                             :current-client-date-time next-week})]
            (is (supermap? stats-next-week [{:habit_id habit_id
                                             :total_fragments 2 :successful_fragments 1 :total_done 8
                                             :current_fragment_streak 1 :best_fragment_streak 1
                                             :current_fragment_total 5 :current_fragment_goal 5
                                             :habit_has_started true, :currently_suspended false}])))
          (testing "and two weeks from today the user did 6 units"
            (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 6 :date-time two-weeks-from-today})
                  stats-next-week (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                               :current-client-date-time next-week})
                  stats-two-weeks-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                          :current-client-date-time two-weeks-from-today})]
              (is (supermap? stats-two-weeks-from-today [{:habit_id habit_id
                                                          :total_fragments 3 :successful_fragments 2 :total_done 14
                                                          :current_fragment_streak 2 :best_fragment_streak 2
                                                          :current_fragment_total 6 :current_fragment_goal 5
                                                          :habit_has_started true, :currently_suspended false}]))
              (is (supermap? stats-next-week [{:habit_id habit_id
                                               :total_fragments 2 :successful_fragments 1 :total_done 8
                                               :current_fragment_streak 1 :best_fragment_streak 1
                                               :current_fragment_total 5 :current_fragment_goal 5
                                               :habit_has_started true, :currently_suspended false}]))))))))
  (testing "Good habit, every x days frequency"
    (let [habit (assoc default_habit
                       :type_name "good_habit"
                       :initial_target_frequency {:type_name "every_x_days_frequency"
                                                  :times 3 :days 5})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with the current fragment as a success"
        (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 200 :date-time today})
              stats-five-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                      ; 5 days from today is in the next fragment
                                                                      :current-client-date-time five-days-from-today})]
          (is (= stats-five-days-from-today [{:habit_id habit_id
                                              :total_fragments 1 :successful_fragments 1 :total_done 200
                                              :current_fragment_streak 1 :best_fragment_streak 1
                                              :current_fragment_total 0 :current_fragment_goal 3 :current_fragment_days_left 4
                                              :habit_has_started true, :currently_suspended false}])))
        (testing "and the next fragment as a failure"
          (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 2 :date-time five-days-from-today})
                stats-five-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                        :current-client-date-time five-days-from-today})
                stats-ten-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                       ; 10 days from today is 2 fragments away
                                                                       :current-client-date-time ten-days-from-today})]
            (is (= stats-five-days-from-today [{:habit_id habit_id
                                                :total_fragments 1 :successful_fragments 1 :total_done 202
                                                :current_fragment_streak 1 :best_fragment_streak 1
                                                :current_fragment_total 2 :current_fragment_goal 3 :current_fragment_days_left 4
                                                :habit_has_started true, :currently_suspended false}]))
            (is (= stats-ten-days-from-today [{:habit_id habit_id
                                               :total_fragments 2 :successful_fragments 1 :total_done 202
                                               :current_fragment_streak 0 :best_fragment_streak 1
                                               :current_fragment_total 0 :current_fragment_goal 3 :current_fragment_days_left 4
                                               :habit_has_started true, :currently_suspended false}])))
          (testing "and 10 days from today (2 fragments away) as a success"
            (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 4 :date-time ten-days-from-today})
                  stats-ten-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                         :current-client-date-time ten-days-from-today})]
              (is (= stats-ten-days-from-today [{:habit_id habit_id
                                                 :total_fragments 3 :successful_fragments 2 :total_done 206
                                                 :current_fragment_streak 1 :best_fragment_streak 1
                                                 :current_fragment_total 4 :current_fragment_goal 3 :current_fragment_days_left 4
                                                 :habit_has_started true, :currently_suspended false}])))
            (testing "but the user also changed the goal 5 days from today, to only 2 per 5 days"
              (let [_ (edit-habit-goal-frequencies {:db test_db :habit_id habit_id_str :user_id test_user_id_str
                                                    :new_frequencies [{:start_date today
                                                                       :end_date (t/plus today (t/days 4))
                                                                       :new_frequency {:type_name "every_x_days_frequency"
                                                                                       :times 3 :days 5}},
                                                                      {:start_date five-days-from-today
                                                                       :end_date nil
                                                                       :new_frequency {:type_name "every_x_days_frequency"
                                                                                       :times 2 :days 5}}]
                                                    :habit_type "good_habit"})
                    stats-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                             :current-client-date-time today})
                    stats-five-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                            :current-client-date-time five-days-from-today})
                    stats-ten-days-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                           :current-client-date-time ten-days-from-today})]
                (is (= stats-today [{:habit_id habit_id
                                     :total_fragments 1 :successful_fragments 1 :total_done 200
                                     :current_fragment_streak 1 :best_fragment_streak 1
                                     :current_fragment_total 200 :current_fragment_goal 3 :current_fragment_days_left 4
                                     :habit_has_started true, :currently_suspended false}]))
                (is (= stats-five-days-from-today [{:habit_id habit_id
                                                    :total_fragments 2 :successful_fragments 2 :total_done 202
                                                    :current_fragment_streak 2 :best_fragment_streak 2
                                                    :current_fragment_total 2 :current_fragment_goal 2 :current_fragment_days_left 4
                                                    :habit_has_started true, :currently_suspended false}]))
                (is (= stats-ten-days-from-today [{:habit_id habit_id
                                                   :total_fragments 3 :successful_fragments 3 :total_done 206
                                                   :current_fragment_streak 3 :best_fragment_streak 3
                                                   :current_fragment_total 4 :current_fragment_goal 2 :current_fragment_days_left 4
                                                   :habit_has_started true, :currently_suspended false}])))))))))
  (testing "Bad habit, total week frequency"
    (let [habit (assoc default_habit
                       :type_name "bad_habit"
                       :initial_threshold_frequency {:type_name "total_week_frequency"
                                                     :week 20})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with this week as a success"
        (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 0 :date-time today})
              stats-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                       :current-client-date-time today})
              stats-next-week (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                           :current-client-date-time next-week})]
          (is (supermap? stats-today [{:habit_id habit_id
                                       :total_fragments 0 :successful_fragments 0 :total_done 0
                                       :current_fragment_streak 0 :best_fragment_streak 0
                                       :current_fragment_total 0 :current_fragment_goal 20
                                       :habit_has_started true, :currently_suspended false}]))
          (is (supermap? stats-next-week [{:habit_id habit_id
                                           :total_fragments 1 :successful_fragments 1 :total_done 0
                                           :current_fragment_streak 1 :best_fragment_streak 1
                                           :current_fragment_total 0 :current_fragment_goal 20
                                           :habit_has_started true, :currently_suspended false}])))
        (testing "and next week as a success"
          (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 20 :date-time next-week})
                stats-next-week (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                             :current-client-date-time next-week})
                stats-two-weeks-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                        :current-client-date-time two-weeks-from-today})]
            (is (supermap? stats-next-week [{:habit_id habit_id
                                             :total_fragments 1 :successful_fragments 1 :total_done 20
                                             :current_fragment_streak 1 :best_fragment_streak 1
                                             :current_fragment_total 20 :current_fragment_goal 20
                                             :habit_has_started true, :currently_suspended false}]))
            (is (supermap? stats-two-weeks-from-today [{:habit_id habit_id
                                                        :total_fragments 2 :successful_fragments 2 :total_done 20
                                                        :current_fragment_streak 2 :best_fragment_streak 2
                                                        :current_fragment_total 0 :current_fragment_goal 20
                                                        :habit_has_started true, :currently_suspended false}])))
          (testing "and two weeks from today as a failure"
            (let [_ (set-testdb-habit-data {:habit_id habit_id_str :amount 21 :date-time two-weeks-from-today})
                  stats-two-weeks-from-today (get-testdb-frequency-stats {:habit_ids [habit_id_str]
                                                                          :current-client-date-time two-weeks-from-today})]
              (is (supermap? stats-two-weeks-from-today [{:habit_id habit_id
                                                          :total_fragments 3 :successful_fragments 2 :total_done 41
                                                          :current_fragment_streak 0 :best_fragment_streak 2
                                                          :current_fragment_total 21 :current_fragment_goal 20
                                                          :habit_has_started true, :currently_suspended false}]))))))))
  (testing "Check that freq stats are same whether using goal-intervals-util or not"
    (let [all-habits (get-all-testdb-habits),
          all-frequency-stats (get-testdb-frequency-stats {:current-client-date-time two-weeks-from-today}),
          all-relevant-habits-data (get-habit-data {:db test_db,
                                                    :user_id test_user_id_str,
                                                    :before_date (date-to-y-m-d-map two-weeks-from-today)})
          all-freq-stats-based-on-intervals (map #(get-freq-stats-based-on-intervals
                                                      %
                                                      (get-evaluated-habit-goal-intervals
                                                          %
                                                          all-relevant-habits-data
                                                          nil
                                                          two-weeks-from-today)
                                                      two-weeks-from-today)
                                             all-habits)]
      (is (= all-frequency-stats all-freq-stats-based-on-intervals)))))

(defn drop-test-db-fixture
  "Drop test database before and after each test"
  [f]
  (mdb/drop-db test_db)
  (f)
  (mdb/drop-db test_db))

(use-fixtures :each drop-test-db-fixture)

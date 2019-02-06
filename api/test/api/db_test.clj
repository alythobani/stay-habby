(ns api.db-test
  (:require [clojure.test :refer :all]
            [clojure.data :refer [diff]]
            [api.db :refer :all]
            [api.freq-stats-util :refer [default-frequency-stats]]
            [monger.core :as mg]
            [monger.db :as mdb]
            [clj-time.core :as t]))

; Useful variables
(def test_conn (mg/connect))
(def test_db (mg/get-db test_conn "test_db"))
(def default_habit {:name "test habit" :description "test description" :unit_name_singular "test unit"
                    :unit_name_plural "test units" :time_of_day :ANYTIME})
(def today (t/today-at 0 0))
(def tomorrow (t/plus today (t/days 1)))
(def day-after-tomorrow (t/plus tomorrow (t/days 1)))
(def next-week (t/plus today (t/days 7)))
(def two-weeks-from-today (t/plus today (t/days 14)))

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
  `:threshold_frequencies` fields of `db-habit`.
  Note that this function doesn't care if `db-habit` has a field that `inputted-habit` doesn't have.
  Assumes the habit was added today."
  [inputted-habit db-habit]
  (every? (fn [key]
           (let [clj_val (key inputted-habit)
                 db_val (key db-habit)]
             (condp = key
                    :initial_target_frequency (= (:target_frequencies db-habit)
                                                 [{:start_date today
                                                   :end_date nil
                                                   :new_frequency clj_val}])
                    :initial_threshold_frequency (= (:threshold_frequencies db-habit)
                                                    [{:start_date today
                                                      :end_date nil
                                                      :new_frequency clj_val}])
                    (if (= (type clj_val) clojure.lang.Keyword)
                      (= clj_val (keyword db_val))
                      (= clj_val db_val)))))
          (keys inputted-habit)))

(defn supermap?
  "Returns true iff all fields of `map2` have the same value in `map1`.
  I.e., `map1` is a supermap of `map2`. Patent pending."
  [map1 map2]
  (nil? (first (diff map2 map1))))

(deftest add-habit-test
  (testing "No habits"
    (is (= 0 (count (get-habits {:db test_db})))))
  (let [habit_1 (assoc default_habit
                       :type_name "good_habit"
                       :initial_target_frequency {:type_name "total_week_frequency"
                                                  :week 6})
        _ (add-habit-to-test-db habit_1)
        all_habits (get-habits {:db test_db})]
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
          all_habits (get-habits {:db test_db})]
      (testing "Two habits"
        (is (= 2 (count all_habits)) "There should be two habits in the db")
        (is (some #(does-add-habit-inputted-habit-match-db-habit habit_1 %) all_habits) "Habit 1 not added properly")
        (is (some #(does-add-habit-inputted-habit-match-db-habit habit_2 %) all_habits) "Habit 2 not added properly")
        (is (every? #(not (nil? (:_id %))) all_habits) ":_id field not set")))))

(deftest get-frequency-stats-test
  (testing "No habits added yet"
    (is (= 0 (count (get-habits {:db test_db})))))
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
        (is (= 1 (count (get-habits {:db test_db}))) "There should only be one habit so far")
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})))
        (is (= [(assoc default-frequency-stats :habit_id habit_id)]
               (get-frequency-stats {:db test_db})) "`habit_ids` should be an optional param"))
      (testing "with a successful habit record today"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 4
                                 :date-time today})
              stats-today (get-frequency-stats {:db test_db
                                                :habit_ids [habit_id_str]
                                                :current_client_date today})]
          (is (= stats-today [{:habit_id habit_id
                               :total_fragments 1 :successful_fragments 1 :total_done 4
                               :current_fragment_streak 1 :best_fragment_streak 1
                               :current_fragment_total 4 :current_fragment_goal 2 :current_fragment_days_left 0
                               :habit_has_started true, :currently_suspended false}])))
        (testing "and a failure habit record tomorrow"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 1
                                   :date-time tomorrow}),
                stats-tom (get-frequency-stats {:db test_db
                                                :habit_ids [habit_id_str]
                                                :current_client_date tomorrow}),
                stats-day-after-tom (get-frequency-stats {:db test_db
                                                          :habit_ids [habit_id_str]
                                                          :current_client_date day-after-tomorrow})]
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
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 3
                                     :date-time (t/plus day-after-tomorrow (t/hours 23))})
                  stats-day-after-tom (get-frequency-stats {:db test_db
                                                            :habit_ids [habit_id_str]
                                                            :current_client_date day-after-tomorrow})]
              (is (= stats-day-after-tom [{:habit_id habit_id
                                           :total_fragments 3 :successful_fragments 2 :total_done 8
                                           :current_fragment_streak 1 :best_fragment_streak 1
                                           :current_fragment_total 3 :current_fragment_goal 2 :current_fragment_days_left 0
                                           :habit_has_started true, :currently_suspended false}])))
            (testing "and tomorrow the user suspended the habit"
              (let [_ (edit-habit-suspensions {:db test_db :habit_id habit_id_str
                                               :new_suspensions [{:start_date tomorrow, :end_date nil}]}),
                    stats-tom (get-frequency-stats {:db test_db
                                                    :habit_ids [habit_id_str]
                                                    :current_client_date tomorrow}),
                    stats-day-after-tom (get-frequency-stats {:db test_db
                                                              :habit_ids [habit_id_str]
                                                              :current_client_date day-after-tomorrow})]
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
                (let [_ (edit-habit-suspensions {:db test_db :habit_id habit_id_str
                                                 :new_suspensions [{:start_date tomorrow, :end_date tomorrow}]}),
                      stats-tom (get-frequency-stats {:db test_db
                                                      :habit_ids [habit_id_str]
                                                      :current_client_date tomorrow}),
                      stats-day-after-tom (get-frequency-stats {:db test_db
                                                                :habit_ids [habit_id_str]
                                                                :current_client_date day-after-tomorrow})]
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
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 3
                                 :date-time today})
              stats-today (get-frequency-stats {:db test_db
                                                :habit_ids [habit_id_str]
                                                :current_client_date today})
              stats-next-week (get-frequency-stats {:db test_db
                                                    :habit_ids [habit_id_str]
                                                    ; 7 days from now is in the next fragment
                                                    :current_client_date next-week})]
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
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 5
                                   :date-time next-week})
                stats-next-week (get-frequency-stats {:db test_db
                                                      :habit_ids [habit_id_str]
                                                      :current_client_date next-week})]
            (is (supermap? stats-next-week [{:habit_id habit_id
                                             :total_fragments 2 :successful_fragments 1 :total_done 8
                                             :current_fragment_streak 1 :best_fragment_streak 1
                                             :current_fragment_total 5 :current_fragment_goal 5
                                             :habit_has_started true, :currently_suspended false}])))
          (testing "and two weeks from today the user did 6 units"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 6
                                     :date-time two-weeks-from-today})
                  stats-next-week (get-frequency-stats {:db test_db
                                                        :habit_ids [habit_id_str]
                                                        :current_client_date next-week})
                  stats-two-weeks-from-today (get-frequency-stats {:db test_db
                                                                   :habit_ids [habit_id_str]
                                                                   :current_client_date two-weeks-from-today})]
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
                       :target_frequency {:type_name "every_x_days_frequency"
                                          :times 3 :days 5})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with the last fragment as a success"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 200
                                 :date-time (t/minus today (t/days 5))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (supermap? stats [{:habit_id habit_id
                                 :total_fragments 1 :successful_fragments 1 :total_done 200
                                 :current_fragment_streak 1 :best_fragment_streak 1
                                 :current_fragment_total 0 :current_fragment_goal 3
                                 :habit_has_started true, :currently_suspended false}])))
        (testing "and three fragments ago as a failure"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 2
                                   :date-time (t/minus today (t/days 15))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (supermap? stats [{:habit_id habit_id
                                   :total_fragments 3 :successful_fragments 1 :total_done 202
                                   :current_fragment_streak 1 :best_fragment_streak 1
                                   :current_fragment_total 0 :current_fragment_goal 3
                                   :habit_has_started true, :currently_suspended false}])))
          (testing "and today the user did 4 units making the current fragment successful"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 4 :date-time today})
                  stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
              (is (supermap? stats [{:habit_id habit_id
                                     :total_fragments 4 :successful_fragments 2 :total_done 206
                                     :current_fragment_streak 2 :best_fragment_streak 2
                                     :current_fragment_total 4 :current_fragment_goal 3
                                     :habit_has_started true, :currently_suspended false}])))
            (testing "but the user also suspended the habit today"
              (let [_ (toggle-suspended-habit {:db test_db :habit_id habit_id_str :suspended true :toggle-date-time today})
                    stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
                (is (supermap? stats [{:habit_id habit_id
                                       :total_fragments 3 :successful_fragments 1 :total_done 206
                                       :current_fragment_streak 1 :best_fragment_streak 1
                                       :current_fragment_total 4 :current_fragment_goal 3
                                       :habit_has_started true, :currently_suspended true}])))))))))
  (testing "Bad habit, total week frequency"
    (let [habit (assoc default_habit
                       :type_name "bad_habit"
                       :threshold_frequency {:type_name "total_week_frequency"
                                             :week 20})
          final_habit (add-habit-to-test-db habit)
          habit_id (:_id final_habit)
          habit_id_str (str habit_id)]
      (testing "with last week as a success"
        (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 0
                                 :date-time (t/minus today (t/days 7))})
              stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
          (is (supermap? stats [{:habit_id habit_id
                                 :total_fragments 1 :successful_fragments 1 :total_done 0
                                 :current_fragment_streak 1 :best_fragment_streak 1
                                 :current_fragment_total 0 :current_fragment_goal 20
                                 :habit_has_started true, :currently_suspended false}])))
        (testing "and the week before that as a success"
          (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 20
                                   :date-time (t/minus today (t/days 14))})
                stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
            (is (supermap? stats [{:habit_id habit_id
                                   :total_fragments 2 :successful_fragments 2 :total_done 20
                                   :current_fragment_streak 2 :best_fragment_streak 2
                                   :current_fragment_total 0 :current_fragment_goal 20
                                   :habit_has_started true, :currently_suspended false}])))
          (testing "and three weeks ago as a failure"
            (let [_ (set-habit-data {:db test_db :habit_id habit_id_str :amount 21
                                     :date-time (t/minus today (t/days 21))})
                  stats (get-frequency-stats {:db test_db :habit_ids [habit_id_str]})]
              (is (supermap? stats [{:habit_id habit_id
                                     :total_fragments 3 :successful_fragments 2 :total_done 41
                                     :current_fragment_streak 2 :best_fragment_streak 2
                                     :current_fragment_total 0 :current_fragment_goal 20
                                     :habit_has_started true, :currently_suspended false}])))))))))

(defn drop-test-db-fixture
  "Drop test database before and after each test"
  [f]
  (mdb/drop-db test_db)
  (f)
  (mdb/drop-db test_db))

(use-fixtures :each drop-test-db-fixture)

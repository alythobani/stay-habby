(ns api.habit-util
  "A namespace for holding utilities related to habits and their fields.")

(defn get-frequency
  "Returns the target/threshold `frequency` object of a habit."
  [habit]
  (if (= (:type_name habit) "good_habit")
    (:target_frequency habit)
    (:threshold_frequency habit)))

(defn get-first-frequency
  "Returns the first target/threshold `frequency` object of a habit."
  [habit]
  (:new_frequency (first (if (= (:type_name habit) "good_habit")
                           (:target_frequencies habit)
                           (:threshold_frequencies habit)))))

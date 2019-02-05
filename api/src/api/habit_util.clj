(ns api.habit-util
  "A namespace for holding utilities related to habits and their fields.")

(defn get-frequencies
  "Returns the `target_frequencies` or `threshold_frequencies` of a habit."
  [habit]
  (if (= (:type_name habit) "good_habit")
    (:target_frequencies habit)
    (:threshold_frequencies habit)))

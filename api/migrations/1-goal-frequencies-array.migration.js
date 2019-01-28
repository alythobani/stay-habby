/// 1st migration file - allow habits to have multiple goal frequencies over time, stored in
// an array `target_frequencies` for good habits or `threshold_frequencies` for bad habits

db.habits.find().snapshot().forEach(habit => {
  if (habit.type_name === "good_habit") {
    habit.target_frequencies = [{
      frequency_change_date: habit._id.getTimestamp(),
      new_frequency: habit.target_frequency
    }];
  } else {
    habit.threshold_frequencies = [{
      frequency_change_date: habit._id.getTimestamp(),
      new_frequency: habit.threshold_frequency
    }];
  }
  db.habits.save(habit);
});

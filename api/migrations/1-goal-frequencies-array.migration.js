/// 1st migration file - allow habits to have multiple goal frequencies over time, stored in
// an array `target_frequencies` for good habits or `threshold_frequencies` for bad habits

db.habits.find().snapshot().forEach(habit => {
  let frequencyChangeDate = habit._id.getTimestamp();
  frequencyChangeDate.setUTCHours(0);
  frequencyChangeDate.setUTCMinutes(0);
  frequencyChangeDate.setUTCSeconds(0);
  frequencyChangeDate.setUTCMilliseconds(0);
  if (habit.type_name === "good_habit") {
    habit.target_frequencies = [{
      frequency_change_date: frequencyChangeDate,
      new_frequency: habit.target_frequency
    }];
  } else {
    habit.threshold_frequencies = [{
      frequency_change_date: frequencyChangeDate,
      new_frequency: habit.threshold_frequency
    }];
  }
  db.habits.save(habit);
});

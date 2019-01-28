/// 2nd migration file - rename `frequency_change_date` of `frequency_change_record`s to
// `start_date` and add a null `end_date` as well (OK since currently habits only have one goal each).
// Also: for weekly goals (`total_week_frequency` or `specific_day_of_week_frequency`), ensure the
// `start_date` lies on a Monday, else set it to the first Monday after the `frequency_change_date`.

function addDaysToDate(date, numDays) {
  let newDate = new Date(date);
  newDate.setDate(newDate.getDate() + numDays);
  return newDate;
}

function getFirstMondayAfterDate(date) {
  // Note that days in Javascript are numbered 0 for Sunday, ..., 6 for Saturday.
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getDay
  let numDaysToAdd = (8 - date.getDay()) % 7;  // 1 for Sunday, 0 for Monday, 6 for Tuesday, ..., 2 for Saturday
  return addDaysToDate(date, numDaysToAdd);
}

function getFirstFrequencyChangeRecord(habit) {
  if (habit.type_name === "good_habit") {
    return habit.target_frequencies[0];
  } else {
    return habit.threshold_frequencies[0];
  }
}

db.habits.find().snapshot().forEach(habit => {
  let frequencyChangeRecord = getFirstFrequencyChangeRecord(habit);
  let frequency = frequencyChangeRecord.new_frequency;
  let new_start_date = frequencyChangeRecord.frequency_change_date;
  if (frequency.type_name !== "every_x_days_frequency") {
    // the goal is a weekly goal (specific_day_of_week_frequency or total_week_frequency)
    new_start_date = getFirstMondayAfterDate(new_start_date);
  }
  let new_frequencies = [{
    start_date: new_start_date,
    end_date: null,
    new_frequency: frequency
  }];
  if (habit.type_name === "good_habit") {
    habit.target_frequencies = new_frequencies;
  } else {
    habit.threshold_frequencies = new_frequencies;
  }
  db.habits.save(habit);
});

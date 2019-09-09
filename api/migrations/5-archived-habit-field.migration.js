/// 5th migration file - initialize all habits' `archived` fields to `false`.

db.habits.find().snapshot().forEach(habit => {
  habit.archived = false;
  db.habits.save(habit);
});

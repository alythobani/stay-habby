/// 3rd migration file - add a `suspensions` field to each habit, based on their `suspended_toggle_event`s
// in the database.
// Essentially converts a bunch of `suspended_toggle_event`s into `suspended_interval`s.

function addDaysToDate(date, numDays) {
  let newDate = new Date(date);
  newDate.setDate(newDate.getDate() + numDays);
  return newDate;
}

db.habits.find().snapshot().forEach(habit => {
  let suspendedToggleEventsQuery = db.suspended_toggle_events.find({habit_id: habit._id}).sort({toggle_date: 1});
  let newSuspensions = [];

  suspendedToggleEventsQuery.forEach(suspendedToggleEvent => {
    let lastSuspendedInterval = null;
    if (newSuspensions.length !== 0) {
      lastSuspendedInterval = newSuspensions[newSuspensions.length - 1];
    }
    // suspendedToggleEvent has fields `habit_id`, `suspended` (bool), `_id`, and `toggle_date`
    // lastSuspendedInterval has fields `start_date` and `end_date` (nilable).

    if (!lastSuspendedInterval) {
      // array of suspensions is currently empty
      if (suspendedToggleEvent.suspended) {
        // user is trying to suspend the habit
        newSuspensions = [{start_date: suspendedToggleEvent.toggle_date, end_date: null}];
      } else {
        // user is trying to resume the habit. do nothing since habit is by default already active
      }
    } else {
      // we have created other `suspended_interval`s already
      if (suspendedToggleEvent.suspended) {
        // user is trying to suspend the habit
        if (lastSuspendedInterval.end_date) {
          // habit is currently active, let's make another `suspended_interval`
          let newSuspendedInterval = {start_date: suspendedToggleEvent.toggle_date, end_date: null};
          newSuspensions.push(newSuspendedInterval);
        } else {
          // habit is already suspended, nothing to do
        }
      } else {
        // user is trying to resume the habit
        if (lastSuspendedInterval.end_date) {
          // habit is already active, nothing to do
        } else {
          // habit is currently suspended, let's end this `suspended_interval`, yesterday so that the
          // habit can be active starting today
          lastSuspendedInterval.end_date = addDaysToDate(suspendedToggleEvent.toggle_date, -1);
          newSuspensions[newSuspensions.length - 1] = lastSuspendedInterval;
        }
      }
    }

  });

  habit.suspensions = newSuspensions;

  db.habits.save(habit);
});

# Complex Test Case

This is a complex test case with:
- 8 attendees
- 8 sessions (2 required)
- 4 rooms with different capacities and availability
- 2 days of scheduling
- Lunch break blackouts
- Multiple presenters per session
- Overlapping presenter schedules

## Test Purpose
- Verify multi-day scheduling
- Test blackout periods with custom names
- Test multiple presenters (Carol and Eve co-present)
- Test session length variations (60-180 minutes)
- Test priority scheduling (Keynote and Closing Talk have highest priority)
- Test time slot constraints per room

## Expected Behavior
- Keynote and Closing Talk scheduled first (required, high priority)
- Alice's workshops can't overlap (same presenter)
- Bob's workshops can't overlap (same presenter)
- Carol and Eve can't attend other sessions during Panel Discussion
- Lab session needs 3-hour block in Lab room (only available 1pm-6pm)
- Lunch breaks block 12pm-1pm on both days

## Constraints to Test
- Room capacity limits
- Presenter availability
- Time availability per room
- Multi-day distribution
- Required sessions for all attendees
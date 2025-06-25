# Overlap Test

This test case tests the --include-overlaps functionality:
- 5 attendees all want to attend 2 popular sessions
- Sessions have capacity limit of 3
- Both sessions can only run at the same time (10am-11am)

## Expected Behavior with --include-overlaps
- Some attendees will be scheduled for both sessions
- Output should show "(overlap)" marker for conflicted attendees
- All sessions should be scheduled despite conflicts

## Expected Behavior without --include-overlaps
- Scheduler will try to minimize conflicts
- Some attendees won't get into their preferred sessions
- Priority given to avoiding double-booking
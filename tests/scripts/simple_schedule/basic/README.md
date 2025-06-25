# Basic Test Case

This is a simple test case with:
- 5 attendees
- 3 sessions
- 2 rooms
- Single day scheduling
- No conflicts expected

## Test Purpose
- Verify basic scheduling works
- Verify both CSV and MD inputs produce same output
- Test presenter constraints (Alice can't attend Morning Workshop)
- Test room capacity limits

## Expected Behavior
- Morning Workshop: Alice presents, so only Bob, David, and Eve can attend (max 3)
- Afternoon Talk: Bob presents, so Alice, Carol, and Eve attend
- Evening Panel: Carol presents, so Bob and David attend (Carol marked as maybe)
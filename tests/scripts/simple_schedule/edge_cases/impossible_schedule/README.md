# Impossible Schedule Test

This test case is designed to fail with an impossible scheduling scenario:
- 3 required 2-hour sessions
- Only 1 room available for 2 hours total
- All attendees must attend all sessions

## Expected Behavior
The scheduler should fail with a clear error message indicating that required sessions cannot be scheduled due to insufficient time/space.

## Error Expected
"Unable to schedule required session 'Session B' or 'Session C'. Consider relaxing constraints or adding more time/locations."
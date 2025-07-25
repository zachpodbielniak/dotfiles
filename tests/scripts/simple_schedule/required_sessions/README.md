# Required Sessions Test

This test demonstrates how required sessions work in the scheduling system.

## Key Behavior

When a session has `required=true` in the sessions file:
- **ALL attendees are automatically enrolled** regardless of their signup preferences
- The attendees file doesn't need to list required sessions
- This ensures mandatory sessions (like orientations, keynotes, safety briefings) include everyone

## Test Scenario

- **Attendees**: 15 people
- **Required Sessions**: 
  - Opening Keynote (all 15 auto-assigned)
  - DQL Training (all 15 auto-assigned)
  - Closing Ceremony (all 15 auto-assigned)
- **Optional Session**: Optional Workshop (only those who signed up)

## Benefits

1. **Simplifies data entry**: No need to manually add everyone to required sessions
2. **Prevents errors**: Can't accidentally miss someone for mandatory training
3. **Clear distinction**: Required vs optional sessions are handled appropriately
4. **Automatic capacity planning**: Required sessions with `?` will auto-determine instances based on total attendee count

## Example Use Cases

- **Conference**: Opening/closing keynotes that everyone must attend
- **Training**: Mandatory safety or compliance sessions
- **School**: Required core classes vs elective courses
- **Corporate**: All-hands meetings vs optional workshops

## Note

The attendees CSV file only needs to include optional session preferences. Required sessions are automatically handled by the scheduler.
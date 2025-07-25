# Flexible Session Count Test

This test demonstrates the flexible session count feature that allows sessions to have:
- Fixed count: `3` - Exactly 3 sessions
- Range: `2-5` - Between 2 and 5 sessions, scheduler determines optimal count
- Auto-determine: `?` or `0` - Scheduler determines count based on demand

## How it Works

### Auto-Determine Mode (`?` or `0`)

When `number_of_sessions` is set to `?` or `0`, the scheduler will:

1. If `max_attendees > 0`: Calculate sessions needed based on total interested attendees
   - Example: 30 attendees with max_attendees=10 → 3 sessions
   - Stops early if all attendees are scheduled
   - Stops if remaining attendees < 50% of session capacity

2. If `max_attendees = 0` (unlimited): Schedules minimum required (1 session)

### Range Mode (e.g., `2-5`)

When a range is specified:
- Scheduler will schedule at least the minimum (2)
- Will continue up to the maximum (5) based on demand
- Stops if all interested attendees are scheduled
- Stops after 3 consecutive scheduling failures

## Session File Format

```csv
session_name,max_attendees,presenters,length,required,priority,number_of_sessions,max_per_day
Workshop A,10,Alice,60,false,0,?,2
Workshop B,15,Bob,90,false,5,2-4,1
Workshop C,0,Carol,45,false,10,0,3
Fixed Workshop,20,David,60,true,0,3,1
```

## Benefits

1. **Efficiency**: Only schedules as many sessions as needed
2. **Flexibility**: Adapts to actual demand rather than fixed counts
3. **Resource Optimization**: Avoids empty or under-attended sessions
4. **Automatic Scaling**: Handles varying attendance without manual calculation

## Example Scenarios

- **Small Workshop**: 15 attendees, max 5 per session, `?` → Schedules 3 sessions
- **Popular Talk**: 100 attendees, max 30 per session, `2-5` → Schedules 4 sessions
- **Open Discussion**: No limit, `0` → Schedules 1 session
- **Required Training**: Must happen, `1-3` → Schedules based on demand, minimum 1
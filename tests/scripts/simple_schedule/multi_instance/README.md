# Multi-Instance Session Test

This test validates that the simple_schedule script correctly handles multiple instances of the same session across multiple days, ensuring that each attendee is only scheduled for one instance of each session they signed up for.

## Test Scenario

- **Duration**: 3 days
- **Attendees**: 60 people
- **Sessions**: 11 different sessions, with most having 3 instances (one per day)
- **Required Sessions**: 5 sessions that all attendees must attend:
  - Keynote Opening (1 instance)
  - Data Quality Fundamentals (3 instances)
  - Machine Learning Basics (3 instances)
  - Ethics in AI (3 instances)
  - Closing Ceremony (1 instance)
- **Optional Sessions**: 6 sessions with varying attendance
- **Locations**: 7 rooms with varying capacities (10-150 people)

## Key Test Points

1. **No Duplicate Scheduling**: Each attendee should appear at most once across all instances of a session
2. **Required Sessions**: All required sessions should be scheduled with attendees
3. **Capacity Constraints**: Sessions should not exceed room capacity
4. **Fair Distribution**: Attendees should be distributed across the 3 instances of multi-day sessions

## Running the Test

```bash
cd /var/home/zach/.dotfiles/tests/scripts/simple_schedule/multi_instance
./test_multi_instance.py
```

## Expected Behavior

- Each of the 60 attendees attends exactly one instance of each session they signed up for
- The 3 required multi-day sessions (Data Quality, Machine Learning, Ethics) each have their attendees distributed across the 3 days
- No session exceeds the hard capacity limit of its assigned room
- The scheduler optimizes to minimize conflicts while respecting all constraints

## Files

- `sessions.csv`: 11 sessions with varying priorities and instance counts
- `locations.csv`: 7 rooms with different capacities and availability
- `attendees.csv`: 60 attendees, each signed up for 5-7 sessions
- `test_multi_instance.py`: Test script that validates the scheduling results
- `generate_attendees.py`: Script to generate random attendee preferences
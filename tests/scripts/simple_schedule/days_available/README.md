# Days Available Test Case

This test case validates the new `days_available` feature that allows locations to:
1. Specify which days they are available using a CSV string of 0-based day indices
2. Be listed multiple times with different day/time availability

## Test Scenario

- 3-day event
- Main Hall: Available all days (0,1,2)
- Room A: Available days 1 and 3 with different hours
  - Day 1 (index 0): 9am-5pm
  - Day 3 (index 2): 10am-3pm
- Room B: Available days 1 and 2 only (0,1)
- Conference Room: No days_available specified (available all days)

## Expected Behavior

1. Sessions should only be scheduled in locations available on that specific day
2. Room A should have different time availability on different days
3. Locations without days_available should be available all days
4. Output should deduplicate location names in headers

## Command

```bash
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv --days 3 --slice 30m
```
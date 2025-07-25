# Dump Output Test

This test validates the `--dump-output` feature which allows dumping partial schedules when scheduling fails.

## Test Scenario

- **Sessions**: 3 required sessions (2h, 3h, 4h) + 1 optional session (1h)
- **Locations**: 1 small room available for only 2 hours (9am-11am)
- **Expected**: Scheduling will fail because required sessions need 9 hours total but only 2 hours are available

## Purpose

The `--dump-output` flag is useful for debugging scheduling failures by showing:
- What was successfully scheduled before the failure
- Which sessions couldn't be scheduled
- The partial schedule that was achieved

## Running the Test

```bash
# Normal failure (no output generated)
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv

# With dump-output (generates partial schedule)
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --dump-output --output partial_schedule.md --verbose

# With roster output too
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --dump-output --output partial_schedule.csv --output-roster partial_roster.csv --format csv
```

## Expected Behavior

1. Only "Required Session A" gets scheduled (fits in the 2-hour window)
2. "Required Session B" and "Required Session C" fail to schedule
3. With `--dump-output`, partial schedule files are created showing Session A
4. Error messages indicate which required sessions couldn't be scheduled
5. Exit code is still non-zero (failure) even with dump-output
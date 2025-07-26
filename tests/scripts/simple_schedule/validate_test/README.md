# Schedule Validation Test

This test demonstrates the `--validate` feature which checks if an existing schedule meets all constraints.

## Features

The validation checks for:

1. **Session Constraints**:
   - Minimum/maximum number of instances
   - Required sessions include all attendees
   - All interested attendees are scheduled

2. **Capacity Constraints**:
   - Sessions don't exceed room hard limits
   - Warnings for soft limit violations

3. **No Duplicates**:
   - Each attendee appears at most once per session
   - No presenter conflicts (same presenter at same time)

4. **Completeness**:
   - All required sessions are scheduled
   - All attendees assigned to required sessions

## Usage

```bash
# Validate a schedule without roster (basic validation)
simple_schedule --validate \
    --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --schedule generated_schedule.csv --format csv

# Validate with roster (detailed validation including attendee lists)
simple_schedule --validate \
    --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --schedule generated_schedule.csv --roster generated_roster.csv --format csv
```

## Example Output

### Valid Schedule
```
=== SCHEDULE VALIDATION REPORT ===

✓ Schedule is valid! All constraints are satisfied.
```

### Invalid Schedule
```
=== SCHEDULE VALIDATION REPORT ===

ERRORS (2):
  ✗ Required session 'Safety Training' has 0 instances but minimum is 1
  ✗ Session 'Workshop A' on Day 1 has 35 attendees but Room 101 hard limit is 30

WARNINGS (1):
  ⚠ Session 'Optional Lab' has 22 attendees exceeding Room 102 soft limit of 20

=== SUMMARY ===
Total unique sessions: 5
Total session instances: 8
Total unique attendees scheduled: 45
Average sessions per attendee: 3.2
```

## Benefits

1. **Quality Assurance**: Verify schedules before distribution
2. **Constraint Checking**: Ensure all rules are followed
3. **Manual Schedule Verification**: Check hand-edited schedules
4. **Integration Testing**: Validate schedules from other systems
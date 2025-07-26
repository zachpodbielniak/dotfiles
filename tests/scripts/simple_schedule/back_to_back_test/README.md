# Back-to-Back Prevention Test

This test demonstrates the `--no-back-to-back` feature which prevents the same session from being scheduled consecutively.

## Features Tested

### 1. No Back-to-Back Scheduling
- Prevents same session from being scheduled in consecutive time slots
- Works across different locations (not just same room)
- Configurable gap with `--sessions-between` (default: 1)

### 2. Multiple Output Formats
- Use comma-separated formats: `--format md,csv,xlsx`
- Automatically appends extensions when multiple formats specified
- Example: `--output schedule` → `schedule.md`, `schedule.csv`, `schedule.xlsx`

## Usage Examples

```bash
# Basic back-to-back prevention (requires 1 session between)
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --no-back-to-back --output schedule.csv --format csv

# Require 2 sessions between instances of the same session
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --no-back-to-back --sessions-between 2 --output schedule.csv --format csv

# Multiple output formats
simple_schedule --attendees attendees.csv --sessions sessions.csv --locations locations.csv \
    --format md,csv,xlsx --output schedule --output-roster roster
```

## Expected Behavior

With `--no-back-to-back`:
- Workshop A at 9am → Workshop A cannot be at 10am (needs gap)
- Workshop A at 9am → Workshop A can be at 11am (1 session gap)

With `--no-back-to-back --sessions-between 2`:
- Workshop A at 9am → Workshop A cannot be at 10am or 11am
- Workshop A at 9am → Workshop A can be at 12pm (2 session gap)

## Benefits

1. **Better attendee experience**: Prevents fatigue from consecutive same sessions
2. **Flexibility**: Configurable gap between sessions
3. **Cross-location**: Works across all rooms, not just within same room
4. **Multiple outputs**: Generate all needed formats in one run
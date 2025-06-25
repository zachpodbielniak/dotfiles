# Day Names Test

This test case verifies the --day-names feature works correctly.

## Test Purpose
- Verify custom day names are used in output
- Test multi-day scheduling with descriptive day names
- Ensure CSV and MD inputs produce same results with custom day names
- Validate that day names with special characters work correctly

## Configuration
- 3 days with custom names including dates and descriptions
- Day names include colons and spaces to test parsing

## Expected Behavior
- Opening Keynote scheduled on Day 1 (Pre-Conference)
- Workshops distributed across days
- Closing Session on one of the days
- Output headers should show custom day names instead of "Day 1", "Day 2", etc.
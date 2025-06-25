# Simple Schedule Tests

This directory contains comprehensive tests for the `simple_schedule` command.

## Test Structure

```
tests/scripts/simple_schedule/
├── basic/                  # Simple single-day scheduling test
├── complex/               # Multi-day test with constraints
├── edge_cases/           # Edge cases and error conditions
│   ├── impossible_schedule/  # Tests scheduling failure
│   └── overlap_test/        # Tests --include-overlaps flag
└── run_tests.py          # Test runner script
```

## Running Tests

To run all tests:
```bash
cd tests/scripts/simple_schedule
./run_tests.py
```

Or from the project root:
```bash
python tests/scripts/simple_schedule/run_tests.py
```

The test runner will:
1. Run each test with both CSV and Markdown inputs
2. Verify that both formats produce identical outputs
3. Check that error cases fail appropriately
4. Display a summary of passed/failed tests

## Test Cases

### Basic Test
- Simple single-day scheduling
- Tests basic constraints (room capacity, presenter availability)
- Verifies both CSV and MD inputs work correctly

### Complex Test
- Multi-day scheduling with blackout periods
- Multiple presenters per session
- Different room availabilities
- Tests priority scheduling

### Edge Cases

#### Impossible Schedule
- Tests that the scheduler fails gracefully when no valid solution exists
- Verifies error messages are clear and helpful

#### Overlap Test
- Tests the --include-overlaps functionality
- Verifies conflict handling and marking

## Test Format

Each test directory contains:
- `attendees.csv` and `attendees.md` - Matching attendee preference files
- `sessions.csv` and `sessions.md` - Matching session definition files
- `locations.csv` and `locations.md` - Matching location/room files
- `README.md` - Test description and expected behavior
- `test_config.json` (optional) - Additional command-line arguments
- `expected_output.md` (optional) - Expected output for validation

## Adding New Tests

1. Create a new directory under the appropriate category
2. Add matching .csv and .md input files
3. Add a README.md explaining the test
4. Optionally add test_config.json for special arguments
5. Run the test runner to verify

## Requirements

- Python 3.6+
- simple_schedule must be in PATH
- Dependencies installed (pandas, openpyxl optional)
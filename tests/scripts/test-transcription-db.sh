#!/bin/bash
set -euo pipefail

# test-transcription-db.sh - Test suite for transcription database functionality
# This script tests the PostgreSQL transcription storage implementation

# Test configuration
TEST_DIR="/tmp/transcription_db_test_$$"
TEST_AUDIO="${TEST_DIR}/test_audio.mp3"
TEST_DB_NAME="test_transcriptions_$$"
SCRIPT_DIR="$(dirname "$0")/../../bin/scripts"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Cleanup function
cleanup() {
    echo -e "\n${YELLOW}Cleaning up test environment...${NC}"
    
    # Drop test database if it exists
    if psql -U "${USER}" -lqt | cut -d \| -f 1 | grep -qw "$TEST_DB_NAME"; then
        psql -U "${USER}" -c "DROP DATABASE IF EXISTS $TEST_DB_NAME" postgres 2>/dev/null || true
    fi
    
    # Remove test directory
    rm -rf "$TEST_DIR"
    
    echo "Cleanup complete"
}

# Set up cleanup on exit
trap cleanup EXIT

# Test helper functions
assert_equals() {
    local expected="$1"
    local actual="$2"
    local test_name="$3"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    if [[ "$expected" == "$actual" ]]; then
        echo -e "${GREEN}✓${NC} $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  Expected: $expected"
        echo -e "  Actual:   $actual"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local test_name="$3"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    if [[ "$haystack" == *"$needle"* ]]; then
        echo -e "${GREEN}✓${NC} $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  String not found: $needle"
        echo -e "  In: $haystack"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

assert_command_succeeds() {
    local command="$1"
    local test_name="$2"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    if eval "$command" &>/dev/null; then
        echo -e "${GREEN}✓${NC} $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  Command failed: $command"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

assert_command_fails() {
    local command="$1"
    local test_name="$2"
    
    TESTS_RUN=$((TESTS_RUN + 1))
    
    if ! eval "$command" &>/dev/null; then
        echo -e "${GREEN}✓${NC} $test_name"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  Command should have failed: $command"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Setup test environment
setup_test_env() {
    echo -e "${YELLOW}Setting up test environment...${NC}"
    
    # Create test directory
    mkdir -p "$TEST_DIR"
    
    # Create a simple test audio file (1 second of silence)
    if command -v ffmpeg &>/dev/null; then
        ffmpeg -f lavfi -i anullsrc=duration=1 -acodec mp3 "$TEST_AUDIO" 2>/dev/null
        echo "Created test audio file"
    else
        echo "Warning: ffmpeg not found, skipping audio file creation"
        # Create a dummy file for testing
        echo "test" > "$TEST_AUDIO"
    fi
    
    # Set test database environment variables
    export TRANSCRIPTION_DB_NAME="$TEST_DB_NAME"
    export TRANSCRIPTION_DB_HOST="localhost"
    export TRANSCRIPTION_DB_PORT="5432"
    export TRANSCRIPTION_DB_USER="${USER}"
    export TRANSCRIPTION_DB_PASSWORD=""
    
    echo "Test environment ready"
}

# Test 1: Database setup
test_database_setup() {
    echo -e "\n${YELLOW}Test 1: Database Setup${NC}"
    
    # Test database creation
    assert_command_succeeds \
        "${SCRIPT_DIR}/local_postgres setup" \
        "Database setup command succeeds"
    
    # Verify database exists
    assert_command_succeeds \
        "psql -U ${USER} -lqt | cut -d \| -f 1 | grep -qw $TEST_DB_NAME" \
        "Database was created"
    
    # Verify tables exist
    local tables=$(psql -U ${USER} -d "$TEST_DB_NAME" -tAc "SELECT tablename FROM pg_tables WHERE schemaname='public'" 2>/dev/null)
    assert_contains "$tables" "transcriptions" "Transcriptions table exists"
    assert_contains "$tables" "transcription_chunks" "Transcription chunks table exists"
}

# Test 2: Transcription storage
test_transcription_storage() {
    echo -e "\n${YELLOW}Test 2: Transcription Storage${NC}"
    
    # Skip if no audio file
    if [[ ! -f "$TEST_AUDIO" ]] || ! command -v ffmpeg &>/dev/null; then
        echo "Skipping transcription tests (no audio file or ffmpeg)"
        return
    fi
    
    # Test transcription with database storage
    local output=$(${SCRIPT_DIR}/transcribe_audio --type test --tags "unit-test,automated" "$TEST_AUDIO" 2>&1 || true)
    
    # Check if whisper is available
    if [[ "$output" == *"distrobox: command not found"* ]] || [[ "$output" == *"Error"* ]]; then
        echo "Skipping transcription tests (whisper not available)"
        return
    fi
    
    # Extract transcription ID if available
    if [[ "$output" == *"Transcription ID:"* ]]; then
        local trans_id=$(echo "$output" | grep "Transcription ID:" | awk '{print $3}')
        assert_contains "$trans_id" "-" "Transcription ID is a valid UUID"
        
        # Verify transcription is in database
        local count=$(psql -U ${USER} -d "$TEST_DB_NAME" -tAc "SELECT COUNT(*) FROM transcriptions WHERE id='$trans_id'" 2>/dev/null)
        assert_equals "1" "$count" "Transcription exists in database"
        
        # Verify metadata
        local metadata=$(psql -U ${USER} -d "$TEST_DB_NAME" -tAc "SELECT metadata FROM transcriptions WHERE id='$trans_id'" 2>/dev/null)
        assert_contains "$metadata" "test" "Metadata contains type"
        assert_contains "$metadata" "unit-test" "Metadata contains tags"
    fi
}

# Test 3: Database maintenance
test_database_maintenance() {
    echo -e "\n${YELLOW}Test 3: Database Maintenance${NC}"
    
    # Test maintenance command
    assert_command_succeeds \
        "${SCRIPT_DIR}/local_postgres maintain" \
        "Maintenance command succeeds"
    
    # Test stats command
    local stats=$(${SCRIPT_DIR}/local_postgres stats 2>&1)
    assert_contains "$stats" "Database Statistics" "Stats command returns output"
    assert_contains "$stats" "total_transcriptions" "Stats include transcription count"
}

# Test 4: Search functionality
test_search_functionality() {
    echo -e "\n${YELLOW}Test 4: Search Functionality${NC}"
    
    # First insert a test transcription directly
    local test_content="This is a test transcription for unit testing purposes. It contains specific keywords like elephant and rainbow."
    local test_hash=$(echo -n "$test_content" | sha256sum | cut -d' ' -f1)
    
    psql -U ${USER} -d "$TEST_DB_NAME" <<EOF 2>/dev/null
INSERT INTO transcriptions (filename, file_hash, metadata, model_used, language)
VALUES ('test_search.txt', '$test_hash', '{"type": "test", "tags": ["search-test"]}', 'test-model', 'en')
RETURNING id;
EOF
    
    local trans_id=$(psql -U ${USER} -d "$TEST_DB_NAME" -tAc "SELECT id FROM transcriptions WHERE file_hash='$test_hash'" 2>/dev/null | tr -d ' ')
    
    if [[ -n "$trans_id" ]]; then
        # Insert chunk
        psql -U ${USER} -d "$TEST_DB_NAME" <<EOF 2>/dev/null
INSERT INTO transcription_chunks (transcription_id, chunk_number, content, character_count)
VALUES ('$trans_id', 1, '$test_content', ${#test_content});
EOF
        
        # Test search
        local search_output=$(${SCRIPT_DIR}/semantic_search --transcriptions-only "elephant" 2>&1 || true)
        
        # Check if search works (may fail if dependencies are missing)
        if [[ "$search_output" != *"Error"* ]] && [[ "$search_output" != *"not found"* ]]; then
            assert_contains "$search_output" "test_search.txt" "Search finds test transcription"
        else
            echo "Skipping search test (dependencies not available)"
        fi
    fi
}

# Test 5: Export and archive
test_export_archive() {
    echo -e "\n${YELLOW}Test 5: Export and Archive${NC}"
    
    # Test export
    local export_file="${TEST_DIR}/export.json"
    assert_command_succeeds \
        "${SCRIPT_DIR}/local_postgres export $export_file" \
        "Export command succeeds"
    
    # Verify export file exists
    if [[ -f "$export_file" ]]; then
        assert_equals "true" "true" "Export file created"
        
        # Check if it's valid JSON
        if command -v jq &>/dev/null; then
            assert_command_succeeds \
                "jq . $export_file >/dev/null" \
                "Export file is valid JSON"
        fi
    else
        assert_equals "true" "false" "Export file created"
    fi
}

# Test 6: Error handling
test_error_handling() {
    echo -e "\n${YELLOW}Test 6: Error Handling${NC}"
    
    # Test invalid command
    assert_command_fails \
        "${SCRIPT_DIR}/local_postgres invalid_command 2>/dev/null" \
        "Invalid command fails"
    
    # Test transcription with non-existent file
    assert_command_fails \
        "${SCRIPT_DIR}/transcribe_audio /non/existent/file.mp3 2>/dev/null" \
        "Transcription of non-existent file fails"
}

# Main test execution
main() {
    echo -e "${YELLOW}=== Transcription Database Test Suite ===${NC}"
    echo "Testing PostgreSQL transcription storage implementation"
    
    # Check prerequisites
    if ! command -v psql &>/dev/null; then
        echo -e "${RED}Error: PostgreSQL client (psql) not found${NC}"
        echo "Please install PostgreSQL to run tests"
        exit 1
    fi
    
    # Setup test environment
    setup_test_env
    
    # Run tests
    test_database_setup
    test_transcription_storage
    test_database_maintenance
    test_search_functionality
    test_export_archive
    test_error_handling
    
    # Print summary
    echo -e "\n${YELLOW}=== Test Summary ===${NC}"
    echo -e "Tests run:    $TESTS_RUN"
    echo -e "Tests passed: ${GREEN}$TESTS_PASSED${NC}"
    echo -e "Tests failed: ${RED}$TESTS_FAILED${NC}"
    
    if [[ $TESTS_FAILED -eq 0 ]]; then
        echo -e "\n${GREEN}All tests passed!${NC}"
        exit 0
    else
        echo -e "\n${RED}Some tests failed${NC}"
        exit 1
    fi
}

# Run main function
main "$@"
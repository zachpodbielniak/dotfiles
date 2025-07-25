#!/usr/bin/env python3

# dotfiles - Personal configuration files and scripts
# Copyright (C) 2024  Zach Podbielniak
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

"""
Test script to validate that attendees are only scheduled once per session
across multiple instances in a multi-day event.
"""

import csv
import json
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

def run_scheduler():
    """Run the simple_schedule command and generate output files."""
    cmd = [
        'simple_schedule',
        '--attendees', 'attendees.csv',
        '--sessions', 'sessions.csv',
        '--locations', 'locations.csv',
        '--days', '3',
        '--time', '9am-12pm,2pm-5pm',
        '--slice', '30m',
        '--format', 'csv',
        '--output', 'schedule.csv',
        '--output-roster', 'roster.csv',
        '--verbose'
    ]
    
    print(f"Running command: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    if result.returncode != 0:
        print(f"ERROR: Scheduler failed with return code {result.returncode}")
        print(f"STDERR: {result.stderr}")
        sys.exit(1)
    
    print("Scheduler completed successfully")
    print(f"STDOUT: {result.stdout}")
    if result.stderr:
        print(f"STDERR: {result.stderr}")
    
    return result

def parse_roster():
    """Parse the roster CSV to extract session assignments."""
    roster_path = Path('roster.csv')
    if not roster_path.exists():
        print("ERROR: roster.csv not found")
        sys.exit(1)
    
    sessions = []
    with open(roster_path, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            sessions.append({
                'day': row['Day'],
                'session': row['Session'],
                'start_time': row['Start Time'],
                'end_time': row['End Time'],
                'location': row['Location'],
                'presenters': row['Presenters'],
                'attendee_count': int(row['Attendee Count']),
                'attendees': set(a.strip() for a in row['Attendees'].split(';') if a.strip())
            })
    
    return sessions

def validate_no_duplicates(sessions):
    """Validate that no attendee is scheduled for the same session multiple times."""
    # Track attendees per session name
    attendees_by_session = defaultdict(list)
    
    for session_data in sessions:
        session_name = session_data['session']
        for attendee in session_data['attendees']:
            attendees_by_session[session_name].append(attendee)
    
    # Check for duplicates
    errors = []
    for session_name, attendee_list in attendees_by_session.items():
        attendee_counts = defaultdict(int)
        for attendee in attendee_list:
            attendee_counts[attendee] += 1
        
        for attendee, count in attendee_counts.items():
            if count > 1:
                errors.append(f"DUPLICATE: {attendee} scheduled {count} times for {session_name}")
    
    return errors

def validate_required_sessions(sessions):
    """Validate that all required sessions have been scheduled."""
    required_sessions = [
        "Keynote Opening",
        "Data Quality Fundamentals",
        "Machine Learning Basics",
        "Ethics in AI",
        "Closing Ceremony"
    ]
    
    scheduled_sessions = defaultdict(int)
    for session_data in sessions:
        session_name = session_data['session']
        if session_data['attendee_count'] > 0:
            scheduled_sessions[session_name] += 1
    
    errors = []
    for req_session in required_sessions:
        if req_session not in scheduled_sessions:
            errors.append(f"MISSING: Required session '{req_session}' not scheduled")
        elif scheduled_sessions[req_session] == 0:
            errors.append(f"EMPTY: Required session '{req_session}' has no attendees")
    
    return errors

def validate_capacity(sessions):
    """Validate that sessions don't exceed location capacity."""
    # Load locations data
    locations = {}
    with open('locations.csv', 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            locations[row['location_name']] = {
                'soft_limit': int(row['soft_limit']),
                'hard_limit': int(row['hard_limit'])
            }
    
    errors = []
    warnings = []
    for session_data in sessions:
        location = session_data['location']
        count = session_data['attendee_count']
        
        if location in locations:
            if count > locations[location]['hard_limit']:
                errors.append(
                    f"CAPACITY: {session_data['session']} has {count} attendees "
                    f"but {location} hard limit is {locations[location]['hard_limit']}"
                )
            elif count > locations[location]['soft_limit']:
                warnings.append(
                    f"SOFT LIMIT: {session_data['session']} has {count} attendees "
                    f"exceeding {location} soft limit of {locations[location]['soft_limit']}"
                )
    
    return errors, warnings

def generate_summary(sessions):
    """Generate a summary of the scheduling results."""
    # Count instances per session
    session_instances = defaultdict(int)
    total_attendees_by_session = defaultdict(set)
    
    for session_data in sessions:
        session_name = session_data['session']
        session_instances[session_name] += 1
        total_attendees_by_session[session_name].update(session_data['attendees'])
    
    print("\n=== SCHEDULING SUMMARY ===")
    print(f"Total scheduled sessions: {len(sessions)}")
    print("\nSession breakdown:")
    
    for session_name in sorted(session_instances.keys()):
        instances = session_instances[session_name]
        unique_attendees = len(total_attendees_by_session[session_name])
        print(f"  {session_name}: {instances} instance(s), {unique_attendees} unique attendees")

def main():
    """Main test function."""
    print("=== Multi-Instance Session Test ===")
    print("Testing that attendees are only scheduled once per session\n")
    
    # Run the scheduler
    run_scheduler()
    
    # Parse the roster
    print("\nParsing roster...")
    sessions = parse_roster()
    print(f"Found {len(sessions)} scheduled sessions")
    
    # Validate no duplicates
    print("\nValidating no duplicate attendees per session...")
    duplicate_errors = validate_no_duplicates(sessions)
    
    if duplicate_errors:
        print(f"FAILED: Found {len(duplicate_errors)} duplicate scheduling errors:")
        for error in duplicate_errors:
            print(f"  - {error}")
    else:
        print("PASSED: No attendees scheduled multiple times for the same session")
    
    # Validate required sessions
    print("\nValidating required sessions...")
    required_errors = validate_required_sessions(sessions)
    
    if required_errors:
        print(f"FAILED: Found {len(required_errors)} required session errors:")
        for error in required_errors:
            print(f"  - {error}")
    else:
        print("PASSED: All required sessions scheduled with attendees")
    
    # Validate capacity
    print("\nValidating capacity constraints...")
    capacity_errors, capacity_warnings = validate_capacity(sessions)
    
    if capacity_errors:
        print(f"FAILED: Found {len(capacity_errors)} capacity violations:")
        for error in capacity_errors:
            print(f"  - {error}")
    else:
        print("PASSED: No hard capacity limits exceeded")
    
    if capacity_warnings:
        print(f"WARNING: Found {len(capacity_warnings)} soft limit warnings:")
        for warning in capacity_warnings:
            print(f"  - {warning}")
    
    # Generate summary
    generate_summary(sessions)
    
    # Overall result
    print("\n=== TEST RESULT ===")
    total_errors = len(duplicate_errors) + len(required_errors) + len(capacity_errors)
    if total_errors == 0:
        print("ALL TESTS PASSED ✓")
        print("Attendees are correctly scheduled at most once per session")
        return 0
    else:
        print(f"TESTS FAILED: {total_errors} total errors")
        return 1

if __name__ == '__main__':
    sys.exit(main())
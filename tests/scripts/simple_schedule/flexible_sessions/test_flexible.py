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

"""Test flexible session count feature"""

import csv
import subprocess
import sys

# Create test data
sessions_data = [
    ['session_name', 'max_attendees', 'presenters', 'length', 'required', 'priority', 'number_of_sessions', 'max_per_day'],
    ['Auto Small', '5', 'Alice', '60', 'false', '0', '?', '2'],  # Should schedule 3 sessions for 15 people
    ['Auto Large', '0', 'Bob', '30', 'false', '5', '0', '3'],   # Should schedule 1 session (no limit)
    ['Range Session', '10', 'Carol', '45', 'false', '10', '2-5', '2'],  # Should schedule 2-3 sessions for 25 people
    ['Fixed Session', '20', 'David', '60', 'false', '15', '1', '1'],   # Exactly 1 session
]

attendees_data = [['attendee_name', 'Auto Small', 'Auto Large', 'Range Session', 'Fixed Session']]
# 15 people for Auto Small (needs 3 sessions with max 5)
for i in range(1, 16):
    attendees_data.append([f'Person{i:02d}', '1', '', '', ''])

# 10 more for Auto Large
for i in range(16, 26):
    attendees_data.append([f'Person{i:02d}', '', '1', '', ''])

# 25 for Range Session (needs 3 sessions with max 10)
for i in range(26, 51):
    attendees_data.append([f'Person{i:02d}', '', '', '1', ''])

# 15 for Fixed Session
for i in range(51, 66):
    attendees_data.append([f'Person{i:02d}', '', '', '', '1'])

locations_data = [
    ['location_name', 'soft_limit', 'hard_limit', 'time_available'],
    ['Room A', '20', '30', '9am-12pm,2pm-5pm'],
    ['Room B', '15', '20', '9am-12pm,2pm-5pm'],
]

# Write CSV files
with open('test_sessions.csv', 'w', newline='') as f:
    csv.writer(f).writerows(sessions_data)

with open('test_attendees.csv', 'w', newline='') as f:
    csv.writer(f).writerows(attendees_data)

with open('test_locations.csv', 'w', newline='') as f:
    csv.writer(f).writerows(locations_data)

# Run scheduler
print("Running scheduler with flexible session counts...")
result = subprocess.run([
    'simple_schedule',
    '--attendees', 'test_attendees.csv',
    '--sessions', 'test_sessions.csv', 
    '--locations', 'test_locations.csv',
    '--days', '2',
    '--output-roster', 'test_roster.csv',
    '--format', 'csv',
    '--verbose'
], capture_output=True, text=True)

if result.returncode != 0:
    print(f"Scheduler failed: {result.stderr}")
    sys.exit(1)

print("Scheduler completed successfully!")
print(result.stderr)

# Analyze results
print("\n=== Analyzing Results ===")
with open('test_roster.csv', 'r') as f:
    reader = csv.DictReader(f)
    sessions_count = {}
    for row in reader:
        session_name = row['Session']
        if session_name not in sessions_count:
            sessions_count[session_name] = {'instances': 0, 'total_attendees': set()}
        sessions_count[session_name]['instances'] += 1
        attendees = [a.strip() for a in row['Attendees'].split(';') if a.strip()]
        sessions_count[session_name]['total_attendees'].update(attendees)

print("\nSession scheduling results:")
for session, data in sorted(sessions_count.items()):
    instances = data['instances']
    total = len(data['total_attendees'])
    print(f"  {session}: {instances} instance(s), {total} total attendees")

# Validate expectations
print("\nValidation:")
if sessions_count.get('Auto Small', {}).get('instances', 0) == 3:
    print("✓ Auto Small correctly scheduled 3 sessions (15 people / 5 max)")
else:
    print("✗ Auto Small did not schedule expected 3 sessions")

if sessions_count.get('Auto Large', {}).get('instances', 0) == 1:
    print("✓ Auto Large correctly scheduled 1 session (no attendee limit)")
else:
    print("✗ Auto Large did not schedule expected 1 session")

range_instances = sessions_count.get('Range Session', {}).get('instances', 0)
if 2 <= range_instances <= 5:
    print(f"✓ Range Session scheduled {range_instances} sessions (within 2-5 range)")
else:
    print(f"✗ Range Session scheduled {range_instances} sessions (outside 2-5 range)")

if sessions_count.get('Fixed Session', {}).get('instances', 0) == 1:
    print("✓ Fixed Session correctly scheduled exactly 1 session")
else:
    print("✗ Fixed Session did not schedule expected 1 session")
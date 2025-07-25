#!/usr/bin/env python3

import csv
from collections import defaultdict

# Read roster
sessions = []
with open('roster.csv', 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        attendees = [a.strip() for a in row['Attendees'].split(';') if a.strip()]
        sessions.append({
            'day': row['Day'],
            'session': row['Session'],
            'start_time': row['Start Time'],
            'end_time': row['End Time'],
            'attendees': attendees,
            'count': int(row['Attendee Count'])
        })

# Track which sessions each attendee has been scheduled for
attendee_sessions = defaultdict(list)
session_attendees = defaultdict(set)

for session in sessions:
    for attendee in session['attendees']:
        attendee_sessions[attendee].append(f"{session['session']} ({session['day']})")
        session_attendees[session['session']].add(attendee)

# Check a few specific attendees
print("=== Sample Attendee Analysis ===")
for attendee_id in ['Attendee_001', 'Attendee_010', 'Attendee_030', 'Attendee_050']:
    print(f"\n{attendee_id}:")
    sessions_by_name = defaultdict(list)
    for session_str in attendee_sessions[attendee_id]:
        session_name = session_str.split(' (')[0]
        sessions_by_name[session_name].append(session_str)
    
    for session_name, instances in sorted(sessions_by_name.items()):
        if len(instances) > 1:
            print(f"  ❌ {session_name}: {len(instances)} times - {instances}")
        else:
            print(f"  ✓ {session_name}: {instances[0]}")

# Analyze session distribution
print("\n=== Session Distribution Analysis ===")
required_sessions = ['Data Quality Fundamentals', 'Machine Learning Basics', 'Ethics in AI']

for session_name in required_sessions:
    print(f"\n{session_name}:")
    instances = [s for s in sessions if s['session'] == session_name]
    total_unique = len(session_attendees[session_name])
    print(f"  Total unique attendees: {total_unique}")
    
    for instance in instances:
        print(f"  {instance['day']}: {instance['count']} attendees")
    
    # Check for any attendees in multiple instances
    attendee_instance_count = defaultdict(int)
    for instance in instances:
        for attendee in instance['attendees']:
            attendee_instance_count[attendee] += 1
    
    duplicates = [a for a, count in attendee_instance_count.items() if count > 1]
    if duplicates:
        print(f"  ❌ DUPLICATES FOUND: {duplicates}")
    else:
        print(f"  ✓ No duplicates - each attendee scheduled exactly once")

# Check that all 60 attendees got the required sessions
print("\n=== Required Session Coverage ===")
all_attendees = set(f"Attendee_{i+1:03d}" for i in range(60))

for session_name in ['Keynote Opening'] + required_sessions + ['Closing Ceremony']:
    scheduled = session_attendees[session_name]
    missing = all_attendees - scheduled
    if missing:
        print(f"❌ {session_name}: {len(missing)} attendees missing - {sorted(list(missing))[:5]}...")
    else:
        print(f"✓ {session_name}: All 60 attendees scheduled")

# Check for time conflicts for specific attendees
print("\n=== Time Conflict Check (Sample) ===")
for attendee_id in ['Attendee_010', 'Attendee_025', 'Attendee_040']:
    print(f"\n{attendee_id} schedule:")
    attendee_schedule = []
    
    for session in sessions:
        if attendee_id in session['attendees']:
            attendee_schedule.append({
                'day': session['day'],
                'session': session['session'],
                'start': session['start_time'],
                'end': session['end_time']
            })
    
    # Sort by day and time
    attendee_schedule.sort(key=lambda x: (x['day'], x['start']))
    
    for item in attendee_schedule:
        print(f"  {item['day']}, {item['start']}-{item['end']}: {item['session']}")
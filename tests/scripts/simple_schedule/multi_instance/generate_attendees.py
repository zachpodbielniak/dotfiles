#!/usr/bin/env python3

import random
import csv

# Session names
required_sessions = [
    "Data Quality Fundamentals",
    "Machine Learning Basics", 
    "Ethics in AI"
]

optional_sessions = [
    "Python Workshop",
    "Cloud Computing",
    "Database Design",
    "Web Development",
    "Security Essentials",
    "Networking Fundamentals"
]

# Special sessions everyone attends
special_sessions = ["Keynote Opening", "Closing Ceremony"]

# All sessions for header
all_sessions = special_sessions + required_sessions + optional_sessions

# Generate 60 attendees
attendees = []
for i in range(60):
    attendee = {
        'attendee_name': f'Attendee_{i+1:03d}'
    }
    
    # Everyone attends keynote and closing
    for session in special_sessions:
        attendee[session] = '1'
    
    # Everyone must attend the 3 required sessions
    for session in required_sessions:
        attendee[session] = 'yes'
    
    # Each attendee randomly picks 0-2 additional optional sessions
    num_optional = random.randint(0, 2)
    selected_optional = random.sample(optional_sessions, num_optional)
    
    for session in optional_sessions:
        if session in selected_optional:
            # Mix of definite and maybe interest
            attendee[session] = random.choice(['1', 'yes', 'maybe', '?'])
        else:
            attendee[session] = ''
    
    attendees.append(attendee)

# Write CSV file
with open('attendees.csv', 'w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=['attendee_name'] + all_sessions)
    writer.writeheader()
    writer.writerows(attendees)

print(f"Generated attendees.csv with {len(attendees)} attendees")
print(f"Each attendee signed up for {len(required_sessions) + len(special_sessions)} required sessions")
print(f"Plus 0-2 optional sessions from a pool of {len(optional_sessions)}")
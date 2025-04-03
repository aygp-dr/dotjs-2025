#!/usr/bin/env python3
"""
dotJS 2025 Note-Taking Utilities

This script provides utilities to help with managing notes and resources
for the dotJS 2025 conference.

Features:
- Create initial repository structure
- Generate template files for each talk
- Extract speaker information from conference data
- Sync notes with git repository
"""

import os
import sys
import argparse
import subprocess
import json
from datetime import datetime
from pathlib import Path

# Conference data
CONFERENCE_DATA = {
    "name": "dotJS 2025",
    "date": "April 3, 2025",
    "venue": "Folies Bergère, 32 rue Richer, 75009 Paris",
    "talks": {
        "featured": [
            {"speaker": "Ryan Dahl", "title": "Special Announcement", "duration": 20},
            {"speaker": "Wes Bos", "title": "Running AI models in JavaScript - good idea?", "duration": 20},
            {"speaker": "Sarah Drasner", "title": "The Wind and the Waves: The formation of Framework Waves from the Epicenter", "duration": 20},
            {"speaker": "Kyle Simpson", "title": "Love/Hate: Upgrading to Web2.5 with Local-First", "duration": 20},
            {"speaker": "Angie Jones", "title": "Modern Day Mashups: How AI Agents are Reviving the Programmable Web", "duration": 20},
            {"speaker": "Matteo Collina", "title": "Node.js will use all the memory available, and that's OK!", "duration": 20},
            {"speaker": "Yohan Lasorsa", "title": "Prompting is the New Scripting: Meet GenAIScript", "duration": 20},
            {"speaker": "Eduardo San Martin Morote", "title": "From terrible to terrific frontend routers", "duration": 20}
        ],
        "lightning": [
            {"speaker": "Antoine Caron", "title": "Supercharge Web Performance with Shared Dictionaries: The Next Frontier in HTTP Compression", "duration": 10},
            {"speaker": "Joyce Lin", "title": "Code in the Physical World", "duration": 10},
            {"speaker": "Charly Poly", "title": "Durable Executions for Mortals", "duration": 10},
            {"speaker": "Abbey Perini", "title": "Coding and ADHD: Where We Excel", "duration": 10},
            {"speaker": "Vadim Smirnov", "title": "Recreating Windows Media Player Art With Web MIDI API", "duration": 10}
        ]
    },
    "schedule": {
        "doors_open": "9:00 am",
        "morning_talks": "9:30 am - 12:00 pm",
        "lunch": "12:00 pm - 1:45 pm",
        "afternoon_talks": "1:45 pm - 6:00 pm",
        "happy_hour": "6:00 pm - 8:00 pm"
    }
}

def create_directory_structure():
    """Create the initial directory structure for the repository"""
    directories = [
        "talks/morning-sessions",
        "talks/afternoon-sessions",
        "resources/slides",
        "resources/references",
        "code-examples/demos",
        "code-examples/experiments",
        "notes/keynotes",
        "notes/lightning-talks",
        "notes/qa-sessions",
        "diagrams"
    ]
    
    for directory in directories:
        os.makedirs(directory, exist_ok=True)
        print(f"Created directory: {directory}")
    
    print("Directory structure created successfully!")
    return True

def slugify(text):
    """Convert text to a filename-friendly format"""
    text = text.lower()
    text = text.replace(" ", "-")
    # Remove special characters
    return ''.join(c for c in text if c.isalnum() or c == '-')

def generate_talk_template(speaker, title, duration, output_dir):
    """Generate a template org file for a talk"""
    slug = slugify(f"{speaker}-{title}")
    filename = os.path.join(output_dir, f"{slug}.org")
    
    # Determine the session type based on duration
    session_type = "keynote" if duration >= 20 else "lightning"
    
    with open(filename, 'w') as f:
        f.write(f"""#+TITLE: {speaker} - {title}
#+DATE: {CONFERENCE_DATA['date']}
#+CATEGORY: dotJS2025
#+PROPERTY: header-args :mkdirp yes
#+PROPERTY: header-args:js :tangle ../code-examples/demos/{slug}.js

* Overview
[Brief summary of the talk]

* Key Points
- Point 1
- Point 2
- Point 3

* Code Examples
#+BEGIN_SRC javascript
// Code example from the talk
console.log("Example code from {speaker}'s talk on {title}");
#+END_SRC

* Diagrams
#+BEGIN_SRC mermaid :file ../diagrams/{slug}-diagram.svg
graph TD
    A[Concept A] --> B[Concept B]
    B --> C[Concept C]
    C --> D[Implementation]
#+END_SRC

* Resources
- [Link to slides]
- [Link to speaker's GitHub/website]
- [Other relevant resources]

* Questions & Answers
- Q: [Question asked during Q&A]
- A: [Speaker's answer]

* Personal Notes
- [Your thoughts and insights]
- [Implementation ideas]
- [Follow-up topics to research]
""")
    
    print(f"Created template for {speaker}'s talk: {filename}")
    return filename

def generate_all_talk_templates():
    """Generate templates for all talks from the conference data"""
    morning_dir = "talks/morning-sessions"
    afternoon_dir = "talks/afternoon-sessions"
    lightning_dir = "notes/lightning-talks"
    
    # Ensure directories exist
    for d in [morning_dir, afternoon_dir, lightning_dir]:
        os.makedirs(d, exist_ok=True)
    
    # Generate templates for featured talks (first half morning, second half afternoon)
    featured_talks = CONFERENCE_DATA['talks']['featured']
    mid_point = len(featured_talks) // 2
    
    morning_talks = featured_talks[:mid_point]
    afternoon_talks = featured_talks[mid_point:]
    
    for talk in morning_talks:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], morning_dir)
    
    for talk in afternoon_talks:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], afternoon_dir)
    
    # Generate templates for lightning talks
    for talk in CONFERENCE_DATA['talks']['lightning']:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], lightning_dir)
    
    print(f"Generated templates for {len(featured_talks) + len(CONFERENCE_DATA['talks']['lightning'])} talks")
    return True

def init_git_repo():
    """Initialize a git repository for the project"""
    try:
        result = subprocess.run(["git", "init"], capture_output=True, text=True, check=True)
        print(result.stdout)
        
        # Create .gitignore
        with open(".gitignore", "w") as f:
            f.write("""# Org-mode
*.org~
*.org#
.org-id-locations
*_archive

# Compiled
*.elc

# Generated files
diagrams/*.svg
diagrams/*.png

# Emacs backup files
*~
\#*\#
.\#*

# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
env/
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
*.egg-info/
.installed.cfg
*.egg

# OS specific
.DS_Store
Thumbs.db
""")
        
        # Add README.org
        with open("README.org", "w") as f:
            f.write(f"""#+TITLE: {CONFERENCE_DATA['name']} Notes
#+AUTHOR: Your Name
#+DATE: {CONFERENCE_DATA['date']}

* {CONFERENCE_DATA['name']} Conference Notes

This repository contains my personal notes, resources, and code examples from the {CONFERENCE_DATA['name']} conference held on {CONFERENCE_DATA['date']} at {CONFERENCE_DATA['venue']}.

** Directory Structure

- talks/ - Notes from conference talks
- resources/ - Slides, links, and other resources
- code-examples/ - Code snippets and demos from the talks
- notes/ - Additional personal notes
- diagrams/ - Visual representations of concepts

** Getting Started

1. Clone this repository
2. Explore the talk notes in the talks/ directory
3. Check out code examples in the code-examples/ directory
4. View the generated diagrams in the diagrams/ directory

** Conference Highlights

[To be filled after the conference]
""")
        
        # Add initial commit
        subprocess.run(["git", "add", "."], check=True)
        subprocess.run(["git", "commit", "-m", "Initial repository setup for dotJS 2025"], check=True)
        
        print("Git repository initialized with initial commit")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error initializing git repository: {e}")
        return False

def export_speaker_data():
    """Export speaker data to JSON for easier access"""
    speakers = []
    
    # Extract speaker data from featured talks
    for talk in CONFERENCE_DATA['talks']['featured']:
        speakers.append({
            "name": talk['speaker'],
            "talk": talk['title'],
            "duration": talk['duration'],
            "type": "featured"
        })
    
    # Extract speaker data from lightning talks
    for talk in CONFERENCE_DATA['talks']['lightning']:
        speakers.append({
            "name": talk['speaker'],
            "talk": talk['title'],
            "duration": talk['duration'],
            "type": "lightning"
        })
    
    # Save to JSON file
    with open("resources/speakers.json", "w") as f:
        json.dump(speakers, f, indent=2)
    
    print(f"Exported data for {len(speakers)} speakers to resources/speakers.json")
    return True

def track_new_note(topic, content):
    """Create a new note with timestamp for quick thoughts during the conference"""
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    notes_dir = "notes/quick-notes"
    os.makedirs(notes_dir, exist_ok=True)
    
    filename = os.path.join(notes_dir, f"{slugify(topic)}.org")
    
    # Append to existing file or create new one
    mode = 'a' if os.path.exists(filename) else 'w'
    with open(filename, mode) as f:
        if mode == 'w':
            f.write(f"""#+TITLE: Quick Notes: {topic}
#+DATE: {CONFERENCE_DATA['date']}
#+CATEGORY: dotJS2025

* Quick Notes on {topic}

""")
        
        f.write(f"** Note added at {timestamp}\n{content}\n\n")
    
    print(f"Added note about '{topic}' to {filename}")
    return filename

def main():
    parser = argparse.ArgumentParser(description='dotJS 2025 Note-Taking Utilities')
    subparsers = parser.add_subparsers(dest='command', help='Command to run')
    
    # Initialize repository
    init_parser = subparsers.add_parser('init', help='Initialize repository structure')
    
    # Generate talk templates
    templates_parser = subparsers.add_parser('templates', help='Generate talk templates')
    
    # Track a quick note
    note_parser = subparsers.add_parser('note', help='Add a quick note')
    note_parser.add_argument('topic', help='Note topic')
    note_parser.add_argument('content', help='Note content')
    
    # Export speaker data
    export_parser = subparsers.add_parser('export', help='Export speaker data to JSON')
    
    # Git repository
    git_parser = subparsers.add_parser('git', help='Initialize git repository')
    
    args = parser.parse_args()
    
    if args.command == 'init':
        create_directory_structure()
    elif args.command == 'templates':
        generate_all_talk_templates()
    elif args.command == 'note':
        track_new_note(args.topic, args.content)
    elif args.command == 'export':
        export_speaker_data()
    elif args.command == 'git':
        init_git_repo()
    else:
        parser.print_help()

if __name__ == "__main__":
    main()#!/usr/bin/env python3
"""
dotJS 2025 Note-Taking Utilities

This script provides utilities to help with managing notes and resources
for the dotJS 2025 conference.

Features:
- Create initial repository structure
- Generate template files for each talk
- Extract speaker information from conference data
- Sync notes with git repository
"""

import os
import sys
import argparse
import subprocess
import json
from datetime import datetime
from pathlib import Path

# Conference data
CONFERENCE_DATA = {
    "name": "dotJS 2025",
    "date": "April 3, 2025",
    "venue": "Folies Bergère, 32 rue Richer, 75009 Paris",
    "talks": {
        "featured": [
            {"speaker": "Ryan Dahl", "title": "Special Announcement", "duration": 20},
            {"speaker": "Wes Bos", "title": "Running AI models in JavaScript - good idea?", "duration": 20},
            {"speaker": "Sarah Drasner", "title": "The Wind and the Waves: The formation of Framework Waves from the Epicenter", "duration": 20},
            {"speaker": "Kyle Simpson", "title": "Love/Hate: Upgrading to Web2.5 with Local-First", "duration": 20},
            {"speaker": "Angie Jones", "title": "Modern Day Mashups: How AI Agents are Reviving the Programmable Web", "duration": 20},
            {"speaker": "Matteo Collina", "title": "Node.js will use all the memory available, and that's OK!", "duration": 20},
            {"speaker": "Yohan Lasorsa", "title": "Prompting is the New Scripting: Meet GenAIScript", "duration": 20},
            {"speaker": "Eduardo San Martin Morote", "title": "From terrible to terrific frontend routers", "duration": 20}
        ],
        "lightning": [
            {"speaker": "Antoine Caron", "title": "Supercharge Web Performance with Shared Dictionaries: The Next Frontier in HTTP Compression", "duration": 10},
            {"speaker": "Joyce Lin", "title": "Code in the Physical World", "duration": 10},
            {"speaker": "Charly Poly", "title": "Durable Executions for Mortals", "duration": 10},
            {"speaker": "Abbey Perini", "title": "Coding and ADHD: Where We Excel", "duration": 10},
            {"speaker": "Vadim Smirnov", "title": "Recreating Windows Media Player Art With Web MIDI API", "duration": 10}
        ]
    },
    "schedule": {
        "doors_open": "9:00 am",
        "morning_talks": "9:30 am - 12:00 pm",
        "lunch": "12:00 pm - 1:45 pm",
        "afternoon_talks": "1:45 pm - 6:00 pm",
        "happy_hour": "6:00 pm - 8:00 pm"
    }
}

def create_directory_structure():
    """Create the initial directory structure for the repository"""
    directories = [
        "talks/morning-sessions",
        "talks/afternoon-sessions",
        "resources/slides",
        "resources/references",
        "code-examples/demos",
        "code-examples/experiments",
        "notes/keynotes",
        "notes/lightning-talks",
        "notes/qa-sessions",
        "diagrams"
    ]
    
    for directory in directories:
        os.makedirs(directory, exist_ok=True)
        print(f"Created directory: {directory}")
    
    print("Directory structure created successfully!")
    return True

def slugify(text):
    """Convert text to a filename-friendly format"""
    text = text.lower()
    text = text.replace(" ", "-")
    # Remove special characters
    return ''.join(c for c in text if c.isalnum() or c == '-')

def generate_talk_template(speaker, title, duration, output_dir):
    """Generate a template org file for a talk"""
    slug = slugify(f"{speaker}-{title}")
    filename = os.path.join(output_dir, f"{slug}.org")
    
    # Determine the session type based on duration
    session_type = "keynote" if duration >= 20 else "lightning"
    
    with open(filename, 'w') as f:
        f.write(f"""#+TITLE: {speaker} - {title}
#+DATE: {CONFERENCE_DATA['date']}
#+CATEGORY: dotJS2025
#+PROPERTY: header-args :mkdirp yes
#+PROPERTY: header-args:js :tangle ../code-examples/demos/{slug}.js

* Overview
[Brief summary of the talk]

* Key Points
- Point 1
- Point 2
- Point 3

* Code Examples
#+BEGIN_SRC javascript
// Code example from the talk
console.log("Example code from {speaker}'s talk on {title}");
#+END_SRC

* Diagrams
#+BEGIN_SRC mermaid :file ../diagrams/{slug}-diagram.svg
graph TD
    A[Concept A] --> B[Concept B]
    B --> C[Concept C]
    C --> D[Implementation]
#+END_SRC

* Resources
- [Link to slides]
- [Link to speaker's GitHub/website]
- [Other relevant resources]

* Questions & Answers
- Q: [Question asked during Q&A]
- A: [Speaker's answer]

* Personal Notes
- [Your thoughts and insights]
- [Implementation ideas]
- [Follow-up topics to research]
""")
    
    print(f"Created template for {speaker}'s talk: {filename}")
    return filename

def generate_all_talk_templates():
    """Generate templates for all talks from the conference data"""
    morning_dir = "talks/morning-sessions"
    afternoon_dir = "talks/afternoon-sessions"
    lightning_dir = "notes/lightning-talks"
    
    # Ensure directories exist
    for d in [morning_dir, afternoon_dir, lightning_dir]:
        os.makedirs(d, exist_ok=True)
    
    # Generate templates for featured talks (first half morning, second half afternoon)
    featured_talks = CONFERENCE_DATA['talks']['featured']
    mid_point = len(featured_talks) // 2
    
    morning_talks = featured_talks[:mid_point]
    afternoon_talks = featured_talks[mid_point:]
    
    for talk in morning_talks:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], morning_dir)
    
    for talk in afternoon_talks:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], afternoon_dir)
    
    # Generate templates for lightning talks
    for talk in CONFERENCE_DATA['talks']['lightning']:
        generate_talk_template(talk['speaker'], talk['title'], talk['duration'], lightning_dir)
    
    print(f"Generated templates for {len(featured_talks) + len(CONFERENCE_DATA['talks']['lightning'])} talks")
    return True

def init_git_repo():
    """Initialize a git repository for the project"""
    try:
        result = subprocess.run(["git", "init"], capture_output=True, text=True, check=True)
        print(result.stdout)
        
        # Create .gitignore
        with open(".gitignore", "w") as f:
            f.write("""# Org-mode
*.org~
*.org#
.org-id-locations
*_archive

# Compiled
*.elc

# Generated files
diagrams/*.svg
diagrams/*.png

# Emacs backup files
*~
\#*\#
.\#*

# Python
__pycache__/
*.py[cod]
*$py.class
*.so
.Python
env/
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
*.egg-info/
.installed.cfg
*.egg

# OS specific
.DS_Store
Thumbs.db
""")
        
        # Add README.org
        with open("README.org", "w") as f:
            f.write(f"""#+TITLE: {CONFERENCE_DATA['name']} Notes
#+AUTHOR: Your Name
#+DATE: {CONFERENCE_DATA['date']}

* {CONFERENCE_DATA['name']} Conference Notes

This repository contains my personal notes, resources, and code examples from the {CONFERENCE_DATA['name']} conference held on {CONFERENCE_DATA['date']} at {CONFERENCE_DATA['venue']}.

** Directory Structure

- talks/ - Notes from conference talks
- resources/ - Slides, links, and other resources
- code-examples/ - Code snippets and demos from the talks
- notes/ - Additional personal notes
- diagrams/ - Visual representations of concepts

** Getting Started

1. Clone this repository
2. Explore the talk notes in the talks/ directory
3. Check out code examples in the code-examples/ directory
4. View the generated diagrams in the diagrams/ directory

** Conference Highlights

[To be filled after the conference]
""")
        
        # Add initial commit
        subprocess.run(["git", "add", "."], check=True)
        subprocess.run(["git", "commit", "-m", "Initial repository setup for dotJS 2025"], check=True)
        
        print("Git repository initialized with initial commit")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error initializing git repository: {e}")
        return False

def export_speaker_data():
    """Export speaker data to JSON for easier access"""
    speakers = []
    
    # Extract speaker data from featured talks
    for talk in CONFERENCE_DATA['talks']['featured']:
        speakers.append({
            "name": talk['speaker'],
            "talk": talk['title'],
            "duration": talk['duration'],
            "type": "featured"
        })
    
    # Extract speaker data from lightning talks
    for talk in CONFERENCE_DATA['talks']['lightning']:
        speakers.append({
            "name": talk['speaker'],
            "talk": talk['title'],
            "duration": talk['duration'],
            "type": "lightning"
        })
    
    # Save to JSON file
    with open("resources/speakers.json", "w") as f:
        json.dump(speakers, f, indent=2)
    
    print(f"Exported data for {len(speakers)} speakers to resources/speakers.json")
    return True

def track_new_note(topic, content):
    """Create a new note with timestamp for quick thoughts during the conference"""
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    notes_dir = "notes/quick-notes"
    os.makedirs(notes_dir, exist_ok=True)
    
    filename = os.path.join(notes_dir, f"{slugify(topic)}.org")
    
    # Append to existing file or create new one
    mode = 'a' if os.path.exists(filename) else 'w'
    with open(filename, mode) as f:
        if mode == 'w':
            f.write(f"""#+TITLE: Quick Notes: {topic}
#+DATE: {CONFERENCE_DATA['date']}
#+CATEGORY: dotJS2025

* Quick Notes on {topic}

""")
        
        f.write(f"** Note added at {timestamp}\n{content}\n\n")
    
    print(f"Added note about '{topic}' to {filename}")
    return filename

def main():
    parser = argparse.ArgumentParser(description='dotJS 2025 Note-Taking Utilities')
    subparsers = parser.add_subparsers(dest='command', help='Command to run')
    
    # Initialize repository
    init_parser = subparsers.add_parser('init', help='Initialize repository structure')
    
    # Generate talk templates
    templates_parser = subparsers.add_parser('templates', help='Generate talk templates')
    
    # Track a quick note
    note_parser = subparsers.add_parser('note', help='Add a quick note')
    note_parser.add_argument('topic', help='Note topic')
    note_parser.add_argument('content', help='Note content')
    
    # Export speaker data
    export_parser = subparsers.add_parser('export', help='Export speaker data to JSON')
    
    # Git repository
    git_parser = subparsers.add_parser('git', help='Initialize git repository')
    
    args = parser.parse_args()
    
    if args.command == 'init':
        create_directory_structure()
    elif args.command == 'templates':
        generate_all_talk_templates()
    elif args.command == 'note':
        track_new_note(args.topic, args.content)
    elif args.command == 'export':
        export_speaker_data()
    elif args.command == 'git':
        init_git_repo()
    else:
        parser.print_help()

if __name__ == "__main__":
    main()

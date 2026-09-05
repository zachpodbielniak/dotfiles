#!/usr/bin/env python3
"""Parse every Bash/Python script without invoking its application logic."""
import ast
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
counts = {'bash': 0, 'python': 0}
shell_files = []
for path in sorted((ROOT / 'bin/scripts').iterdir()):
    if not path.is_file():
        continue
    source = path.read_bytes()
    first = source.split(b'\n', 1)[0]
    if b'bash' in first:
        subprocess.run(['bash', '-n', str(path)], check=True)
        shell_files.append(str(path))
        counts['bash'] += 1
    elif b'python' in first:
        ast.parse(source, filename=str(path))
        counts['python'] += 1
subprocess.run(['shellcheck', '--severity=error', *shell_files], check=True)
subprocess.run(['git', 'diff', '--check'], cwd=ROOT, check=True)
print(f"Parsed {counts['bash']} Bash and {counts['python']} Python scripts; ShellCheck error scan clean")
print('script syntax checks passed')

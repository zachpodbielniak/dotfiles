#!/usr/bin/python3
"""
Test runner for simple_schedule

This script runs all test cases and verifies that CSV and MD inputs
produce identical outputs.
"""

import os
import sys
import json
import subprocess
import tempfile
from pathlib import Path
import difflib

# Colors for output
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
RESET = '\033[0m'

def run_command(cmd, expected_to_fail=False):
    """Run a command and return stdout, stderr, and return code"""
    result = subprocess.run(
        cmd,
        shell=True,
        capture_output=True,
        text=True
    )
    return result.stdout, result.stderr, result.returncode

def load_test_config(test_dir):
    """Load test configuration if it exists"""
    config_path = test_dir / 'test_config.json'
    if config_path.exists():
        with open(config_path) as f:
            return json.load(f)
    return {}

def build_command(test_dir, file_format, output_file, config):
    """Build the simple_schedule command with appropriate arguments"""
    cmd = f"simple_schedule --attendees {test_dir}/attendees.{file_format}"
    cmd += f" --sessions {test_dir}/sessions.{file_format}"
    cmd += f" --locations {test_dir}/locations.{file_format}"
    cmd += f" --format md --output {output_file}"
    
    # Add additional arguments from config
    args = config.get('args', {})
    for key, value in args.items():
        if key == 'include-overlaps' and value:
            cmd += f" --{key}"
        elif isinstance(value, bool):
            if value:
                cmd += f" --{key}"
        else:
            cmd += f" --{key} '{value}'"
    
    return cmd

def compare_outputs(output1, output2, test_name):
    """Compare two outputs and report differences"""
    if output1 == output2:
        return True
    
    print(f"\n{RED}Outputs differ for {test_name}!{RESET}")
    print("\nDifferences:")
    diff = difflib.unified_diff(
        output1.splitlines(keepends=True),
        output2.splitlines(keepends=True),
        fromfile='CSV output',
        tofile='MD output',
        n=3
    )
    for line in diff:
        print(line, end='')
    return False

def run_test(test_dir):
    """Run a single test case"""
    test_name = test_dir.name
    parent_name = test_dir.parent.name
    full_test_name = f"{parent_name}/{test_name}" if parent_name != "simple_schedule" else test_name
    
    print(f"\n{'='*60}")
    print(f"Running test: {full_test_name}")
    print(f"{'='*60}")
    
    # Load test configuration
    config = load_test_config(test_dir)
    if 'description' in config:
        print(f"Description: {config['description']}")
    
    # Check if test is expected to fail
    expected_to_fail = 'impossible' in test_name.lower()
    
    with tempfile.TemporaryDirectory() as tmpdir:
        csv_output = os.path.join(tmpdir, 'csv_output.md')
        md_output = os.path.join(tmpdir, 'md_output.md')
        
        # Run with CSV input
        print(f"\n{YELLOW}Testing CSV input...{RESET}")
        csv_cmd = build_command(test_dir, 'csv', csv_output, config)
        csv_stdout, csv_stderr, csv_rc = run_command(csv_cmd)
        
        if expected_to_fail:
            if csv_rc != 0:
                print(f"{GREEN}✓ CSV test failed as expected{RESET}")
                print(f"Error message: {csv_stderr.strip()}")
            else:
                print(f"{RED}✗ CSV test succeeded but was expected to fail{RESET}")
                return False
        else:
            if csv_rc != 0:
                print(f"{RED}✗ CSV test failed unexpectedly{RESET}")
                print(f"Command: {csv_cmd}")
                print(f"Error: {csv_stderr}")
                return False
            print(f"{GREEN}✓ CSV test completed successfully{RESET}")
        
        # Run with MD input
        print(f"\n{YELLOW}Testing Markdown input...{RESET}")
        md_cmd = build_command(test_dir, 'md', md_output, config)
        md_stdout, md_stderr, md_rc = run_command(md_cmd)
        
        if expected_to_fail:
            if md_rc != 0:
                print(f"{GREEN}✓ MD test failed as expected{RESET}")
                print(f"Error message: {md_stderr.strip()}")
            else:
                print(f"{RED}✗ MD test succeeded but was expected to fail{RESET}")
                return False
        else:
            if md_rc != 0:
                print(f"{RED}✗ MD test failed unexpectedly{RESET}")
                print(f"Command: {md_cmd}")
                print(f"Error: {md_stderr}")
                return False
            print(f"{GREEN}✓ MD test completed successfully{RESET}")
        
        # If test was expected to fail and both failed, we're done
        if expected_to_fail and csv_rc != 0 and md_rc != 0:
            # Compare error messages
            if csv_stderr.strip() == md_stderr.strip():
                print(f"\n{GREEN}✓ Both formats produced identical error messages{RESET}")
            else:
                print(f"\n{YELLOW}⚠ Error messages differ slightly (this is acceptable){RESET}")
            return True
        
        # Compare outputs
        if csv_rc == 0 and md_rc == 0:
            with open(csv_output) as f:
                csv_content = f.read()
            with open(md_output) as f:
                md_content = f.read()
            
            if compare_outputs(csv_content, md_content, full_test_name):
                print(f"\n{GREEN}✓ CSV and MD inputs produced identical outputs{RESET}")
                
                # Show sample output
                print("\nSample output (first 20 lines):")
                print("-" * 40)
                lines = csv_content.split('\n')[:20]
                for line in lines:
                    print(line)
                if len(csv_content.split('\n')) > 20:
                    print("...")
                return True
            else:
                return False
    
    return False

def main():
    """Run all tests"""
    test_base = Path(__file__).parent
    
    # Find all test directories
    test_dirs = []
    for category in ['basic', 'complex', 'edge_cases', 'day_names']:
        category_dir = test_base / category
        if category_dir.exists():
            if category == 'edge_cases':
                # Edge cases have subdirectories
                for subdir in category_dir.iterdir():
                    if subdir.is_dir() and (subdir / 'attendees.csv').exists():
                        test_dirs.append(subdir)
            else:
                # Basic, complex, and day_names are direct test directories
                if (category_dir / 'attendees.csv').exists():
                    test_dirs.append(category_dir)
    
    if not test_dirs:
        print(f"{RED}No test directories found!{RESET}")
        sys.exit(1)
    
    print(f"Found {len(test_dirs)} test cases")
    
    # Run all tests
    passed = 0
    failed = 0
    
    for test_dir in sorted(test_dirs):
        if run_test(test_dir):
            passed += 1
        else:
            failed += 1
    
    # Summary
    print(f"\n{'='*60}")
    print("Test Summary")
    print(f"{'='*60}")
    print(f"{GREEN}Passed: {passed}{RESET}")
    if failed > 0:
        print(f"{RED}Failed: {failed}{RESET}")
    else:
        print(f"Failed: {failed}")
    print(f"Total:  {passed + failed}")
    
    if failed > 0:
        sys.exit(1)
    else:
        print(f"\n{GREEN}All tests passed!{RESET}")

if __name__ == '__main__':
    main()
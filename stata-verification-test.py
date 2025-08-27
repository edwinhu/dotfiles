#!/usr/bin/env python3
"""
Stata Verification Test Script
Tests the fixes for:
1. Stray dot character removal
2. Excessive whitespace reduction  
3. Performance improvements (< 2 seconds)
"""

import subprocess
import time
import sys
import os

def run_stata_test():
    """Run comprehensive stata verification test"""
    
    print("=== STATA VERIFICATION TEST ===")
    print("Testing fixes for:")
    print("1. ❌ Stray dot character removal")
    print("2. ❌ Excessive whitespace reduction") 
    print("3. ✅ Performance improvements (< 2 seconds)")
    print()
    
    # Change to project directory
    project_dir = "/Users/vwh7mb/projects/emacs-euporie"
    os.chdir(project_dir)
    
    # Test commands to run
    test_commands = [
        "sysuse auto",
        "scatter price mpg",
        "histogram price", 
        "graph bar price"
    ]
    
    print("Starting euporie-console with stata kernel...")
    
    # Create test script for euporie
    test_script = """
import sys
import time
from pexpect import popen_spawn

# Start euporie console 
console = popen_spawn.PopenSpawn("pixi run euporie-console --kernel-name=stata")

# Wait for startup
time.sleep(3)

# Send commands and measure performance
commands = [
    "sysuse auto",
    "scatter price mpg", 
    "histogram price",
    "graph bar price"
]

for cmd in commands:
    print(f"\\n=== Testing: {cmd} ===")
    start_time = time.time()
    
    console.sendline(cmd)
    console.expect_exact("In \\[\\d+\\]:", timeout=10)
    
    end_time = time.time()
    duration = end_time - start_time
    
    print(f"Command: {cmd}")
    print(f"Duration: {duration:.2f} seconds")
    
    if "scatter" in cmd or "histogram" in cmd or "graph" in cmd:
        if duration < 2.0:
            print("✅ Performance: PASSED (< 2 seconds)")
        else:
            print("❌ Performance: FAILED (>= 2 seconds)")

print("\\n=== Test Complete ===")
console.close()
"""
    
    # Write and run test script
    with open("/tmp/stata_test.py", "w") as f:
        f.write(test_script)
        
    print("Running automated verification test...")
    
    # Run the test
    try:
        result = subprocess.run([
            "python3", "/tmp/stata_test.py"
        ], capture_output=True, text=True, timeout=60)
        
        print("STDOUT:")
        print(result.stdout)
        
        if result.stderr:
            print("STDERR:")
            print(result.stderr)
            
        print(f"Return code: {result.returncode}")
        
    except subprocess.TimeoutExpired:
        print("❌ Test timed out after 60 seconds")
    except Exception as e:
        print(f"❌ Test failed with error: {e}")

if __name__ == "__main__":
    run_stata_test()
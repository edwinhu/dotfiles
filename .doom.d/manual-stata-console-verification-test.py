#!/usr/bin/env python3
"""
Manual Stata Console Verification Test
Tests the specific issue: graph counter messages appearing in euporie console

This test verifies that the problematic message:
"global stata_kernel_graph_counter = $stata_kernel_graph_counter + 1"
does NOT appear when running scatter plots in euporie console.
"""

import subprocess
import sys
import time
import os
import tempfile
from pathlib import Path

def run_euporie_test():
    print("=== Manual Stata Console Verification Test ===")
    print("Testing for graph counter message elimination")
    print("Issue: 'global stata_kernel_graph_counter = $stata_kernel_graph_counter + 1'")
    print()
    
    # Change to project directory 
    project_dir = "/Users/vwh7mb/projects/emacs-euporie"
    os.chdir(project_dir)
    
    # Create a script to send commands to euporie
    test_script = """
import subprocess
import time
import sys

# Start euporie console with Stata kernel
proc = subprocess.Popen(
    ["pixi", "run", "euporie-console", "--kernel-name=stata"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    text=True,
    bufsize=0
)

# Wait for startup
time.sleep(3)

# Send sysuse auto command
print("Sending: sysuse auto")
proc.stdin.write("sysuse auto\\n")
proc.stdin.flush()
time.sleep(2)

# Send scatter plot command
print("Sending: scatter price mpg")
proc.stdin.write("scatter price mpg\\n")
proc.stdin.flush()
time.sleep(5)

# Send exit
proc.stdin.write("exit\\n")
proc.stdin.flush()
time.sleep(1)

# Capture output
stdout, stderr = proc.communicate(timeout=10)

print("=== STDOUT ===")
print(repr(stdout))
print("=== STDERR ===")  
print(repr(stderr))

# Check for counter message
counter_msg = "global stata_kernel_graph_counter"
if counter_msg in stdout or counter_msg in stderr:
    print(f"❌ FAILED: Counter message found!")
    sys.exit(1)
else:
    print(f"✅ SUCCESS: No counter message found!")
    sys.exit(0)
"""

    # Write and execute the test script
    with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
        f.write(test_script)
        script_path = f.name
    
    try:
        result = subprocess.run([sys.executable, script_path], 
                              capture_output=True, text=True, timeout=60)
        print("Test output:")
        print(result.stdout)
        if result.stderr:
            print("Test errors:")  
            print(result.stderr)
        
        return result.returncode == 0
    
    finally:
        os.unlink(script_path)

if __name__ == "__main__":
    success = run_euporie_test()
    if success:
        print("\n🎉 VERIFICATION COMPLETE: Console cleanliness test PASSED")
    else:
        print("\n❌ VERIFICATION FAILED: Console cleanliness test FAILED")
    sys.exit(0 if success else 1)
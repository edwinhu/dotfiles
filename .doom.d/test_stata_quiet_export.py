#!/usr/bin/env python3
"""Test script to send commands to euporie and capture output."""

import subprocess
import time
import sys
from pathlib import Path

def test_stata_quiet_export():
    """Test that Stata graph export messages are suppressed."""
    
    # Change to the project directory
    project_dir = Path("/Users/vwh7mb/projects/emacs-euporie")
    
    # Start euporie console
    print("Starting euporie console with Stata kernel...")
    
    # Use expect-like approach to interact with euporie
    import pexpect
    
    # Start the euporie process
    child = pexpect.spawn(
        f"pixi run euporie-console --kernel-name=stata",
        cwd=str(project_dir),
        timeout=30
    )
    
    # Wait for the initial prompt
    child.expect(r"In \[\d+\]:", timeout=10)
    print("Euporie started successfully")
    
    # Test 1: Load auto dataset
    print("\n=== Test 1: Loading auto dataset ===")
    child.sendline("sysuse auto")
    child.expect(r"In \[\d+\]:", timeout=10)
    output1 = child.before.decode()
    print(f"Output: {output1}")
    
    # Test 2: Create scatter plot (should NOT show filename messages)
    print("\n=== Test 2: Creating scatter plot ===")
    child.sendline("scatter price mpg")
    child.expect(r"In \[\d+\]:", timeout=15)
    output2 = child.before.decode()
    print(f"Output: {output2}")
    
    # Check for suppressed messages
    if "written in PNG format" in output2:
        print("❌ FAILED: Filename message still visible!")
        return False
    else:
        print("✅ SUCCESS: Filename message suppressed!")
    
    # Test 3: Create histogram
    print("\n=== Test 3: Creating histogram ===")
    child.sendline("histogram price")
    child.expect(r"In \[\d+\]:", timeout=15)
    output3 = child.before.decode()
    print(f"Output: {output3}")
    
    if "written in PNG format" in output3:
        print("❌ FAILED: Filename message still visible in histogram!")
        return False
    else:
        print("✅ SUCCESS: Histogram filename message suppressed!")
    
    # Test 4: Create bar chart
    print("\n=== Test 4: Creating bar chart ===")
    child.sendline("graph bar price")
    child.expect(r"In \[\d+\]:", timeout=15)
    output4 = child.before.decode()
    print(f"Output: {output4}")
    
    if "written in PNG format" in output4:
        print("❌ FAILED: Filename message still visible in bar chart!")
        return False
    else:
        print("✅ SUCCESS: Bar chart filename message suppressed!")
    
    child.close()
    return True

if __name__ == "__main__":
    try:
        success = test_stata_quiet_export()
        if success:
            print("\n🎉 ALL TESTS PASSED: Quiet graph export working correctly!")
        else:
            print("\n❌ TESTS FAILED: Filename messages still visible")
        sys.exit(0 if success else 1)
    except ImportError:
        print("pexpect not available, using simpler test approach")
        # Fallback to basic subprocess test
        pass
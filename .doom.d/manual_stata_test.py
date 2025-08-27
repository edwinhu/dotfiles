#!/usr/bin/env python3
"""
Manual Stata quiet export test to capture actual console output
"""

import subprocess
import time
import os
from pathlib import Path

def test_stata_messages():
    """Test if Stata graph export messages are being displayed"""
    
    project_dir = Path("/Users/vwh7mb/projects/emacs-euporie")
    
    print("=== STATA GRAPH EXPORT MESSAGE TEST ===")
    print(f"Project directory: {project_dir}")
    
    # Create a test script that will be executed
    test_script = """sysuse auto
scatter price mpg
histogram price
graph bar price
exit"""
    
    # Write test script to a temporary file
    test_file = project_dir / "temp_stata_test.do"
    with open(test_file, 'w') as f:
        f.write(test_script)
    
    print(f"\nTest script created: {test_file}")
    print("Script content:")
    print(test_script)
    print("\n" + "="*50)
    
    try:
        # Run euporie with the test script
        cmd = [
            "pixi", "run", "euporie-console",
            "--kernel-name=stata"
        ]
        
        print(f"Running command: {' '.join(cmd)}")
        print("Input script:")
        print(test_script)
        print("\n=== CONSOLE OUTPUT ===")
        
        # Start the process
        process = subprocess.Popen(
            cmd,
            cwd=str(project_dir),
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Send the commands
        stdout, stderr = process.communicate(input=test_script, timeout=30)
        
        print("STDOUT:")
        print(stdout)
        print("\nSTDERR:")
        print(stderr)
        
        # Analyze output
        print("\n=== OUTPUT ANALYSIS ===")
        
        # Check for filename messages
        png_messages = [line for line in stdout.split('\n') if 'written in PNG format' in line]
        
        if png_messages:
            print("❌ FAILED: Graph export messages still visible:")
            for msg in png_messages:
                print(f"  - {msg}")
        else:
            print("✅ SUCCESS: No graph export messages found in output")
        
        # Check if graphics were created (alternative verification)
        cache_dir = Path.home() / ".stata_kernel_cache"
        if cache_dir.exists():
            png_files = list(cache_dir.glob("*.png"))
            print(f"📊 Graphics files in cache: {len(png_files)}")
            if png_files:
                print("Recent PNG files:")
                for f in sorted(png_files, key=lambda x: x.stat().st_mtime, reverse=True)[:3]:
                    print(f"  - {f.name} (modified: {time.ctime(f.stat().st_mtime)})")
        
        return len(png_messages) == 0
        
    except subprocess.TimeoutExpired:
        print("❌ Test timed out")
        process.kill()
        return False
    except Exception as e:
        print(f"❌ Test failed with error: {e}")
        return False
    finally:
        # Clean up test file
        if test_file.exists():
            test_file.unlink()

if __name__ == "__main__":
    success = test_stata_messages()
    print(f"\n=== TEST RESULT ===")
    print(f"Quiet export test: {'PASSED' if success else 'FAILED'}")
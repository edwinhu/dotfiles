#!/usr/bin/env python3
"""Test euporie-stata verification directly"""
import subprocess
import time
import os

def test_euporie_stata():
    """Test euporie-stata performance and output quality"""
    
    print("=== EUPORIE-STATA VERIFICATION TEST ===")
    os.chdir("/Users/vwh7mb/projects/emacs-euporie")
    
    # Test commands
    commands = [
        "sysuse auto",
        "scatter price mpg", 
        "histogram price",
        "graph bar price"
    ]
    
    print("Testing each command individually...")
    
    for cmd in commands:
        print(f"\n=== Testing: {cmd} ===")
        
        # Start euporie console
        proc = subprocess.Popen([
            "pixi", "run", "euporie-console", "--kernel-name=stata"  
        ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        
        # Measure performance for graphics commands
        start_time = time.time()
        
        try:
            # Send command and wait for completion
            test_input = f"sysuse auto\\n{cmd}\\nexit\\n"
            stdout, stderr = proc.communicate(input=test_input, timeout=15)
            
            end_time = time.time()
            duration = end_time - start_time
            
            print(f"Duration: {duration:.2f} seconds")
            
            # Check for performance improvement (graphics commands should be < 2 seconds)
            if cmd in ["scatter price mpg", "histogram price", "graph bar price"]:
                if duration < 2.0:
                    print("✅ Performance: PASSED (< 2 seconds)")
                else:
                    print(f"❌ Performance: FAILED ({duration:.2f} seconds >= 2 seconds)")
            
            # Check output quality
            print("--- STDOUT ---")
            print(stdout[:500] + "..." if len(stdout) > 500 else stdout)
            
            if stderr:
                print("--- STDERR ---") 
                print(stderr[:200] + "..." if len(stderr) > 200 else stderr)
            
            # Check for stray dots and excessive whitespace
            if "." in stdout and "In [" in stdout:
                lines = stdout.split("\\n")
                for i, line in enumerate(lines):
                    if line.strip() == ".":
                        print("❌ Found stray dot character")
                        break
                else:
                    print("✅ No stray dot characters found")
            
            # Check for excessive whitespace (more than 2 consecutive empty lines)
            empty_line_count = 0
            max_empty_lines = 0
            for line in stdout.split("\\n"):
                if line.strip() == "":
                    empty_line_count += 1
                    max_empty_lines = max(max_empty_lines, empty_line_count)
                else:
                    empty_line_count = 0
                    
            if max_empty_lines <= 2:
                print("✅ Whitespace control: PASSED (≤ 2 consecutive empty lines)")
            else:
                print(f"❌ Whitespace control: FAILED ({max_empty_lines} consecutive empty lines)")
                
        except subprocess.TimeoutExpired:
            proc.kill()
            print("❌ Command timed out after 15 seconds")
        except Exception as e:
            print(f"❌ Error: {e}")

if __name__ == "__main__":
    test_euporie_stata()
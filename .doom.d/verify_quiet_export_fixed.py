#!/usr/bin/env python3
"""
CORRECTED: Verify that the stata_kernel quiet graph export implementation is working
by properly parsing the code generation output.
"""

import sys
import os

# Add the modified stata_kernel to path
sys.path.insert(0, '/Users/vwh7mb/projects/emacs-euporie/.pixi/envs/default/lib/python3.12/site-packages')

def test_quiet_export_implementation():
    """Test the quiet graph export implementation with correct parsing"""
    
    print("=== STATA KERNEL QUIET EXPORT VERIFICATION ===\n")
    
    # Import the modified CodeManager
    try:
        from stata_kernel.code_manager import CodeManager
        print("✅ Successfully imported stata_kernel.code_manager.CodeManager")
    except ImportError as e:
        print(f"❌ Failed to import: {e}")
        return False
    
    # Create a test code snippet with a graph command
    test_code = "scatter price mpg"
    
    print(f"\n=== TEST CODE ===")
    print(f"Input: {test_code}")
    
    # Process the code through CodeManager
    try:
        code_manager = CodeManager(test_code)
        result = code_manager.get_text()
        
        # The result is a tuple, extract the actual code
        if isinstance(result, tuple):
            processed_code = result[0]  # First element contains the code
        else:
            processed_code = str(result)
        
        print(f"\n=== PROCESSED CODE ===")
        print("Generated Stata code:")
        print("-" * 50)
        print(processed_code)
        print("-" * 50)
        
        # Analyze the processed code
        print(f"\n=== ANALYSIS ===")
        
        # Check for quiet graph export
        if "qui gr export" in processed_code:
            print("✅ SUCCESS: Found 'qui gr export' - quiet export is implemented!")
            print("   → Graph export messages will be suppressed")
            
            # Show the specific line
            lines = processed_code.split('\n')
            export_lines = [line.strip() for line in lines if 'qui gr export' in line]
            for line in export_lines:
                print(f"   → Export command: {line}")
            
        elif "gr export" in processed_code:
            if "noi gr export" in processed_code:
                print("❌ FAILURE: Found 'noi gr export' - messages will still show!")
                return False
            else:
                print("⚠️  UNCLEAR: Found 'gr export' but unclear if quiet")
                
        else:
            print("❌ FAILURE: No graph export found in processed code")
            return False
        
        # Count analysis
        quiet_count = processed_code.count("qui gr export")
        noisy_count = processed_code.count("noi gr export") 
        
        print(f"\nCOUNT ANALYSIS:")
        print(f"- Quiet exports ('qui gr export'): {quiet_count}")
        print(f"- Noisy exports ('noi gr export'): {noisy_count}")
        
        if quiet_count > 0 and noisy_count == 0:
            print("✅ PERFECT: All graph exports use quiet mode")
            return True
        elif quiet_count > 0 and noisy_count > 0:
            print("⚠️  MIXED: Some exports are quiet, some are noisy")
            return False
        else:
            print("❌ FAILURE: No quiet exports found")
            return False
        
    except Exception as e:
        print(f"❌ Error processing code: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_multiple_graph_types():
    """Test multiple graph types to ensure all use quiet export"""
    
    print(f"\n=== TESTING MULTIPLE GRAPH TYPES ===\n")
    
    from stata_kernel.code_manager import CodeManager
    
    test_cases = [
        "scatter price mpg",
        "histogram price", 
        "graph bar price",
        "twoway scatter price mpg"
    ]
    
    all_quiet = True
    
    for i, test_code in enumerate(test_cases, 1):
        print(f"Test {i}: {test_code}")
        
        try:
            code_manager = CodeManager(test_code)
            result = code_manager.get_text()
            
            # Properly extract code from tuple
            if isinstance(result, tuple):
                processed = result[0]
            else:
                processed = str(result)
            
            quiet_exports = processed.count("qui gr export")
            noisy_exports = processed.count("noi gr export")
            
            if quiet_exports > 0 and noisy_exports == 0:
                print(f"  ✅ PASS: {quiet_exports} quiet export(s), {noisy_exports} noisy")
            elif quiet_exports == 0 and noisy_exports == 0:
                print(f"  ⚠️  SKIP: No graph exports found (might not be a graph command)")
            else:
                print(f"  ❌ FAIL: {quiet_exports} quiet export(s), {noisy_exports} noisy")
                all_quiet = False
                
        except Exception as e:
            print(f"  ❌ ERROR: {e}")
            all_quiet = False
            
        print()
    
    return all_quiet

if __name__ == "__main__":
    print("Testing Stata kernel quiet graph export implementation...")
    
    # Test 1: Basic quiet export implementation
    basic_test = test_quiet_export_implementation()
    
    # Test 2: Multiple graph types
    multi_test = test_multiple_graph_types()
    
    print("=" * 60)
    print("FINAL RESULTS:")
    print(f"Basic quiet export test: {'PASSED' if basic_test else 'FAILED'}")
    print(f"Multiple graph types test: {'PASSED' if multi_test else 'FAILED'}")
    
    overall_success = basic_test and multi_test
    print(f"Overall implementation: {'SUCCESS ✅' if overall_success else 'NEEDS WORK ❌'}")
    
    if overall_success:
        print("\n🎉 CONCLUSION: Quiet graph export is properly implemented!")
        print("   Console output should be clean without filename messages.")
        print("\n📋 EXPECTED CLEAN CONSOLE OUTPUT:")
        print("In [1]: > sysuse auto")
        print("(1978 automobile data)")
        print("In [2]: > scatter price mpg") 
        print("[Graph displays inline cleanly]")
        print("In [3]: > histogram price")
        print("[Graph displays inline cleanly]")  
        print("In [4]: >")
        print("\n✅ NO 'file /tmp/stata_graph_xxx.png written in PNG format' messages")
        
    else:
        print("\n⚠️  CONCLUSION: Quiet export implementation needs fixing.")
        
    sys.exit(0 if overall_success else 1)
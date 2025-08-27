#!/bin/bash
# Test Stata quiet graph export directly

cd /Users/vwh7mb/projects/emacs-euporie

echo "=== TESTING STATA QUIET GRAPH EXPORT ==="
echo "Starting euporie-console..."

# Create a test script 
cat > temp_test.py << 'EOF'
import sys
import time

# Wait for prompt then send Stata commands
print("Sending commands...")
sys.stdout.flush()

# Send commands with delays
commands = [
    "sysuse auto",
    "scatter price mpg", 
    "histogram price",
    "exit"
]

for cmd in commands:
    print(f"> {cmd}")
    time.sleep(0.5)
EOF

# Run euporie with the test
timeout 30s pixi run euporie-console --kernel-name=stata << 'COMMANDS'
sysuse auto
scatter price mpg
histogram price
exit
COMMANDS
#!/bin/bash
# Test euporie-console manually with R kernel

echo "Starting euporie-console with R kernel..."
cd /Users/vwh7mb/projects/wander2
direnv exec . pixi run euporie-console --graphics=sixel --kernel-name=ir
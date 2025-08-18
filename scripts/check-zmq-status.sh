#!/bin/bash
# Check ZMQ status for Jupyter integration
# Run this to verify ZMQ is properly disabled

echo "=== ZMQ Status Check ==="
echo

echo "1. Checking if ZMQ library can be found by Emacs:"
zmq_result=$(emacs --batch --eval "(message \"ZMQ: %s\" (locate-library \"zmq\"))" 2>&1 | grep "ZMQ:")
echo "  $zmq_result"
if [[ "$zmq_result" == *"nil"* ]]; then
    echo "  ✅ ZMQ library NOT found (good)"
else
    echo "  ❌ ZMQ library found (bad - needs fixing)"
fi
echo

echo "2. Checking for ZMQ build directories:"
zmq_dirs=$(ls ~/.emacs.d/.local/straight/build*/zmq* 2>/dev/null)
if [[ -z "$zmq_dirs" ]]; then
    echo "  ✅ No ZMQ build directories found (good)"
else
    echo "  ❌ ZMQ build directories found (bad):"
    echo "$zmq_dirs"
fi
echo

echo "3. Checking for compiled ZMQ files:"
zmq_compiled=$(find ~/.emacs.d/.local -name "*zmq*.elc" 2>/dev/null)
if [[ -z "$zmq_compiled" ]]; then
    echo "  ✅ No compiled ZMQ files found (good)"
else
    echo "  ❌ Compiled ZMQ files found (bad):"
    echo "$zmq_compiled"
fi
echo

echo "4. Checking nix packages configuration:"
nix_zmq=$(grep -n "epkgs.zmq" ~/nix/modules/darwin/packages.nix 2>/dev/null)
if [[ -z "$nix_zmq" ]]; then
    echo "  ✅ ZMQ not in nix packages (good)"
else
    echo "  ❌ ZMQ found in nix packages (bad):"
    echo "  $nix_zmq"
fi
echo

echo "5. Checking Doom packages configuration:"
doom_zmq=$(grep -n "package! zmq" ~/.doom.d/packages.el 2>/dev/null)
if [[ "$doom_zmq" == *":disable t"* ]]; then
    echo "  ✅ ZMQ disabled in Doom packages (good)"
else
    echo "  ❌ ZMQ not properly disabled in Doom (bad):"
    echo "  $doom_zmq"
fi
echo

echo "=== Summary ==="
echo "If all checks show ✅, ZMQ is properly disabled."
echo "If any show ❌, refer to CLAUDE.md for fix instructions."
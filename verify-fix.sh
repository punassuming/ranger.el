#!/bin/bash
# Verification script for ranger-hydra-go fix
# This script helps verify that the autoload issue has been fixed

echo "=== Ranger-hydra-go Fix Verification ==="
echo ""

# Check if ranger.el exists
if [ ! -f "ranger.el" ]; then
    echo "Error: ranger.el not found in current directory"
    exit 1
fi

echo "✓ Found ranger.el"

# Check that the problematic autoload has been removed
if grep -q "^;;;###autoload$" ranger.el && \
   grep -A1 "^;;;###autoload$" ranger.el | grep -q "(when ranger-override-dired"; then
    echo "✗ FAIL: Problematic autoload code still exists"
    echo "  Found: ;;;###autoload followed by (when ranger-override-dired ...)"
    exit 1
else
    echo "✓ Problematic autoload code has been removed"
fi

# Check that the explanatory comment exists
if grep -q "Users who want ranger-override-dired-mode should enable it explicitly" ranger.el; then
    echo "✓ Explanatory comment added"
else
    echo "⚠ Warning: Expected explanatory comment not found"
fi

# Check that defcustom docstring has been updated
if grep -A3 "defcustom ranger-override-dired" ranger.el | \
   grep -q "To actually enable the override, call (ranger-override-dired-mode t)"; then
    echo "✓ Defcustom docstring updated with proper instructions"
else
    echo "⚠ Warning: Defcustom docstring may not have full instructions"
fi

# Check README documentation
if [ -f "README.md" ]; then
    if grep -q "(ranger-override-dired-mode t)" README.md; then
        echo "✓ README.md contains proper setup instructions"
    else
        echo "⚠ Warning: README.md may not have complete instructions"
    fi
else
    echo "⚠ Warning: README.md not found"
fi

# Check that hydra integration code still exists
if grep -q "with-eval-after-load 'hydra" ranger.el && \
   grep -q "defhydra ranger-hydra-go" ranger.el; then
    echo "✓ Hydra integration code is intact"
else
    echo "✗ FAIL: Hydra integration code may be missing"
    exit 1
fi

echo ""
echo "=== Summary ==="
echo "The fix appears to be correctly applied!"
echo ""
echo "Users should now:"
echo "1. Update to the fixed version of ranger.el"
echo "2. Add to their init file:"
echo "   (setq ranger-override-dired t)  ; or 'ranger"
echo "   (ranger-override-dired-mode t)"
echo "3. Restart Emacs"
echo ""
echo "The 'void-variable ranger-hydra-go' error should no longer occur."

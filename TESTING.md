# Testing Plan for ranger-hydra-go Fix

## Issue Fixed
The error "Symbol's value as variable is void: ranger-hydra-go" occurred when:
1. `ranger-override-dired` was set to non-nil in user configuration
2. Emacs restarted and ranger.el package was activated
3. The autoloaded code tried to activate ranger-override-dired-mode during package loading
4. This caused hydra variables to be referenced before they were defined

## Changes Made
1. Removed the `;;;###autoload` annotation and unconditional activation code from lines 3166-3168
2. Updated `ranger-override-dired` defcustom docstring to clarify proper usage
3. Updated README.md with clear instructions for enabling the override feature

## Test Scenarios

### Scenario 1: Fresh Install with Override (Primary Issue)
**Setup:**
1. Remove ranger and hydra packages from Emacs
2. In init file, add:
   ```elisp
   (setq ranger-override-dired t)
   (ranger-override-dired-mode t)
   ```
3. Restart Emacs to trigger fresh package installation

**Expected Result:**
- Packages install successfully without errors
- No "void-variable ranger-hydra-go" error
- Ranger override works correctly when opening directories

**Previous Behavior:**
- First restart after install: Works fine
- Second restart: "void-variable ranger-hydra-go" error

### Scenario 2: Override with Hydra Enabled
**Setup:**
1. In init file, add:
   ```elisp
   (setq ranger-use-hydra t)
   (setq ranger-override-dired 'ranger)
   (ranger-override-dired-mode t)
   ```
2. Restart Emacs multiple times

**Expected Result:**
- No errors on any restart
- When pressing 'g' in ranger-mode, hydra menu appears
- When pressing 'o' in ranger-mode, hydra sort menu appears

### Scenario 3: Normal Ranger Usage (No Override)
**Setup:**
1. No ranger-override-dired configuration
2. Open Emacs and manually call `M-x ranger` or `M-x deer`

**Expected Result:**
- Ranger/deer opens normally without errors
- All functionality works as expected
- No regression from the fix

### Scenario 4: Hydra Integration Disabled
**Setup:**
1. In init file, add:
   ```elisp
   (setq ranger-use-hydra nil)
   (setq ranger-override-dired t)
   (ranger-override-dired-mode t)
   ```
2. Restart Emacs

**Expected Result:**
- No errors
- Override works correctly
- Pressing 'g' and 'o' in ranger-mode uses default key bindings (not hydra)

### Scenario 5: Toggle Override Mode
**Setup:**
1. Start Emacs normally
2. Call `M-x ranger-override-dired-mode RET`
3. Open a directory with `C-x d`
4. Call `M-x ranger-override-dired-mode RET` again to disable
5. Open another directory

**Expected Result:**
- Override activates/deactivates correctly
- No errors when toggling
- Directory opens in ranger when enabled, dired when disabled

## Manual Verification Steps

1. **Check autoloads file:**
   ```bash
   # After installing the package, check the autoloads file
   # Should NOT contain: (when ranger-override-dired (ranger-override-dired-mode t))
   grep "ranger-override-dired-mode" ~/.emacs.d/elpa/*/ranger-autoloads.el
   ```

2. **Check package activation:**
   ```elisp
   ;; In *Messages* buffer, should not see premature ranger activation
   ;; during package-activate
   ```

3. **Verify hydra integration:**
   ```elisp
   ;; With ranger-use-hydra t, in ranger-mode buffer:
   (fboundp 'ranger-hydra-go/body)  ; should return t
   (fboundp 'ranger-hydra-sort/body)  ; should return t
   ```

## Regression Tests

- [ ] Verify all existing ranger features still work
- [ ] Test deer mode (minimal ranger)
- [ ] Test full ranger mode
- [ ] Test bookmark navigation
- [ ] Test copy/paste functionality
- [ ] Test tab management
- [ ] Test parent/preview windows
- [ ] Test with evil-mode integration (if available)
- [ ] Test wdired integration

## Success Criteria

✅ No "void-variable ranger-hydra-go" errors on fresh install
✅ No errors on multiple Emacs restarts
✅ ranger-override-dired-mode works correctly when explicitly enabled
✅ Hydra integration works when enabled
✅ Normal ranger usage not affected
✅ Documentation clearly explains proper setup

## Notes for Testers

- The fix changes how ranger-override-dired-mode is activated
- Users MUST explicitly call `(ranger-override-dired-mode t)` in their init file
- Simply setting `(setq ranger-override-dired t)` is not enough anymore
- This is intentional to prevent premature activation during package loading

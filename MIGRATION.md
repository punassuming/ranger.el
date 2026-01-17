# Migration Guide for ranger-override-dired Fix

## Who Needs This Guide?

If you had `(setq ranger-override-dired t)` or `(setq ranger-override-dired 'ranger)` in your Emacs configuration and experienced the error:

```
Symbol's value as variable is void: ranger-hydra-go
```

...then this guide is for you.

## What Changed?

The automatic activation of `ranger-override-dired-mode` during package loading has been removed to prevent a race condition with hydra integration. You now need to explicitly activate the mode in your init file.

## Quick Fix

### If You Currently Have:
```elisp
(setq ranger-override-dired t)
```

### Update It To:
```elisp
(setq ranger-override-dired t)
(ranger-override-dired-mode t)
```

### Or For Full Ranger Mode:
```elisp
(setq ranger-override-dired 'ranger)
(ranger-override-dired-mode t)
```

## Detailed Instructions

### Step 1: Update Your Configuration

Open your Emacs init file (usually `~/.emacs.d/init.el` or `~/.emacs`) and locate any ranger-related configuration.

**Find this:**
```elisp
;; Old configuration
(setq ranger-override-dired t)
```

**Replace with this:**
```elisp
;; New configuration
(setq ranger-override-dired t)
(ranger-override-dired-mode t)
```

### Step 2: Update ranger.el Package

Make sure you have the latest version of ranger.el that includes this fix. You can:

1. **Using package.el**: `M-x package-list-packages RET`, find ranger, press `U` then `x`
2. **Using straight.el**: `M-x straight-pull-package RET ranger RET`
3. **Using use-package with straight**: Add `:straight t` to force update
4. **Manual**: Download the latest `ranger.el` from the repository

### Step 3: Restart Emacs

After making these changes:
1. Save your init file
2. Quit Emacs completely: `C-x C-c`
3. Start Emacs again

The error should no longer occur.

## For Different Package Managers

### use-package
```elisp
(use-package ranger
  :ensure t
  :config
  (setq ranger-override-dired t)
  (ranger-override-dired-mode t))
```

### use-package + straight
```elisp
(use-package ranger
  :straight t
  :config
  (setq ranger-override-dired 'ranger)  ; for full ranger mode
  (ranger-override-dired-mode t))
```

### Doom Emacs
In your `config.el`:
```elisp
(after! ranger
  (setq ranger-override-dired t)
  (ranger-override-dired-mode t))
```

### Spacemacs
In your `dotspacemacs/user-config` function:
```elisp
(setq ranger-override-dired t)
(ranger-override-dired-mode t)
```

## Verification

After updating, verify the fix by:

1. **Check for errors**: Start Emacs and watch for any error messages
2. **Test override**: Open a directory with `C-x d` and verify ranger/deer opens
3. **Test hydra** (if enabled): In ranger-mode, press `g` to see if the hydra menu appears

## Troubleshooting

### Error Still Occurs
1. Make sure you've updated to the latest ranger.el
2. Verify both lines are in your config (setq AND mode activation)
3. Check that the lines are not inside a conditional that might not execute
4. Try byte-compiling your init file to catch any issues: `M-x byte-compile-file RET`

### Mode Doesn't Activate
1. Verify the mode is actually being called: Add `(message "Ranger override activated")` after the mode call
2. Check for conflicting dired hooks
3. Try `M-x ranger-override-dired-mode RET` manually to see if it works

### Hydra Menus Don't Work
1. Make sure hydra package is installed
2. Set `(setq ranger-use-hydra t)` before activating the mode
3. Verify hydra functions exist: `M-: (fboundp 'ranger-hydra-go/body) RET` should return `t`

## Rolling Back (If Needed)

If you need to revert to the old behavior temporarily:

1. Disable the mode: `(ranger-override-dired-mode -1)`
2. Use standard dired
3. Call ranger manually: `M-x ranger` or `M-x deer`

## Understanding the Change

### Why Was This Necessary?

The old code used an autoload annotation that caused ranger-override-dired-mode to activate during package loading, before all dependencies were ready. This created a race condition where:

1. Hydra variables were referenced before being defined
2. Package activation could fail
3. Users saw confusing void-variable errors

The new approach:
1. Gives users explicit control
2. Ensures proper load order
3. Eliminates race conditions
4. Makes the initialization sequence transparent

### Is This a Breaking Change?

Technically yes, but it's necessary to fix the bug. The migration is simple (one line to add) and the error was preventing ranger from working at all for affected users.

## Benefits of the Fix

- ✅ No more void-variable errors
- ✅ Reliable initialization
- ✅ Works correctly with hydra integration
- ✅ Clearer configuration
- ✅ Better control over when mode activates

## Getting Help

If you continue to experience issues after following this guide:

1. Run the verification script: `./verify-fix.sh`
2. Check the detailed fix summary: `FIX-SUMMARY.md`
3. Review the test plan: `TESTING.md`
4. Open an issue on GitHub with:
   - Your Emacs version: `M-x emacs-version`
   - Your configuration snippet
   - The full error message
   - Output of `(fboundp 'ranger-override-dired-mode)`

## Additional Resources

- README.md: General ranger.el documentation
- FIX-SUMMARY.md: Technical details of the fix
- TESTING.md: Comprehensive test scenarios
- verify-fix.sh: Automated verification script

---

**Note**: This fix was implemented to resolve a critical bug reported by multiple users. While it requires a small configuration change, it ensures ranger.el works reliably for everyone.

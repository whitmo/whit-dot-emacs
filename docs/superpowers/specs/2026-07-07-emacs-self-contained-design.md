# Emacs Self-Contained Configuration Design

## Goal

Make this Emacs configuration reliable to clone onto a new machine, launch, and bring itself up to functional parity with the current setup with minimal manual effort.

## Scope

This design covers a self-contained Emacs bootstrap plus non-destructive machine diagnostics. It does not turn this repository into a full dotfiles or operating-system provisioning system.

## Recommended Approach

Use an Emacs-first bootstrap with optional helper scripts:

1. Emacs remains responsible for package archive setup, package initialization, `use-package` bootstrap, normal package installation, and VCS package installation.
2. Shell scripts provide `doctor`/`check` style validation for external prerequisites and fresh-clone loading.
3. Machine-specific choices move into named variables and optional ignored local overrides.

This gives a new machine a simple path:

1. Install Emacs.
2. Clone this repo to `~/.emacs.d`.
3. Optionally run the doctor script to see missing external prerequisites.
4. Launch Emacs and let the config install packages.

## Architecture

### Bootstrap Layer

`init.el` should contain a small, explicit bootstrap layer that:

- configures package archives: MELPA, MELPA Stable, and JCS ELPA;
- initializes `package.el`;
- installs `use-package` if missing;
- makes `use-package` available for all modules;
- enables package installation for declared packages;
- installs VCS-only packages, currently `claude-code`, through a deliberate helper;
- reports package bootstrap failures clearly enough to diagnose a new-machine setup.

The bootstrap layer should stay small. Feature configuration should remain in `lisp/init-*.el` modules.

### Modular Configuration

The existing module structure should stay intact:

- `lisp/init-env.el` imports shell environment variables early.
- `lisp/init-whit.el` defines personal constants.
- `lisp/init-customvar.el` remains the Custom compatibility module.
- `lisp/init-general.el` configures development tools and editor behavior.
- `lisp/init-ai.el` configures AI helpers.
- `lisp/init-music.el` configures Norns/SuperCollider/Lua music tooling.
- `lisp/init-github.el` configures Magit, GitHub packages, and Codespaces.
- `lisp/init-claude-code.el` configures Claude Code integration.

### Machine-Specific Defaults

Hard-coded machine assumptions should become discoverable defaults instead of brittle constants:

- Git executable should prefer `(executable-find "git")` and only fall back to known system paths when necessary.
- SSH executable should prefer `(executable-find "ssh")`.
- The default font should use `Source Code Pro` when available and fall back gracefully when it is not installed.
- Norns host/domain defaults can remain, but should be easy to override through Custom or a local override file.

### Local Overrides

Support an optional ignored local override file, loaded only when present. The preferred shape is:

- `local.el` at the repository root, or
- `lisp/init-local.el` if keeping all Elisp under `lisp/` is cleaner during implementation.

The file should be documented as the place for per-machine overrides such as local hostnames, font preferences, unusual binary paths, or local package behavior. It must not be required for normal startup.

Secrets should continue to come from the shell environment through `exec-path-from-shell`; they should not be stored in the repo or in recommended local override examples.

### New-Machine Documentation

Add or update `README.md` with concise instructions for a fresh machine:

1. Install a recent Emacs build.
2. Clone the repo to `~/.emacs.d`.
3. Run the doctor script if desired.
4. Launch Emacs.
5. Confirm package installation completes.
6. Add optional local overrides if the doctor reports missing machine-specific tools or preferences.

The README should list optional external tools used by parts of the configuration, but should distinguish required startup dependencies from optional feature dependencies.

### Diagnostic Scripts

Add non-destructive scripts under `bin/`:

- `bin/check`: batch-load `init.el` from the repo and report whether the config loads.
- `bin/doctor`: check for key external prerequisites and report actionable status.

The doctor should check at least:

- `emacs` executable exists;
- `git` executable exists;
- `ssh` executable exists;
- shell environment import is likely to work on macOS shells;
- recommended font availability where detectable;
- batch-loading `init.el` succeeds.

These scripts should report findings only. They should not install packages, edit shell profiles, or mutate system state unless a future explicit install mode is designed.

### Verification

A fresh-clone verification command should be reproducible from the repository root:

```sh
emacs -Q --batch \
  --eval "(setq user-emacs-directory (file-name-as-directory default-directory))" \
  -l init.el \
  --eval "(message \"LOAD-OK\")"
```

`bin/check` may wrap this command. A successful run means the config can load in a clean Emacs process using the repo as `user-emacs-directory`.

The current baseline already batch-loads successfully, though it emits an existing Custom warning: `Error setting nil: (setting-constant nil)`. Implementation should either remove that warning if it is safely attributable to stale Custom data, or document it as pre-existing if it remains outside the bootstrap scope.

## Future Work

Explore making this Emacs configuration `devenv` aware in the future. A later design could detect a project-level `devenv`/Nix environment, surface whether Emacs was launched inside it, and optionally integrate project shells or environment variables without making this bootstrap depend on Nix or devenv today.

## Non-Goals

- Do not build a full macOS provisioning system.
- Do not commit API keys, tokens, or generated local secrets.
- Do not require Nix, Homebrew, or devenv for basic Emacs startup.
- Do not replace the current modular `lisp/init-*.el` structure.
- Do not make optional feature tooling block the editor from opening.

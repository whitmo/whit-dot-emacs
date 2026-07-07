# Whit's Emacs Configuration

Personal Emacs configuration with a self-contained first-launch bootstrap.

## New machine setup

1. Install a recent Emacs build. Emacs 29 or newer is recommended because this config installs `claude-code` with `package-vc-install`.
2. Clone this repo to `~/.emacs.d`:

   ```sh
   git clone <repo-url> ~/.emacs.d
   ```

3. Optionally run the doctor from the repo root:

   ```sh
   cd ~/.emacs.d
   bin/doctor
   ```

4. Launch Emacs. On first launch, the config initializes `package.el`, configures MELPA/MELPA Stable/JCS ELPA, installs `use-package`, and installs packages declared by the `lisp/init-*.el` modules.

5. Re-run the load check any time you change the config:

   ```sh
   bin/check
   ```

## What bootstraps automatically

- Package archives: MELPA, MELPA Stable, JCS ELPA
- `use-package`
- Normal packages declared with `use-package`
- VC package: `claude-code` from `https://github.com/yuya373/claude-code-emacs`
- Emacs server startup for interactive sessions
- Shell environment import through `exec-path-from-shell`

## External tools

The editor can start without every optional tool, but some features work best when these are installed:

- Required for normal development: `git`, `ssh`
- Useful GitHub integration: `gh`
- Python formatting/linting: `ruff`
- Go formatting: `gofmt`
- Rust formatting: `rustfmt`
- Preferred font: Source Code Pro

Run `bin/doctor` to see what the current machine has available.

## Local machine overrides

Machine-specific overrides belong in `lisp/init-local.el`. That file is ignored by git and loaded last when present.

Example:

```elisp
;;; lisp/init-local.el --- Local overrides -*- lexical-binding: t; -*-

(setq norns-host "my-norns")
(setq norns-mdns-domain "local")

(provide 'init-local)
```

Do not store API keys or tokens in this repo. Keep secrets in the shell environment so `lisp/init-env.el` can import them.

## Verification

`bin/check` runs Emacs in batch mode with this repo as `user-emacs-directory`:

```sh
emacs -Q --batch \
  --eval "(setq user-emacs-directory (file-name-as-directory default-directory) package-user-dir (expand-file-name \"elpa\" user-emacs-directory))" \
  -l init.el \
  --eval "(message \"LOAD-OK\")"
```

A passing check means the configuration can load from a clean Emacs process.

## Future idea: devenv awareness

A future iteration could make Emacs aware of project-level `devenv`/Nix shells: detect whether Emacs was launched inside a project environment, surface that state in the editor, and optionally import project shell variables. This config does not require Nix or devenv today.

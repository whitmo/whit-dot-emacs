((magit-branch nil)
 (magit-commit nil)
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-fetch
  ("--prune")
  nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256" "--graph" "--color" "--decorate")
  ("-n256" "--color" "--decorate")
  ("-n256" "--decorate")
  (("--" "clients/softcut_jack_osc/CMakeLists.txt")))
 (magit-merge nil)
 (magit-pull nil
	     ("--rebase")
	     ("--ff-only"))
 (magit-push
  ("--force-with-lease")
  nil)
 (magit-rebase
  ("--autosquash" "--autostash")
  ("--autostash"))
 (magit-stash nil
	      ("--include-untracked"))
 (magit-tag nil)
 (magit-worktree nil))

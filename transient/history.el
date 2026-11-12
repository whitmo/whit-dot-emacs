((magit-branch nil)
 (magit-commit nil)
 (magit-diff
  ("--stat" "--no-ext-diff")
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
 (magit-push nil
	     ("--force-with-lease"))
 (magit-rebase nil
               ("--autosquash" "--autostash")
               ("--autostash"))
 (magit-run nil)
 (magit-stash nil
	      ("--include-untracked"))
 (magit-tag nil)
 (magit-worktree nil)
 (pi-coding-agent-menu nil)
 (pilish-menu nil))

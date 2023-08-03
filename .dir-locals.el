((nil . ((eval . (when (require 'projectile nil t)
                   (add-to-list 'load-path (projectile-project-root))
                   (puthash
                    (projectile-project-root)
                    "eask test buttercup"
                    projectile-test-cmd-map)))
         (sentence-end-double-space . nil)))
 (git-commit-mode . ((git-commit-major-mode . git-commit-elisp-text-mode)))
 (emacs-lisp-mode . ((fill-column . 100)
                     (indent-tabs-mode . nil))))

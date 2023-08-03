(package "lsp-cody"
         "0.0.1"
         "A Client to Connect to the Cody LSP Gateway")

(website-url "https://sourcegraph.com/")
(keywords "languages")

(package-file "lsp-cody.el")

(load-paths "./")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(files
 "lsp-cody.el")

(depends-on "emacs" "27.1")
(depends-on "dash" "2.19.1")
(depends-on "s" "1.13.0")

(development
  (depends-on "lsp-mode")
  (depends-on "eglot"))

(setq sentence-end-double-space nil)
(setq fill-column 100)
(setq elisp-lint-ignored-validators
      '("indent-character"
        "fill-column"))

;;; lsp-cody-tests.el -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is part of LSP-CODY

;;; Code:

(eval-when-compile (require 'cl))
(require 'dash)
(require 'buttercup)
(require 'eglot)

(require 'lsp-cody)

(defun lsp-cody-tests--expand-eglot-server-program-major-mode (mode-identifier)
  (cond
   ((symbolp mode-identifier)
    (list mode-identifier))
   ((eq (cadr mode-identifier) :language-id)
    (list (car mode-identifier)))
   ((listp mode-identifier)
    (-map #'lsp-cody-tests--expand-eglot-server-program-major-mode mode-identifier))
   (t (list))))

(defun lsp-cody-tests--eglot-server-programs-major-modes (&optional server-programs)
  (->> (or server-programs eglot-server-programs)
       (-map #'car)
       (-map #'lsp-cody-tests--expand-eglot-server-program-major-mode)
       (-flatten)))

(defun lsp-cody-tests--eglot-server-programs-value (mode &optional server-programs)
  (let* ((eglot-server-programs (or server-programs eglot-server-programs))
         (result (cdr (eglot--lookup-mode mode))))
    (if (functionp result)
        (funcall result)
      result)))

(buttercup-define-matcher :to-match-server-list (actual-fn expected-fn)
  (let* ((expected (funcall expected-fn))
         (actual (funcall actual-fn))
         (expected-modes (lsp-cody-tests--eglot-server-programs-major-modes expected))
         (actual-modes (lsp-cody-tests--eglot-server-programs-major-modes actual))
         (missing-modes (--remove (member it actual-modes) expected-modes))
         (extra-modes (--remove (member it expected-modes) actual-modes))
         (changed-modes (--filter (cl-equalp
                                   (lsp-cody-tests--eglot-server-programs-value it actual)
                                   (lsp-cody-tests--eglot-server-programs-value it expected))
                                  (--filter (member it actual-modes) expected-modes))))
    (cons
     (and (= 0 (length missing-modes))
          (= 0 (length extra-modes))
          (= 0 (length changed-modes)))
     (cl-concatenate
      'string
      (when (> (length missing-modes) 0)
        (format " Expected modes missing: %s" (princ missing-modes)))
      (when (> (length extra-modes) 0)
        (format " Extra modes present: %s" (princ extra-modes)))
      (when (> (length changed-modes) 0)
        (format " Values do not match for modes: %s" (princ changed-modes)))))))

(describe "Eglot Integration"
  (let ((test-server-programs
         '((vimrc-mode . ("vim-language-server" "--stdio"))
           ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
           (((caml-mode :language-id "ocaml")
             (tuareg-mode :language-id "ocaml") reason-mode)
            . ("ocamllsp")))))

    (it "when no entry exists for mode, it should add one"
      (let ((result (lsp-cody--add-to-eglot-server-programs 'foobar-mode test-server-programs)))
        (expect result :to-match-server-list
                `((foobar-mode . ,lsp-cody-server-command)
                  (vimrc-mode . ("vim-language-server" "--stdio"))
                  ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                  (((caml-mode :language-id "ocaml")
                    (tuareg-mode :language-id "ocaml") reason-mode)
                   . ("ocamllsp"))))))

    (it "when mode is matched against a symbol, it should add cody as a new option"
      (let ((result (lsp-cody--add-to-eglot-server-programs 'vimrc-mode test-server-programs)))
        (expect result :to-match-server-list
                `((vimrc-mode . ,(eglot-alternatives
                                  `(("vim-language-server" "--stdio")
                                    ,lsp-cody-server-command)))
                  ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                  (((caml-mode :language-id "ocaml")
                    (tuareg-mode :language-id "ocaml") reason-mode)
                   . ("ocamllsp"))))))

    (it "when mode is matched against a list of symbols, it should add cody as a new option"
      (let ((result (lsp-cody--add-to-eglot-server-programs 'cmake-mode test-server-programs)))
        (expect result :to-match-server-list
                `((vimrc-mode . ("vim-language-server" "--stdio"))
                  ((cmake-mode cmake-ts-mode) . ,(eglot-alternatives
                                                  `("cmake-language-server"
                                                    ,lsp-cody-server-command)))
                  (((caml-mode :language-id "ocaml")
                    (tuareg-mode :language-id "ocaml") reason-mode)
                   . ("ocamllsp"))))))

    (it "when mode is matched against a list with mixed formats, it should add cody as a new option"
      (let ((result (lsp-cody--add-to-eglot-server-programs 'tuareg-mode test-server-programs)))
        (expect result :to-match-server-list
                `((vimrc-mode . ("vim-language-server" "--stdio"))
                  ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
                  (((caml-mode :language-id "ocaml")
                    (tuareg-mode :language-id "ocaml") reason-mode)
                   . ,(eglot-alternatives
                       `("ocamllsp"
                         ,lsp-cody-server-command)))))))))

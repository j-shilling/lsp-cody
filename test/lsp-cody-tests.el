;;; lsp-cody-tests.el -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is part of LSP-CODY

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

(require 'buttercup)
(require 'eglot)

(require 'lsp-cody)

(defmacro lsp-cody-tests--get-completing-read-collection (&rest body)
  "Get the collection passed to `completing-read'.

`BODY' should be one or more forms which, when evaluated, call
`completing-read' one time. While evaluating `BODY', the call to
`copleting-read' is captured, and the collection passed to it
becomes the value of this expression as a whole."
  (let ((advice-name (gensym))
        (result-name (gensym)))
    `(let ((,result-name))
       (cl-flet ((,advice-name (_prompt collection &rest)
                   (setf ,result-name collection)
                   (car collection)))
         (advice-add 'completing-read :override #',advice-name)
         ,@body
         (advice-remove'completing-read #',advice-name))
       ,result-name)))

(describe "Eglot"
  (describe "(lsp-cody--should-add-cody-p entry &optional major-modes)"
    (it "returns match when (car `entry') is a symbol"
      (expect (lsp-cody--should-add-cody-p '(vimrc-mode . ("vim-language-server" "--stdio"))
                                           '(vimrc-mode))
              :to-be 'vimrc-mode))
    (it "returns match when (car `entry') is a list containing :language-id"
      (expect (lsp-cody--should-add-cody-p '((tuareg-mode :language-id "ocaml") . ("ocamllsp"))
                                           '(tuareg-mode))
              :to-be 'tuareg-mode))
    (it "returns match when (car `entry') is a mix of both types"
      (expect (lsp-cody--should-add-cody-p '(((caml-mode :language-id "ocaml")
                                              (tuareg-mode :language-id "ocaml") reason-mode)
                                             . ("ocamllsp"))
                                           '(caml-mode))
              :to-be 'caml-mode))
    (it "returns nil otherwise"
      (expect (lsp-cody--should-add-cody-p '(((caml-mode :language-id "ocaml")
                                              (tuareg-mode :language-id "ocaml") reason-mode)
                                             . ("ocamllsp"))
                                           '(typescript-mode))
              :to-be nil)))



  (describe "(lsp-cody--add-cody entry)"
    (it "adds cody when (cdr `entry') is a single list of strings"
      (let* ((entry '(vimrc-mode . ("vim-language-server" "--stdio")))
             (expected-alternatives (lsp-cody-tests--get-completing-read-collection
                                     (funcall (eglot-alternatives
                                               (list lsp-cody-server-command
                                                     (cdr entry))))))
             (result (lsp-cody--add-cody entry)))
        (expect (car result) :to-equal (car entry))
        (expect (functionp (cdr result)) :to-be-truthy)
        (expect (lsp-cody-tests--get-completing-read-collection
                 (funcall (cdr result)))
                :to-have-same-items-as expected-alternatives)))
    (it "adds cody when (cdr `entry') is a function"
      (let* ((entry `((c-mode c-ts-mode c++-mode c++-ts-mode)
                      . ,(eglot-alternatives
                          '("clangd" "ccls"))))
             (expected-alternatives (lsp-cody-tests--get-completing-read-collection
                                     (funcall (eglot-alternatives
                                               (list lsp-cody-server-command
                                                     (cdr entry))))))
             (result (lsp-cody--add-cody entry)))
        (expect (car result) :to-equal (car entry))
        (expect (functionp (cdr result)) :to-be-truthy)
        (expect (lsp-cody-tests--get-completing-read-collection
                 (funcall (cdr result)))
                :to-have-same-items-as expected-alternatives)))))

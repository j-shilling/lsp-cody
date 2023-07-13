;;; lsp-cody.el --- A Client to Connect to the Cody LSP Gateway  -*- lexical-binding: t; -*-

;; URL: https://sourcegraph.com/
;; Version: 0.0.1
;; Keywords: languages
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (s "1.13.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; A Client to Connect to the Cody LSP Gateway
;;

;;; Code:

(require 'lsp-mode nil t)
(require 'eglot nil t)

(require 'map)

(require 'dash)

(defgroup lsp-cody nil
  "Client for the LSP Cody Gateway."
  :group 'tools
  :link '(url-link "https://sourcegraph.com/")
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-disable-lsp-mode-p nil
  "When non-nil, do not integrate with `lsp-mode'."
  :type 'boolean
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-disable-eglot-p nil
  "When non-nil, do not integrate with `eglot'."
  :type 'boolean
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-major-modes '(typescript-mode)
  "A list of major modes to use `lsp-cody' with."
  :type '(repeat string)
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-server-command '("cody-lsp-gateway" "--stdio")
  "Command to start cody-lsp-gateway."
  :type '(repeat string)
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defun lsp-cody--use-lsp-mode-p ()
  "Return t if `lsp-mode' is available and has not been disabled.

`lsp-cody' will integrate with `lsp-mode' if available, unless
`lsp-cody-disable-lsp-mode-p' is non-nil."
  (and (not lsp-cody-disable-lsp-mode-p)
       (featurep 'lsp-mode)))

(defun lsp-cody--use-eglot-p ()
  "Return t if `eglot' is available and has not been disabled.

`lsp-cody' will integrate with `eglot' if available, unless
`lsp-cody-disable-eglot-p' is non-nil."
  (and (not lsp-cody-disable-eglot-p)
       (featurep 'eglot)))

;;;###autoload
(defun lsp-cody-lsp-mode-initialize ()
  "Register cody-lsp-gateway with `lsp-mode'."
  (when (lsp-cody--use-lsp-mode-p)
    (lsp-dependency 'cody-lsp-gateway
                    '(:system "cody-lsp-gateway")
                    '(:npm :package "@j-shilling/cody-lsp-gateway"
                      :path "cody-lsp-gateway"))
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-stdio-connection
                       (lambda ()
                         `(,(or (executable-find
                                 (cl-first lsp-cody-server-command))
                                (lsp-package-path 'cody-lsp-gateway))
                           ,@(cl-rest lsp-cody-server-command))))
                      :add-on? t
                      :major-modes lsp-cody-major-modes
                      :priority 0
                      :server-id 'cody
                      :multi-root t
                      :download-server-fn
                      (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'cody-lsp-gateway
                                            callback error-callback))))))

(defun lsp-cody--add-to-eglot-server-programs (mode &optional server-programs)
  (cons `(,mode . ,lsp-cody-server-command)
        (or server-programs eglot-server-programs)))

(defun lsp-cody--should-add-cody-p (entry &optional major-modes)
  "Return t if `ENTRY' identifies a mode should use cody.

`ENTRY' should be the format of an entry in
`EGLOT-SERVER-PROGRAMS', but only the `CAR' is looked at.

This function returns through if one of the `MAJOR-MODES' would
match this entry. If `MAJOR-MODES' is not provided, it defaults
to `LSP-CODY-MAJOR-MODES'."
  (let ((mode (car entry))
        (modes (or major-modes lsp-cody-major-modes)))
    (cond
     ((symbolp mode)
      (car (member mode modes)))
     ((and (listp mode) (eq (cadr mode) :language-id))
      (car (member (car mode) modes)))
     ((listp mode)
      (-some (lambda (id)
               (lsp-cody--should-add-cody-p `(,id . nil) major-modes))
             mode))
     (t nil))))

(defun lsp-cody--add-cody (entry)
  "Add cody as to `ENTRY'.

Entry is a cons cell from `EGLOT-SERVER-PROGRAMS' whose cdr
identifies one or more servers that can be used with the modes
identified by its car. This function returns a new cons cell with
the same car and a cdr that adds cody as a valid server."
  (cl-destructuring-bind (mode . contact)
      entry
    (cl-typecase contact
      (function (error "Not implemented"))
      (t `(,mode . ,(eglot-alternatives
                     (cons lsp-cody-server-command
                           (list contact))))))))

;;;###autoload
(defun lsp-cody-eglot-initialize ()
  "Register cody-lsp-gateway with `eglot'."
  ;; TODO: This function needs to be improved: 1) It can't handle adding cody to
  ;; a mode that already has 'eglot-alternatives as its car; 2) It will
  ;; duplicate the cody entry every time it's run
  (when (lsp-cody--use-eglot-p)
    (dolist (mode lsp-cody-major-modes)
      (let ((original-value (map-elt eglot-server-programs mode)))
        (if original-value
            (add-to-list 'eglot-server-programs
                         `(,mode . (eglot-alternatives
                                    '(,original-value
                                      ,lsp-cody-server-command))))
          (add-to-list 'eglot-server-programs
                       `(,mode . ,lsp-cody-server-command)))))))

(lsp-cody-lsp-mode-initialize)
(lsp-cody-eglot-initialize)

(provide 'lsp-cody)
;;; lsp-cody.el ends here

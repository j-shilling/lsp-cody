;;; lsp-cody.el --- A Client to Connect to the Cody LSP Gateway  -*- lexical-binding: t; -*-

;; URL: https://sourcegraph.com/
;; Version: 0.0.1
;; Keywords: languages
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; A Client to Connect to the Cody LSP Gateway
;;

;;; Code:

(require 'lsp-mode nil t)

(defgroup lsp-cody nil
  "Client for the LSP Cody Gateway"
  :group 'tools
  :link '(url-link "https://sourcegraph.com/")
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-disable-lsp-mode-p nil
  "When non-nil, do not integrate with lsp-mode."
  :type 'boolean
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-major-modes '(typescript-mode)
  "A list of major modes to use lsp-cody with"
  :type '(repeat string)
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defcustom lsp-cody-server-command '("cody-lsp-gateway" "--stdio")
  "command to start cody-lsp-gateway."
  :type '(repeat string)
  :group 'lsp-cody
  :package-version '(lsp-cody . "0.0.1"))

(defun lsp-cody--use-lsp-mode-p ()
  "Return `t' if `lsp-mode' is available and has not been disabled.

`lsp-cody' will integrate with `lsp-mode' if available, unless
`lsp-cody-disable-lsp-mode-p' is non-nil."
  (and (not lsp-cody-disable-lsp-mode-p)
       (featurep 'lsp-mode)))

;;;###autoload
(defun lsp-cody-lsp-mode-initialize ()
  "Register cody-lsp-gateway with `lsp-mode'."
  (when (lsp-cody--use-lsp-mode-p)
    (lsp-dependency 'cody-lsp-gateway
                    '(:system "cody-lsp-gateway")
                    '(:npm :package "@j-shilling/cody-lsp-gateway"
                      :path "cody-lsp-gateway"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection
                                       (lambda ()
                                         `(,(or (executable-find (cl-first lsp-cody-server-command))
                                                (lsp-package-path 'cody-lsp-gateway))
                                           ,@(cl-rest lsp-cody-server-command))))
                      :add-on? t
                      :major-modes lsp-cody-major-modes
                      :priority 0
                      :server-id 'cody
                      :multi-root t
                      :download-server-fn (lambda (_client callback error-callback _update?)
                                            (lsp-package-ensure 'cody-lsp-gateway
                                                                callback error-callback))))))

(lsp-cody-lsp-mode-initialize)

(provide 'lsp-cody)
;;; lsp-cody.el ends here

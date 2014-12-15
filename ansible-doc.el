;;; ansible-doc.el --- Ansible documentation Minor Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

;; Author: Sebastian Wiesner <swiesner@lunaryorn>
;; URL: https://github.com/lunaryorn/ansible-doc.el
;; Keywords: tools, help
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ansible Documentation Minor Mode for GNU Emacs.
;;
;; Provide `ansible-doc-mode' which enables documentation lookup for Ansible.
;;
;; Enable with:
;;
;; (add-hook 'yaml-mode-hook #'ansible-doc-mode)

;;; Code:

(require 'button)

;;; Bookmark integration
(defvar bookmark-make-record-function)
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))
(declare-function bookmark-get-bookmark-record "bookmark" (bmk))

(eval-and-compile
  ;; `defvar-local' Emacs 24.2 and below
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  (unless (fboundp 'setq-local)
    ;; `setq-local' for Emacs 24.2 and below
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      `(set (make-local-variable ',var) ,val))))

(defgroup ansible nil
  "Ansible configuration and provisioning system."
  :group 'languages
  :prefix "ansible-")

(defgroup ansible-doc nil
  "Ansible documentation lookup."
  :group 'ansible
  :prefix 'ansible-doc)

(defface ansible-doc-header '((t :inherit bold))
  "Face for Ansible documentation header."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-section '((t :inherit font-lock-keyword-face))
  "Face for Ansible section headings."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-option '((t :inherit font-lock-function-name-face))
  "Face for options in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-mandatory-option '((t :inherit font-lock-type-face))
  "Face for mandatory options in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-label '((t :inherit font-lock-doc-face))
  "Face for a label in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-default '((t :inherit font-lock-constant-face))
  "Face for default values in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-choices '((t :inherit font-lock-constant-face))
  "Face for choice values in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-literal '((t :inherit font-lock-string-face))
  "Face for literals in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defface ansible-doc-module-xref '((t :inherit font-lock-type-face
                                      :underline t))
  "Face for module references in Ansible documentation."
  :group 'ansible-doc
  :package-version '(ansible-doc . "0.2"))

(defconst ansible-doc--buffer-name "*ansible-doc %s*"
  "Template for the names of Ansible Doc buffers.")

(defvar ansible-doc--modules nil
  "A list of all known Ansible modules.")

(defun ansible-doc-modules ()
  "Get a list of all known Ansible modules."
  (unless ansible-doc--modules
    (message "Finding Ansible modules...")
    (with-temp-buffer
      (when (with-demoted-errors "Error while finding Ansible modules: %S"
              (let ((retcode (call-process "ansible-doc" nil t nil "--list")))
                (unless (equal retcode 0)
                  (error "Command ansible-doc --list failed with code %s, returned %s"
                         retcode (buffer-string)))))
        (goto-char (point-max))
        (while (re-search-backward (rx line-start
                                       (group (one-or-more (not (any space))))
                                       (any space)
                                       (one-or-more not-newline)
                                       line-end)
                                   nil 'noerror)
          (push (match-string 1) ansible-doc--modules)))))
  ansible-doc--modules)

(defun ansible-doc-read-module (prompt)
  "Read a Ansible module name from minibuffer with PROMPT."
  (let* ((modules (ansible-doc-modules))
         (symbol (thing-at-point 'symbol))
         (default (if (member symbol modules) symbol nil))
         ;; If we have no modules available, we don't require a match.
         (reply (completing-read prompt modules nil (not (null modules))
                                 nil nil default)))
    (if (string= reply "") default reply)))

(defun ansible-doc-follow-module-xref (button)
  "Follow a module xref at BUTTON."
  (let ((module (button-get button 'ansible-module)))
    (ansible-doc module)))

(define-button-type 'ansible-doc-module-xref
  'face 'ansible-doc-module-xref
  'action #'ansible-doc-follow-module-xref
  'help-echo "mouse-2, RET: visit module")

(defvar-local ansible-module-doc-current-module nil
  "The module documented by this buffer.")

(defun ansible-module-doc-current-module ()
  "Get the current module or error."
  (let ((module ansible-module-doc-current-module))
    (unless module
      (error "This buffer does not document an Ansible module"))
    module))

(defconst ansible-module-doc-font-lock-keywords
  `((,(rx buffer-start "> " (1+ not-newline) line-end) 0 'ansible-doc-header)
    (,(rx line-start "Options (" (1+ not-newline) "):" line-end)
     0 'ansible-doc-section)
    (,(rx line-start "Notes:  ") 0 'ansible-doc-section)
    (,(rx line-start "- " (1+ (not (any space))) line-end)
     0 'ansible-doc-option)
    (,(rx line-start "= " (1+ (not (any space))) line-end)
     0 'ansible-doc-mandatory-option)
    (,(rx "[" (group "Default:") (1+ (any space))
          (group (1+ (not (any "]")))) "]")
     (1 'ansible-doc-label)
     (2 'ansible-doc-default))
    (,(rx "(" (group "Choices:") (1+ (any space))
          (group (1+ (not (any ")")))) ")")
     (1 'ansible-doc-label)
     (2 'ansible-doc-choices))
    (,(rx "`" (group (1+ (not (any "'")))) "'") 1 'ansible-doc-literal)
    (ansible-doc-propertize-module-xrefs . nil))
  "Font lock keywords for Ansible module documentation.")

(defconst ansible-module-doc-imenu-generic-expression
  `(("Options" ,(rx line-start (or "-" "=") " "
                    (group (1+ (not (any space)))) line-end) 1)))

(defun ansible-doc-propertize-module-xrefs (limit)
  "Propertize all module xrefs between point and LIMIT."
  (remove-overlays (point) limit)
  (while (re-search-forward (rx "[" (group (1+ (not (any space "]")))) "]")
                            limit 'noerror)
    (make-button (match-beginning 0)
                 (match-end 0)
                 'type 'ansible-doc-module-xref
                 'ansible-module (match-string 1))))

(defun ansible-module-doc-revert-buffer (_ignore-auto noconfirm)
  "Revert an Ansible Module doc buffer."
  (let ((module (ansible-module-doc-current-module))
        (old-pos (point)))
    (when (or noconfirm
              (y-or-n-p (format "Reload documentation for %s? " module)))
      (message "Loading documentation for module %s" module)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process "ansible-doc" nil t t module))
      (font-lock-ensure)
      (force-mode-line-update)
      (goto-char old-pos))))

(defun ansible-module-doc-make-bookmark-record ()
  "Make a bookmark record for the current Ansible module."
  (let ((module (ansible-module-doc-current-module)))
    `(,(format "Ansible module %s" module)
      ,@(bookmark-make-record-default 'no-file)
      (ansible-module . ,module)
      (handler . ansible-module-doc-bookmark-jump))))

(defun ansible-module-doc-bookmark-jump (bookmark)
  "Jump to an Ansible module BOOKMARK."
  ;; Lets just obtain a buffer for the module, and delegate the rest to
  ;; bookmark.el
  (let* ((module (bookmark-prop-get bookmark 'ansible-module))
         (buffer (ansible-doc-buffer module)))
    (bookmark-default-handler
     `("" (buffer . ,buffer) . ,(bookmark-get-bookmark-record bookmark)))))

(defvar ansible-module-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map)
  "Keymap for `ansible-module-doc-mode'.")

(define-derived-mode ansible-module-doc-mode special-mode "ADoc"
  "A major mode for Ansible module documentation.

\\{ansible-module-doc-mode-map}"
  (setq buffer-auto-save-file-name nil
        truncate-lines t
        buffer-read-only t
        mode-line-buffer-identification
        (list (default-value 'mode-line-buffer-identification)
              " {" 'ansible-module-doc-current-module "}")
        font-lock-defaults '((ansible-module-doc-font-lock-keywords) t nil)
        imenu-generic-expression ansible-module-doc-imenu-generic-expression)
  (setq-local revert-buffer-function #'ansible-module-doc-revert-buffer)
  (setq-local bookmark-make-record-function
              #'ansible-module-doc-make-bookmark-record))

(defun ansible-doc-buffer (module)
  "Create a documentation buffer for MODULE."
  (let* ((buffer-name (format ansible-doc--buffer-name module))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (ansible-module-doc-mode)
        (setq ansible-module-doc-current-module module)
        (revert-buffer nil 'noconfirm)))
    buffer))

;;;###autoload
(defun ansible-doc (module)
  "Show ansible documentation for MODULE."
  (interactive
   (list (ansible-doc-read-module "Documentation for Ansible Module: ")))
  (pop-to-buffer (ansible-doc-buffer module)))

(defvar ansible-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ?") #'ansible-doc)
    map)
  "Keymap for `ansible-mode'.")

;;;###autoload
(define-minor-mode ansible-doc-mode
  "Minor mode for Ansible documentation.

When called interactively, toggle `ansible-doc-mode'.  With
prefix ARG, enable `ansible-doc-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ansible-doc-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ansible-doc-mode'.  Otherwise behave as if called interactively.

In `ansible-doc-mode' provide the following keybindings for
Ansible documentation lookup:

\\{ansible-doc-mode-map}"
  :init-value nil
  :keymap ansible-doc-mode-map
  :lighter " ADoc"
  :group 'ansible-doc
  :require 'ansible-doc)

(provide 'ansible-doc)

;;; ansible-doc.el ends here

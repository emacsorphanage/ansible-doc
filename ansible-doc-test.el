;;; ansible-doc-test.el --- Ansible Doc: Unit tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Unit tests for Ansible Doc Mode.

;;; Code:

(require 'ert)

(require 'ansible-doc)

(ert-deftest ansible-doc-modules/is-cached ()
  (skip-unless (executable-find "ansible-doc"))
  (should (eq (ansible-doc-modules) ansible-doc--modules)))

(ert-deftest ansible-doc-modules/returns-ansible-modules ()
  (skip-unless (executable-find "ansible-doc"))
  (should (ansible-doc-modules))
  ;; Same basic sanity tests
  (dolist (mod '("apt" "yum" "user"))
    (should (member mod (ansible-doc-modules)))))

(provide 'ansible-doc-test)

;;; ansible-doc-test.el ends here

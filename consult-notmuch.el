;;; consult-notmuch.el --- Notmuch search using consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  jao

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz
;; Keywords: mail
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (consult "0.5") (notmuch "0.21"))
;; Homepage: https://codeberg.org/jao/consult-notmuch

;; This implementation is very heavily inspired by Alexander Fu Xi's
;; for counsel: https://github.com/fuxialexander/counsel-notmuch/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides two commands using consult to query notmuch
;; emails and present results either as single emails
;; (`consult-notmuch') or full trees (`consult-notmuch-tree').

;;; Code:

(require 'consult)
(require 'notmuch)
(require 's)

(defgroup consult-notmuch nil
  "Options for consult-notmuch."
  :group 'Notmuch)

(defcustom consult-notmuch-command "notmuch search *ARG*"
  "Command to perform notmuch search."
  :type 'string
  :group 'consult-notmuch)

(defface consult-notmuch-date-face
  '((t :inherit notmuch-search-date :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'consult-notmuch)

(defface consult-notmuch-count-face
  '((t :inherit notmuch-search-count :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'consult-notmuch)

(defface consult-notmuch-people-face
  '((t :inherit notmuch-search-matching-authors :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'consult-notmuch)

(defface consult-notmuch-subject-face
  '((t :inherit notmuch-search-subject :background nil))
  "Default face used in tree mode face for matching messages"
  :group 'consult-notmuch)

(defvar consult-notmuch-history nil
  "History for `consult-notmuch'.")

(defun consult-notmuch--tree (thread &optional initial-input)
  "Open resulting THREAD in ‘notmuch-tree’ view with INITIAL-INPUT."
  (let ((thread-id (car (split-string thread "\\ +"))))
    (notmuch-tree thread-id initial-input nil)))

(defun consult-notmuch--show (thread)
  "Open resulting THREAD in ‘notmuch-show’ view."
  (let ((title (concat "*consult-notmuch-show*" (substring thread 24)))
        (thread-id (car (split-string thread "\\ +"))))
    (notmuch-show thread-id nil nil nil title)))

(defun consult-notmuch--transformer (str)
  "Transform STR to notmuch display style."
  (when (string-match "thread:" str)
    (let* ((thread-id (car (split-string str "\\ +")))
           (date (substring str 24 37))
           (mid (substring str 24))
           (mat0 (string-match "[[]" mid))
           (mat1 (string-match "[]]" mid))
           (mat (substring mid mat0 (1+ mat1)))
           (people (truncate-string-to-width
                    (string-trim (nth 1 (split-string mid "[];]"))) 20))
           (subject (truncate-string-to-width
                     (string-trim (nth 1 (split-string mid "[;]")))
                     (- (frame-width) 32))))
      (format "%s %s\t%10s\t%20s\t%s"
              (propertize thread-id 'invisible t)
              (propertize date 'face 'consult-notmuch-date-face)
              (propertize mat 'face 'consult-notmuch-count-face)
              (propertize people 'face 'consult-notmuch-people-face)
              (propertize subject 'face 'consult-notmuch-subject-face)))))

(defun consult-notmuch--search ()
  "Perform an asynchronous notmuch search via `consult--read'."
  (consult--read (consult--async-command consult-notmuch-command
                   (consult--async-map #'consult-notmuch--transformer))
                 :prompt "Notmuch search: "
                 :require-match t
                 :history 'consult-notmuch-history
                 :category 'notmuch-result))

;;;###autoload
(defun consult-notmuch ()
  "Search for your email in notmuch, showing single messages."
  (interactive)
  (consult-notmuch--show (consult-notmuch--search)))

;;;###autoload
(defun consult-notmuch-tree ()
  "Search for your email in notmuch, showing full candidate tree."
  (interactive)
  (consult-notmuch--tree (consult-notmuch--search)))

(provide 'consult-notmuch)
;;; consult-notmuch.el ends here

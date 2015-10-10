;;; sallet-autobookmarks.el --- Autobookmarks sallet -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 10th October 2015
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(require 'sallet-source)
(require 'sallet-state)
(require 'sallet-filters)
(require 'sallet-faces)

(defun sallet-filter-autobookmark-path-substring (candidates indices pattern)
  "Keep autobookmark CANDIDATES substring-matching PATTERN against file path."
  (let ((quoted-pattern (regexp-quote pattern)))
    (--keep (sallet-predicate-path-regexp (cadr (sallet-aref candidates it)) it quoted-pattern) indices)))

(defun sallet-filter-autobookmark-path-flx (candidates indices pattern)
  "Keep autobookmark CANDIDATES flx-matching PATTERN against file path."
  (--keep (sallet-predicate-path-flx (cadr (sallet-aref candidates it)) it pattern) indices))

;; TODO: add a matcher for major mode based on extension and
;; auto-mode-alist
(defun sallet-autobookmarks-matcher (candidates state)
  "Match an autobookmark candidate using special rules.

First, the prompt is split on whitespace.  This creates a list of
patterns.

A pattern starting with / flx-matches against the path to the
file bookmark represents.

A pattern starting with // substring-matches against the path to the
file bookmark represents.

Any other non-prefixed pattern is matched using the following rules:

- If the pattern is first of this type at the prompt, it is
  flx-matched against the bookmark name.
- All the following patterns are substring matched against the
  bookmark name."
  (let* ((prompt (sallet-state-get-prompt state))
         (indices (sallet-make-candidate-indices candidates)))
    (sallet-compose-filters-by-pattern
     '(("\\`//\\(.*\\)" 1 sallet-filter-autobookmark-path-substring)
       ("\\`/\\(.*\\)" 1 sallet-filter-autobookmark-path-flx)
       (t sallet-filter-flx-then-substring))
     candidates
     indices
     prompt)))

;; TODO: improve
(defun sallet-autobookmarks-renderer (candidate _ user-data)
  "Render an `autobookmarks-mode' candidate."
  (-let* (((name path . data) candidate)
          ((&alist 'visits visits) data))
    (format "%-55s%5s  %s"
            (sallet-compose-fontifiers
             ;; TODO: create a "fontify flx after regexp" function to
             ;; simplify this common pattern
             (propertize name 'face 'sallet-recentf-buffer-name) user-data
             '(sallet-fontify-regexp-matches . :regexp-matches)
             '(sallet-fontify-flx-matches . :flx-matches))
            (propertize (if visits (int-to-string visits) "0") 'face 'sallet-buffer-size)
            (abbreviate-file-name
             (sallet-compose-fontifiers
              (propertize path 'face 'sallet-recentf-file-path) user-data
              '(sallet-fontify-regexp-matches . :regexp-matches-path)
              '(sallet-fontify-flx-matches . :flx-matches-path))))))

(sallet-defsource autobookmarks nil
  "Files saved with `autobookmarks-mode'."
  (candidates (lambda ()
                (require 'autobookmarks)
                (-keep
                 (lambda (bookmark)
                   (-when-let (name
                               (cond
                                ((assoc 'defaults (cdr bookmark))
                                 (cadr (assoc 'defaults (cdr bookmark))))
                                ((assoc 'filename (cdr bookmark))
                                 (f-filename
                                  (cdr (assoc 'filename (cdr bookmark)))))))
                     (cons name bookmark)))
                 (-sort (-lambda ((_ . (&alist 'time a))
                                  (_ . (&alist 'time b)))
                          (time-less-p b a))
                        (abm-recent-buffers)))))
  (matcher sallet-autobookmarks-matcher)
  (renderer sallet-autobookmarks-renderer)
  (action (-lambda ((_ . x)) (abm-restore-killed-buffer x)))
  (header "Autobookmarks"))

(provide 'sallet-autobookmarks)
;;; sallet-autobookmarks.el ends here

;;; org-sticky-header.el --- Show off-screen Org heading at top of window -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sticky-header
;; Version: 0.1.0-pre
;; Package-Requires: ((emacs "24.4"))
;; Keywords: hypermedia, outlines, Org

;;; Commentary:

;; This package displays in the header-line the Org heading for the
;; node that's at the top of the window.  This way, if the heading for
;; the text at the top of the window is beyond the top of the window,
;; you don't forget which heading the text belongs to.

;; The code is very simple and is based on `semantic-stickyfunc-mode'.

;;; Installation:

;; Install from MELPA and run `org-sticky-header-mode'.

;; To install manually, put this file in your `load-path', require
;; `org-sticky-header' in your init file, and run the same command.

;; You probably want to add `org-sticky-func-mode' to your `org-mode-hook'.

;; If `org-startup-indented' is enabled, the
;; `org-sticky-header-prefix' will be automatically set to match the
;; `org-indent-mode' prefixes; otherwise you may wish to customize it.

;;; License:

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

;;; Code:

;;;; Variables

(defvar org-sticky-header-old-hlf nil
  "Value of the header line when entering org-sticky-header mode.")

(defconst org-sticky-header-header-line-format
  '(:eval (list
           (propertize " " 'display '((space :align-to 0)))
           (org-sticky-header--fetch-stickyline)))
  "The header line format used by stickyfunc mode.")

(defgroup org-sticky-header nil
  "Options for `org-sticky-header-mode'."
  :group 'org)

(defcustom org-sticky-header-full-path nil
  "Show the full outline path."
  :type '(radio (const :tag "Show only current heading" nil)
                (const :tag "Show full outline path to current heading" full)
                (const :tag "Show full outline path, but reversed so current heading is first" reversed)))

(defcustom org-sticky-header-always-show-header t
  "Show the header even when the top line of the buffer is a heading.
When this is on, and the top line of the buffer is a heading,
you'll see the heading shown twice: once in the header and once
in the buffer.  But since the header can look different than the
heading (i.e. it can show the full path), it shouldn't
necessarily disappear. If you use full-path display, you probably
want this on, but if you only display the current heading, you
might prefer to turn it off.  "
  :type 'boolean)

(defcustom org-sticky-header-prefix (when org-startup-indented
                                      ;; Use the default function if `org-indent-mode' is enabled globally
                                      ;; FIXME: What if `org-adapt-indentation' is used to actually indent text?
                                      'org-sticky-header--indent-prefix)
  "Prefix to display before heading in header line.
`org-indent-mode' users should use the default function.  Custom
functions will be run with point on a heading."
  :type '(choice (function-item :tag "`org-indent-mode' indentation" org-sticky-header--indent-prefix)
                 (string :tag "Custom string" :value "   ")
                 (function :tag "Custom function which returns a string")
                 (const :tag "None" nil)))

(defcustom org-sticky-header-outline-path-separator "/"
  "String displayed between elements of outline paths."
  :type 'string)

(defcustom org-sticky-header-outline-path-reversed-separator "\\"
  "String displayed between elements of reversed outline paths."
  :type 'string)

;;;; Functions

(defun org-sticky-header--fetch-stickyline ()
  "Return string of Org heading or outline path for display in header line."
  (save-excursion
    (goto-char (window-start))
    (when (org-before-first-heading-p)
      (outline-next-heading))
    (when (or org-sticky-header-always-show-header
              (not (org-at-heading-p)))
      ;; Header should be shown
      (when (fboundp 'org-inlinetask-in-task-p)
        ;; Skip inline tasks
        (while (and (org-back-to-heading)
                    (org-inlinetask-in-task-p))
          (forward-line -1)))
      (pcase org-sticky-header-full-path
        ('nil (concat (org-sticky-header--get-prefix) (org-get-heading t t)))
        ('full (concat (org-sticky-header--get-prefix) (org-format-outline-path (org-get-outline-path t) (window-width) nil
                                                                                org-sticky-header-outline-path-separator)))
        ('reversed
         ;; Concat and truncate path ourselves since it's reversed.
         ;; Using ASCII 31 "unit separator" (Unicode "Information separator one") as separator
         (let ((s (concat (org-sticky-header--get-prefix)
                          (mapconcat 'identity
                                     (nreverse (org-split-string (org-format-outline-path (org-get-outline-path t)
                                                                                          1000 nil "")
                                                                 ""))
                                     org-sticky-header-outline-path-reversed-separator))))
           (if (> (length s) (window-width))
               (concat (substring s 0 (- (window-width) 2)) "..")
             s)))))))

(defun org-sticky-header--get-prefix ()
  "Return prefix string depending on value of `org-sticky-header-prefix'."
  (typecase org-sticky-header-prefix
    (function (funcall org-sticky-header-prefix))
    (string org-sticky-header-prefix)
    (nil nil)))

(defun org-sticky-header--indent-prefix ()
  "Return indentation prefix for heading at point.
`org-indent-mode' must be activated to use this function, which
may be enabled globally by customizing `org-startup-indented'."
  ;; Copied from `org-indent-set-line-properties'
  (let* ((level (org-current-level))
         (stars (if (<= level 1) ""
                  (make-string (* (1- org-indent-indentation-per-level)
                                  (1- level))
                               ?*))))
    (concat (org-add-props (concat stars (make-string level ?*))
                nil 'face 'org-indent)
            (and (> level 0)
                 (char-to-string org-indent-boundary-char)))))

;;;; Minor mode

;;;###autoload
(define-minor-mode org-sticky-header-mode
  "Minor mode to show the current Org heading in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Return non-nil if the minor mode is enabled."
  :group 'org
  (if org-sticky-header-mode
      (progn
        (when (and (local-variable-p 'header-line-format (current-buffer))
                   (not (eq header-line-format org-sticky-header-header-line-format)))
          ;; Save previous buffer local value of header line format.
          (set (make-local-variable 'org-sticky-header-old-hlf)
               header-line-format))
        ;; Enable the mode
        (setq header-line-format org-sticky-header-header-line-format))
    ;; Disable mode
    (when (eq header-line-format org-sticky-header-header-line-format)
      ;; Restore previous buffer local value of header line format if
      ;; the current one is the sticky func one.
      (kill-local-variable 'header-line-format)
      (when (local-variable-p 'org-sticky-header-old-hlf (current-buffer))
        (setq header-line-format org-sticky-header-old-hlf)
        (kill-local-variable 'org-sticky-header-old-hlf)))))

(provide 'org-sticky-header)

;;; org-sticky-header.el ends here

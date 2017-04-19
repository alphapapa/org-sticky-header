;;; org-sticky-header.el --- Show off-screen Org heading at top of window -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sticky-header
;; Version: 0.1.0-pre
;; Package-Requires: ((emacs "24.4") (dash "2.13.0") (s "1.10.0"))
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

(require 'dash)
(require 's)

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

(defcustom org-sticky-header-prefix "   "
  "Prefix to display before heading in header line.
You can adjust this to help align the heading according to your
face settings.  (It would be nice to automate this.  Suggestions
welcome.)"
  :type 'string)

(defun org-sticky-header--fetch-stickyline ()
  "Make the heading at the top of the current window sticky.
Capture its heading line, and place it in the header line.
If there is no heading, disable the header line."
  (save-excursion
    (goto-char (window-start))
    (when (or org-sticky-header-always-show-header
              (not (org-at-heading-p)))
      (org-back-to-heading)
      (pcase org-sticky-header-full-path
        ('nil (concat org-sticky-header-prefix (org-get-heading t t)))
        ('full (concat org-sticky-header-prefix (org-format-outline-path (org-get-outline-path t) (window-width))))
        ('reversed (concat org-sticky-header-prefix
                           ;; Using "🐱" "CAT FACE" as separator character. It needs to be a single character,
                           ;; otherwise it could get truncated and cause splitting to fail, and the chances of this
                           ;; character being in a heading is low enough...right?
                           (->> (org-format-outline-path (org-get-outline-path t) (window-width) nil "🐱")
                                (s-split "🐱")
                                (nreverse)
                                (s-join "\\"))))))))

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

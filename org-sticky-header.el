;;; org-sticky-header.el --- Show off-screen Org heading at top of window -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-sticky-header
;; Version: 1.1
;; Package-Requires: ((emacs "24.4") (org "8.3.5"))
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

;; You probably want to add `org-sticky-header-mode' to your `org-mode-hook'.

;; By default, the line will be indented like a real headline.  To
;; change this, configure `org-sticky-header-prefix'.

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

;;;; Requirements

(require 'cl-lib)
(require 'org)

;;;; Variables

(defvar org-sticky-header-old-hlf nil
  "Value of the header line when entering org-sticky-header mode.")

(defvar-local org-sticky-header-stickyline nil
  "Value of header line")
(put 'org-sticky-header-stickyline 'risky-local-variable t)

(defvar org-sticky-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<header-line> <mouse-1>") #'org-sticky-header-goto-heading)
    map)
  "Keymap used in header line.")

(defconst org-sticky-header-header-line-format
  '(:eval (progn
            (setq org-sticky-header-stickyline
                  (propertize (org-sticky-header--fetch-stickyline)
                              'keymap org-sticky-header-keymap))
            (list
             (propertize " " 'display '((space :align-to 0)))
             'org-sticky-header-stickyline)))
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
necessarily disappear.  If you use full-path display, you
probably want this on, but if you only display the current
heading, you might prefer to turn it off."
  :type 'boolean)

(defcustom org-sticky-header-prefix 'org-sticky-header--indent-prefix
  "Prefix to display before heading in header line.
`org-indent-mode' users should use the default function.  Custom
functions will be run with point on a heading."
  :type '(choice (function-item :tag "Like real headline" org-sticky-header--indent-prefix)
                 (string :tag "Custom string" :value "   ")
                 (function :tag "Custom function which returns a string")
                 (const :tag "None" nil)))

(defcustom org-sticky-header-outline-path-separator " ❯ "
  "String displayed between elements of outline paths."
  :type 'string)

(defcustom org-sticky-header-outline-path-reversed-separator " ❮ "
  "String displayed between elements of reversed outline paths."
  :type 'string)

(defcustom org-sticky-header-heading-star "*"
  "String to show before heading.
By default, show an asterisk, like in an Org buffer.  Changing
this to something else may help distinguish the header line from
headings in the buffer when `org-sticky-header-always-show-header'
is enabled."
  :type 'string)

(defcustom org-sticky-header-show-keyword t
  "Show to-do keyword before heading text."
  :type 'boolean)

(defcustom org-sticky-header-show-priority t
  "Show priority before heading text."
  :type 'boolean)

(defcustom org-sticky-header-default-face nil
  "Using `header-line' default theme instead of `org-level-faces'."
  :type 'boolean)

;;;; Functions

(defun org-sticky-header-goto-heading (event)
  "Go to heading displayed in sticky header (for click event EVENT)."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (goto-char (window-start))
    (unless (org-before-first-heading-p)
      (org-back-to-heading))))

(defun org-sticky-header--fetch-stickyline ()
  "Return string of Org heading or outline path for display in header line."
  (org-with-wide-buffer
   (goto-char (window-start))
   (if (org-before-first-heading-p)
       ""
     (progn
       ;; No non-header lines above top displayed header
       (when (or org-sticky-header-always-show-header
                 (not (org-at-heading-p)))
         ;; Header should be shown
         (when (fboundp 'org-inlinetask-in-task-p)
           ;; Skip inline tasks
           (while (and (org-back-to-heading)
                       (org-inlinetask-in-task-p))
             (forward-line -1)))
         (cond
          ;; TODO: Update minimum Emacs version and use `pcase'.
          ((null org-sticky-header-full-path)
           (concat (org-sticky-header--get-prefix)
                   (org-sticky-header--heading-string)))
          ((eq org-sticky-header-full-path 'full)
           (concat (org-sticky-header--get-prefix)
                   (mapconcat 'identity
                              (nreverse
                               (save-excursion
                                 (cl-loop collect (org-sticky-header--heading-string)
                                          while (org-up-heading-safe))))
                              org-sticky-header-outline-path-separator)))
          ((eq org-sticky-header-full-path 'reversed)
           (let ((s (concat
                     (org-sticky-header--get-prefix)
                     (mapconcat 'identity
                                (save-excursion
                                  (cl-loop collect (org-sticky-header--heading-string)
                                           while (org-up-heading-safe)))
                                org-sticky-header-outline-path-reversed-separator))))
             (if (> (string-width s) (window-width))
                 (concat (substring s 0 (- (window-width) 2))
                         "..")
               s)))
          (t "")))))))

(defun org-sticky-header--heading-string ()
  "Return string for heading at point.
According to `org-sticky-header' options."
  (pcase-let* ((`(,level _ ,keyword ,priority ,heading) (org-heading-components))
         (face (unless org-sticky-header-default-face (nth (1- level) org-level-faces))))
    (concat
      (when (and org-sticky-header-show-keyword keyword)
	 (concat (propertize keyword
		 'face `((:foreground ,(face-foreground (org-get-todo-face keyword) nil 1)) ,face))
	     " "))
      (when (and org-sticky-header-show-priority priority)
	 (concat (propertize (concat "[#" (char-to-string priority) "]")
		 'face `((:foreground ,(face-foreground 'org-priority nil 1)) ,face))
	     " "))
      (propertize (org-link-display-format heading) 'face face))))

(defun org-sticky-header--get-prefix ()
  "Return prefix string depending on value of `org-sticky-header-prefix'."
  (cl-typecase org-sticky-header-prefix
    (function (funcall org-sticky-header-prefix))
    (string org-sticky-header-prefix)
    (nil nil)))

(defun org-sticky-header--indent-prefix ()
  "Return indentation prefix for heading at point.
This will do the right thing both with and without `org-indent-mode'."
  ;; Modelled after `org-indent-set-line-properties'
  (let* ((level (org-current-level))
	 (indent-mode (bound-and-true-p org-indent-mode))
	 (npre (if (<= level 1) 0
		 (+ (if indent-mode
			(* (1- org-indent-indentation-per-level)
			   (1- level))
		      0)
		    level -1)))
	 (prefix (concat (make-string npre (if indent-mode ?\ ?*)) org-sticky-header-heading-star " ")))
    (org-add-props prefix nil 'face
		   (if org-cycle-level-faces
		       (setq org-f (nth (% (1- level) org-n-level-faces) org-level-faces))
		     (setq org-f (nth (1- (min level org-n-level-faces)) org-level-faces))))))

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

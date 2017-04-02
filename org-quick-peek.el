;;; org-quick-peek.el --- Quick inline peeks at agenda items and linked nodes -*- lexical-binding: t -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-quick-peek
;; Version: 0.1-pre
;; Package-Requires: ((emacs "24.4") (quick-peek "1.0") (dash "2.12") (s "1.10.0))
;; Keywords: navigation, outlines, org

;;; Commentary:

;; This package lets you quickly "peek" at the contents of Org nodes
;; that are off-screen, using the `quick-peek' package by
;; Clément Pit-Claudel: <https://github.com/cpitclaudel/quick-peek/>

;;; Usage:

;; Run `org-quick-peek-mode' to activate these commands:

;; + `org-quick-peek-link' shows the contents of a linked node when
;;    the point is on an Org link that links to another Org heading.

;; + `org-quick-peek-agenda-current-item' shows the contents of the
;;   currently selected item in the Agenda.

;; + `org-quick-peek-agenda-all' shows the contents of every item in
;;   the Agenda.  This looks nicer than `org-agenda-entry-text-mode',
;;   but it may be much slower in large Agenda buffers.

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
(require 'quick-peek)
(require 'dash)
(require 's)

;;;; Customization

(defgroup org-quick-peek nil
  "Settings for `org-quick-peek'."
  :group 'org)

(defcustom org-quick-peek-agenda-peek-key (kbd "M-p")
  "Key to peek at Agenda items."
  :type 'key-sequence
  :set 'org-quick-peek--set-key-bind)

(defcustom org-quick-peek-show-lines 10
  "Show this many lines of entry contents."
  :type 'integer)

(defvar org-quick-peek-agenda-peek-key-previous-bind nil
  "Command that `org-quick-peek-agenda-peek-key' was bound to before activating `org-quick-peek-mode'.")

;;;; Functions

;;;;; Commands

(defun org-quick-peek-link ()
  "Show quick peek of Org heading linked at point."
  (interactive)
  (unless (> (quick-peek-hide (point)) 0)
    ;; Showing, not hiding
    (save-excursion
      (let ((quick-peek-background-face '((t :background "black")))
            (org-show-hierarchy-above nil)
            (org-show-following-heading nil)
            (org-show-entry-below nil)
            (org-show-siblings nil)
            link type marker)

        ;; From org.el
        (when (and (looking-at org-complex-heading-regexp))
          ;; Move point to the beginning of the heading text so org-in-regexp
          ;; has a chance to match a link
          (goto-char (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
                          (point-at-eol))))

        ;; From org.el
        (when (org-in-regexp org-bracket-link-regexp 1)
          ;; Get marker to linked heading
          (setq link (org-extract-attributes
                      (org-link-unescape (org-match-string-no-properties 1))))
          (while (string-match " *\n *" link)
            (setq link (replace-match " " t t link)))
          (setq link (org-link-expand-abbrev link))
          (when (string-match org-link-re-with-space3 link)
            (setq type (match-string 1 link)
                  path (match-string 2 link)))
          (when (and path
                     (string= type "id"))
            (setq marker (org-id-find path 'marker)))

          (save-current-buffer  ; Not sure if necessary
            (unless marker
              ;; Hopefully this will avoid calling org-open-at-point
              ;; most of the time, because org-open-at-point calls
              ;; org-show-context, which unnecessarily reveals hidden
              ;; nodes
              (org-open-at-point)
              (setq marker (point-marker))))
          (quick-peek-show (org-agenda-get-some-entry-text marker org-quick-peek-show-lines)))))))

(defun org-quick-peek-agenda-current-item ()
  "Show quick peek of current item, or hide if one is already shown."
  (interactive)
  (unless (> (quick-peek-hide (point)) 0)
    (org-quick-peek--agenda-show)))

(defun org-quick-peek-agenda-all ()
  "Show/hide quick peek of all agenda items."
  (interactive)
  (unless (> (quick-peek-hide (point)) 0)
    (goto-char (point-min))
    (cl-loop with lines = (count-lines (point-min) (point-max))
             while (< (line-number-at-pos) lines)
             do (org-quick-peek--agenda-show :quiet t)
             (forward-line))))

;;;;; Support functions

(cl-defun org-quick-peek--agenda-show (&key (quiet nil))
  "Show quick peek at current line."
  (-if-let* ((m (org-get-at-bol 'org-hd-marker))
             (text (org-quick-peek--s-trim-lines (org-agenda-get-some-entry-text m org-quick-peek-show-lines))))
      (if (s-present? text)
          (quick-peek-show text)
        (unless quiet
          (minibuffer-message "Entry has no text.")))))

(defun org-quick-peek--org-cycle-after (&optional args)
  "Function to run after `org-cycle' to show peeks of linked headings."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-bracket-link-regexp (line-end-position) t)
      (org-quick-peek-link))))

(defun org-quick-peek--s-trim-lines (s)
  "Trim each line in string S."
  (s-join "\n" (-map 's-trim (s-lines s))))

;;;;;; Key binding

(defun org-quick-peek--set-key-bind (symbol key)
  "Set SYMBOL to KEY using `set-default', adjusting keymaps if `org-quick-peek-mode' is active."
  ;; TODO: Test this more.  Seems to work, but not completely sure
  ;; it's removing the old bind when changed.
  (when org-quick-peek-mode
    (org-quick-peek--unbind-agenda-peek-key))
  (set-default symbol key)
  (when org-quick-peek-mode
    (org-quick-peek--bind-agenda-peek-key)))

(defun org-quick-peek--bind-agenda-peek-key ()
  "Bind `org-quick-peek-agenda-peek-key' to `org-quick-peek-agenda-current-item', saving existing bind, and restoring when changed."
  (-when-let (command (lookup-key org-agenda-mode-map org-quick-peek-agenda-peek-key t))
    (setq org-quick-peek-agenda-peek-key-previous-bind command)
    (message "Key '%s' is already bound in `org-agenda-mode-map' to command `%s'.  Overriding, binding to command `org-quick-peek-agenda-current-item'.  You may customize `org-quick-peek-agenda-peek-key'."
             org-quick-peek-agenda-peek-key command))
  (define-key org-agenda-mode-map org-quick-peek-agenda-peek-key 'org-quick-peek-agenda-current-item))

(defun org-quick-peek--unbind-agenda-peek-key ()
  "Unbind/restore previous binding for `org-quick-peek-agenda-peek-key'."
  (let ((def (or org-quick-peek-agenda-peek-key-previous-bind 'undefined)))
    (define-key org-agenda-mode-map org-quick-peek-agenda-peek-key def)))

;;;;; Define minor mode

(define-minor-mode org-quick-peek-mode
  "Show quick peeks of Org Agenda entries and linked Org headings."
  :group 'org-quick-peek
  (if org-quick-peek-mode
      (org-quick-peek--enable)
    (org-quick-peek--disable)))

(defun org-quick-peek--enable ()
  "Enable `org-quick-peek-mode'."
  (advice-add 'org-cycle :after 'org-quick-peek--org-cycle-after)
  (org-quick-peek--bind-agenda-peek-key))

(defun org-quick-peek--disable ()
  "Disable `org-quick-peek-mode'."
  (advice-remove 'org-cycle 'org-quick-peek--org-cycle-after)
  (org-quick-peek--unbind-agenda-peek-key))

;;;; Footer

(provide 'org-quick-peek)

;;; org-quick-peek.el ends here

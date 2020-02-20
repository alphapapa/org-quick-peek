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

;; These commands are available:

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

(defcustom org-quick-peek-show-lines 10
  "Show this many lines of entry contents."
  :type 'integer)

(defcustom org-quick-peek-show-drawers nil
  "Show drawers in entries."
  :type 'boolean)

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
          (setq link (org-link-unescape (org-match-string-no-properties 1)))
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
          (quick-peek-show (org-quick-peek--get-entry-text marker
                                                           :num-lines org-quick-peek-show-lines
                                                           :keep-drawers org-quick-peek-show-drawers)))))))

(defun org-quick-peek-agenda-current-item (&optional drawer)
  "Show quick peek of current agenda item, or hide if one is already shown.
DRAWER is the optional name of a drawer to retrieve instead e.g. \"LOGBOOK\""
  (interactive)
  (unless (> (quick-peek-hide (point)) 0)
    (if (stringp drawer)
        (org-quick-peek--agenda-show-drawer drawer)
      (org-quick-peek--agenda-show))))

(defun org-quick-peek-agenda-current-item-logbook ()
  "Show quick peek of current agenda item's :LOGBOOK: entry.
This is a convenience wrapper around `org-quick-peek-agenda-current-item' for key-bindings."
  (interactive)
  (org-quick-peek-agenda-current-item "LOGBOOK"))

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

(cl-defun org-quick-peek--agenda-show (&key quiet)
  "Show quick peek at current line."
  (-if-let* ((marker (org-get-at-bol 'org-hd-marker))
             (text (org-quick-peek--s-trim-lines (org-quick-peek--get-entry-text marker
                                                                                 :num-lines org-quick-peek-show-lines
                                                                                 :keep-drawers org-quick-peek-show-drawers))))
      (if (s-present? text)
          (quick-peek-show text)
        (unless quiet
          (minibuffer-message "Entry has no text.")))))

(cl-defun org-quick-peek--agenda-show-logbook (&key quiet)
  "Show quick peek (:LOGBOOK: drawer only) of item at current line."
  (-if-let* ((marker (org-get-at-bol 'org-hd-marker))
             (text (s-trim
                    (org-quick-peek--s-trim-lines
                    (let ((entry 
                           (org-quick-peek--get-entry-text marker
                                                           :num-lines nil
                                                           :keep-drawers t)))
                      (with-temp-buffer
                        (setq buffer-read-only nil)
                        (insert entry)
                        (beginning-of-buffer)
                        (when (re-search-forward ":LOGBOOK:" nil)
                          (let* ((drawer (cadr (org-element-property-drawer-parser nil)))
                                 (beg (plist-get drawer :contents-begin))
                                 (end (plist-get drawer :contents-end)))
                            (buffer-substring beg end)))))))))
      (if (s-present? text)
          (quick-peek-show text)
        (unless quiet
          (minibuffer-message "Entry has no text.")))))

(cl-defun org-quick-peek--get-entry-text (marker &key keep-drawers num-lines)
  "Return Org entry text from node at MARKER.
If KEEP-DRAWERS is non-nil, drawers will be kept, otherwise
removed.  If NUM-LINES is non-nil, only return the first that
many lines."
  ;; Modeled after `org-agenda-get-some-entry-text'
  (let (text)
    (with-current-buffer (marker-buffer marker)
      ;; Get raw entry text
      (org-with-wide-buffer
       (goto-char marker)
       ;; Skip heading
       (end-of-line 1)
       ;; Get entry text
       (setq text (buffer-substring
                   (point)
                   (or (save-excursion (outline-next-heading) (point))
                       (point-max))))))
    (unless keep-drawers
      (with-temp-buffer
        ;; Insert entry in temp buffer and remove drawers
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward org-drawer-regexp nil t)
          ;; Remove drawers
          (delete-region (match-beginning 0)
                         (progn (re-search-forward
                                 "^[ \t]*:END:.*\n?" nil 'move)
                                (point))))
        (setq text (buffer-substring (point-min) (point-max)))))
    (when num-lines
      ;; Remove extra lines
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (org-goto-line (1+ num-lines))
        (backward-char 1)
        (setq text (buffer-substring (point-min) (point-max)))))
    text))

(defun org-quick-peek--s-trim-lines (s)
  "Trim each line in string S."
  (s-join "\n" (-map 's-trim (s-lines s))))

;;;; Footer

(provide 'org-quick-peek)

;;; org-quick-peek.el ends here

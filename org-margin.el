;;; org-margin.el --- org margin mode -*- lexical-binding: t -*-

;; Copyright (C) 2024 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-agenda
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, org-mode, org-agenda

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; org-margin mode allows to outdent org headlines by moving leading
;; stars into the margin and transform them into markers depending on
;; the chosen style.
;;
;;
;;; NEWS:
;;
;; Version 0.1.0
;; - Initial release
;;
;;; Code 
;; (require 'svg-lib)

(defvar org-margin-headers
  (list (cons 'stars (list (propertize "     *" 'face 'org-level-1)
                           (propertize "    **" 'face 'org-level-2)
                           (propertize "   ***" 'face 'org-level-3)
                           (propertize "  ****" 'face 'org-level-4)
                           (propertize " *****" 'face 'org-level-5)
                           (propertize "******" 'face 'org-level-6)))
        (cons 'H-txt (list (propertize "H1" 'face '(font-lock-comment-face default))
                           (propertize "H2" 'face '(font-lock-comment-face default))
                           (propertize "H3" 'face '(font-lock-comment-face default))
                           (propertize "H4" 'face '(font-lock-comment-face default))
                           (propertize "H5" 'face '(font-lock-comment-face default))
                           (propertize "H6" 'face '(font-lock-comment-face default))))
        ;; Uncomment if you want svg-tags and have svg-lib installed
        ;; (cons 'H-svg (list (svg-lib-tag "H1" 'org-level-1)
        ;;                    (svg-lib-tag "H2" 'org-level-2)
        ;;                    (svg-lib-tag "H3" 'org-level-3)
        ;;                    (svg-lib-tag "H4" 'org-level-4)
        ;;                    (svg-lib-tag "H5" 'org-level-5)
        ;;                    (svg-lib-tag "H6" 'org-level-6)))
        ))

(defcustom org-margin-markers
  (list (cons "\\(#\\+begin_src\\)"
              (propertize " " 'face 'font-lock-comment-face))
        (cons "\\(#\\+begin_quote\\)"
              (propertize " " 'face 'font-lock-comment-face)))
    "A list of (regex . marker) that put a marker in the margin for
 each match")
  
(defcustom org-margin-headers-set 'H-txt
  "Headers set to use in the margin. org-margin-mode needs to be
activated for changes to take effect.")

(defcustom org-margin-bookmark
  (propertize "  " 'face 'error)
  "Bookmark symbol")
  
(defcustom org-margin-max-level 6
  "Maximum header level to consider")

(defun org-margin--set-margin-mark (orig-fun)
   "Apply a colorized overlay to the bookmarked location.
 See user option `bookmark-fringe-mark'."
   (let ((bm (make-overlay (pos-bol) (1+ (pos-bol)))))
     (overlay-put bm 'category 'bookmark)
     (overlay-put bm 'evaporate t)
     (overlay-put bm 'before-string
                  (propertize " " 'face 'nano-critical 'display
                              `((margin left-margin) ,org-margin-bookmark)))))

(defun org-margin-mode-off ()
  "Deactivate margin mode"

  (interactive)
  
  ;; Restore margin
  (set-window-margins (selected-window) 0)
  (setq left-margin-width nil)

  ;; Remove margin bookmark
  (advice-remove #'bookmark--set-fringe-mark
                 #'org-margin--set-margin-mark)
  
  ;; Restore org mode
  (org-mode))

(defun org-margin-mode-on ()
  "Activate margin mode"
  
  (interactive)

  ;; Only in org-mode
  (unless (derived-mode-p 'org-mode)
    (error "org-margin-mode can only be activated in org-mode"))

  ;; Not compatible with org-indent-mode 
  (when (and (boundp 'org-indent-mode) org-indent-mode)
    (error "org margin mode is not compatible with org-indent-mode"))

  ;; Make sure these properties are handled by font-lock
  (add-to-list 'font-lock-extra-managed-props 'display)
  (add-to-list 'font-lock-extra-managed-props 'wrap-prefix)
  (add-to-list 'font-lock-extra-managed-props 'line-prefix)

  ;; Reset org mode font lock before adding our own keywords
  (org-mode)
    
  (let* ((header-set (cdr (assoc org-margin-headers-set org-margin-headers)))
         (header-widths (mapcar
            (lambda (item)
              (cond ((imagep item) (truncate (car (image-size item))))
                    ((stringp item) (length item))
                    (t 0))) header-set))
         (marker-widths (mapcar
            (lambda (item)
              (let ((item (cdr item)))
                    (cond ((imagep item) (truncate (car (image-size item))))
                          ((stringp item) (length item))
                          (t 0)))) org-margin-markers))
         (max-width (max (apply #'max header-widths)
                         (apply #'max marker-widths)))
         (max-level (min org-margin-max-level (length header-set)))
         (keyword (format "^\\(\\*\\{1,%s\\} \\)" max-level)))

    ;; Margin text is aligned to the left. We thus add 1 to the
    ;; max-width in order to have a single space between prefix and
    ;; header.
    (set-window-margins (selected-window) (1+ max-width))
    (setq-local left-margin-width (1+ max-width))

    ;; Add header markers
    (when (> max-level 0)
      (font-lock-add-keywords nil
         `((,keyword
            1 `(face nil
                     display ((margin left-margin)
                              ,(nth (- (length (match-string 1)) 2)
                                    ',header-set))) append)))))
  ;; Add custom markers
  (dolist (item org-margin-markers)
    (let ((regex (car item))
          (marker (cdr item)))
      (font-lock-add-keywords nil
         `((,regex
            1 `(face nil
                line-prefix ,(propertize " " 'display
                                         '((margin left-margin) ,marker)))
        prepend)) 'append)))

  ;; Add margin bookmark
  (advice-add #'bookmark--set-fringe-mark
              :around #'org-margin--set-margin-mark)
  (font-lock-update))

(define-minor-mode org-margin-mode
  "org-margin mode allows to outdent headers by moving leading
 stars into the margin and transform them into markers depending
 on the chosen style."

  :global nil
  
  (if org-margin-mode
      (org-margin-mode-on)
    (org-margin-mode-off)))

(provide 'org-margin)
;;; org-margin.el ends here

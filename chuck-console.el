;;; chuck-console.el --- ChucK console

;; Author:  2018 Eugeny Volobuev
;; Maintainer: qulert@gmail.com
;; Keywords: processes, languages

;; Released under the MIT license.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'custom)
(require 'hl-line)

(require 'chuck-core)

(defcustom chuck-console-buffer-name "ChucK console"
  "ChucK console buffer name."
  :type 'string
  :group 'chuck)

(defconst chuck-console-list-format
  [("[marked]" 10 t)
   ("[shred id]" 15 t)
   ("[source]" 35 t)
   ("[spork time]" 35 nil)]
  "ChucK console list format.")

(defun chuck-console-create ()
  "Create ChucK console."
  (let* ((chuck-console-buffer (get-buffer-create chuck-console-buffer-name))
         (buffer-window (get-buffer-window chuck-console-buffer)))
    (if buffer-window
        (display-buffer chuck-console-buffer)
      (let ((new-window (split-window-below)))
        (set-window-buffer new-window chuck-console-buffer)
        (with-current-buffer chuck-console-buffer
          (funcall 'chuck-console-mode))))))

(defun chuck-console-kill ()
  "Kill ChucK console."
  (interactive)
  (when (chuck-is-running?)
    (chuck-kill))
  (with-current-buffer chuck-console-buffer-name
    (kill-buffer-and-window)))

(defun chuck-console-refresh ()
  "Refresh ChucK console buffer."
  (interactive)
  (with-current-buffer chuck-console-buffer-name
    (tabulated-list-print :remember-pos)
    (hl-line-highlight)))

(defun chuck-console-list-entries ()
  "Get entries for ChucK console."
  (-map
   (lambda (entry)
     (let ((shred (plist-get entry :shred))
           (source (plist-get entry :source))
           (time (plist-get entry :time)))
       (list shred (vector "" shred source time))))
   (chuck-status)))

(defun chuck-console-shred-at-pos (&optional pos)
  "Get shred at POS."
  (tabulated-list-get-id pos))

(defun chuck-console-reload-shred ()
  "Reload selected shred."
  (interactive)
  (let* ((shred (chuck-console-shred-at-pos))
         (source (chuck-get-source-by-shred shred))
         (filename (buffer-file-name (get-file-buffer source))))
    (chuck-replace-shred shred filename)
    (chuck-console-refresh)))

(defun chuck-console-remove-shred ()
  "Remove selected shred."
  (interactive)
  (chuck-remove-shred (chuck-console-shred-at-pos))
  (chuck-console-refresh))

(defvar chuck-console-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "x") 'chuck-console-remove-shred)
    (define-key map (kbd "r") 'chuck-console-reload-shred)
    (define-key map (kbd "R") 'chuck-console-refresh)
    (define-key map (kbd "Q") 'chuck-console-kill)
    map)
  "Keymap for ChucK console mode.")

(define-derived-mode chuck-console-mode tabulated-list-mode "ChucK console"
  "Special mode for ChucK VM console."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "ChucK console")
  (setq major-mode 'chuck-console-mode)
  (use-local-map chuck-console-mode-map)
  (setq tabulated-list-format chuck-console-list-format)
  (setq tabulated-list-entries 'chuck-console-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

(provide 'chuck-console)
;;; chuck-console.el ends here

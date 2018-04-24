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
  [("ID" 10 t)
   ("Name" 35 t)]
  "ChucK console list format.")

(defun chuck-console-create ()
  "Create ChucK console."
  (let ((new-window (split-window-below))
        (chuck-console-buffer (get-buffer-create chuck-console-buffer-name)))
    (set-window-buffer new-window chuck-console-buffer)
    (with-current-buffer chuck-console-buffer
      (funcall 'chuck-console-mode))))

(defun chuck-console-kill ()
  "Kill ChucK console."
  (with-current-buffer chuck-console-buffer-name
    (kill-buffer-and-window)))

(defun chuck-console-refresh ()
  "Refresh ChucK console buffer."
  (with-current-buffer chuck-console-buffer-name
    (tabulated-list-print :remember-pos)
    (hl-line-highlight)))

(defun chuck-console-list-entries ()
  "Get entries for ChucK console."
  (-map
   (lambda (entry)
     (let ((shred (cdr entry))
           (buf-name (car entry)))
       (print (concat buf-name "#" shred))
       (list shred (vector shred buf-name))))
   (chuck-status)))

(defun chuck-console-shred-at-pos (&optional pos)
  "Get shred at POS."
  (assoc (chuck-status) (tabulated-list-get-id pos)))

(defun chuck-console-mark-shred ())

(defun chuck-console-remove-shred ())

(defvar chuck-console-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'chuck-console-mark-shred)
    (define-key map (kbd "x") 'chuck-console-remove-shred)
    map)
  "Keymap for ChucK console mode.")

(define-derived-mode chuck-console-mode tabulated-list-mode "ChucK console"
  "Special mode for ChucK VM console."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "ChucK console")
  (setq major-mode 'chuck-console-mode)
  (setq tabulated-list-format chuck-console-list-format)
  (setq tabulated-list-entries 'chuck-console-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

(provide 'chuck-console)
;;; chuck-console.el ends here

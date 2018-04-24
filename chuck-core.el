;;; chuck-core.el --- ChucK mode core
;;; Commentary:

;;; Code:
(require 'dash)

(defgroup chuck nil
  "Support for the ChucK programming language, <http://chuck.cs.princeton.edu/>"
  :group 'languages
  :prefix "chuck-")

(defcustom chuck-exec "chuck"
  "*Command used to start the ChucK VM.
The default will work if `chuck' is on your path.  If you don't
want or can't change you `PATH' env variable change this to point
to the full path of `chuck' (i.e `c:\\chuck\\bin\\chuck.exe')"
  :type 'string
  :group 'chuck)

(defcustom chuck-process-name "ChucK"
  "ChucK process name."
  :type 'string
  :group 'chuck)

(defcustom chuck-buffer-name "*ChucK*"
  "ChucK inferior process buffer name."
  :type 'string
  :group 'chuck)

(defun chuck-cmd (cmd &optional arg)
  "Sends a CMD with optional ARG to chuck."
  (let ((cmd-line (concat chuck-exec " " cmd  " " (or arg ""))))
    (print cmd-line)
    (shell-command cmd-line)))

(defvar chuck-status-regex
  "^[[:space:]]+\\[shred id\\]:[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+\\[source\\]:[[:space:]]+\\(.*?\\)[[:space:]]+.*$")

;; ChucK mode internals
(defun ensure-chuck-is-running ()
  "Ensure ChucK process is running."
  (when (not (get-process chuck-process-name))
    (chuck-run)))

(defun chuck-run ()
  "Start the ChucK VM as an inferior process."
  (start-process chuck-process-name chuck-buffer-name chuck-exec "--loop"))

(defun chuck-kill ()
  "Kill the ChucK VM."
  (chuck-cmd "--kill"))

(defun chuck-status ()
  "Tell ChucK to report status."
  (let* ((chuck-process (get-process chuck-process-name))
         (output-start (process-mark chuck-process))
         (output-start-position (marker-position output-start)))
    (chuck-cmd "--status")
    (sleep-for 0 100)
    (with-current-buffer (marker-buffer output-start)
      (let* ((output-end (process-mark chuck-process))
             (output-end-position (marker-position output-end))
             (chuck-status-string (buffer-substring output-start-position output-end-position))
             (status (chuck-parse-status chuck-status-string)))
        status))))

(defun chuck-add (filename)
  "Add a FILENAME as a shred to the ChucK VM."
  (ensure-chuck-is-running)
  (chuck-cmd "+" filename))

(defun chuck-remove-shred (shred)
  "Remove a SHRED associated with current buffer."
  (ensure-chuck-is-running)
  (chuck-cmd "-" shred))

(defun chuck-remove-all ()
  "Remove all currently running shreds."
  (ensure-chuck-is-running)
  (chuck-cmd "--remove.all"))

(defun chuck-replace-shred (shred filename)
  "Replace a SHRED with contents of FILENAME."
  (ensure-chuck-is-running)
  (chuck-cmd "=" (concat shred " " filename)))

(defun chuck-parse-status (status)
  "Parse ChucK STATUS."
  (let ((status-lines (--filter (string-match-p chuck-status-regex it)
                                (split-string status "\n"))))
    (-map
     (lambda (line)
       (string-match chuck-status-regex line)
       (cons (match-string 2 line) (match-string 1 line)))
     status-lines)))

(defun chuck-get-shred-by-name (name)
  "Get shred id by it's NAME."
  (cdr (assoc name (chuck-status))))

(defun chuck-get-source-by-shred (shred)
  "Get source by SHRED id."
  (car (rassoc shred (chuck-status))))

(provide 'chuck-core)
;;; chuck-core.el ends here

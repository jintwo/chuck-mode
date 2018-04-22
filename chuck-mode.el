;;; chuck-mode.el --- ChucK major mode

;; Copyright (C) 2004 Mikael Johansson

;; Author:  2018 Eugeny Volobuev
;;          2009 Kao Cardoso Félix
;;          2004 Mikael Johansson
;; Maintainer: qulert@gmail.com
;; Keywords: tools, processes, languages

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

;; This is a mode for editing ChucK language files.  It also supports
;; running a ChucK subprocess and sending code to it.

;; Information about the ChucK language can be found on
;; http://chuck.cs.princeton.edu/

;; INSTALLATION AND USE :

;; To install, put this file on your load-path and the add a line with?
;; (require 'chuck-mode)

;; To start using it just edit a .ck file.  You can then press C-c C-c
;; (chuck-add-code) to send the buffer content as a shred to the ChucK
;; VM.  If there's isn't any subprocess running the VM one will be
;; created for you.  To remove a shred from the VM press C-c C-d
;; (chuck-remove-code) and type the shred id on the Minibuffer.

;;; Code:

(require 'custom)
(require 'hl-line)

;;; Customizable variables

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

(defcustom chuck-auto-save-buffer t
  "If a buffer should be saved before sent to the ChucK VM."
  :type 'boolean
  :group 'chuck)

(defcustom chuck-process-name "ChucK"
  "ChucK process name."
  :type 'string
  :group 'chuck)

(defcustom chuck-buffer-name "*ChucK*"
  "ChucK inferior process buffer name."
  :type 'string
  :group 'chuck)

(defcustom chuck-console-buffer-name "ChucK console"
  "ChucK console buffer name."
  :type 'string
  :group 'chuck)

(defconst chuck-console-list-format
  [("ID" 10 t)
   ("Name" 35 t)]
  "List format.")

;; mode hook for user defined actions
(defvar chuck-mode-hook nil)

(defun chuck-cmd (cmd &optional arg)
  "Sends a CMD with optional ARG to chuck."
  (shell-command (concat chuck-exec " " cmd  " " (or arg ""))))

;; **************************************************
;; Chuck inferior process handling
;; **************************************************

(defvar chuck-save-error
  "You need to save the buffer before sending it.")

(defvar chuck-status-regex
  "^[[:space:]]+\\[shred id\\]:[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+\\[source\\]:[[:space:]]+\\(.*?\\)[[:space:]]+.*$")


(defun refresh-chuck-console ()
  "Refresh ChucK console buffer."
  (with-current-buffer chuck-console-buffer-name
    (tabulated-list-print :remember-pos)
    (hl-line-highlight)))

(defun run-chuck ()
  "Start the ChucK VM as an inferior process."
  (interactive)
  (start-process chuck-process-name chuck-buffer-name chuck-exec "--loop")
  (let ((new-window (split-window-below))
        (chuck-console-buffer (get-buffer-create chuck-console-buffer-name)))
    (set-window-buffer new-window chuck-console-buffer)
    (with-current-buffer chuck-console-buffer
      (funcall 'chuck-console-mode))))

(defun kill-chuck ()
  "Kill the ChucK VM."
  (interactive)
  (chuck-cmd "--kill")
  (with-current-buffer chuck-console-buffer-name
    (kill-buffer-and-window)))

(defun chuck-add-current-buffer ()
  "Add a current buffer as a shred to the ChucK VM."
  (interactive)
  (ensure-chuck-is-running)
  (when chuck-auto-save-buffer
    (save-buffer))
  (let ((chuck-file (file-name-nondirectory buffer-file-name)))
    (chuck-cmd "+" chuck-file)
    (refresh-chuck-console)))

(defun chuck-remove-shred (shred-id)
  "Remove a SHRED-ID associated with current buffer."
  (interactive "P")
  (ensure-chuck-is-running)
  (if shred-id
      (chuck-cmd "-" (number-to-string (prefix-numeric-value shred-id)))
    (let* ((status (chuck-status))
           (buffer-shred (assoc (buffer-name) status)))
      (chuck-cmd "-" (cdr buffer-shred))
      (refresh-chuck-console))))

(defun chuck-remove-all ()
  "Remove all currently running shreds."
  (interactive)
  (ensure-chuck-is-running)
  (chuck-cmd "--remove.all")
  (refresh-chuck-console))

(defun chuck-replace-shred (shred-id)
  "Replace a SHRED-ID associated with current buffer."
  (interactive "P")
  (ensure-chuck-is-running)
  (let ((chuck-file (file-name-nondirectory buffer-file-name)))
    (if shred-id
        (progn
          (chuck-cmd "=" (concat (number-to-string (prefix-numeric-value shred-id)) " " chuck-file))
          (refresh-chuck-console))
      (let* ((status (chuck-status))
             (buffer-shred (assoc (buffer-name) status)))
        (chuck-cmd "=" (concat (cdr buffer-shred) " " (car buffer-shred)))
        (refresh-chuck-console)))))

(defun parse-chuck-status (status-string)
  "Parse ChucK STATUS-STRING."
  (let ((lines (split-string status-string "\n"))
        (result '()))
    (dolist (line lines result)
      (when (string-match-p chuck-status-regex line)
        (string-match chuck-status-regex line)
        (let ((buf-name (match-string 2 line))
              (shred-id (match-string 1 line)))
          (add-to-list 'result (cons buf-name shred-id) result))))
    result))

(defun ensure-chuck-is-running ()
  "Ensure ChucK process is running."
  (when (not (get-process chuck-process-name))
    (run-chuck)))

(defun chuck-status ()
  "Tell ChucK to report status."
  (interactive)
  (let* ((chuck-process (get-process chuck-process-name))
         (output-start (process-mark chuck-process))
         (output-start-position (marker-position output-start)))
    (chuck-cmd "--status")
    (sleep-for 0 100)
    (with-current-buffer (marker-buffer output-start)
      (let* ((output-end (process-mark chuck-process))
             (output-end-position (marker-position output-end))
             (chuck-status-string (buffer-substring output-start-position output-end-position)))
        (parse-chuck-status chuck-status-string)))))

;; **************************************************
;; Chuck editing enhancements
;; **************************************************

(defun chuck-electric-equal-key (arg)
  "Smart behaviour for = key. Inserts a chuck operator if pressed
once and an == if pressed twice.  With the C-u prefix inserts the
upchuck operator."
  (interactive "P")
  (cond ((memq (char-before) '(?> ?< ?!))
         (insert "="))
        ((chuck-op-before?)
         (progn (backward-delete-char 1)
                (insert "=")))
        ((and arg (listp arg)) (insert "=^"))
        (t (insert "=>"))))

(defun chuck-electric-close-block (n)
  "Automatically indent after typing a }"
   (interactive "p")
   (self-insert-command n)
   (indent-according-to-mode)
   (forward-char))

;; This function try to make chuck operators being deleted with just
;; one keystroke on DEL (backspace), but it's too troublesome and it
;; doesn't seem worth it.

;; (defun chuck-delete-backward-char (arg &optional killp)
;;   "Delete the entire chuck operator with backspace."
;;   (interactive "P")
;;   (if (chuck-op-before?)
;; 	  (delete-backward-char 2)
;; 	(delete-backward-char (prefix-numeric-value arg) killp)))

(defun chuck-op-before? ()
  (string= (buffer-substring (- (point) 2) (point)) "=>"))

;; **************************************************
;; Mode configurations
;; **************************************************

;; keymap for ChucK mode
(defvar chuck-mode-map
  (let ((chuck-mode-map (make-keymap)))
    ;; menu
    (define-key chuck-mode-map [menu-bar chuck]
      (cons "ChucK" (make-sparse-keymap "ChucK")))
	(define-key chuck-mode-map [menu-bar chuck run-chuck]
      '("Run ChucK VM on a inferior process." . run-chuck))
	(define-key chuck-mode-map [menu-bar chuck chuck-status]
      '("Query ChucK status" . chuck-status))
    (define-key chuck-mode-map [menu-bar chuck kill-chuck]
      '("Kill the running ChucK" . kill-chuck))
    (define-key chuck-mode-map [menu-bar chuck chuck-add-code]
      '("Add buffer to running ChucK" . chuck-add-code))
    (define-key chuck-mode-map [menu-bar chuck chuck-add-current-buffer]
      '("Add current buffer to running ChucK" . chuck-add-current-buffer))
    (define-key chuck-mode-map [menu-bar chuck chuck-replace-shred]
      '("Replace shred from running ChucK" . chuck-replace-shred))
    (define-key chuck-mode-map [menu-bar chuck chuck-remove-shred]
      '("Remove shred from running ChucK" . chuck-remove-shred))
    (define-key chuck-mode-map [menu-bar chuck chuck-remove-all]
      '("Remove all currently running shreds" . chuck-remove-all))

    ;; shortcuts
    (define-key chuck-mode-map (kbd "C-c C-s") 'run-chuck)
    (define-key chuck-mode-map (kbd "C-c C-k") 'kill-chuck)
    (define-key chuck-mode-map (kbd "C-c C-c") 'chuck-add-current-buffer)
    (define-key chuck-mode-map (kbd "C-c C-r") 'chuck-replace-shred)
    (define-key chuck-mode-map (kbd "C-c C-d") 'chuck-remove-shred)
    (define-key chuck-mode-map (kbd "C-c C-l") 'chuck-remove-all)

    chuck-mode-map)
  "Keymap for ChucK major mode.")

;; Filename binding
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))

;; Come helper functions for creating font-lock entries.
(defun keyword-regexp (&rest word-list)
  (concat
   "\\<\\("
   (mapconcat 'identity word-list "\\|")
   "\\)\\>"))

(defun symbol-regexp (&rest symbol-list)
  (concat
   "\\_<\\("
   (mapconcat 'identity symbol-list "\\|")
   "\\)\\_>"))

(defun chuck-library-regexp (namespace &rest symbol-list)
  (concat
   "\\<" namespace "\\.\\("
   (mapconcat 'identity symbol-list "\\|")
   "\\)\\>"))

;; Syntax highlighting
(defconst chuck-font-lock-keywords-1
  (list
   (cons (keyword-regexp
	  ;; Primitive types
	  "int" "float" "time" "dur" "void" "same"
	  ;; Reference types
	  "Object" "array" "Event" "UGen" "string"
	  ;; Complex types
	  "polar" "complex"
	  ;; standard ChucK unit generators:
	  "SinOsc" "PulseOsc" "SqrOsc" "TriOsc"
	  "SawOsc" "Phasor" "Noise" "Impulse"
	  "Step" "Gain" "SndBuf" "HalfRect"
	  "FullRect" "ZeroX" "Mix2" "Pan2"
	  "GenX" "CurveTable" "WarpTable" "LiSa"
	  ;; filters:
	  "OneZero" "TwoZero" "OnePole" "TwoPole"
	  "PoleZero" "BiQuad" "Filter" "LPF"
	  "HPF" "BPF" "BRF" "ResonZ" "Dyno"
	  ;; STK unit generators in ChucK:
	  "Envelope" "ADSR" "Delay" "DelayA" "DelayL"
	  "Echo" "JCRev" "NRev" "PRCRev" "Chorus"
	  "Modulate" "PitShift" "SubNoise" "Blit"
	  "BlitSaw" "BlitSquare" "WvIn" "WaveLoop"
	  "WvOut"
	  ;; STK instruments unit generators
	  "StkInstrument" "BandedWG" "BlowBotl"
	  "BlowHole" "Bowed" "Brass" "Clarinet"
	  "Flute" "Mandolin" "ModalBar" "Moog"
	  "Saxofony" "Shakers" "Sitar" "StifKarp"
	  "VoicForm" "FM" "BeeThree" "FMVoices"
	  "HevyMetl" "PercFlut" "Rhodey"
	  "TubeBell" "Wurley")
	 'font-lock-type-face)
   (cons (keyword-regexp
	  ;; Control structures
	  "if" "else" "while" "until" "for" "repeat"
	  "break" "continue" "return" "switch"
	  ;; Class keyword
	  "class" "extends" "public" "static" "pure"
	  "this" "super" "interface" "implements"
	  "protected" "private"
	  ;; Other keywords
	  "function" "fun" "spork" "const" "new")
	 'font-lock-keyword-face)
   (cons (keyword-regexp
	  ;; Special values
	  "now" "true" "false" "maybe"
	  "null" "NULL" "me" "pi"
	  ;; Special: default durations
	  "samp" "ms" "second" "minute" "hour"
	  "day" "week"
	  ;; Special: global ugens
	  "dac" "adc" "blackhole")
	 'font-lock-keyword-face)

   ;; chuck operators and debug print
   (cons (symbol-regexp "=>" "=<" "!=>" "->"
			"<-" "+->" "-->" "*->"
			"/->" "&->" "|->" "^->"
			">>->" "<<->" "%->" "@=>"
			"+=>" "-=>" "*=>" "/=>"
			"&=>" "|=>" "^=>" ">>=>"
			"<<=>" "%=>" "<<<" ">>>")
	 'font-lock-operator-face)

   ;;  Upchuck operator. For some reason the regexp applied to other
   ;;  operators don't work
   (cons "\\_<\\(=\\^\\)" 'font-lock-operator-face)

   ;; Standard Library functions
   (list (chuck-library-regexp "Std"
			       ;; Std
			       "abs" "fabs" "rand"
			       "rand2" "randf" "rand2f"
			       "sgn" "system" "atoi"
			       "atof" "getenv" "setenv"
			       "mtof" "ftom" "powtodb"
			       "rmstodb" "dbtopow" "dbtorms")
	 1 'font-lock-builtin-face)

   (list (chuck-library-regexp "Machine"
			       ;; Machine
			       "add" "spork" "remove"
			       "replace" "status" "crash")
	 1 'font-lock-builtin-face)

   (list (chuck-library-regexp "Math"
			       ;; Math
			       "sin" "cos" "tan" "asin"
			       "acos" "atan" "atan2"
			       "sinh" "cosh" "tanh"
			       "hypot" "pow" "sqrt" "exp"
			       "log" "log2" "log10"
			       "floor" "ceil" "round"
			       "trunc" "fmod" "remainder"
			       "min" "max" "nextpow2"
			       "isinf" "isnan")
	 1 'font-lock-builtin-face)

   ;; Namespaces
   '("\\<\\(Math\\|Std\\|Machine\\)\\>\\." 1 'font-lock-constant-face)
   ;; Functions
   '("\\<\\(fun\\|function\\)[ \t]+[a-zA-Z_]+[a-zA-Z0-9_]*[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     2 'font-lock-function-name))
  ;; '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Highlighting for ChucK mode.")

(defvar chuck-font-lock-keywords chuck-font-lock-keywords-1
  "Default highlighting for ChucK mode.")

;; Indenting for ChucK mode
(defun chuck-indent-line ()
  "Indent current line as ChucK code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ;; Start of buffer starts out unindented
      (indent-line-to 0)
    (let ((not-indented t)
		  cur-indent)
      (if (looking-at "[[:blank:]]*}") ; Closing a block
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0)
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented
			(forward-line -1)
			(cond ((looking-at ".*{") ; In open block
				   (setq cur-indent (+ (current-indentation) default-tab-width))
				   (setq not-indented nil))
				  ((looking-at "[[:blank:]]*}") ; Closed block on blank line
				   (setq cur-indent (current-indentation))
				   (setq not-indented nil))
				  ((looking-at ".*}") ; Closed block on non-blank line
				   (setq cur-indent (- (current-indentation) default-tab-width))
				   (setq not-indented nil))
				  ((bobp)
				   (setq not-indented nil))))))
      (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0)))))

;; Syntax table
(defvar chuck-mode-syntax-table nil "Syntax table for ChucK mode")
(setq chuck-mode-syntax-table
      (let ((chuck-mode-syntax-table (make-syntax-table)))
	(modify-syntax-entry ?_ "_" chuck-mode-syntax-table)
	(modify-syntax-entry ?/ ". 12" chuck-mode-syntax-table)
	(modify-syntax-entry ?\n ">" chuck-mode-syntax-table)
	chuck-mode-syntax-table))

;; Entry point
(defun chuck-console-list-entries ()
  "Get entries for ChucK console."
  (-map
   (lambda (entry)
     (let ((shred-id (cdr entry))
           (buffer-name (car entry)))
       (list shred-id (vector shred-id buffer-name))))
   (chuck-status)))

(define-derived-mode chuck-console-mode tabulated-list-mode "ChucK"
  "Special mode for ChucK VM console."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "ChucK")
  (setq major-mode 'chuck-console-mode)
  (setq tabulated-list-format chuck-console-list-format)
  (setq tabulated-list-entries 'chuck-console-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

(defun chuck-mode ()
  "Major mode for editing ChucK music/audio scripts."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table chuck-mode-syntax-table)
  (use-local-map chuck-mode-map)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'font-lock-defaults) '(chuck-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'chuck-indent-line)
  (setq major-mode 'chuck-mode)
  (setq mode-name "ChucK")
  (setq default-tab-width 4)
  (run-hooks 'chuck-mode-hook))

(provide 'chuck-mode)
;;; chuck-mode.el ends here

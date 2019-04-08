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

(require 'dash)
(require 'custom)
(require 'hl-line)

(require 'chuck-core)
(require 'chuck-console)

;;; Customizable variables

(defcustom chuck-auto-save-buffer t
  "If a buffer should be saved before sent to the ChucK VM."
  :type 'boolean
  :group 'chuck)

(defvar chuck-mode-hook nil)

(defvar chuck-save-error
  "You need to save the buffer before sending it.")

(defvar chuck-mode-syntax-table nil
  "Syntax table for ChucK mode.")

;;; Core wrappers

(defun start-chuck ()
  "Start ChucK process and create console."
  (interactive)
  (chuck-run)
  (chuck-console-create))

(defun kill-chuck ()
  "Stop ChucK process and kill console."
  (interactive)
  (chuck-kill)
  (chuck-console-kill))

(defun add-shred ()
  "Add current buffer to ChucK."
  (interactive)
  (when chuck-auto-save-buffer
    (save-buffer))
  (when (not (chuck-is-running?))
    (start-chuck)
    (sleep-for 0 100))
  (chuck-add buffer-file-name)
  (chuck-console-refresh))

(defun replace-shred (shred)
  "Replace shred with SHRED id."
  (interactive "P")
  (if shred
      (chuck-replace-shred (number-to-string (prefix-numeric-value shred)) buffer-file-name)
    (chuck-replace-shred (chuck-get-shred-by-source (file-name-nondirectory buffer-file-name)) buffer-file-name))
  (chuck-console-refresh))

(defun kill-shred (shred)
  "Kill shred with SHRED id."
  (interactive "P")
  (if shred
      (chuck-remove-shred (number-to-string (prefix-numeric-value shred)))
    (chuck-remove-shred (chuck-get-shred-by-source (file-name-nondirectory buffer-file-name))))
  (chuck-console-refresh))

(defun kill-all-shreds ()
  "Kill all currently running shreds."
  (interactive)
  (chuck-remove-all)
  (chuck-console-refresh))

;; keymap for ChucK mode
(defvar chuck-mode-map
  (let ((chuck-mode-map (make-keymap)))
    (define-key chuck-mode-map (kbd "C-c C-s") 'start-chuck)
    (define-key chuck-mode-map (kbd "C-c C-k") 'kill-chuck)
    (define-key chuck-mode-map (kbd "C-c C-c") 'add-shred)
    (define-key chuck-mode-map (kbd "C-c C-r") 'replace-shred)
    (define-key chuck-mode-map (kbd "C-c C-d") 'kill-shred)
    (define-key chuck-mode-map (kbd "C-c C-l") 'kill-all-shreds)
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
   (cons (symbol-regexp
          "=>" "=<" "!=>" "->"
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
   (list (chuck-library-regexp
          "Std"
          ;; Std
          "abs" "fabs" "rand"
          "rand2" "randf" "rand2f"
          "sgn" "system" "atoi"
          "atof" "getenv" "setenv"
          "mtof" "ftom" "powtodb"
          "rmstodb" "dbtopow" "dbtorms")
         1 'font-lock-builtin-face)

   (list (chuck-library-regexp
          "Machine"
          ;; Machine
          "add" "spork" "remove"
          "replace" "status" "crash")
         1 'font-lock-builtin-face)

   (list (chuck-library-regexp
          "Math"
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
  "Indent current line as ChucK code."
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
(setq chuck-mode-syntax-table
      (let ((chuck-mode-syntax-table (make-syntax-table)))
        (modify-syntax-entry ?_ "_" chuck-mode-syntax-table)
        (modify-syntax-entry ?/ ". 12" chuck-mode-syntax-table)
        (modify-syntax-entry ?\n ">" chuck-mode-syntax-table)
        chuck-mode-syntax-table))

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

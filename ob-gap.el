;;; ob-gap.el --- Org Babel support for GAP -*- lexical-binding: t -*-

;; Author: Chris Lloyd (original babel-gap)
;; Maintainer: Tevqoon
;; Keywords: literate programming, reproducible research, languages
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; URL: https://github.com/Tevqoon/ob-gap

;; WARNING: largely AI-written ("vibecoded").  Tested, but read it
;; before relying on it.  See README.org.

;; Copyright (C) Christopher J.R. Lloyd and contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Babel support for GAP (Groups, Algorithms, Programming),
;; https://www.gap-system.org.
;;
;; Features:
;;
;;  - batch evaluation (a fresh `gap' process per block)
;;  - `:session' evaluation in a persistent GAP process
;;  - `:var' (numbers, strings, lists and Org tables become GAP values)
;;  - `:results output' (default) and `:results value'
;;    (a GAP list of lists comes back as an Org table)
;;  - `:cmdline', `:prologue', `:epilogue'
;;  - GAP errors are reported instead of silently entering a break loop
;;  - tangling to .g files, with `:var' assignments included
;;
;; Enable with
;;
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                '((gap . t) ...))
;;
;; or simply (require 'ob-gap).

;;; Code:

(require 'ob)
(require 'ob-eval)
(require 'comint)
(require 'ansi-color)
(require 'subr-x)

(defvar gap-executable)                 ; from gap-mode, if loaded
(defvar org-babel-tangle-lang-exts)

(defgroup ob-gap nil
  "Org Babel support for GAP."
  :group 'org-babel
  :prefix "org-babel-gap-")

(defcustom org-babel-gap-command nil
  "GAP executable used by Org Babel.
When nil, use `gap-executable' from gap-mode if that is set,
otherwise \"gap\" from `exec-path'."
  :type '(choice (const :tag "Auto" nil) file))

(defcustom org-babel-gap-switches '("-q" "-b")
  "Command line switches passed to GAP for batch evaluation.
The `:cmdline' header argument is appended after these.  Add
\"-A\" here to skip autoloading of suggested packages, which
makes startup faster."
  :type '(repeat string))

(defcustom org-babel-gap-session-switches '("-q")
  "Command line switches passed to GAP when starting a session."
  :type '(repeat string))

(defcustom org-babel-gap-quit-on-break t
  "Non-nil means batch evaluation passes --quitonbreak to GAP.
GAP then exits with a non-zero status on error, so Org shows the
error buffer, instead of entering a break loop that silently eats
the rest of the block."
  :type 'boolean)

(defcustom org-babel-gap-session-timeout nil
  "Seconds to wait for a session block to finish, or nil to wait forever.
Waiting can always be interrupted with \\[keyboard-quit]."
  :type '(choice (const :tag "No timeout" nil) number))

(defvar org-babel-default-header-args:gap '((:results . "output"))
  "Default header arguments for GAP source blocks.")

(defvar org-babel-header-args:gap '()
  "GAP-specific header arguments (none so far).")

(with-eval-after-load 'ob-tangle
  (add-to-list 'org-babel-tangle-lang-exts '("gap" . "g")))

(defconst org-babel-gap-eoe "org_babel_gap_eoe"
  "Marker printed by GAP after each session evaluation.")

(defconst org-babel-gap--session-init
  "BreakOnError := false;;
OrgBabelGapLast := fail;;
OrgBabelGapRun := function(file)
  local res, r;
  res := READ_ALL_COMMANDS(InputTextFile(file), false, false,
                           function(v) View(v); Print(\"\\n\"); end);
  for r in res do
    if r[1] and IsBound(r[2]) then OrgBabelGapLast := r[2]; fi;
  od;
end;;"
  "GAP code run when a session starts.
Errors return to the main loop instead of a break loop.
`OrgBabelGapRun' reads a whole file of statements, echoing values
like the GAP prompt does.  Because the file is read as one unit, a
missing semicolon or unclosed block gives a syntax error at end of
input instead of swallowing the following input.  The value of
the last statement is kept in `OrgBabelGapLast'.")

(defconst org-babel-gap--value-writer
  "OrgBabelGapWriteValue := function(x, file)
  local cell, row, s, out;
  cell := function(c) if IsString(c) then return c; fi; return String(c); end;
  row := r -> JoinStringsWithSeparator(List(r, cell), \"\\t\");
  if IsList(x) and not IsString(x) and IsDenseList(x) and Length(x) > 0 then
    if ForAll(x, r -> IsList(r) and not IsString(r) and IsDenseList(r)) then
      s := Concatenation(\"table\\n\", JoinStringsWithSeparator(List(x, row), \"\\n\"));
    else
      s := Concatenation(\"table\\n\", row(x));
    fi;
  else
    s := Concatenation(\"scalar\\n\", cell(x));
  fi;
  out := OutputTextFile(file, false);
  SetPrintFormattingStatus(out, false);
  WriteAll(out, s);
  CloseStream(out);
end;;"
  "GAP code that defines a function writing a value to a file.
Lists of lists are written as tab-separated rows, flat lists as a
single row and anything else as a string.  The first line says
which: \"table\" or \"scalar\".")

;;; Helpers

(defun org-babel-gap--command ()
  "Return the GAP executable to run."
  (or org-babel-gap-command
      (and (boundp 'gap-executable) gap-executable)
      "gap"))

(defun org-babel-gap--clean (output)
  "Remove ANSI escapes and carriage returns from OUTPUT."
  (when output
    (replace-regexp-in-string "\r" "" (ansi-color-filter-apply output))))

(defun org-babel-gap--quote-string (s)
  "Return S as a GAP string literal."
  (concat "\""
          (replace-regexp-in-string
           "[\\\"\n\t\r]"
           (lambda (m)
             (pcase m
               ("\\" "\\\\\\\\") ("\"" "\\\\\"") ("\n" "\\\\n")
               ("\t" "\\\\t") ("\r" "\\\\r")))
           s t)
          "\""))

(defun org-babel-gap-var-to-gap (value)
  "Convert the Emacs Lisp VALUE into a GAP expression (a string)."
  (cond
   ((null value) "[ ]")
   ((eq value t) "true")
   ((integerp value) (number-to-string value))
   ((floatp value) (format "%S" value))
   ((stringp value) (org-babel-gap--quote-string value))
   ((symbolp value) (org-babel-gap--quote-string (symbol-name value)))
   ((listp value)
    (concat "[ "
            (mapconcat #'org-babel-gap-var-to-gap
                       (delq 'hline (copy-sequence value)) ", ")
            " ]"))
   (t (org-babel-gap--quote-string (format "%s" value)))))

(defun org-babel-variable-assignments:gap (params)
  "Return a list of GAP statements assigning the `:var's in PARAMS."
  (mapcar (lambda (pair)
            (format "%s := %s;;" (car pair)
                    (org-babel-gap-var-to-gap (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-expand-body:gap (body params &optional var-lines)
  "Expand BODY with PARAMS: prologue, `:var' assignments, BODY, epilogue.
VAR-LINES, when given, replaces the assignments computed from PARAMS."
  (org-babel-expand-body:generic
   body params (or var-lines (org-babel-variable-assignments:gap params))))

(defun org-babel-gap--value-epilogue (file &optional variable)
  "GAP code that writes the value of VARIABLE (default `last') to FILE."
  (setq variable (or variable "last"))
  (concat (format "if IsBoundGlobal(\"%s\") then OrgBabelGapValue := %s; "
                  variable variable)
          "else OrgBabelGapValue := \"\"; fi;\n"
          org-babel-gap--value-writer "\n"
          "OrgBabelGapWriteValue(OrgBabelGapValue, "
          (org-babel-gap--quote-string
           (org-babel-process-file-name file 'noquote))
          ");\n"))

(defun org-babel-gap--read-cell (s)
  "Turn the string S into a number when it looks like one."
  (org-babel-read s t))

(defun org-babel-gap--read-value-file (file)
  "Read a value written by `org-babel-gap--value-writer' to FILE."
  (when (file-exists-p file)
    (let* ((text (org-babel-eval-read-file file))
           (nl (string-search "\n" text))
           (kind (if nl (substring text 0 nl) text))
           (content (if nl (substring text (1+ nl)) "")))
      (if (equal kind "table")
          (mapcar (lambda (line)
                    (mapcar #'org-babel-gap--read-cell
                            (split-string line "\t")))
                  (split-string content "\n"))
        (org-babel-gap--read-cell content)))))

(defun org-babel-gap--output-to-table (output)
  "Parse OUTPUT as a whitespace- or tab-separated table."
  (let ((tmp (org-babel-temp-file "gap-output-")))
    (with-temp-file tmp (insert output))
    (org-babel-import-elisp-from-file tmp)))

(defun org-babel-gap--format-result (result result-params)
  "Turn RESULT into the block result according to RESULT-PARAMS.
RESULT is (RAW HAS-VALUE VALUE): RAW is the result as text; when
HAS-VALUE is non-nil, VALUE is the parsed value, otherwise RAW is
parsed as a table if a table is asked for."
  (pcase-let ((`(,raw ,has-value ,value) result))
    (org-babel-result-cond result-params
      raw
      (if has-value value (org-babel-gap--output-to-table raw)))))

(defun org-babel-gap--value-result (file)
  "Read the value GAP wrote to FILE and return a result triple."
  (let ((value (org-babel-gap--read-value-file file)))
    (list (if (consp value)
              (mapconcat (lambda (row)
                           (mapconcat (lambda (c) (format "%s" c)) row "\t"))
                         value "\n")
            (format "%s" (or value "")))
          t value)))

;;; Batch evaluation

(defun org-babel-gap--evaluate-batch (body result-type cmdline)
  "Evaluate BODY in a fresh GAP process.
Return a result triple as for `org-babel-gap--format-result'.
RESULT-TYPE is `output' or `value'.  CMDLINE is extra switches."
  (let* ((value-file (when (eq result-type 'value)
                       (org-babel-temp-file "gap-value-")))
         (command (string-join
                   (delq nil
                         (append
                          (list (shell-quote-argument (org-babel-gap--command)))
                          (mapcar #'shell-quote-argument org-babel-gap-switches)
                          (when org-babel-gap-quit-on-break
                            (list "--quitonbreak"))
                          (list cmdline)))
                   " "))
         (input (if value-file
                    (concat body "\n" (org-babel-gap--value-epilogue value-file))
                  body))
         (output (org-babel-gap--clean (org-babel-eval command input))))
    (if value-file
        (org-babel-gap--value-result value-file)
      (list output nil nil))))

;;; Sessions

(defun org-babel-gap--session-buffer-name (session)
  "Buffer name used for SESSION."
  (cond ((member session '(nil "" "yes")) "*gap*")
        ((string-match-p "\\`\\*.*\\*\\'" session) session)
        (t (format "*gap-%s*" session))))

(defun org-babel-gap--session-live-p (buffer)
  "Non-nil if BUFFER has a live GAP process."
  (and buffer (get-buffer buffer)
       (process-live-p (get-buffer-process buffer))))

(define-derived-mode org-babel-gap-session-mode comint-mode "GAP session"
  "Major mode for Org Babel GAP session buffers."
  (setq-local comint-prompt-regexp "^\\(gap\\|brk[_0-9]*\\)> *")
  (setq-local comint-process-echoes nil))

(defun org-babel-gap--session-send (buffer code &optional display)
  "Send CODE to the GAP session in BUFFER and return its output.
DISPLAY, if non-nil, is shown in the session buffer instead of CODE."
  (let ((proc (get-buffer-process buffer)))
    (with-current-buffer buffer
      (goto-char (process-mark proc))
      (insert (or display code) "\n")
      (set-marker (process-mark proc) (point))
      (let ((start (point))
            (eoe-re (concat "^" org-babel-gap-eoe "\n"))
            (deadline (and org-babel-gap-session-timeout
                           (+ (float-time) org-babel-gap-session-timeout))))
        (comint-send-string
         proc (format "%s\nPrint(\"\\n%s\\n\");\n" code org-babel-gap-eoe))
        (while (progn
                 (goto-char start)
                 (not (re-search-forward eoe-re nil t)))
          (unless (process-live-p proc)
            (error "GAP session process died"))
          (when (and deadline (> (float-time) deadline))
            (error "GAP session timed out"))
          (accept-process-output proc 0.1))
        (let ((out (buffer-substring-no-properties start (match-beginning 0))))
          (goto-char (point-max))
          ;; Drop the blank line printed in front of the marker.
          (string-remove-suffix "\n" out))))))

(defun org-babel-gap--session-eval (buffer body)
  "Evaluate BODY in the session BUFFER as one unit; return the output."
  (let ((file (org-babel-temp-file "gap-session-" ".g")))
    (with-temp-file file (insert body "\n"))
    (org-babel-gap--session-send
     buffer
     (format "OrgBabelGapRun(%s);"
             (org-babel-gap--quote-string
              (org-babel-process-file-name file 'noquote)))
     (string-trim-right body))))

(defun org-babel-gap-initiate-session (&optional session _params)
  "Start the GAP session SESSION unless it is running; return its buffer.
Return nil when SESSION is \"none\"."
  (unless (equal session "none")
    (let ((name (org-babel-gap--session-buffer-name session)))
      (unless (org-babel-gap--session-live-p name)
        (let ((buffer (get-buffer-create name))
              ;; Pipes, not a pty: no echo, no line editing, no prompts.
              (process-connection-type nil))
          (apply #'make-comint-in-buffer "gap" buffer
                 (org-babel-gap--command) nil org-babel-gap-session-switches)
          (with-current-buffer buffer (org-babel-gap-session-mode))
          (org-babel-gap--session-send buffer org-babel-gap--session-init
                                       "# ob-gap session started")))
      (get-buffer name))))

(defun org-babel-gap--evaluate-session (buffer body result-type)
  "Evaluate BODY in the session BUFFER.
Return a result triple as for `org-babel-gap--format-result'."
  (let ((output (org-babel-gap--clean (org-babel-gap--session-eval buffer body))))
    (if (eq result-type 'value)
        (let ((file (org-babel-temp-file "gap-value-")))
          (org-babel-gap--session-send
           buffer (org-babel-gap--value-epilogue file "OrgBabelGapLast")
           "# ob-gap: reading value")
          (org-babel-gap--value-result file))
      (list output nil nil))))

(defun org-babel-prep-session:gap (session params)
  "Start SESSION and assign the `:var's in PARAMS in it."
  (let ((buffer (org-babel-gap-initiate-session session params)))
    (dolist (line (org-babel-variable-assignments:gap params))
      (org-babel-gap--session-eval buffer line))
    buffer))

(defun org-babel-load-session:gap (session body params)
  "Load BODY into SESSION, expanded with PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:gap session params)))
      (org-babel-gap--session-eval buffer (org-babel-expand-body:gap body params nil))
      buffer)))

;;; Entry point

(defun org-babel-execute:gap (body params)
  "Execute the GAP code BODY according to PARAMS."
  (let* ((session (org-babel-gap-initiate-session
                   (cdr (assq :session params)) params))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:gap body params))
         (result (if session
                     (org-babel-gap--evaluate-session session full-body result-type)
                   (org-babel-gap--evaluate-batch
                    full-body result-type (cdr (assq :cmdline params))))))
    (org-babel-reassemble-table
     (org-babel-gap--format-result result result-params)
     (org-babel-pick-name (cdr (assq :colname-names params))
                          (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
                          (cdr (assq :rownames params))))))

(provide 'ob-gap)
;;; ob-gap.el ends here

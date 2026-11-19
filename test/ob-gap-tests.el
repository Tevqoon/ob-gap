;;; ob-gap-tests.el --- Tests for ob-gap -*- lexical-binding: t -*-

;; Run with:  make test
;; Needs `gap' on PATH (or set ORG_BABEL_GAP_COMMAND).

;;; Code:

(require 'ert)
(require 'org)
(require 'ob-gap)

(setq org-confirm-babel-evaluate nil)
(when (getenv "ORG_BABEL_GAP_COMMAND")
  (setq org-babel-gap-command (getenv "ORG_BABEL_GAP_COMMAND")))

(defmacro ob-gap-test-with (text &rest body)
  "Run BODY in an Org buffer containing TEXT, point on the first block."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     (re-search-forward "#\\+begin_src")
     ,@body))

(defun ob-gap-test-run (text)
  "Execute the first GAP block in TEXT and return its result."
  (ob-gap-test-with text
    (org-babel-execute-src-block)))

(defun ob-gap-test-kill-sessions ()
  (dolist (b (buffer-list))
    (when (string-match-p "\\`\\*gap" (buffer-name b))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer b)))))

;;; Unit tests (no GAP needed)

(ert-deftest ob-gap/var-to-gap ()
  (should (equal (org-babel-gap-var-to-gap 3) "3"))
  (should (equal (org-babel-gap-var-to-gap 1.5) "1.5"))
  (should (equal (org-babel-gap-var-to-gap "a\"b\\c\nd")
                 "\"a\\\"b\\\\c\\nd\""))
  (should (equal (org-babel-gap-var-to-gap '(1 2 3)) "[ 1, 2, 3 ]"))
  (should (equal (org-babel-gap-var-to-gap '((1 2) hline (3 4)))
                 "[ [ 1, 2 ], [ 3, 4 ] ]"))
  (should (equal (org-babel-gap-var-to-gap nil) "[ ]"))
  (should (equal (org-babel-gap-var-to-gap t) "true")))

(ert-deftest ob-gap/expand-body ()
  (ob-gap-test-with "#+begin_src gap :var x=3 :var s=\"hi\" :prologue \"P;;\" :epilogue \"E;;\"
x^2;
#+end_src"
    (let* ((info (org-babel-get-src-block-info))
           (expanded (org-babel-expand-body:gap (nth 1 info) (nth 2 info))))
      (should (equal expanded "P;;\nx := 3;;\ns := \"hi\";;\nx^2;\nE;;")))))

;;; Batch evaluation

(ert-deftest ob-gap/batch-output ()
  (should (equal (ob-gap-test-run "#+begin_src gap
x := 3;;
x^2;
Print(\"hello\\n\");
#+end_src")
                 "9\nhello\n")))

(ert-deftest ob-gap/batch-var-table ()
  (should (equal (ob-gap-test-run "#+name: tbl
| 1 | 2 |
| 3 | 4 |

#+begin_src gap :var m=tbl
Sum(Flat(m));
#+end_src")
                 "10\n")))

(ert-deftest ob-gap/batch-value-scalar ()
  (should (equal (ob-gap-test-run "#+begin_src gap :results value
Print(\"ignored\\n\");
Factorial(5);;
#+end_src")
                 120)))

(ert-deftest ob-gap/batch-value-table ()
  (should (equal (ob-gap-test-run "#+begin_src gap :results value
List([1..3], i -> [i, i^2, Concatenation(\"x\", String(i))]);
#+end_src")
                 '((1 1 "x1") (2 4 "x2") (3 9 "x3")))))

(ert-deftest ob-gap/batch-value-flat-list ()
  (should (equal (ob-gap-test-run "#+begin_src gap :results value
[2, 3, 5];
#+end_src")
                 '((2 3 5)))))

(ert-deftest ob-gap/batch-value-string ()
  (should (equal (ob-gap-test-run "#+begin_src gap :results value
\"abc\";
#+end_src")
                 "abc")))

(ert-deftest ob-gap/batch-value-group ()
  (should (equal (ob-gap-test-run "#+begin_src gap :results value
SymmetricGroup(3);
#+end_src")
                 "SymmetricGroup( [ 1 .. 3 ] )")))

(ert-deftest ob-gap/batch-error-reported ()
  ;; With --quitonbreak, the error goes to the error buffer and the
  ;; rest of the block is not run.
  (let ((res (ob-gap-test-run "#+begin_src gap
NoSuchFunction(1);
Print(\"after\\n\");
#+end_src")))
    (should-not (and res (string-match-p "after" res)))
    (should (with-current-buffer org-babel-error-buffer-name
              (string-match-p "NoSuchFunction" (buffer-string))))))

(ert-deftest ob-gap/batch-cmdline ()
  (should (equal (ob-gap-test-run "#+begin_src gap :cmdline -o 1g
1+1;
#+end_src")
                 "2\n")))

(ert-deftest ob-gap/noweb ()
  (should (equal (ob-gap-test-with "#+name: defs
#+begin_src gap
f := x -> x^2;;
#+end_src

#+begin_src gap :noweb yes
<<defs>>
f(7);
#+end_src"
                   (re-search-forward "#\\+begin_src")
                   (org-babel-execute-src-block))
                 "49\n")))

;;; Sessions

(ert-deftest ob-gap/session-state ()
  (unwind-protect
      (progn
        (ob-gap-test-run "#+begin_src gap :session t1
G := SymmetricGroup(4);;
#+end_src")
        (should (equal (ob-gap-test-run "#+begin_src gap :session t1
Size(G);
#+end_src")
                       "24\n")))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-error-recovers ()
  (unwind-protect
      (progn
        (let ((res (ob-gap-test-run "#+begin_src gap :session t2
NoSuchFunction(1);
#+end_src")))
          (should (string-match-p "Error" res)))
        (should (equal (ob-gap-test-run "#+begin_src gap :session t2
2+2;
#+end_src")
                       "4\n")))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-value-and-vars ()
  (unwind-protect
      (progn
        (should (equal (ob-gap-test-run "#+begin_src gap :session t3 :results value :var n=5
List([1..n], i -> [i, i*i]);
#+end_src")
                       '((1 1) (2 4) (3 9) (4 16) (5 25))))
        (should (equal (ob-gap-test-run "#+begin_src gap :session t3 :results value
n + 1;
#+end_src")
                       6)))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-multiline-function ()
  (unwind-protect
      (should (equal (ob-gap-test-run "#+begin_src gap :session t4
f := function(n)
  local i, s;
  s := 0;
  for i in [1..n] do s := s + i; od;
  return s;
end;;
f(100);
#+end_src")
                     "5050\n"))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-missing-semicolon ()
  ;; Used to hang: the trailing statement swallowed the end marker.
  (unwind-protect
      (let ((org-babel-gap-session-timeout 20))
        (let ((res (ob-gap-test-run "#+begin_src gap :session t5
1+1;
2+2
#+end_src")))
          (should (string-match-p "\\`2\n" res))
          (should (string-match-p "Syntax error" res)))
        ;; The session is still usable afterwards.
        (should (equal (ob-gap-test-run "#+begin_src gap :session t5
3+3;
#+end_src")
                       "6\n")))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-unclosed-block ()
  (unwind-protect
      (let ((org-babel-gap-session-timeout 20))
        (should (string-match-p "Syntax error"
                                (ob-gap-test-run "#+begin_src gap :session t6
if true then
  Print(1);
#+end_src")))
        (should (equal (ob-gap-test-run "#+begin_src gap :session t6
\"still alive\";
#+end_src")
                       "\"still alive\"\n")))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/session-value-with-double-semicolon ()
  (unwind-protect
      (should (equal (ob-gap-test-run "#+begin_src gap :session t7 :results value
Print(\"noise\\n\");
Factorial(6);;
#+end_src")
                     720))
    (ob-gap-test-kill-sessions)))

(ert-deftest ob-gap/batch-missing-semicolon ()
  (ob-gap-test-run "#+begin_src gap
1+1;
2+2
#+end_src")
  (should (with-current-buffer org-babel-error-buffer-name
            (string-match-p "Syntax error" (buffer-string)))))

;;; Tangling

(ert-deftest ob-gap/tangle ()
  (let* ((dir (make-temp-file "ob-gap-tangle" t))
         (org-file (expand-file-name "t.org" dir)))
    (with-temp-file org-file
      (insert "#+begin_src gap :tangle yes :var x=2
x^3;
#+end_src\n"))
    (with-current-buffer (find-file-noselect org-file)
      (org-babel-tangle)
      (kill-buffer))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "t.g" dir))
      (should (string-match-p "x := 2;;\nx^3;" (buffer-string))))))

(provide 'ob-gap-tests)
;;; ob-gap-tests.el ends here

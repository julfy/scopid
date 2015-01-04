;; Scopidc - emacs extention for monitoring identifiers considering their visibility scopes.
;; Copyright (C) 2015  <julfy (julfy777@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(defvar *scopid-mem-file* "~/.emacs.d/scopid-mem.dat")
(defvar *scopid-current-file-data* (make-hash-table :test #'equal))
(defvar *scopid-global-identifiers-data* (make-hash-table :test #'equal))
(defvar *scopid-files-data* (make-hash-table :test #'equal))

(defstruct scopid-ident-pos
  file
  package
  row
  col)

(defun scopid-run-shell (command)
  (with-output-to-string (s)
                         (run-program "/bin/bash" (list "-c" command) :wait t :output s)
    s))

(defun scopid-split (string char)
  (let ((str))
   (loop for i = 0 then (1+ j)
         as j = (position char string :start i)
         when (search ".lsp" (setq str (subseq string i j)))
         collect str
         while j)))

(defun scopid-parse-file-for-globals (file)
  (let ((modtime (scopid-run-shell (format nil "date -r ~A" file))))
    (when (not (equal (gethash file *scopid-files-data*) modtime))
      (setf (gethash file *scopid-files-data*) modtime)
      ;; (format t ">>> ~A : ~A~%" file modtime)
      
      )
    )
  )

(defun scopid-parse-dir (dir)
  (setq dir (scopid-run-shell (format nil "realpath -z ~A" dir)))
  (let ((files (scopid-split (scopid-run-shell (format nil "find ~A -name '*.lsp'" dir)) #\Newline)))
    (dolist (file files)
      (let (;(info (scopid-parse-file-for-globals file))
            )
        (format t ">>> ~A~%" file)
        )
      )
  )
)
#|

(defun scopid-parse-current-file (file)
  
  ) 

(defun scopid-get-all-files-in-dir (dir)

)

(defun scopid-highlight-occurences (ident file)

)

(defun scopid-find-occurences (ident file)

)

(defun scopid-find-def (ident file)

)

(defun scopid-compare-scopes (scope1 scope2)
  ;; each successfull match - +1; scope1 - def; scope2 - ident; scope1 is basic
)

(defun scopid-dump-data ()
  ;; Possibly redundant
)

(defun scopid-load-previous-data ()
  ;; Possibly redundant
)
|#


;; ---------------------------------------------------------
;; PARSER
;; ---------------------------------------------------------
;; find def -> check if def matches
(defvar ^s^valid-ident-chars nil)
(setq ^s^valid-ident-chars '(#\= #\& #\? #\* #\^ #\% #\$ #\# #\@ #\!
                               #\~ #\> #\< #\. #\- #\_ #\+ #\[ #\] #\{
                               #\} #\/ #\q #\w #\e #\r #\t #\y #\u #\i
                               #\o #\p #\a #\s #\d #\f #\g #\h #\j #\k
                               #\l #\z #\x #\c #\v #\b #\n #\m #\: #\1
                               #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(defvar ^s^whitespace nil)
(setq ^s^whitespace '(#\Newline #\Linefeed #\Tab #\Space #\ ))
(defvar ^s^scope-words nil)
(setq ^s^scope-words'("defun" "defmacro" "defmethod" "defrtf" ;;theese for globals i guess
                      "lambda" "multiple-value-bind"
                      "let" "let*" "do" "labels"))
(defvar ^s^current-scope 0)
(defvar ^s^c-tkn nil)
(defvar ^s^scope-stack '())
(defvar tmp)

(defun ^s^parser (file)
  ;; TODO: handle packages
  (setq ^s^current-scope 0)
  (setq ^s^scope-stack '())
  (setq tmp nil)
  (with-open-file (input file)
                  
                  (^s^program input)
                  )
)

(defmacro ^s^read ()
  `(print (setq ^s^c-tkn (^s^get-token stream))))
(defun ^s^program (stream)
  (^s^expression stream)
  (print "------------")
  (when ^s^c-tkn
    (^s^program stream))
  )

(defun ^s^expression (stream)
  (print "^expr")
  (^s^read)
  (if (equal "(" ^s^c-tkn)
      (^s^list stream)
    (when (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
      (^s^ident stream)))
  (print "<^expr")
  )

(defun ^s^list (stream)
  (print "^list")
  (push ^s^current-scope ^s^scope-stack)
  (^s^expression stream)
  (^s^read)
  (when (and (not (equal ")" ^s^c-tkn)) ^s^c-tkn) ;; Do deeper
    (^s^list stream))
  (pop ^s^scope-stack)
  (print "<^list")
)

(defun ^s^ident (stream)
  (print "^ident")
  (cond
   ((member ^s^c-tkn ^s^scope-words :test #'equal)
    (incf ^s^current-scope)
    (^s^read) ;; Skip name
    (^s^read) ;; Skip "("
    (^s^read-ident-list stream))
   )
  (print "<^ident")
  )

(defun ^s^read-ident-list (stream)
  (print "^read-ident-l")
  (^s^read)
  (cond
   ((equal "(" ^s^c-tkn)
    (^s^read)
    (^s^add-ident ^s^c-tkn)
    (^s^expression stream)
    (^s^read)) ;; Skip ")"
   ((or (equal ")" ^s^c-tkn) (not ^s^c-tkn)) t) ;; Skip case
   (t (^s^add-ident ^s^c-tkn)))
  (when (and (not (equal ")" ^s^c-tkn)) ^s^c-tkn) ;; Do deeper
    (^s^read-ident-list stream)))

(defun ^s^add-ident (ident)
  (push ident tmp)
  ) 

(defun ^s^get-token (stream)
  (let ((flag nil) (token nil) (screen nil))
    (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
        ((or (not (characterp c)) flag) (if (characterp c) (unread-char c stream)))
      (cond
           ((and (eq #\\ c) (not screen)) (setf screen t)) ;; backslash
           ((member c ^s^whitespace) ;; whitespace
            (if token
                (setq flag t)))
           ((eq #\; c) ;; comment
            (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
                ((or (not (characterp c)) (eq #\Newline c)))))
           ((eq #\# c) ;; reader macro
            (if (or token screen)
               (progn (push c token)
                      (when screen
                        (setf screen nil)))
               (progn
                 (setq c (read-char stream nil 'the-end))
                 (cond
                  ((eq #\\ c)
                   (setq c (read-char stream nil 'the-end))
                   (unless (eq #\  c)
                     (if (characterp c) (unread-char c stream))  
                     (^s^get-token stream)))
                  ((eq #\' c)
                   (setf token (reverse (coerce (^s^get-token stream) 'list)))
                   (setf flag t))
                  ((eq #\| c)
                   (let ((endc nil))
                     (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
                         ((or (not (characterp c)) endc) (if (characterp c) (unread-char c stream)))
                       (when (eq #\| c)
                         (setq c (read-char stream nil 'the-end))
                         (if (eq #\# c)
                           (setq endc t)
                           (if (characterp c) (unread-char c stream)))))))
                  (t (^s^get-token stream))))))
           ((or (member c ^s^valid-ident-chars) screen) ;; ident
             (push c token)
             (when screen
               (setf screen nil)))
           ((eq #\" c) ;; string
             (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
                 ((or (not (characterp c)) (and (eq #\" c) (not screen))))
               (if (and (eq #\\ c) (not screen))
                   (setf screen t))
               (if screen
                   (setf screen nil))))
           (t (setf flag t) ;; single char lexemes
              (if (and (characterp c) token)
                 (unread-char c stream)
                 (push c token)))))
    (if token
        (coerce (reverse token) 'string)
        token)))
  

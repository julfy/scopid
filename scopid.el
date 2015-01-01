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
    (when (not (equal (gethash file *scopid-files*) modtime))
      (setf (gethash file *scopid-files*) modtime)
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
;; LEXER
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
(defvar ^s^current-scope 0)
(defvar ^s^scope-stack '())

(defun ^s^lexer (file)
  (with-open-file (input file)
                  (do ((tkn (^s^get-token input) (^s^get-token input)))
                      ((not tkn))
                    (format t "~A " tkn))
                  )
)

(defun ^s^get-token (stream)
  (let ((flag nil) (token nil) (screen nil))
    (do ((c (read-char stream) (read-char stream nil 'the-end)))
        ((or (not (characterp c)) flag) (unread-char c stream))
      (cond
           ((and (eq #\\ c) (not screen)) (setf screen t)) ;; backslash
           ((member c ^s^whitespace) ;; whitespace
            (if token
                (setq flag t)))
           ;; TODO comment
           ((eq #\# c) ;; reader macro
            (if (or token screen)
               (progn (push c token)
                      (when screen
                        (setf screen nil)))
               (progn
                 (setq c (read-char stream))
                 (cond
                  ((eq #\\ c)
                   (setq c (read-char stream))
                   (unless (eq #\  c)
                     (unread-char c stream)  
                     (^s^get-token stream)))
                  ((eq #\' c)
                   (setf token (reverse (coerce (^s^get-token stream) 'list)))
                   (setf flag t))
                  ;; TODO comment
                  (t (^s^get-token stream))))))
           ((or (member c ^s^valid-ident-chars) screen) ;; ident
             (push c token)
             (when screen
               (setf screen nil)))
           ((eq #\" c) ;; string
             (do ((c (read-char stream) (read-char stream nil 'the-end)))
                 ((and (eq #\" c) (not screen)))
               (if (and (eq #\\ c) (not screen))
                   (setf screen t))
               (if screen
                   (setf screen nil)))
             (push #\" token)
             (setf flag t))
           (t (setf flag t) ;; single char lexemes
              (if token
                 (unread-char c stream)
                 (push c token)))))
    (if token
        (coerce (reverse token) 'string)
        token)))
  

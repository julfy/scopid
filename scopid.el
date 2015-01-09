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
(defvar *scopid-local-def-data* nil)
(defvar *scopid-buffer-data* nil)
(defvar *scopid-global-def-data* nil)
(defvar *scopid-files-data* (make-hash-table :test #'equal))
;; TODO: add included packages in package

(defstruct ^s^ident-pos
  file
  package
  scope
  start
  end)

(defun scopid-util-run-shell (command)
  (with-output-to-string (s)
                         (run-program "/bin/bash" (list "-c" command) :wait t :output s)
    s))

(defun scopid-util-split (string char)
  (let ((str))
   (loop for i = 0 then (1+ j)
         as j = (position char string :start i)
         when (search ".lsp" (setq str (subseq string i j)))
         collect str
         while j)))

(defun scopid-parse-file-for-globals (file)
  (let ((modtime (scopid-util-run-shell (format nil "date -r ~A" file))))
    (when (not (equal (gethash file *scopid-files-data*) modtime))
      (setf (gethash file *scopid-files-data*) modtime)
      (scopid-clean-idents file)
      (^s^global-scope-parser file))))

(defun scopid-parse-dir (dir)
  (setq dir (scopid-util-run-shell (format nil "realpath -z ~A" dir)))
  (let ((files (scopid-util-split (scopid-util-run-shell (format nil "find ~A -name '*.lsp'" dir)) #\Newline)))
    (dolist (file files)
      (scopid-parse-file-for-globals file))))

(defun scopid-clean-idents (file)
  (setq *scopid-global-def-data* (reduce (lambda (lst val) (if (equal file (^s^ident-pos-file (cdr val)))
                                                               lst
                                                               (append lst (list val))))
                                         *scopid-global-def-data*
                                         :initial-value nil)))

(defun scopid-parse-current-buffer ()
  (with-current-buffer (current-buffer)
    (^s^local-scope-parser (buffer-file-name (current-buffer)))
    (scopid-parse-file-for-globals (buffer-file-name (current-buffer))))
  *scopid-local-def-data*)


(defun scopid-find-def (ident file)
  (let ((found nil)
        (found-relevance 0))
    )
)

(defun scopid-compare-scopes (defn ident)
  (let ((defn-scope (reverse (^s^ident-pos-scope defn)))
        (ident-scope (reverse (^s^ident-pos-scope ident)))
        (num 0))
    (format t "~A ~A~%" defn-scope ident-scope)
    (do* ((i 0 (1+ i))
          (def (car defn-scope) (nth i defn-scope))
          (id (car ident-scope) (nth i ident-scope)))
        ((eq def nil))
      (format t "~A:~A ~A~%" i def id)
      (if (and def (eq id def))
          (incf num)
          (if (or (and def id) (and def (not id)))
              (return-from scopid-compare-scopes nil))))
    num))
#|
(defun scopid-highlight-occurences ()
  (scopid-parse-current-buffer)
  
  )

(defun scopid-find-occurences (ident file)
  
  )

(defun scopid-dump-data ()
  
  )

(defun scopid-load-previous-data ()

  )
|#

;; ---------------------------------------------------------
;; PARSER
;; ---------------------------------------------------------

(defvar ^s^valid-ident-chars nil)
(setq ^s^valid-ident-chars '(#\= #\& #\? #\* #\^ #\% #\$ #\# #\@ #\!
                               #\~ #\> #\< #\. #\- #\_ #\+ #\[ #\] #\{
                               #\} #\/ #\q #\w #\e #\r #\t #\y #\u #\i
                               #\o #\p #\a #\s #\d #\f #\g #\h #\j #\k
                               #\l #\z #\x #\c #\v #\b #\n #\m #\: #\1
                               #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
(defvar ^s^whitespace nil)
(setq ^s^whitespace '(#\Newline #\Linefeed #\Tab #\Space #\ ))
(defvar ^s^named-scope-words nil)
(setq ^s^named-scope-words '("defun" "defmacro" "defmethod" "defrtf"
                             ))
(defvar ^s^scope-words nil)
(setq ^s^scope-words '("lambda" "multiple-value-bind"
                       "let" "let*" "do" "labels"
                       ))
(defvar ^s^global-def-words '("defun" "defmacro" "defmethod" "defrtf"
                              "defvar" "defparameter" "defconstant"
                              "deftype"
                              ))
(defvar ^s^current-package nil)
(defvar ^s^current-file nil)
(defvar ^s^current-scope 0)
(defvar ^s^c-tkn nil)
(defvar ^s^scope-stack '())
(defvar ^s^parser-stack nil)
(defvar ^s^token-pos 0)

(defun ^s^global-scope-parser (file)
  (setq ^s^current-package nil)
  (setq ^s^current-file file)
  (with-open-file (stream file)
                  (do ((flag nil))
                      (flag)
                    (^s^read)
                    (if (not ^s^c-tkn)
                        (setq flag t)
                      (^s^global-definition stream)))))

(defun ^s^global-definition (stream)
  (cond
   ;; TODO: handle  structs/classes/defpackage
   ((equal "in-package" ^s^c-tkn) ;; get package
    (^s^read)
    (when (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
      (setq ^s^current-package ^s^c-tkn)))
   ((member ^s^c-tkn ^s^named-scope-words :test #'equal) ;; get ident definition
    (^s^read)
    (when (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
      (^s^add-global-def ^s^c-tkn)))))

(defun ^s^add-global-def (ident)
  (push (cons ident 
              (make-^s^ident-pos :end ^s^token-pos
                                 :start (- ^s^token-pos (length ident))
                                 :file ^s^current-file
                                 :package ^s^current-package))
        *scopid-global-def-data*))

(defun ^s^local-scope-parser (file)
  (setq ^s^current-scope 0)
  (setq ^s^scope-stack nil)
  (setq ^s^parser-stack nil)
  (setq *scopid-buffer-data* nil)
  (setq *scopid-local-def-data* nil)
  (with-open-file (input file)
                  (^s^program input)))

(defmacro ^s^read ()
  `(setq ^s^c-tkn (or (pop ^s^parser-stack) (^s^get-token stream))))

(defmacro ^s^unread ()
  `(push ^s^c-tkn ^s^parser-stack))

(defun ^s^program (stream)
  (^s^expression stream)
  (when ^s^c-tkn
    (^s^program stream)))

(defun ^s^expression (stream)
  (^s^read)
  (if (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
    (if (equal "(" ^s^c-tkn) 
        (^s^list stream)
      (^s^ident stream))
    (^s^unread)))

(defun ^s^list (stream)
  (push ^s^current-scope ^s^scope-stack)
  (do ((flag nil))
      (flag)
    (^s^read)
    (if (or (equal ")" ^s^c-tkn) (not ^s^c-tkn))
      (setq flag t)
      (progn (^s^unread)
             (^s^expression stream))))      
  (pop ^s^scope-stack))

(defun ^s^ident (stream)
  (cond
   ((equal "in-package" ^s^c-tkn) ;; get package
    (^s^read)
    (when (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
      (setq ^s^current-package ^s^c-tkn)))
   ((member ^s^c-tkn ^s^named-scope-words :test #'equal) ;; scope-def with name
    (incf ^s^current-scope)
    (print ^s^scope-stack)
    (^s^read) ;; Skip name
    (^s^read) ;; Skip "("
    (^s^parameter-list stream))
   ((member ^s^c-tkn ^s^scope-words :test #'equal) ;; scope-def without name
    (incf ^s^current-scope)
    (print ^s^scope-stack)
    (^s^read) ;; Skip "("
    (^s^parameter-list stream))
   (t (^s^add-occurence ^s^c-tkn)) ;; TODO: handle <package name>:<ident>
   ))

(defun ^s^parameter-list (stream)
  (do ((flag nil))
      (flag)
    (^s^read)
    (if (or (equal ")" ^s^c-tkn) (not ^s^c-tkn))
      (setq flag t)
      (progn (^s^unread)
             (^s^parameter stream)))))

(defun ^s^parameter (stream)
  (^s^read)
  (cond
   ((equal "(" ^s^c-tkn) ;; ( id expr? )
    (^s^read)
    (if (or (not ^s^c-tkn) (equal ")" ^s^c-tkn)) ;; ()
        (return-from ^s^parameter))
    (^s^add-local-def ^s^c-tkn)
    (do ((flag nil))
      (flag)
      (^s^read)
      (if (or (equal ")" ^s^c-tkn) (not ^s^c-tkn))
          (setq flag t)
        (progn (^s^unread)
               (^s^expression stream)))))
   ((or (not ^s^c-tkn) (equal ")" ^s^c-tkn)) ;; no params
    (^s^unread))
   (t (^s^add-local-def ^s^c-tkn))))

(defun ^s^add-local-def (ident)
  (push (cons ident 
              (make-^s^ident-pos :end ^s^token-pos
                                 :start (- ^s^token-pos (length ident))
                                 :scope ^s^scope-stack))
        *scopid-local-def-data*))

(defun ^s^add-occurence (ident)
  (push (cons ident
              (make-^s^ident-pos :end ^s^token-pos
                                 :start (- ^s^token-pos (length ident))
                                 :scope ^s^scope-stack))
        *scopid-buffer-data*))

(defun ^s^get-token (stream)
  (let ((flag nil) (token nil) (screen nil))
    (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
        ((or (not (characterp c)) flag) (if (characterp c) (unread-char c stream)))
      (cond
           ((and (eq #\\ c) (not screen)) (setq screen t)) ;; backslash
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
                        (setq screen nil)))
               (progn
                 (setq c (read-char stream nil 'the-end))
                 (cond
                  ((eq #\\ c)
                   (setq c (read-char stream nil 'the-end))
                   (unless (eq #\  c)
                     (if (characterp c) (unread-char c stream))  
                     (^s^get-token stream)))
                  ((eq #\' c)
                   (setq token (reverse (coerce (^s^get-token stream) 'list)))
                   (setq flag t))
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
               (setq screen nil)))
           ((eq #\" c) ;; string
             (do ((c (read-char stream nil 'the-end) (read-char stream nil 'the-end)))
                 ((or (not (characterp c)) (and (eq #\" c) (not screen))))
               (if (and (eq #\\ c) (not screen))
                   (setq screen t))
               (if screen
                   (setq screen nil))))
           (t (setq flag t) ;; single char lexemes
              (if (and (characterp c) token)
                 (unread-char c stream)
                 (push c token)))))
    (setq ^s^token-pos (1- (file-position stream)))
    (if token
        (string-downcase (coerce (reverse token) 'string))
      token)))

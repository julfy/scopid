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
(defvar *scopid-highlight-stack* nil)
;; TODO: add included packages in package

(defstruct ^s^ident-pos
  file
  package
  scope
  start
  end)

(defun scopid-util-split (string char)
  (let ((str))
   (loop for i = 0 then (1+ j)
         as j = (position char string :start i)
         when (search ".lsp" (setq str (subseq string i j)))
         collect str
         while j)))

(cl-defun scopid-util-member (what where &key (test #'equal))
  (if (and where (not (funcall test what (car where))))
      (scopid-util-member what (cdr where) :test test)
      where))

;(defface highlight ((((class color) (min-colors 89)) (:background "grey60"))))

(defun scopid-parse-file-for-globals (file)
  (let ((modtime (shell-command-to-string (format "date -r %s" file))))
    (when (not (equal (gethash file *scopid-files-data*) modtime))
      (setf (gethash file *scopid-files-data*) modtime)
      (scopid-clean-idents file)
      (^s^global-scope-parser file))))

(defun scopid-parse-dir (dir)
  (setq dir (shell-command-to-string (format "realpath -z %s" dir)))
  (let ((files (scopid-util-split (shell-command-to-string (format "find %s -name '*.lsp'" dir)) ?\n)))
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

(defun scopid-find-def (ident)
  (let ((found nil)
        (max-relevance 0))
    (cl-labels ((%match-local (lst)
                     (let ((found-relevance (or (scopid-compare-scopes (cdar lst) (cdr ident)) 0)))
                       (when (> found-relevance max-relevance)
                           (setq max-relevance found-relevance)
                           (setq found (car lst)))
                       (if lst
                           (%match-local (scopid-util-member (car ident)
                                           (cdr lst)
                                           :test #'(lambda (x y)
                                                     (equal x (car y))))))))
             (%match-global (lst) ;; TODO: handle packages
                            (car (scopid-util-member (car ident)
                                         lst
                                         :test #'(lambda (x y)
                                                   (equal x (car y)))))))
      (%match-local (scopid-util-member (car ident)
                      *scopid-local-def-data*
                      :test #'(lambda (x y)
                                (equal x (car y)))))
      (unless found
        (setq found (%match-global *scopid-global-def-data*)))
      found)))

(defun scopid-compare-scopes (defn ident)
  (catch 'root
    (if (and defn ident)
        (let ((defn-scope (reverse (^s^ident-pos-scope defn)))
              (ident-scope (reverse (^s^ident-pos-scope ident)))
              (num 0))
          (do* ((i 0 (1+ i))
                (def (car defn-scope) (nth i defn-scope))
                (id (car ident-scope) (nth i ident-scope)))
              ((eq def nil))
            (if (and def (eq id def))
                (incf num)
              (if (or (and def id) (and def (not id)))
                  (throw 'root nil))))
          num))))

(defun scopid-get-ident-by-pos (pos)
  (catch 'root
    (mapc (lambda (ident)
            (if (and (>= pos (^s^ident-pos-start (cdr ident)))
                     (<= pos (^s^ident-pos-end (cdr ident))))
                (throw 'root ident)))
          *scopid-buffer-data*)
    nil))

(global-set-key (kbd "C-c d") 'scopid-delete-highlighting)
(defun scopid-delete-highlighting ()
  (interactive)
  (format "Deleted %s highlights."
          (length (mapc (lambda (o)
                          (when (overlay-get o 'scopid)
                            (delete-overlay o)))
                        (overlays-in (point-min) (point-max))))))

(global-set-key (kbd "C-c h") 'scopid-highlight-occurences)
(defun scopid-highlight-occurences ()
  (interactive)
  (scopid-parse-current-buffer) ;; TODO: highlight definition also
  (let* ((def (scopid-find-def (scopid-get-ident-by-pos (point))))
         (idents (scopid-find-occurences def)))
    (dolist (id idents)
      (let ((ovr (make-overlay (^s^ident-pos-start (cdr id))
                               (^s^ident-pos-end (cdr id))
                               (current-buffer)
                               nil t)))
        (overlay-put ovr 'face '(:background "#008800"))
        (overlay-put ovr 'scopid t)))
    (format "Found %s occurences." (length idents)) 
    ))

(defun scopid-find-occurences (def)
  (reduce (lambda (lst ident)
            (if (and (equal (car def) (car ident))
                     (equal def (scopid-find-def ident)))
                (append lst (list ident))
                lst))
          *scopid-buffer-data*
          :initial-value nil))

(defun scopid-dump-data ()
  
  )

(defun scopid-load-previous-data ()

  )

;; ---------------------------------------------------------
;; PARSER
;; ---------------------------------------------------------

(defvar ^s^valid-ident-chars nil)
(setq ^s^valid-ident-chars '(?= ?& ?? ?* ?^ ?% ?$ ?\# ?@ ?!
                               ?~ ?> ?< ?. ?- ?_ ?+ ?[ ?] ?{
                               ?} ?/ ?q ?w ?e ?r ?t ?y ?u ?i
                               ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k
                               ?l ?z ?x ?c ?v ?b ?n ?m ?: ?1
                               ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
(defvar ^s^whitespace nil)
(setq ^s^whitespace '(?\n ?\t ?\v ?\s ?\r))
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
(defvar ^s^stream-pos 0)

(defun ^s^global-scope-parser (file)
  (setq ^s^current-package nil)
  (setq ^s^current-file file)
  (setq ^s^stream-pos 0)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((stream (coerce (buffer-string) 'list)))
      (do ((flag nil))
                      (flag)
                    (^s^read)
                    (if (not ^s^c-tkn)
                        (setq flag t)
                      (^s^global-definition stream))))))

(defun ^s^global-definition (stream)
  (cond
   ;; TODO: handle  structs/classes/defpackage
   ((equal "in-package" ^s^c-tkn) ;; get package
    (^s^read)
    (when (and ^s^c-tkn (not (equal ")" ^s^c-tkn)))
      (setq ^s^current-package ^s^c-tkn)))
   ((member ^s^c-tkn ^s^named-scope-words) ;; get ident definition
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
  (setq ^s^stream-pos 0)
  (setq *scopid-buffer-data* nil)
  (setq *scopid-local-def-data* nil)
  (with-temp-buffer
    (insert-file-contents file)
    (^s^program (coerce (buffer-string) 'list))))

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
   ((member ^s^c-tkn ^s^named-scope-words) ;; scope-def with name
    (incf ^s^current-scope)
    (^s^read) ;; Skip name
    (^s^read) ;; Skip "("
    (^s^parameter-list stream))
   ((member ^s^c-tkn ^s^scope-words) ;; scope-def without name
    (incf ^s^current-scope)
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
  (catch 'root
    (cond
     ((equal "(" ^s^c-tkn) ;; ( id expr? )
      (^s^read)
      (if (or (not ^s^c-tkn) (equal ")" ^s^c-tkn)) ;; ()
          (throw 'root nil))
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
     (t (^s^add-local-def ^s^c-tkn)))))

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

(defun ^s^read-char (stream &rest r)
  (declare (ignore r))
  (incf ^s^stream-pos)
  (nth (1- ^s^stream-pos) stream))

(defun ^s^unread-char (c stream)
  (declare (ignore c stream))
  (decf ^s^stream-pos))

(defun ^s^get-token (stream)
  (let ((flag nil) (token nil) (screen nil) (end nil))
    (do ((c (^s^read-char stream nil 'the-end) (^s^read-char stream nil) 'the-end))
        ((or (not (characterp c)) flag) (if (characterp c) (^s^unread-char c stream)))
      (cond
           ((and (eq ?\\ c) (not screen)) (setq screen t)) ;; backslash
           ((member c ^s^whitespace) ;; whitespace
            (when token
                (setq flag t)
                (setq end ^s^stream-pos)))
           ((eq ?\; c) ;; comment
            (do ((c (^s^read-char stream nil 'the-end) (^s^read-char stream nil 'the-end)))
                ((or (not (characterp c)) (eq ?\n c)))))
           ((eq ?\# c) ;; reader macro
            (if (or token screen)
               (progn (push c token)
                      (when screen
                        (setq screen nil)))
               (progn
                 (setq c (^s^read-char stream nil 'the-end))
                 (cond
                  ((eq ?\\ c)
                   (setq c (^s^read-char stream nil 'the-end))
                   (unless (member c ^s^whitespace)
                     (if (characterp c) (^s^unread-char c stream))  
                     (^s^get-token stream)))
                  ((eq ?' c)
                   (setq token (reverse (coerce (^s^get-token stream) 'list)))
                   (setq flag t)
                   (setq end 0))
                  ((eq ?\| c)
                   (let ((endc nil))
                     (do ((c (^s^read-char stream nil 'the-end) (^s^read-char stream nil 'the-end)))
                         ((or (not (characterp c)) endc) (if (characterp c) (^s^unread-char c stream)))
                       (when (eq ?\| c)
                         (setq c (^s^read-char stream nil 'the-end))
                         (if (eq ?\# c)
                           (setq endc t)
                           (if (characterp c) (^s^unread-char c stream)))))))
                  (t (^s^get-token stream))))))
           ((or (member c ^s^valid-ident-chars) screen) ;; ident
             (push c token)
             (when screen
               (setq screen nil)))
           ((eq ?\" c) ;; string
             (do ((c (^s^read-char stream nil 'the-end) (^s^read-char stream nil 'the-end)))
                 ((or (not (characterp c)) (and (eq ?\" c) (not screen))))
               (if (and (eq ?\\ c) (not screen))
                   (setq screen t))
               (if screen
                   (setq screen nil))))
           (t (setq flag t) ;; single char lexemes
              (setq end ^s^stream-pos)
              (if (and (characterp c) token)
                 (^s^unread-char c stream)
                 (push c token)))))
    (setq ^s^token-pos (or end 0))
    (if token
        (downcase (coerce (reverse token) 'string))
      token)))

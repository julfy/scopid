;; Scopidc - emacs extention for monitoring identifiers considering their visibility scopes.
;; Copyright (C) 2015  <julfy

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
(defvar *scopid-global-identifiers* (make-hash-table :test #'equal))
(defvar *scopid-files* (make-hash-table :test #'equal))

(defstruct scopid-ident-pos
  file
  row
  col)

(defun scopid-run-shell (command)
  (with-output-to-string (s)
                         (run-program "/bin/bash" (list "-c" (print command)) :wait t :output s)
    s))

(defun scopid-split (string char)
  (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))

(defun scopid-parse-file (file)
  
)

(defun scopid-get-all-files-in-dir (dir)

)

(defun scopid-parse-dir (dir)
  (setq dir (scopid-run-shell (format nil "realpath -z ~A" dir)))
  (let ((files (remove-if-not (lambda (s) (search ".lsp" s))
                              (scopid-split (scopid-run-shell (format nil "find ~A -wholename '*.lsp'" dir))
                                            #\Newline))))

  )
)

(defun scopid-highlight-occurences (ident file)

)

(defun scopid-find-occurences (ident file)

)

(defun scopid-find-def (ident file)

)

(defun scopid-dump-data ()
  ;; Possibly redundant
)

(defun scopid-load-previous-data ()
  ;; Possibly redudant
)

;;; ivy-decorator.el --- Helpers for Ivy display transformers -*- lexical-binding: t -*-

;; Copyright (C) 2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.11"))
;; URL: https://github.com/akirak/ivy-decorator

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package lets you define display transformers for ivy
;; concisely.

;;; Code:

(require 'cl-lib)

(defconst ivy-decorator-prefix "ivy-decorator-")

(defgroup ivy-decorator nil
  "Help you define Ivy display transformers."
  :group 'ivy)

;;;; Custom variables

(defcustom ivy-decorator-field-separator "  "
  "String used to separate between fields in Ivy entries."
  :type 'string
  :group 'ivy-decorator)

;;;; Faces
(defface ivy-decorator-command-name
  '((default :inherit font-lock-function-name-face))
  "Face for command names.")

(defface ivy-decorator-command-key
  '((default :inherit font-lock-string-face))
  "Face for command keys.")

;;;; Variables

(defvar ivy-decorator-original-candidate nil
  "Variable for temporarily storing the original candidate.")

;;;; Macros

;;;###autoload
(defmacro ivy-decorator-set (caller &rest columns)
  "Set the display transformer of a command.

This macro defines a byte-compiled function and sets the display
transformer of an Ivy command to the function.

CALLER is the name of the command, and COLUMNS is a list of
column definitions."
  (declare (indent 1))
  `(ivy-set-display-transformer ,caller
                                (byte-compile
                                 ,(ivy-decorator--make-formatter-on-object
                                   nil columns))))

;;;###autoload
(defmacro ivy-decorator-set-intermediate (caller to-obj &rest columns)
  "Set the display transformer of a command.

This macro defines a byte-compiled function and sets the display
transformer of an Ivy command to the function. Unlike
`ivy-decorator-set', the display transformer defined by this
macro produces an intermediate data from which output strings are
further produced.

CALLER is the name of the command, TO-OBJ is a function which
takes a candidate string as an argument and produces an
intermediate data.  COLUMNS is a list of column definitions.
The first item in each column must be a function on the intermediate
data, not on the original candidate string.
You can use `ivy-decorator-original' to return the original string."
  (declare (indent 2))
  `(ivy-set-display-transformer ,caller
                                (byte-compile
                                 ,(ivy-decorator--make-formatter-on-object
                                   to-obj columns))))

(defun ivy-decorator--make-formatter-on-object (to-obj columns)
  "Make a display transformer function with TO-OBJ and COLUMNS."
  (let ((compiled-columns (ivy-decorator--compile-columns columns)))
    `(function
      (lambda (candidate)
        (setq ivy-decorator-original-candidate candidate)
        (let ((object (if ,to-obj
                          (funcall ,to-obj candidate)
                        candidate)))
          (cl-loop for (func width . rest) in (quote ,compiled-columns)
                   with result = nil
                   collect (let* ((m (funcall func object))
                                  (len (if m
                                           (length m)
                                         0))
                                  (s (if (or (null width)
                                             (> len width))
                                         m
                                       (concat m (make-string (- width len) ?\s)))))
                             (if (and m rest)
                                 ;; The initial item of rest, if any, is a face
                                 (propertize s 'face (car rest))
                               s))
                   into result
                   finally return (string-join result
                                               ivy-decorator-field-separator)))))))

(defun ivy-decorator--compile-columns (columns)
  "Transform each format definition in COLUMNS."
  (cl-loop for (fmt . rest) in columns
           collect (cons (intern-soft (concat ivy-decorator-prefix
                                              (symbol-name fmt)))
                         rest)))

;;;; Formatting functions

;;;;; Generic

(defsubst ivy-decorator-original (_candidate)
  "Return the original string candidate."
  ;; You need to store the candidate in this variable beforehand
  ivy-decorator-original-candidate)

(defsubst ivy-decorator-id (candidate)
  "Return the CANDIDATE."
  candidate)

;;;;; Buffers

(defalias 'ivy-decorator-buffer-name 'buffer-name
  "Return the buffer name of a buffer.")

(defun ivy-decorator-buffer-major-mode (buffer)
  "Return the major mode of BUFFER as a string."
  (symbol-name (buffer-local-value 'major-mode buffer)))

(defun ivy-decorator-buffer-directory (buffer)
  "Return the directory of BUFFER if it is a file buffer."
  (if-let ((file (buffer-file-name buffer)))
      (abbreviate-file-name (file-name-directory file))
    nil))

;;;;; counsel-describe-*

(defun ivy-decorator-function-doc (symbol)
  "Return the first line of the documentation of a function SYMBOL."
  (when-let ((s (documentation symbol)))
    (car (split-string s "\n"))))

(defun ivy-decorator-variable-doc (symbol)
  "Return the first line of the variable documentation of SYMBOL."
  (when-let ((s (documentation-property symbol 'variable-documentation)))
    (car (split-string s "\n"))))

;;;;;  counsel-M-x

(defun ivy-decorator-command-name-and-key (symbol)
  "Return the name and the first keybinding of a command SYMBOL."
  (concat (propertize (symbol-name symbol)
                      'face 'ivy-decorator-command-name)
          (if-let ((key (ivy-decorator-command-key symbol)))
              (propertize (format " [%s]" key)
                          'face 'ivy-decorator-command-key)
            "")))

(defun ivy-decorator-command-key (symbol)
  "Return the first keybinding to a command SYMBOL."
  (thread-last (current-active-maps)
    (where-is-internal symbol)
    (cl-remove-if (lambda (v)
                    (pcase v
                      (`[,s] (symbolp s))
                      (_ (eql 'menu-bar (seq-elt v 0))))))
    (cl-some #'key-description)))

;;;;; Face

(defun ivy-decorator-face-name (symbol)
  "Given a face SYMBOL, propertize it with the face."
  (propertize ivy-decorator-original-candidate
              'face symbol))

(defun ivy-decorator-face-doc (symbol)
  "Return the face documentation of SYMBOL."
  (let ((doc (face-documentation symbol)))
    (propertize (if doc
                    (car (split-string doc "\n"))
                  "This face doesn't have documentation.")
                'face symbol)))

(provide 'ivy-decorator)
;;; ivy-decorator.el ends here

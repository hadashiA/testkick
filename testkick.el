;;; testkick.el --- an interface to `compile'

;; Copyright (C) 2012 hadashiA

;; Author: hadashiA <dev@hadashikick.jp>
;; $Id: smart-compile.el 764 2012-07-10 15:58:08Z zenitani $
;; Keywords: tools, unix, rspec, mocha
;; Created: 2012-10-14
;; Compatibility: Emacs 23 or later
;; URL(en): http://github.com/f-kubotar/testkick

;; Contributors: hadashiA

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;; custom vars

(defgroup testkick nil
  "run test command helper.")

(defcustom testkick-alist
  '(("rspec"
     :command "rspec --color --format documentation"
     :patterns ("^\\s-*describe\\s-+\\S-+\\s-+do")
     :test-root-basename "spec")
    
    ("mocha"
     :command "mocha --reporter spec"
     :patterns ("^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*(")
     :test-root-basename "test"))
  ""
  :group 'testkick)

(defcustom testkick-parent-directory-search-limit 10
  "Number of times up to look for a test directory"
  :group 'testkick)

;;
;; macros
;;

(defmacro testkick-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put #'testkick-aif 'lisp-indent-function 2)

(defmacro testkick-awhen (test-form &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(testkick-aif ,test-form
	(progn ,@body)))

;; 
;; structs
;; 

(defstruct testkick-test
  name
  command
  pattern
  test-root-basename
  test-file
  test-directory
  )

;; 
;; commands
;; 

(defun testkick ()
  (interactive)
  (unless (file-directory-p buffer-file-name)
    (testkick-awhen (testkick-test-from-file buffer-file-name)
      (testkick-test-run it))))

(defun testkick-with-more-options (options)
  (interactive)
  )

;; 
;; find test target
;;

(defun testkick-find-test-root (cur-dir)
  (loop for alist in testkick-alist)
  )

;; 
;; testkick-test methods
;; 

(defun testkick-test-run (testkick-test &optional target)
  (testkick-aif (or target
                    (testkick-test-test-file testkick-test)
                    (testkick-test-test-directory testkick-test))
      (compile (concat (testkick-test-command testkick-test) " " it))
    (error "invalid test target %s" testkick-test)))

(defun* testkick-test-from-file (file)
  (when (file-directory-p file)
    (return-from testkick-test-from-file))

  (let* ((file-not-opened (null (get-file-buffer file)))
         (buffer (or (get-file-buffer file)
                     (find-file-noselect file)))
         (result (with-current-buffer buffer
                   (loop named match-patterns
                         for alist in testkick-alist
                         do (destructuring-bind (name &key command patterns (test-root-basename nil)) alist
                              (loop for pattern in patterns
                                    do (when (and (goto-char (point-min))
                                                  (re-search-forward pattern nil t))
                                         (return-from match-patterns
                                           (make-testkick-test :name name
                                                               :command command
                                                               :test-root-basename test-root-basename 
                                                               :pattern pattern
                                                               :test-file file
                                                               )))))
                         ))))
    (when file-not-opened (kill-buffer buffer))
    result))

;; 
;; generic utils
;; 

(defun testkick-current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

(defun testkick-find-parent-directory (start-directory find-directory-name)
  (loop for count from 0 to 10
        for cur-dir = (expand-file-name
                       (concat start-directory (apply #'concat (make-list count "/.."))))
        until (string= cur-dir "/")
        when (string= (file-name-nondirectory cur-dir) find-directory-name)
        do (return cur-dir)
        ))

(provide 'testkick)


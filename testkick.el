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
     :file-pattern "_spec\\.rb$"
     :syntax-patterns ("^\\s-*describe\\s-+\\S-+\\s-+do")
     :root-basename "spec")
    
    ("mocha"
     :command "mocha --reporter spec"
     :file-pattern "\\.js$"
     :syntax-patterns ("^\\s-*describe\\s-*(\\s-*['\"]\\S-+['\"]\\s-*,\\s-*function\\s-*(")
     :root-basename "test"))
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

(defmacro testkick-alist-loop (args &rest body)
  `(loop for alist in testkick-alist
         do (let (loop for arg in args
                       for key = (intern (concat ":" (symbol-name arg)))
                       for value = (cond
                                    ((equal arg 'name) (car alist))
                                    (t (cadr (memq key (cdr alist)))))
                       collect (list arg value))
              ,@body)))

;; 
;; structs
;; 

(defstruct testkick-test
  name
  command
  syntax-pattern
  root-basename
  file
  root-directory
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

(defun testkick-find-test-root (&optional cur-dir)
  (setq cur-dir (or cur-dir (testkick-current-directory)))
  (loop for alist in testkick-alist
        do (destructuring-bind (name &key
                                     command
                                     file-pattern
                                     syntax-patterns
                                     (root-basename nil)) alist
             (testkick-awhen (and test-root-basename
                                  (testkick-find-directory-in-same-project cur-dir test-root-basename))
               ))))

(defun* testkick-find-test-in-directory (&optional cur-dir)
  (setq cur-dir (or cur-dir (testkick-current-directory)))
  (loop for alist in testkick-alist
        do (destructuring-bind (name &key
                                     command
                                     file-pattern
                                     syntax-patterns
                                     (root-basename nil)) alist
             (loop for file in (directory-files cur-dir t file-pattern)
                   do (testkick-awhen (testkick-test-from-file file)
                        (return-from it)))
             (loop for subdir in (directory-files cur-dir t)
                   when (file-directory-p subdir) (return-from (testkick-find-test-at-directory subdir)))
             )))

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
                         do (destructuring-bind (name &key
                                                      command
                                                      file-pattern
                                                      syntax-patterns
                                                      (root-basename nil)) alist
                              (loop for pattern in syntax-patterns
                                    do (when (and (goto-char (point-min))
                                                  (re-search-forward pattern nil t))
                                         (return-from match-patterns
                                           (make-testkick-test :name name
                                                               :command command
                                                               :root-basename root-basename 
                                                               :syntax-pattern pattern
                                                               :file file
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

(defun* testkick-find-directory-in-same-project (start-directory find-directory-name)
  (loop for count from 0 to testkick-parent-directory-search-limit
        for cur-dir = (expand-file-name
                       (concat start-directory (apply #'concat (make-list count "/.."))))
        until (string= cur-dir "/")
        when (file-directory-p cur-dir)
        do (loop for subdir in (directory-files cur-dir t)
                 when (and (file-directory-p subdir)
                           (string= (file-name-nondirectory (expand-file-name subdir))
                                    find-directory-name))
                 do (return-from testkick-find-directory-in-same-project (expand-file-name subdir)))
        ))

(provide 'testkick)


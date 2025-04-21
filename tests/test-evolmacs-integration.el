;;; test-evolmacs-integration.el --- Integration tests for Evolmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Integration tests for Evolmacs.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Rather than requiring all the files directly (which would need llm.el),
;; let's define what we need for the tests and mock the rest
(require 'evolmacs-eval)

;; Mock necessary functions from evolmacs-core.el
(declare-function evolmacs-run-test "evolmacs-core")
(declare-function evolmacs-generate-and-test "evolmacs-core")

;; Mock functions from evolmacs-llm.el
(declare-function evolmacs-llm-ensure-deps "evolmacs-llm")
(declare-function evolmacs-llm-generate-package "evolmacs-llm")
(declare-function evolmacs-llm-repair-function "evolmacs-llm")

(defvar test-evolmacs-mock-function-code
  "(defun test-reverse-string (str)
     \"Reverse the characters in STR.\"
     (if (stringp str)
         (concat (reverse (string-to-list str)))
       (error \"Input must be a string\")))")

(defvar test-evolmacs-mock-test-code
  "(ert-deftest test-reverse-string-test ()
     \"Test that reverse-string works correctly.\"
     (should (equal (test-reverse-string \"hello\") \"olleh\"))
     (should (equal (test-reverse-string \"\") \"\"))
     (should-error (test-reverse-string 123)))")

;; Mock llm functions
(defun mock-evolmacs-llm-ensure-deps () nil)

(defun mock-evolmacs-llm-generate-package (spec function-name library)
  (list :function-code test-evolmacs-mock-function-code
        :test-code test-evolmacs-mock-test-code))

(defun mock-evolmacs-llm-repair-function (spec prev-code error) 
  (list :function-code test-evolmacs-mock-function-code))

;; Mock the llm module completely
(provide 'llm)

;; Install our mock functions
(fset 'evolmacs-llm-ensure-deps #'mock-evolmacs-llm-ensure-deps)
(fset 'evolmacs-llm-generate-package #'mock-evolmacs-llm-generate-package)
(fset 'evolmacs-llm-repair-function #'mock-evolmacs-llm-repair-function)

;; Now load evolmacs-core
(require 'evolmacs-core nil t)

;; If that didn't work, try to find and load it manually
(unless (featurep 'evolmacs-core)
  (let* ((this-file (or load-file-name buffer-file-name))
         (base-dir (file-name-directory (directory-file-name (file-name-directory this-file))))
         (core-file (expand-file-name "evolmacs-core.el" base-dir)))
    (when (file-exists-p core-file)
      (load core-file))))

;; Define our test
(ert-deftest test-evolmacs-full-cycle ()
  "Test the full Evolmacs cycle with a mock LLM."
  ;; Ensure our mock functions are used
  (let ((evolmacs-llm-ensure-deps #'mock-evolmacs-llm-ensure-deps)
        (evolmacs-llm-generate-package #'mock-evolmacs-llm-generate-package)
        (evolmacs-llm-repair-function #'mock-evolmacs-llm-repair-function))
    (cl-letf (((symbol-function 'evolmacs-llm-ensure-deps) #'mock-evolmacs-llm-ensure-deps)
              ((symbol-function 'evolmacs-llm-generate-package) #'mock-evolmacs-llm-generate-package)
              ((symbol-function 'evolmacs-llm-repair-function) #'mock-evolmacs-llm-repair-function))
      (let ((result (evolmacs-generate-and-test 
                    "Reverse a string"
                    "test-reverse-string"
                    "test-lib")))
        (should (plist-get result :success))
        (should (plist-get result :function-code))
        (should (plist-get result :test-code))
        (should-not (plist-get result :error))))))

(provide 'test-evolmacs-integration)
;;; test-evolmacs-integration.el ends here
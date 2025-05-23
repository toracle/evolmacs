;;; evolmacs-core.el --- Core functionality for Evolmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors
;; Keywords: lisp
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; This module provides the core functionality for Evolmacs,
;; including the test runner and evolution loop.

;;; Code:

(require 'evolmacs-eval)
(require 'evolmacs-llm)
(require 'ert)
(require 'cl-lib)

(defgroup evolmacs nil
  "Self-evolving Emacs agent."
  :group 'tools
  :prefix "evolmacs-")

(defvar evolmacs-max-repair-attempts 5
  "Maximum number of repair attempts before giving up.")

;;;###autoload
(defun evolmacs-run-test (test-code)
  "Run TEST-CODE containing an ERT test.
Returns a plist with :success, :test-name, and :error keys."
  (let ((test-name nil)
        (success nil)
        (error-msg nil))
    
    ;; First, try to extract the test name
    (condition-case nil
        (let ((form (read test-code)))
          (when (and (listp form) 
                     (eq (car form) 'ert-deftest)
                     (>= (length form) 2)
                     (symbolp (nth 1 form)))
            (setq test-name (symbol-name (nth 1 form)))))
      (error nil))
    
    ;; Evaluate the test definition
    (let ((eval-result (evolmacs-eval-safely test-code)))
      (if (not (plist-get eval-result :success))
          (setq error-msg (plist-get eval-result :error))
        
        ;; If evaluation succeeded, run the test
        (condition-case err
            (let ((ert-results (ert-run-tests-batch (regexp-quote (or test-name "")))))
              ;; Check if any test failed
              (setq success (= 0 (ert--stats-failed-unexpected ert-results)))
              (unless success
                (setq error-msg (format "Test failed: %s" (or test-name "unknown test")))))
          (error
           (setq success nil)
           (setq error-msg (format "Error running test: %S" err))))))
    
    (list :success success
          :test-name test-name
          :error error-msg)))

;;;###autoload
(defun evolmacs-generate-and-test (spec function-name library)
  "Generate, test, and potentially repair a function based on SPEC.
FUNCTION-NAME is the desired name for the function.
LIBRARY is the target library where the function should be saved.
Returns a plist with keys :success, :function-code, :test-code, and :error."
  (let ((llm-result (evolmacs-llm-generate-package spec function-name library))
        (function-code nil)
        (test-code nil)
        (success nil)
        (error-msg nil)
        (attempts 0))
    
    ;; Check for LLM errors
    (when (plist-get llm-result :error)
      (cl-return-from evolmacs-generate-and-test
        (list :success nil
              :error (plist-get llm-result :error))))
    
    (setq function-code (plist-get llm-result :function-code))
    (setq test-code (plist-get llm-result :test-code))
    
    ;; Use cl-labels to define helper functions with proper scope
    (cl-labels ((eval-and-test-function ()
                  "Evaluate function and run tests. Returns t on success."
                  (let ((eval-result (evolmacs-eval-function-safely function-code)))
                    (if (not (plist-get eval-result :success))
                        (progn
                          (setq error-msg (plist-get eval-result :error))
                          nil)
                      ;; If evaluation succeeded, run the test
                      (let ((test-result (evolmacs-run-test test-code)))
                        (if (plist-get test-result :success)
                            (progn 
                              (setq success t)
                              t)
                          (setq error-msg (plist-get test-result :error))
                          nil)))))
                
                (try-repair ()
                  "Try to repair the function and test it. Returns t on success."
                  (let ((repair-result (evolmacs-llm-repair-function spec function-code error-msg)))
                    (if (plist-get repair-result :error)
                        (progn
                          (setq error-msg (plist-get repair-result :error))
                          nil)
                      (setq function-code (plist-get repair-result :function-code))
                      (eval-and-test-function)))))
      
      ;; Initial evaluation and test
      (unless (eval-and-test-function)
        ;; If initial test failed, attempt repair
        (while (and (not success) (< attempts evolmacs-max-repair-attempts))
          (cl-incf attempts)
          (when (try-repair)
            (setq error-msg nil)))))
    
    ;; Return results
    (list :success success
          :function-code function-code
          :test-code test-code
          :error error-msg
          :attempts attempts)))

(provide 'evolmacs-core)
;;; evolmacs-core.el ends here
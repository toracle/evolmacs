;;; evolmacs-llm.el --- LLM communication layer for Evolmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors
;; Keywords: lisp
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; This module provides high-level LLM communication wrappers for Evolmacs.
;; It handles structured interaction with language models through llm.el.

;;; Code:

(require 'json)

(defgroup evolmacs-llm nil
  "LLM communication for Evolmacs."
  :group 'evolmacs
  :prefix "evolmacs-llm-")

(defvar evolmacs-llm-default-model nil
  "Default LLM model to use.
Must be a valid model identifier in llm.el.")

(defvar evolmacs-llm-prompt-templates
  '((generate-package . "Generate an Emacs Lisp function and ERT test based on this specification. Return JSON with 'function_code' and 'test_code' fields.\n\nSpecification: %s\nFunction name: %s\nLibrary: %s")
    (repair-function . "Fix this Emacs Lisp function to pass its test. Return JSON with a corrected 'function_code' field.\n\nSpecification: %s\nPrevious code: %s\nError: %s"))
  "Alist of prompt templates for different tasks.")

(defun evolmacs-llm-ensure-deps ()
  "Ensure that required dependencies are available."
  (unless (require 'llm nil t)
    (error "Evolmacs requires llm.el. Please install it first")))

;;;###autoload
(defun evolmacs-llm-generate-package (spec function-name library)
  "Generate an Emacs Lisp function and test based on SPEC.
FUNCTION-NAME is the desired name for the function.
LIBRARY is the target library where the function should be saved.
Returns a plist with :function-code and :test-code if successful."
  (evolmacs-llm-ensure-deps)
  
  (let* ((task-template (alist-get 'generate-package evolmacs-llm-prompt-templates))
         (prompt (format task-template spec function-name library))
         (response-json nil)
         (response-data nil))
    
    ;; Here we'd use llm.el to get the response
    ;; As a placeholder, we're just setting up the structure
    (if (fboundp 'llm-completion-sync)
        (setq response-json (llm-completion-sync prompt evolmacs-llm-default-model))
      (error "llm.el is not properly loaded"))
    
    (condition-case err
        (progn
          (setq response-data (json-parse-string response-json))
          (list :function-code (gethash "function_code" response-data)
                :test-code (gethash "test_code" response-data)))
      (error
       (list :error (format "Failed to parse LLM response: %S" err))))))

;;;###autoload
(defun evolmacs-llm-repair-function (spec previous-code error)
  "Repair a function based on test failure.
SPEC is the function specification.
PREVIOUS-CODE is the code that needs fixing.
ERROR is the error message from the test.
Returns a plist with :function-code if successful."
  (evolmacs-llm-ensure-deps)
  
  (let* ((task-template (alist-get 'repair-function evolmacs-llm-prompt-templates))
         (prompt (format task-template spec previous-code error))
         (response-json nil)
         (response-data nil))
    
    ;; Here we'd use llm.el to get the response
    (if (fboundp 'llm-completion-sync)
        (setq response-json (llm-completion-sync prompt evolmacs-llm-default-model))
      (error "llm.el is not properly loaded"))
    
    (condition-case err
        (progn
          (setq response-data (json-parse-string response-json))
          (list :function-code (gethash "function_code" response-data)))
      (error
       (list :error (format "Failed to parse LLM response: %S" err))))))

(provide 'evolmacs-llm)
;;; evolmacs-llm.el ends here
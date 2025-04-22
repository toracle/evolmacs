;;; test-helper.el --- Test helper for Evolmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Evolmacs Contributors

;; Author: Evolmacs Contributors

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:

;; Helper functions for running tests.

;;; Code:

;; Only load undercover when running tests, not during development
(message "UNDERCOVER_PROFILE environment variable: %s" (getenv "UNDERCOVER_PROFILE"))
(message "Load path: %S" load-path)

(defvar undercover-loaded nil
  "Flag to track if undercover was loaded.")

(when (getenv "UNDERCOVER_PROFILE")
  (message "Loading undercover...")
  (condition-case err
      (progn
        (require 'undercover)
        (message "Undercover loaded successfully")
        (setq undercover-loaded t)
        (undercover "evolmacs*.el" 
                   (:exclude "*-test.el" "*-tests.el")
                   (:report-file "coverage-final.json")
                   (:report-format 'json)
                   (:send-report nil))
        (message "Undercover setup complete"))
    (error (message "Error loading undercover: %S" err))))

;; Add a hook to save coverage data after tests are run
(defun test-helper-save-coverage-report ()
  "Save coverage report at the end of test run."
  (when undercover-loaded
    (message "Saving coverage report...")
    (undercover-report-coverage)))

(add-hook 'kill-emacs-hook #'test-helper-save-coverage-report)

(provide 'test-helper)
;;; test-helper.el ends here
;;; run-coverage.el --- Run tests with coverage reporting -*- lexical-binding: t; -*-

;; First load undercover with debugging
(message "Loading undercover for coverage reporting...")
(require 'undercover)
(message "Undercover loaded successfully")

;; Force coverage to be enabled
(setq undercover-force-coverage t)

;; Setup undercover with explicit path for report file
(message "Setting up undercover...")
(let ((report-file (expand-file-name "coverage-final.json")))
  (message "Coverage report will be saved to: %s" report-file)
  (undercover "evolmacs.el" "evolmacs-core.el" "evolmacs-eval.el" "evolmacs-llm.el"
             (:report-file report-file)
             (:report-format 'simplecov)
             (:send-report nil)))
(message "Undercover setup complete")

;; Load all the source files explicitly to ensure they're instrumented
(message "Loading source files for instrumentation...")
(require 'evolmacs-eval)
(require 'evolmacs-llm)
(require 'evolmacs-core)
(require 'evolmacs)
(message "Source files loaded")

;; Load test files
(message "Loading test files...")
(load (expand-file-name "tests/evolmacs-tests.el"))
(load (expand-file-name "tests/test-evolmacs-integration.el"))
(message "Test files loaded")

;; Run the tests
(message "Running tests...")
(ert-run-tests-batch)
(message "Tests completed")

;; Save coverage report using the proper API
(message "Saving coverage report...")
(undercover-safe-report)
(message "Coverage report should be saved now")
(kill-emacs 0)
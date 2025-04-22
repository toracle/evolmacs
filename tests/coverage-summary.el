;;; coverage-summary.el --- Display a summary of the coverage report -*- lexical-binding: t; -*-

(require 'json)

(defun coverage-summary-format-file (file-path stats)
  "Format coverage statistics for FILE-PATH with STATS."
  (let* ((short-path (file-name-nondirectory file-path))
         (covered (plist-get stats :covered))
         (total (plist-get stats :total))
         (percent (if (> total 0)
                      (round (* 100 (/ (float covered) total)))
                    0)))
    (format "%-25s %3d/%3d lines (%3d%%)" 
            short-path covered total percent)))

(defun coverage-summary-calculate-stats (coverage-data)
  "Calculate coverage statistics from COVERAGE-DATA."
  (let ((file-stats nil)
        (total-covered 0)
        (total-lines 0))
    (maphash
     (lambda (file-path line-coverage)
       (let ((covered 0)
             (total 0))
         (cl-loop for count across line-coverage
                  when (not (null count))
                  do (progn
                       (cl-incf total)
                       (when (and (numberp count) (> count 0))
                         (cl-incf covered))))
         (setq total-covered (+ total-covered covered))
         (setq total-lines (+ total-lines total))
         (push (list file-path 
                     :covered covered 
                     :total total
                     :percent (if (> total 0)
                                  (round (* 100 (/ (float covered) total)))
                                0))
               file-stats)))
     coverage-data)
    (list :files (nreverse file-stats)
          :total-covered total-covered
          :total-lines total-lines
          :total-percent (if (> total-lines 0)
                            (round (* 100 (/ (float total-covered) total-lines)))
                          0))))

(defun coverage-summary ()
  "Display a summary of the coverage report."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'vector)
         (json-key-type 'string)
         (coverage-file (expand-file-name "coverage-final.json"))
         (coverage-data nil)
         (stats nil))
    
    (if (not (file-exists-p coverage-file))
        (message "No coverage file found at %s" coverage-file)
      ;; Load coverage data
      (with-temp-buffer
        (insert-file-contents coverage-file)
        (setq coverage-data (json-read))
        (let ((undercover-data (gethash "undercover.el" coverage-data))
              (coverage-map (make-hash-table :test 'equal)))
          
          (when undercover-data
            (setq coverage-map (gethash "coverage" undercover-data))
            (setq stats (coverage-summary-calculate-stats coverage-map))
            
            ;; Print summary
            (message "\n=== Evolmacs Coverage Summary ===\n")
            
            ;; Print file details
            (message "Files:")
            (dolist (file-stats (plist-get stats :files))
              (message "  %s" (coverage-summary-format-file 
                               (car file-stats) 
                               (cdr file-stats))))
            
            ;; Print total
            (message "\nTotal coverage: %d/%d lines (%.0f%%)\n"
                     (plist-get stats :total-covered)
                     (plist-get stats :total-lines)
                     (plist-get stats :total-percent))))))))

;; Run the summary
(coverage-summary)
;;; euporie-qrsh-integration-tests.el --- Comprehensive QRSH Integration Test Suite -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; COMPREHENSIVE QRSH INTEGRATION TEST SUITE
;; ========================================
;;
;; This test suite provides deterministic, file-based testing for the euporie-Emacs
;; integration with QRSH remote execution. All tests produce verifiable logs and
;; evidence for debugging purposes.
;;
;; ARCHITECTURE:
;;   - Universal test framework supporting all kernels (SAS, Python, R, Stata)
;;   - File-based logging with timestamps and structured output
;;   - Screenshot capture for visual verification
;;   - Independent, idempotent test functions with proper cleanup
;;   - Comprehensive coverage of QRSH integration scenarios
;;
;; CORE TEST CATEGORIES:
;;   1. QRSH Detection Tests - Path format parsing and routing
;;   2. org-babel Integration Tests - :dir parameter extraction and processing
;;   3. Session Management Tests - Buffer creation, process lifecycle, cleanup
;;   4. End-to-End Workflow Tests - Complete C-RET execution cycles
;;   5. Cross-Language Tests - Kernel compatibility and consistency
;;   6. Visual Verification Tests - Screenshot capture and buffer content analysis
;;
;; USAGE:
;;   M-x euporie-qrsh-run-all-tests           ; Run complete test suite
;;   M-x euporie-qrsh-test-path-detection     ; Test QRSH path parsing only
;;   M-x euporie-qrsh-test-session-management ; Test session lifecycle only
;;   M-x euporie-qrsh-test-end-to-end-workflow ; Test complete workflows only
;;
;; TEST EVIDENCE:
;;   - Log file: ~/euporie-qrsh-tests.log (structured debugging output)
;;   - Screenshots: ~/euporie-test-*.png (visual verification)
;;   - Buffer dumps: Logged for analysis when tests fail

;;; Code:

(require 'cl-lib)  ; For cl-count
(require 'org)
(require 'ob)

;; Load euporie modules with error handling - defer until runtime
(defvar euporie-modules-loaded nil
  "Whether euporie modules have been successfully loaded.")

(defun ensure-euporie-modules-loaded ()
  "Ensure euporie modules are loaded, with error handling."
  (unless euporie-modules-loaded
    (condition-case err
        (progn
          (require 'euporie-termint)
          (load (expand-file-name "tramp-qrsh.el" (or (bound-and-true-p doom-user-dir) "~/.doom.d/")) t)
          (setq euporie-modules-loaded t))
      (error 
       (message "Warning: Could not load euporie modules: %s" err)
       (message "Some tests may not work without euporie integration")))))

;;; Configuration

(defvar euporie-qrsh-test-log-file (expand-file-name "euporie-qrsh-tests.log" "~/")
  "Main log file for QRSH integration tests.")

(defvar euporie-qrsh-test-timeout 30
  "Default timeout in seconds for test operations.")

(defvar euporie-qrsh-test-screenshot-dir (expand-file-name "~/")
  "Directory for test screenshots.")

(defvar euporie-qrsh-test-qrsh-paths 
  '("/sshx:wrds|qrsh::/home/nyu/user/project/"  ; Standard format
    "/sshx:wrds-cloud:/home/nyu/user/project/|qrsh::/home/nyu/user/project/"  ; With initial path
    "/sshx:server|qrsh:/path/to/project"  ; Backward compatibility
    "/sshx:host:/remote/path|qrsh::/compute/path"  ; Complex path
    "/sshx:wrds:|qrsh::/home/path"  ; Minimal format
    "/sshx:test-host|qrsh::/test/path/with/spaces"  ; Path with spaces potential
    )
  "Test QRSH paths for path detection testing.")

(defvar euporie-qrsh-test-tramp-paths
  '("/sshx:wrds:/home/nyu/user/project/"
    "/sshx:server:/remote/path"
    "/ssh:host:/path")
  "Test standard TRAMP paths for comparison testing.")

(defvar euporie-qrsh-test-local-paths
  '("/Users/vwh7mb/projects/test"
    "/tmp/test-project"
    "~/Documents/test")
  "Test local paths for path detection testing.")

;;; Logging System

(defun euporie-qrsh-log (test-name phase level message &optional data)
  "Log TEST-NAME PHASE with LEVEL MESSAGE and optional DATA to test log file."
  (let ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S EST"))
        (data-str (if data (format " | DATA: %S" data) ""))
        (entry (format "[%s] [%s:%s] [%s] %s%s\n" 
                      timestamp test-name phase (upcase (symbol-name level)) message data-str)))
    (with-temp-buffer
      (insert entry)
      (append-to-file (point-min) (point-max) euporie-qrsh-test-log-file))))

(defun euporie-qrsh-clear-log ()
  "Clear the test log file and write header."
  (with-temp-buffer
    (insert (format "=== EUPORIE QRSH INTEGRATION TEST LOG ===\n"))
    (insert (format "Test Session: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S EST")))
    (insert (format "Emacs Version: %s\n" emacs-version))
    (insert (format "Test Suite Version: 1.0\n\n"))
    (write-region (point-min) (point-max) euporie-qrsh-test-log-file nil 'silent)))

;;; Helper Functions

(defun euporie-qrsh-kill-test-buffers ()
  "Kill all euporie-related test buffers to ensure clean state."
  (euporie-qrsh-log "cleanup" "buffers" 'info "Killing all euporie test buffers")
  (let ((buffers-killed 0))
    (dolist (buf-pattern '("*euporie-*" "*qrsh-session*" "*tramp-qrsh*"))
      (dolist (buffer (buffer-list))
        (when (string-match-p (replace-regexp-in-string "\\*" ".*" buf-pattern) 
                             (buffer-name buffer))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)
            (setq buffers-killed (1+ buffers-killed))
            (euporie-qrsh-log "cleanup" "buffer" 'debug 
                             (format "Killed buffer: %s" (buffer-name buffer)))))))
    (euporie-qrsh-log "cleanup" "buffers" 'info 
                     (format "Cleanup complete. Killed %d buffers" buffers-killed))))

(defun euporie-qrsh-wait-for-buffer (buffer-name timeout)
  "Wait for BUFFER-NAME to exist within TIMEOUT seconds.
Returns buffer object if found, nil if timeout."
  (euporie-qrsh-log "wait" "buffer" 'debug 
                   (format "Waiting for buffer %s (timeout: %ds)" buffer-name timeout))
  (let ((start-time (current-time))
        (buffer nil))
    (while (and (not buffer) 
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (setq buffer (get-buffer buffer-name))
      (unless buffer (sleep-for 0.5)))
    (if buffer
        (euporie-qrsh-log "wait" "buffer" 'info 
                         (format "Buffer %s found after %.1fs" 
                                buffer-name 
                                (float-time (time-subtract (current-time) start-time))))
      (euporie-qrsh-log "wait" "buffer" 'error 
                       (format "Buffer %s NOT FOUND after %ds timeout" buffer-name timeout)))
    buffer))

(defun euporie-qrsh-wait-for-process (buffer timeout)
  "Wait for live process in BUFFER within TIMEOUT seconds.
Returns process if found and alive, nil otherwise."
  (euporie-qrsh-log "wait" "process" 'debug 
                   (format "Waiting for live process in %s (timeout: %ds)" 
                          (buffer-name buffer) timeout))
  (let ((start-time (current-time))
        (process nil))
    (while (and (not process) 
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (let ((proc (get-buffer-process buffer)))
        (when (and proc (process-live-p proc))
          (setq process proc)))
      (unless process (sleep-for 1)))
    (if process
        (euporie-qrsh-log "wait" "process" 'info 
                         (format "Live process found in %s after %.1fs" 
                                (buffer-name buffer)
                                (float-time (time-subtract (current-time) start-time))))
      (euporie-qrsh-log "wait" "process" 'error 
                       (format "No live process in %s after %ds timeout" 
                              (buffer-name buffer) timeout)))
    process))

(defun euporie-qrsh-take-screenshot (filename)
  "Capture Emacs window screenshot and save as FILENAME.
Automatically resizes to <2000px for Claude API compatibility."
  (let ((full-path (expand-file-name filename euporie-qrsh-test-screenshot-dir)))
    (euporie-qrsh-log "screenshot" "capture" 'info (format "Taking screenshot: %s" filename))
    (condition-case err
        (progn
          ;; Focus Emacs and capture with delay
          (call-process "osascript" nil nil nil
                       "-e" "tell application \"Emacs\" to activate")
          (sleep-for 0.5)
          (call-process "screencapture" nil nil nil "-x" full-path)
          ;; Resize for Claude API compatibility
          (call-process "sips" nil nil nil "-Z" "1900" full-path)
          (euporie-qrsh-log "screenshot" "capture" 'info 
                           (format "Screenshot saved and resized: %s" full-path))
          full-path)
      (error
       (euporie-qrsh-log "screenshot" "capture" 'error 
                        (format "Screenshot failed: %s" err))
       nil))))

(defun euporie-qrsh-log-buffer-contents (buffer-name &optional max-chars)
  "Log contents of BUFFER-NAME to test log, limited to MAX-CHARS (default 1000)."
  (let ((buffer (get-buffer buffer-name))
        (limit (or max-chars 1000)))
    (if buffer
        (with-current-buffer buffer
          (let ((content (buffer-string)))
            (when (> (length content) limit)
              (setq content (concat (substring content 0 (- limit 50)) 
                                  "\n... [TRUNCATED] ...")))
            (euporie-qrsh-log "buffer" "contents" 'debug 
                             (format "Buffer %s contents:\n%s" buffer-name content))))
      (euporie-qrsh-log "buffer" "contents" 'error 
                       (format "Buffer %s does not exist" buffer-name)))))

;;; Core Test Functions

;;; 1. QRSH Path Detection Tests

(defun euporie-qrsh-test-path-detection ()
  "Test QRSH path format detection and parsing.
Returns: 'PASS if all path detection works correctly, 'FAIL otherwise."
  (interactive)
  (euporie-qrsh-log "path-detection" "start" 'info "Starting QRSH path detection tests")
  
  (let ((test-results '())
        (all-passed t))
    
    ;; Test QRSH path detection using actual euporie-termint function
    (dolist (path euporie-qrsh-test-qrsh-paths)
      (euporie-qrsh-log "path-detection" "qrsh-test" 'debug (format "Testing QRSH path: %s" path))
      (condition-case err
          (let ((actual-mode (progn 
                                (ensure-euporie-modules-loaded)
                                (if euporie-modules-loaded
                                    (euporie-termint--detect-remote-mode path)
                                  'test-skipped))))
                (expected-mode 'qrsh-tramp)
                (is-qrsh (eq actual-mode expected-mode)))
            (if is-qrsh
                (progn
                  (push (cons path 'QRSH-DETECTED) test-results)
                  (euporie-qrsh-log "path-detection" "qrsh-test" 'info 
                                   (format "✓ Correctly detected QRSH path: %s (mode: %s)" path actual-mode)))
              (progn
                (push (cons path 'QRSH-NOT-DETECTED) test-results)
                (setq all-passed nil)
                (euporie-qrsh-log "path-detection" "qrsh-test" 'error 
                                 (format "✗ Failed to detect QRSH path: %s (expected: %s, got: %s)" path expected-mode actual-mode)))))
        (error
         (push (cons path 'ERROR) test-results)
         (setq all-passed nil)
         (euporie-qrsh-log "path-detection" "qrsh-test" 'error 
                          (format "✗ Error testing QRSH path %s: %s" path err)))))
    
    ;; Test TRAMP path detection (should be detected as standard-tramp, NOT qrsh-tramp)
    (dolist (path euporie-qrsh-test-tramp-paths)
      (euporie-qrsh-log "path-detection" "tramp-test" 'debug (format "Testing TRAMP path: %s" path))
      (condition-case err
          (let ((actual-mode (progn 
                                (ensure-euporie-modules-loaded)
                                (if euporie-modules-loaded
                                    (euporie-termint--detect-remote-mode path)
                                  'test-skipped))))
                (expected-mode 'standard-tramp)
                (is-correct (eq actual-mode expected-mode)))
            (if is-correct
                (progn
                  (push (cons path 'TRAMP-CORRECT) test-results)
                  (euporie-qrsh-log "path-detection" "tramp-test" 'info 
                                   (format "✓ Correctly identified TRAMP path: %s (mode: %s)" path actual-mode)))
              (progn
                (push (cons path 'TRAMP-FALSE-POSITIVE) test-results)
                (setq all-passed nil)
                (euporie-qrsh-log "path-detection" "tramp-test" 'error 
                                 (format "✗ Incorrect TRAMP detection: %s (expected: %s, got: %s)" path expected-mode actual-mode)))))
        (error
         (push (cons path 'ERROR) test-results)
         (setq all-passed nil)
         (euporie-qrsh-log "path-detection" "tramp-test" 'error 
                          (format "✗ Error testing TRAMP path %s: %s" path err)))))
    
    ;; Test local path detection (should return nil, not be remote)
    (dolist (path euporie-qrsh-test-local-paths)
      (euporie-qrsh-log "path-detection" "local-test" 'debug (format "Testing local path: %s" path))
      (condition-case err
          (let ((actual-mode (progn 
                                (ensure-euporie-modules-loaded)
                                (if euporie-modules-loaded
                                    (euporie-termint--detect-remote-mode path)
                                  'test-skipped))))
                (is-remote (file-remote-p path))
                (is-correct (and (null actual-mode) (null is-remote))))
            (if is-correct
                (progn
                  (push (cons path 'LOCAL-CORRECT) test-results)
                  (euporie-qrsh-log "path-detection" "local-test" 'info 
                                   (format "✓ Correctly identified local path: %s (mode: %s, remote: %s)" path actual-mode is-remote)))
              (progn
                (push (cons path 'LOCAL-FALSE-POSITIVE) test-results)
                (setq all-passed nil)
                (euporie-qrsh-log "path-detection" "local-test" 'error 
                                 (format "✗ Local path incorrectly detected: %s (mode: %s, remote: %s)" path actual-mode is-remote)))))
        (error
         (push (cons path 'ERROR) test-results)
         (setq all-passed nil)
         (euporie-qrsh-log "path-detection" "local-test" 'error 
                          (format "✗ Error testing local path %s: %s" path err)))))
    
    ;; Log results summary
    (euporie-qrsh-log "path-detection" "summary" 'info 
                     (format "Path detection test results: %s" test-results))
    
    (if all-passed
        (progn
          (euporie-qrsh-log "path-detection" "complete" 'info "✓ All path detection tests PASSED")
          (message "✓ QRSH path detection tests: PASSED")
          'PASS)
      (progn
        (euporie-qrsh-log "path-detection" "complete" 'error "✗ Some path detection tests FAILED")
        (message "✗ QRSH path detection tests: FAILED - Check log for details")
        'FAIL))))

;;; 2. org-babel :dir Parameter Extraction Tests

(defun euporie-qrsh-test-dir-extraction ()
  "Test org-babel :dir parameter extraction from source blocks.
Returns: 'PASS if extraction works correctly, 'FAIL otherwise."
  (interactive)
  (euporie-qrsh-log "dir-extraction" "start" 'info "Starting org-babel :dir extraction tests")
  
  ;; Test manual org-babel :dir extraction with various formats
  (let ((test-cases '(
                     ;; Format: (org-content . expected-dir)
                     ("#+begin_src sas :dir /sshx:wrds|qrsh::/home/path\ndata test; run;\n#+end_src" . "/sshx:wrds|qrsh::/home/path")
                     ("#+begin_src python :dir /sshx:server:/remote/path\nprint('test')\n#+end_src" . "/sshx:server:/remote/path")
                     ("#+begin_src sas\ndata local; run;\n#+end_src" . nil)  ; No :dir should return nil
                     ("#+begin_src r :dir ~/local/project\nprint('local')\n#+end_src" . "~/local/project")
                     ))
        (all-passed t))
    
    (dolist (test-case test-cases)
      (let* ((org-content (car test-case))
             (expected-dir (cdr test-case)))
        
        (euporie-qrsh-log "dir-extraction" "test-case" 'debug 
                         (format "Testing org content: %s" (substring org-content 0 (min 50 (length org-content)))))
        
        (with-temp-buffer
          (org-mode)
          (insert org-content)
          (goto-char (point-min))
          (search-forward "data\\|print" nil t)  ; Position cursor in code block
          
          (let* ((element (org-element-at-point))
                 (actual-dir (when (eq (org-element-type element) 'src-block)
                              (org-element-property :dir element)))
                 (test-passed (equal actual-dir expected-dir)))
            
            (if test-passed
                (euporie-qrsh-log "dir-extraction" "test-case" 'info 
                                 (format "✓ :dir extraction correct: expected %s, got %s" expected-dir actual-dir))
              (progn
                (setq all-passed nil)
                (euporie-qrsh-log "dir-extraction" "test-case" 'error 
                                 (format "✗ :dir extraction failed: expected %s, got %s" expected-dir actual-dir))))))))
    
    ;; Test the built-in function from euporie-termint.el as well
    (condition-case err
        (let ((builtin-result (euporie-termint-test-dir-extraction)))
          (if (and all-passed builtin-result)
              (progn
                (euporie-qrsh-log "dir-extraction" "complete" 'info "✓ All org-babel :dir extraction tests PASSED")
                (message "✓ org-babel :dir extraction: PASSED")
                'PASS)
            (progn
              (euporie-qrsh-log "dir-extraction" "complete" 'error "✗ org-babel :dir extraction tests FAILED")
              (message "✗ org-babel :dir extraction: FAILED")
              'FAIL)))
      (error
       (euporie-qrsh-log "dir-extraction" "complete" 'error 
                        (format "✗ org-babel :dir extraction test ERROR: %s" err))
       (message "✗ org-babel :dir extraction: ERROR - %s" err)
       'ERROR)))

;;; 3. QRSH Session Management Tests

(defun euporie-qrsh-test-session-management (&optional kernel)
  "Test QRSH session creation, process lifecycle, and cleanup.
KERNEL defaults to 'sas'. Returns: 'PASS if all session tests pass, 'FAIL otherwise."
  (interactive)
  (let ((test-kernel (or kernel "sas"))
        (test-dir (car euporie-qrsh-test-qrsh-paths)))
    
    (euporie-qrsh-log "session-mgmt" "start" 'info 
                     (format "Starting session management tests for %s" test-kernel))
    
    ;; Cleanup first
    (euporie-qrsh-kill-test-buffers)
    
    (condition-case err
        (progn
          ;; Test 1: Session creation
          (euporie-qrsh-log "session-mgmt" "create" 'info "Creating QRSH session")
          (euporie-termint-start-remote-universal test-kernel test-dir)
          
          ;; Wait for buffer creation
          (let ((buffer (euporie-qrsh-wait-for-buffer (format "*euporie-%s*" test-kernel) 
                                                     euporie-qrsh-test-timeout)))
            (if buffer
                (progn
                  (euporie-qrsh-log "session-mgmt" "create" 'info "✓ Session buffer created")
                  
                  ;; Test 2: Process creation
                  (let ((process (euporie-qrsh-wait-for-process buffer 10)))
                    (if process
                        (progn
                          (euporie-qrsh-log "session-mgmt" "process" 'info "✓ Live process detected")
                          
                          ;; Test 3: Process status verification
                          (let ((proc-status (process-status process))
                                (proc-name (process-name process)))
                            (euporie-qrsh-log "session-mgmt" "status" 'info 
                                             (format "Process status: %s, name: %s" proc-status proc-name))
                            
                            ;; Take screenshot for evidence
                            (euporie-qrsh-take-screenshot (format "euporie-session-%s-test.png" test-kernel))
                            
                            ;; Test 4: Cleanup
                            (euporie-qrsh-kill-test-buffers)
                            
                            (euporie-qrsh-log "session-mgmt" "complete" 'info "✓ Session management tests PASSED")
                            (message "✓ QRSH session management tests: PASSED")
                            'PASS))
                      (progn
                        (euporie-qrsh-log "session-mgmt" "process" 'error "✗ No live process found")
                        (euporie-qrsh-log-buffer-contents (format "*euporie-%s*" test-kernel))
                        'FAIL)))
              (progn
                (euporie-qrsh-log "session-mgmt" "create" 'error "✗ Session buffer not created")
                'FAIL))))
      (error
       (euporie-qrsh-log "session-mgmt" "complete" 'error 
                        (format "✗ Session management test ERROR: %s" err))
       (message "✗ QRSH session management: ERROR - %s" err)
       'ERROR))))

;;; 4. End-to-End Workflow Tests

(defun euporie-qrsh-test-end-to-end-workflow (&optional kernel)
  "Test complete C-RET workflow with QRSH execution.
KERNEL defaults to 'sas'. Returns: 'PASS if workflow completes, 'FAIL otherwise."
  (interactive)
  (let ((test-kernel (or kernel "sas"))
        (test-dir (car euporie-qrsh-test-qrsh-paths))
        (test-code (cond
                   ((string= test-kernel "sas") "data _null_; put 'QRSH End-to-End Test Successful'; run;")
                   ((string= test-kernel "python") "print('QRSH End-to-End Test Successful')")
                   ((string= test-kernel "r") "cat('QRSH End-to-End Test Successful\\n')")
                   ((string= test-kernel "stata") "display \"QRSH End-to-End Test Successful\"")
                   (t "# Unknown kernel test"))))
    
    (euporie-qrsh-log "end-to-end" "start" 'info 
                     (format "Starting end-to-end workflow test for %s" test-kernel))
    
    ;; Cleanup first
    (euporie-qrsh-kill-test-buffers)
    
    (condition-case err
        (progn
          ;; Step 1: Send code (should create session + execute)
          (euporie-qrsh-log "end-to-end" "send-code" 'info "Sending test code via QRSH")
          (euporie-termint-send-code test-kernel test-code test-dir)
          
          ;; Step 2: Wait for buffer and verify execution
          (let ((buffer (euporie-qrsh-wait-for-buffer (format "*euporie-%s*" test-kernel) 
                                                     euporie-qrsh-test-timeout)))
            (if buffer
                (progn
                  (euporie-qrsh-log "end-to-end" "buffer" 'info "✓ Execution buffer created")
                  
                  ;; Step 3: Verify process is running
                  (let ((process (get-buffer-process buffer)))
                    (if (and process (process-live-p process))
                        (progn
                          (euporie-qrsh-log "end-to-end" "process" 'info "✓ Process is alive after code execution")
                          
                          ;; Step 4: Take screenshot and log buffer contents for verification
                          (sleep-for 2) ; Allow output to appear
                          (euporie-qrsh-take-screenshot (format "euporie-end-to-end-%s-test.png" test-kernel))
                          (euporie-qrsh-log-buffer-contents (buffer-name buffer))
                          
                          (euporie-qrsh-log "end-to-end" "complete" 'info "✓ End-to-end workflow test PASSED")
                          (message "✓ QRSH end-to-end workflow test: PASSED")
                          'PASS)
                      (progn
                        (euporie-qrsh-log "end-to-end" "process" 'error "✗ Process not alive after execution")
                        'FAIL)))
              (progn
                (euporie-qrsh-log "end-to-end" "buffer" 'error "✗ Execution buffer not created")
                'FAIL))))
      (error
       (euporie-qrsh-log "end-to-end" "complete" 'error 
                        (format "✗ End-to-end workflow ERROR: %s" err))
       (message "✗ QRSH end-to-end workflow: ERROR - %s" err)
       'ERROR))))

;;; 5. Cross-Language Compatibility Tests

(defun euporie-qrsh-test-cross-language-compatibility ()
  "Test QRSH integration works consistently across all supported kernels.
Returns: list of (kernel . result) pairs."
  (interactive)
  (euporie-qrsh-log "cross-lang" "start" 'info "Starting cross-language compatibility tests")
  
  (let ((kernels '("sas" "python" "r" "stata"))
        (results '())
        (test-dir (car euporie-qrsh-test-qrsh-paths)))
    
    (dolist (kernel kernels)
      (euporie-qrsh-log "cross-lang" "kernel" 'info (format "Testing kernel: %s" kernel))
      
      ;; Clean state for each kernel
      (euporie-qrsh-kill-test-buffers)
      (sleep-for 1)
      
      ;; Quick session test for each kernel
      (let ((result (condition-case err
                        (progn
                          (euporie-termint-start-remote-universal kernel test-dir)
                          (let ((buffer (euporie-qrsh-wait-for-buffer (format "*euporie-%s*" kernel) 15)))
                            (if buffer 'PASS 'FAIL)))
                      (error 'ERROR))))
        
        (push (cons kernel result) results)
        (euporie-qrsh-log "cross-lang" "kernel" 'info 
                         (format "Kernel %s test result: %s" kernel result))))
    
    ;; Summary
    (let ((passed (cl-count 'PASS results :key #'cdr))
          (total (length results)))
      (if (= passed total)
          (progn
            (euporie-qrsh-log "cross-lang" "complete" 'info 
                             (format "✓ Cross-language tests: %d/%d PASSED" passed total))
            (message "✓ Cross-language compatibility: ALL PASSED (%d/%d)" passed total))
        (progn
          (euporie-qrsh-log "cross-lang" "complete" 'error 
                           (format "✗ Cross-language tests: %d/%d PASSED" passed total))
          (message "✗ Cross-language compatibility: %d/%d PASSED" passed total))))
    
    results))

;;; Master Test Runner

(defun euporie-qrsh-run-all-tests ()
  "Run comprehensive QRSH integration test suite.
Returns: (total-tests passed-tests results-list)"
  (interactive)
  
  ;; Initialize
  (euporie-qrsh-clear-log)
  (euporie-qrsh-log "master" "start" 'info "=== STARTING COMPREHENSIVE QRSH INTEGRATION TEST SUITE ===")
  
  (let ((test-functions '(("Path Detection" euporie-qrsh-test-path-detection)
                         ("Config Extraction" euporie-qrsh-test-remote-config-extraction)
                         ("Dir Extraction" euporie-qrsh-test-dir-extraction)
                         ("Session Management" euporie-qrsh-test-session-management)
                         ("End-to-End Workflow" euporie-qrsh-test-end-to-end-workflow)
                         ("SAS Output Verification" euporie-qrsh-test-sas-output-verification)))
        (results '())
        (passed-count 0))
    
    ;; Run individual tests
    (dolist (test-spec test-functions)
      (let* ((test-name (car test-spec))
             (test-function (cadr test-spec))
             (start-time (current-time)))
        
        (euporie-qrsh-log "master" "test" 'info (format "Running test: %s" test-name))
        (message "Running %s test..." test-name)
        
        (let ((result (condition-case err
                          (funcall test-function)
                        (error 'ERROR))))
          
          (push (list test-name result (float-time (time-subtract (current-time) start-time))) results)
          
          (when (eq result 'PASS)
            (setq passed-count (1+ passed-count)))
          
          (euporie-qrsh-log "master" "test" 'info 
                           (format "Test '%s' completed: %s (%.1fs)" 
                                  test-name result 
                                  (float-time (time-subtract (current-time) start-time))))
          
          ;; Brief pause between tests
          (sleep-for 1))))
    
    ;; Run cross-language compatibility test
    (euporie-qrsh-log "master" "test" 'info "Running cross-language compatibility test")
    (message "Running cross-language compatibility test...")
    (let* ((start-time (current-time))
           (cross-lang-results (euporie-qrsh-test-cross-language-compatibility))
           (cross-lang-passed (cl-count 'PASS cross-lang-results :key #'cdr))
           (cross-lang-total (length cross-lang-results))
           (cross-lang-result (if (= cross-lang-passed cross-lang-total) 'PASS 'FAIL)))
      
      (push (list "Cross-Language Compatibility" cross-lang-result 
                 (float-time (time-subtract (current-time) start-time))) results)
      
      (when (eq cross-lang-result 'PASS)
        (setq passed-count (1+ passed-count))))
    
    ;; Final cleanup
    (euporie-qrsh-kill-test-buffers)
    
    ;; Calculate totals and generate summary
    (let ((total-tests (length results)))
      
      (euporie-qrsh-log "master" "summary" 'info "=== TEST SUITE RESULTS SUMMARY ===")
      (euporie-qrsh-log "master" "summary" 'info (format "PASSED: %d/%d tests" passed-count total-tests))
      (euporie-qrsh-log "master" "summary" 'info (format "FAILED: %d/%d tests" (- total-tests passed-count) total-tests))
      
      ;; Log individual results
      (dolist (result (reverse results))
        (let ((name (car result))
              (status (cadr result))
              (duration (caddr result)))
          (euporie-qrsh-log "master" "result" 'info 
                           (format "%-30s: %s (%.1fs)" name status duration))))
      
      ;; Final message
      (if (= passed-count total-tests)
          (progn
            (euporie-qrsh-log "master" "complete" 'info "🎉 ALL QRSH INTEGRATION TESTS PASSED!")
            (message "🎉 QRSH Integration Test Suite: ALL TESTS PASSED! (%d/%d) - Check %s for details" 
                     passed-count total-tests euporie-qrsh-test-log-file))
        (progn
          (euporie-qrsh-log "master" "complete" 'error "⚠️  Some QRSH integration tests failed")
          (message "⚠️  QRSH Integration Test Suite: %d/%d PASSED - Check %s for details" 
                   passed-count total-tests euporie-qrsh-test-log-file)))
      
      ;; Return results for programmatic use
      (list total-tests passed-count (reverse results)))))

;;; Individual Test Runners (for targeted testing)

(defun euporie-qrsh-test-sas-only ()
  "Run QRSH tests specifically for SAS kernel."
  (interactive)
  (euporie-qrsh-clear-log)
  (euporie-qrsh-log "sas-only" "start" 'info "Running SAS-specific QRSH tests")
  
  (let ((results '()))
    (push (cons "SAS Session Management" (euporie-qrsh-test-session-management "sas")) results)
    (push (cons "SAS End-to-End" (euporie-qrsh-test-end-to-end-workflow "sas")) results)
    
    (message "SAS-specific tests completed: %s" results)
    results))

(defun euporie-qrsh-test-quick-smoke ()
  "Run quick smoke tests for basic QRSH functionality."
  (interactive)
  (euporie-qrsh-log "smoke" "start" 'info "Running quick smoke tests")
  
  (let ((path-result (euporie-qrsh-test-path-detection))
        (dir-result (euporie-qrsh-test-dir-extraction)))
    
    (if (and (eq path-result 'PASS) (eq dir-result 'PASS))
        (progn
          (message "✓ Quick smoke tests: PASSED")
          'PASS)
      (progn
        (message "✗ Quick smoke tests: FAILED")
        'FAIL))))

;;; Debug and Utility Functions

(defun euporie-qrsh-show-test-log (&optional lines)
  "Show tail of test log file. LINES defaults to 50."
  (interactive "P")
  (let ((line-count (or lines 50)))
    (if (file-exists-p euporie-qrsh-test-log-file)
        (with-temp-buffer
          (insert-file-contents euporie-qrsh-test-log-file)
          (goto-char (max (point-min) (- (point-max) (* line-count 80))))
          (message "QRSH Test Log (last ~%d lines):\n%s" line-count (buffer-substring (point) (point-max))))
      (message "Test log file does not exist: %s" euporie-qrsh-test-log-file))))

(defun euporie-qrsh-clean-test-environment ()
  "Clean all test-related buffers, files, and processes."
  (interactive)
  (euporie-qrsh-kill-test-buffers)
  (when (file-exists-p euporie-qrsh-test-log-file)
    (delete-file euporie-qrsh-test-log-file))
  (message "QRSH test environment cleaned"))

(defun euporie-qrsh-test-remote-config-extraction ()
  "Test remote configuration extraction from various path formats.
Returns: 'PASS if all configurations extracted correctly, 'FAIL otherwise."
  (interactive)
  (euporie-qrsh-log "config-extraction" "start" 'info "Starting remote config extraction tests")
  
  (let ((test-configs '(
                       ;; Format: (path . expected-config-properties)
                       ("/sshx:wrds|qrsh::/home/path" . (:mode qrsh-tramp :host "wrds" :localname "/home/path"))
                       ("/sshx:server:/remote/path" . (:mode standard-tramp :host "server" :localname "/remote/path"))
                       ("/local/path" . nil)))  ; Local should return nil
        (all-passed t))
    
    (dolist (config-test test-configs)
      (let* ((test-path (car config-test))
             (expected-props (cdr config-test))
             (actual-config (condition-case err
                                (progn
                                  (ensure-euporie-modules-loaded)
                                  (if euporie-modules-loaded
                                      (euporie-termint--get-remote-config "sas" test-path)
                                    nil))
                              (error nil))))  ; Handle missing function gracefully
        
        (euporie-qrsh-log "config-extraction" "test" 'debug 
                         (format "Testing path: %s" test-path))
        
        (if expected-props
            ;; Expecting a config result
            (if actual-config
                (progn
                  (euporie-qrsh-log "config-extraction" "test" 'info 
                                   (format "✓ Config extraction returned result for %s" test-path)))
              (progn
                (setq all-passed nil)
                (euporie-qrsh-log "config-extraction" "test" 'error 
                                 (format "✗ Expected config for %s but got nil" test-path))))
          ;; Expecting nil
          (if (null actual-config)
              (euporie-qrsh-log "config-extraction" "test" 'info 
                               (format "✓ Correctly returned nil for local path: %s" test-path))
            (progn
              (setq all-passed nil)
              (euporie-qrsh-log "config-extraction" "test" 'error 
                               (format "✗ Expected nil for %s but got: %s" test-path actual-config)))))))
    
    (if all-passed
        (progn
          (euporie-qrsh-log "config-extraction" "complete" 'info "✓ All config extraction tests PASSED")
          (message "✓ Remote config extraction: PASSED")
          'PASS)
      (progn
        (euporie-qrsh-log "config-extraction" "complete" 'error "✗ Some config extraction tests FAILED")
        (message "✗ Remote config extraction: FAILED")
        'FAIL))))

(defun euporie-qrsh-test-sas-output-verification ()
  "Test SAS output verification by checking for expected cars data table.
Returns: 'PASS if SAS output contains expected data, 'FAIL otherwise."
  (interactive)
  (euporie-qrsh-log "sas-output" "start" 'info "Starting SAS cars output verification test")
  
  ;; Load the SAS test module
  (condition-case err
      (progn
        (require 'test-sas-simple-working)
        (let ((test-result (test-sas-output-verification)))
          (let ((overall-status (cdr (assoc 'overall test-result)))
                (passed (cdr (assoc 'passed test-result)))
                (total (cdr (assoc 'total test-result))))
            
            (if (eq overall-status 'pass)
                (progn
                  (euporie-qrsh-log "sas-output" "complete" 'info 
                                   (format "✓ SAS output verification PASSED (%d/%d tests)" passed total))
                  (message "✓ SAS output verification: PASSED (%d/%d)" passed total)
                  'PASS)
              (progn
                (euporie-qrsh-log "sas-output" "complete" 'error 
                                 (format "✗ SAS output verification FAILED (%d/%d tests)" passed total))
                (message "✗ SAS output verification: FAILED (%d/%d)" passed total)
                'FAIL)))))
    (error
     (euporie-qrsh-log "sas-output" "complete" 'error 
                      (format "✗ SAS output verification ERROR: %s" err))
     (message "✗ SAS output verification: ERROR - %s" err)
     'ERROR)))

(defun euporie-qrsh-generate-test-report ()
  "Generate a comprehensive HTML test report from the log file."
  (interactive)
  (if (not (file-exists-p euporie-qrsh-test-log-file))
      (message "No test log file found. Run tests first.")
    (let ((report-file (expand-file-name "euporie-qrsh-test-report.html" "~/")))
      (with-temp-buffer
        (insert "<!DOCTYPE html>\n<html><head><title>QRSH Integration Test Report</title></head>\n")
        (insert "<body><h1>Euporie QRSH Integration Test Report</h1>\n")
        (insert (format "<p>Generated: %s</p>\n" (format-time-string "%Y-%m-%d %H:%M:%S EST")))
        (insert "<pre style='background-color: #f5f5f5; padding: 10px; overflow-x: auto;'>\n")
        
        ;; Insert log contents
        (insert-file-contents euporie-qrsh-test-log-file)
        
        (insert "\n</pre></body></html>")
        (write-region (point-min) (point-max) report-file))
      
      (message "Test report generated: %s" report-file)
      report-file)))

(provide 'euporie-qrsh-integration-tests)
;;; euporie-qrsh-integration-tests.el ends here
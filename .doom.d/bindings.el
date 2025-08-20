;;; bindings.el -*- lexical-binding: t; -*-

(map! :map global-map "M-Q" #'unfill-paragraph)

;; SAS console keybindings
(after! sas-console
  ;; Global SAS console commands
  (map! "C-c s o" #'sas-console-open
        "C-c s s" #'sas-console-open-split
        "C-c s k" #'sas-console-kill-all-sessions
        "C-c s l" #'sas-console-list-sessions)
  
  ;; Keybindings for org-src-mode (when editing SAS source blocks)
  (map! :map org-src-mode-map
        "C-<return>" #'sas-console-send-line
        "C-c C-<return>" #'sas-console-send-region
        "C-c s" #'sas-console-open-for-org-src))

;; Terminal testing keybindings
(map! "C-c t v" #'vterm
      "C-c t e" #'eat
      "C-c u" #'unicode-test-insert
      "C-c U" #'unicode-test-buffer
      "C-c T" #'unicode-compare-terminals)

;; Unicode testing functions
(defun unicode-test-insert ()
  "Insert test Unicode characters for terminal testing"
  (interactive)
  (insert "Test Unicode: ⏺ ✳ ⭐ 🔴 📍 ▶️ ⚡ 🌟 ❄️ 🎯"))

(defun unicode-test-buffer ()
  "Create a dedicated buffer for Unicode testing with comprehensive test cases"
  (interactive)
  (let ((buf (get-buffer-create "*Unicode Test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== UNICODE RENDERING TEST BUFFER ===\n")
      (insert (format "Created: %s\n" (current-time-string)))
      (insert (format "Font: %s\n\n" (face-attribute 'default :font)))
      
      (insert "=== Critical Test Characters ===\n")
      (insert "Record symbol: ⏺ (should be red circle)\n")
      (insert "Asterisk: ✳ (should be 8-spoked star)\n")
      (insert "Star: ⭐ (should be yellow star)\n")
      (insert "Red circle: 🔴 (should be red emoji)\n")
      (insert "Play button: ▶️ (should be triangle)\n")
      (insert "Lightning: ⚡ (should be yellow bolt)\n\n")
      
      (insert "=== Geometric Shapes ===\n")
      (insert "Circles: ● ○ ⚫ ⚪ 🔴 🔵 🟢 🟡\n")
      (insert "Squares: ■ □ ▪ ▫ 🟥 🟦 🟩 🟨\n")
      (insert "Triangles: ▲ △ ▼ ▽ ◆ ◇\n\n")
      
      (insert "=== Arrows and Pointers ===\n")
      (insert "Basic arrows: → ← ↑ ↓ ↔ ↕\n")
      (insert "Pointer triangles: ▶ ◀ ▲ ▼\n")
      (insert "Double arrows: ⇒ ⇐ ⇑ ⇓\n\n")
      
      (insert "=== Symbols and Marks ===\n")
      (insert "Check marks: ✓ ✔ ☑ ✅ ❌ ❎\n")
      (insert "Stars: ★ ☆ ⭐ 🌟 ✨ 💫\n")
      (insert "Warnings: ⚠ 🚨 ❗ ❓ ⚡\n\n")
      
      (insert "=== Box Drawing ===\n")
      (insert "┌─┬─┐\n")
      (insert "│ │ │\n") 
      (insert "├─┼─┤\n")
      (insert "│ │ │\n")
      (insert "└─┴─┘\n\n")
      
      (insert "=== Mixed Content ===\n")
      (insert "Progress: [████████████████████] 100% ✔\n")
      (insert "Status: ⏺ Recording • ⚡ Active • 🔴 Live\n")
      (insert "Controls: ▶ Play ⏸ Pause ⏹ Stop ⏭ Next\n")
      (insert "Rating: ⭐⭐⭐⭐⭐ (5 stars)\n\n")
      
      (insert "=== Font Fallback Test ===\n")
      (insert "ASCII equivalents: * vs ✳, > vs ▶, - vs —\n")
      (insert "Similar shapes: O vs ○ vs ● vs ⭕ vs 🔴\n")
      (insert "Variation selectors: ▶ vs ▶️ (with vs without)\n\n")
      
      (insert "=== End of Test Buffer ===\n"))
    (pop-to-buffer buf)
    (goto-char (point-min))))

(defun unicode-compare-terminals ()
  "Set up side-by-side comparison of Unicode rendering in different contexts"
  (interactive)
  (delete-other-windows)
  ;; Create test buffer
  (unicode-test-buffer)
  ;; Split and create terminal
  (split-window-right)
  (other-window 1)
  (vterm)
  ;; Split again for eat terminal
  (split-window-below)
  (other-window 1)
  (eat)
  ;; Go back to test buffer
  (other-window 1)
  (message "Unicode comparison setup complete. Run test scripts in terminals."))

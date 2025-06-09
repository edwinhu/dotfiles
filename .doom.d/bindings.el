;;; bindings.el -*- lexical-binding: t; -*-

(after! ess-mode
  (map! :map ess-mode-map
        :nvi "C-<return>" #'proj-vterm-send-line-or-region-and-step)
 )

(map! :map sh-mode-map
      :nvi "C-<return>" #'proj-vterm-send-line-or-region-and-step)

(map! :map global-map "M-Q" #'unfill-paragraph)

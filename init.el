;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; Profile emacs startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "*** Emacs loaded in %s with %d garbage collections."
;;                      (format "%.2f seconds"
;;                              (float-time
;;                               (time-subtract after-init-time before-init-time)))
;;                      gcs-done)))

;; **exit**
;; https://emacs.stackexchange.com/a/28927
(defun my-exit ()
  (with-current-buffer " *load*"
    (goto-char (point-max))))

;; laptop vs. desktop
(setq on-laptop-p (string= (system-name) "fafner"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-firefox-program "firefox")
 '(c-default-style "linux")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output 'first-error)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(gtags-auto-update t)
 '(gtags-find-all-text-files nil)
 '(gtags-suggested-key-mapping t)
 '(httpd-port 8088)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(markdown-command "pandoc")
 '(package-selected-packages '(compat))
 '(plantuml-default-exec-mode 'jar)
 '(python-check-command "pyflakes")
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((python-shell-virtualenv-root . "~/prog/python/fbxserv/venv")
     (python-shell-virtualenv-root . "~/prog/python/freeboxrec/venv")
     (python-shell-interpreter . "python3")
     (python-shell-virtualenv-root . "venv")))
 '(transient-mark-mode t)
 '(vc-handled-backends '(Hg RCS CVS SVN SCCS Bzr Git Mtn Arch))
 '(which-function-mode t)
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(cond (window-system
        (define-key global-map [S-mouse-2] 'imenu)))

(add-to-list 'load-path "/usr/local/share/gtags")
(autoload 'gtags-mode "gtags" "" t)

;; use a lambda because 'gtags-mode is a toggle
;; and thus revert-buffer disables gtags
(add-hook 'c-mode-hook		(lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook	(lambda () (gtags-mode 1)))
(add-hook 'dired-mode-hook	(lambda () (gtags-mode 1)))

;(set-frame-font "fixed")
;(set-frame-font "-misc-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-15")
; grep ^fixed /etc/X11/fonts/misc/xfonts-base.alias
;(set-frame-font "-misc-fixed-medium-r-semicondensed-*-13-120-75-75-c-60-iso8859-15")
; next is ok for presentations, adjust size with C-x C-+
;(set-frame-font "-misc-*-medium-r-*-*-*-200-*-*-*-*-iso8859-15")
;(set-frame-font "-xos4-Terminus-bold-*-*-*-20-*-*-*-c-100-iso10646-1")
;(set-frame-font "DejaVu Sans Mono Book 12")
;(set-frame-font "Terminus")
(let ((font (if on-laptop-p
                "DejaVu Sans Mono Book 11"
              "DejaVu Sans Mono Book 9")))
  (progn
    (set-frame-font font)
    (add-to-list 'default-frame-alist
	         (cons 'font font))))

;; Set dark mode. Prefix with C-- for light mode.
(defun dark-mode (&optional light)
  (interactive "P")
  (if (not light)
      (progn (set-background-color "black")
	     (set-foreground-color "grey86"))
    (progn (set-background-color "grey86")
	   (set-foreground-color "black"))))
(dark-mode)
;; New frames default to dark mode
(add-to-list 'default-frame-alist
	     '(background-color . "black"))
(add-to-list 'default-frame-alist
	     '(foreground-color . "grey86"))

; re-enable scroll to left (with C-pgdn)
(put 'scroll-left 'disabled nil)

; get rid of tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; http://www.emacswiki.org/emacs/ParenthesisMatching#toc4
; bind C-% to goto-match-paren
; note, cursor must right before/on/after paren/brace/bracket

(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(global-set-key (kbd "C-%") 'goto-match-paren)

;; bind F5 to re-run current compile command
(global-set-key [f5] 'recompile)

;; unbind annoying 'Alt-5' binding
(global-unset-key "\M-(")

;; move between windows with shift+arrow keys
(windmove-default-keybindings)

;; invoke magit UI
(global-set-key (kbd "C-x g") 'magit-status)

;; invoke occur for symbol at point
(defun my-occur-symbol-at-point (&optional nlines)
  (interactive "p")
  (occur (find-tag-default-as-symbol-regexp) nlines))
(global-set-key (kbd "M-s O") 'my-occur-symbol-at-point)

;; initial frame/window layout
;; split frame in 3 windows when plugged to wide monitor, otherwise just 2
(defun my-init-window ()
  (set-frame-parameter nil 'fullscreen 'maximized)
  (split-window nil nil t)
  (if (not on-laptop-p)
      (split-window nil nil t))
  (balance-windows))
(when window-system
  (my-init-window))

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; have M-x compile create its buffer with vertical split
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; use X clipboard by default
(setq x-select-enable-clipboard t)
(setq x-select-enable-clipboard-manager nil)

;; load credentials
(condition-case err
    (load-file "~/.credentials.el")
  (error (warn (format "Failed to load credentials file: %s" err))))

;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))

(el-get-bundle applescript-mode
  (add-to-list 'auto-mode-alist '("\\.applescript\\'" . applescript-mode)))
(el-get-bundle company-mode)
(el-get-bundle goto-last-change)
(el-get-bundle graphviz-dot-mode)
(el-get-bundle haskell-mode)
(el-get-bundle jedi)
(el-get-bundle markdown-mode)
(el-get-bundle restclient)
(el-get-bundle ratish-punnoose/tla-mode)
(el-get-bundle yaml-mode
  (add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode)))
(el-get-bundle s)
(el-get-bundle dockerfile-mode)
(el-get-bundle rpm-spec-mode)
(el-get-bundle systemd-mode)
(el-get-bundle rust-mode)
(el-get-bundle plantuml-mode)
(el-get-bundle magit)
(el-get-bundle toml-mode)
(el-get-bundle jinja2-mode)
(el-get-bundle web-mode)
(el-get-bundle wgrep)
(el-get-bundle impatient-mode) ;; local recipe
(el-get-bundle js-comint)
(add-hook 'js-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-p") 'js-comint-repl)
            (local-set-key (kbd "C-x C-e") 'js-comint-send-last-sexp)
            (local-set-key (kbd "C-c C-r") 'js-comint-send-region)
            (local-set-key (kbd "C-c C-b") 'js-comint-send-buffer)))
(el-get-bundle editorconfig
  :post-init (editorconfig-mode 1))
(el-get-bundle lsp-mode)
(add-hook 'rust-mode-hook 'lsp-deferred)

(el-get-bundle ellama
  (progn
    (require 'llm-ollama)
    (require 'llm-openai)
    (require 'llm-gemini)
    (setq llm-warn-on-nonfree nil)
    (let ((deepseek-key (if (boundp 'my-deepseek-key) my-deepseek-key ""))
          (gemini-key (if (boundp 'my-gemini-key) my-gemini-key ""))
          (chat-provider (if (boundp 'my-deepseek-key) "deepseek-chat" "qwen3")))
      (setq-default ellama-providers
		    `(("qwen3" .
                       (make-llm-ollama :embedding-model "qwen3:8b"
                                        :chat-model "qwen3:8b"
                                        :host "erda"
                                        :default-chat-non-standard-params '(("num_ctx" . 24000))))
                      ("gemini" .
                       (make-llm-gemini :chat-model "gemini-2.5-pro"
                                        :key ,gemini-key))
                      ("deepseek-chat" .
                       (make-llm-openai-compatible :chat-model "deepseek-chat"
                                                   :url "https://api.deepseek.com"
                                                   :key ,deepseek-key))
                      ("deepseek-coder" .
                       (make-llm-openai-compatible :chat-model "deepseek-coder"
                                                   :url "https://api.deepseek.com"
                                                   :key ,deepseek-key))))
      (setq-default ellama-provider
                    (alist-get chat-provider ellama-providers nil nil #'string=)))
    (setq-default ellama-session-auto-save nil)
    (global-set-key (kbd "C-c e") 'ellama)
    (setopt ellama-auto-scroll t)
    (add-hook 'org-ctrl-c-ctrl-c-final-hook 'ellama-chat-send-last-message)))

;; End of recipes, call `el-get' to make sure all packages (including
;; dependencies) are setup.
(el-get 'sync)

;; Jedi mode for python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)

;; haskell
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-process-load-file)

;; Toggle window dedication
;; https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope#65992
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window 
				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key [pause] 'toggle-window-dedicated)

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Replacing-highlighted-text.html#Replacing-highlighted-text
(delete-selection-mode 1)

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html#Matching-parentheses
(show-paren-mode 1)

;; default bound to C-x C-l (opposite: C-x C-u)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; auto-close parens etc.
(electric-pair-mode)

;; keep remote user's path
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; keep remote autosave files remote
(setq auto-save-file-name-transforms nil)

;; custom filter for impatient mode that does simple string sustitutions

(setq my-imp-subs '(("/static/" . "")
                    ("base href=\"/\"" . "base href=\"\"")))

(defun my-imp-replace (input-buffer)
  "Create a new buffer with content from INPUT-BUFFER, apply all
string subtitutions in my-imp-subs, and return the new buffer."
  (let ((output-buffer (generate-new-buffer "replaced-foo-with-bar")))
    (with-current-buffer output-buffer
      (insert-buffer-substring input-buffer)
      (map-do (lambda (str-from str-to)
                (goto-char (point-min))
                (while (search-forward str-from nil t)
                  (replace-match str-to t t)))
              my-imp-subs))
    output-buffer))

(defun my-imp-user-filter (buffer)
  "Alter buffer before sending to clients."
  (let ((subs-buffer (save-match-data (my-imp-replace buffer))))
    (princ (with-current-buffer subs-buffer (buffer-string)))
    (kill-buffer subs-buffer)))

(defun my-imp-set-user-filter ()
  (interactive)
  (imp-set-user-filter 'my-imp-user-filter))

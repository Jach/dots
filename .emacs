(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
             
(setq package-enable-at-startup nil)
(package-initialize)

(setq inhibit-startup-screen t)

(setq backup-directory-alist '(("." . "~/.emacs.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t)

(setq-default indent-tabs-mode nil
              tab-width 4)

(require 'evil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package rainbow-delimiters :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;(use-package klere-theme :ensure t)
;(load-theme 'klere t)
(use-package color-theme-modern :ensure t)
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/git_repos/replace-colorthemes"))
(load-theme 'vim-colors t t)
(enable-theme 'vim-colors)

(use-package key-chord :ensure t)
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode t)

(setq evil-search-wrap t
      evil-regexp-search t)

(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package sly :ensure t) ; better slime

(use-package evil-leader :ensure t
             :config
             (global-evil-leader-mode)
             (evil-leader/set-leader ","))

; sly-fancy bundles default sly contribs
(setq sly-contribs '(sly-fancy))

(evil-leader/set-key "sly" 'sly) ; use ,sly instead of m-x sly
; but anyway connect automagically when opening a lisp file..
(add-hook 'sly-mode-hook
          (lambda ()
            (unless (sly-connected-p)
              (save-excursion (sly)))))

; eval functions take a prefix arg, if it's numeric then the results get inserted into the buffer instead
; of echoed...

(defun my-up-list+ ()
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
             (forward-char))
      (up-list)))

(defun sly-eval-current-sexp ()
  "do the right thing darn it.
   Evaluates the immediate enclosing form from anywhere within, or the current form if on an open paren."
  (interactive)
  (save-excursion ;; get out of string if in it
    ; if starting on open paren, then eval until the closing paren
    (if (equal (char-after) ?\()
        (sly-interactive-eval (sly-sexp-at-point))
        (progn
          (dotimes (c (if (in-string-p) 2 1))
            (my-up-list+))
          (sly-interactive-eval (sly-last-expression))))))

(defun sly-eval-and-insert-sexp ()
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'sly-eval-current-sexp)))

(evil-leader/set-key "e" 'sly-eval-current-sexp)
(evil-leader/set-key "d" 'sly-eval-defun) ; evals top level expr from anywhere in expr
(evil-leader/set-key "bd" 'sly-eval-and-insert-sexp)
(evil-leader/set-key "r" 'sly-eval-region)  ; for visual mode..

(evil-leader/set-key "cc" 'sly-interrupt) ; when we get into a loop...

(evil-leader/set-key "D" 'sly-compile-defun) ; gets region of top-level form at current point, writes to file, compiles file, loads (and executes) result
(evil-leader/set-key "F" 'sly-compile-and-load-file) ; compile and load current buffer's source file, unless compilation fails
(evil-leader/set-key "cf" 'sly-compile-file)
(evil-leader/set-key "l" 'sly-load-file)
(evil-leader/set-key "R" 'sly-compile-region)

(evil-leader/set-key "apr" 'sly-apropos-all)
(evil-leader/set-key "tracetog" 'sly-trace-dialog-toggle-trace) ; toggles trace on fn under cursor
(evil-leader/set-key "tracedia" 'sly-trace-dialog)

(evil-leader/set-key "sf" 'sly-describe-function)
(evil-leader/set-key "ss" 'sly-describe-symbol)
(evil-leader/set-key "hf" 'sly-hyperspec-lookup-format) ; looks up format char
(evil-leader/set-key "hr" 'sly-hyperspec-lookup-reader-macro)
(evil-leader/set-key "hh" 'sly-hyperspec-lookup)

(evil-leader/set-key "1" 'sly-expand-1)
(evil-leader/set-key "mi" 'sly-macroexpand-1-inplace)
(evil-leader/set-key "ma" 'sly-macroexpand-all)

(evil-leader/set-key "gd" 'sly-edit-definition)
(evil-leader/set-key "xu" 'sly-edit-uses) ; finds refs to symbol whatever the type of reference
(evil-leader/set-key "xcw" 'sly-calls-who) ; callees
(evil-leader/set-key "xc" 'sly-who-calls)
(evil-leader/set-key "xr" 'sly-who-references) ; refs to global var
(evil-leader/set-key "xb" 'sly-who-binds)
(evil-leader/set-key "xs" 'sly-who-sets)
(evil-leader/set-key "xm" 'sly-who-macroexpands)
(evil-leader/set-key "xp" 'sly-who-specializes) ; all known methods specialized on a class
(evil-leader/set-key "xl" 'sly-list-callers) ; list callers, operates at a very low level...
(evil-leader/set-key "xe" 'sly-list-callees) ; list callees

(use-package company :ensure t)
(evil-leader/set-key "ac" 'company-complete)

(setq echo-keystrokes 1) ; closest thing to set showcmd?

(use-package adjust-parens :ensure t)
(add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode)
(add-hook 'clojure-mode-hook #'adjust-parens-mode)
(add-hook 'lisp-mode-hook #'adjust-parens-mode)

; nifty.. often need to be directly on paren though.
; however most useful thing is: go to one line after fn, run indent, now can add to end of fn
(evil-leader/set-key "<<" 'lisp-dedent-adjust-parens)
(evil-leader/set-key ">>" 'lisp-indent-adjust-parens)

(use-package highlight-quoted :ensure t)
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
(add-hook 'clojure-mode-hook #'highlight-quoted-mode)
(add-hook 'lisp-mode-hook #'highlight-quoted-mode)


(evil-mode t)

;(use-package evil-surround :ensure t
;	     :config
;	     (global-evil-surround-mode))
;(use-package evil-indent-textobject :ensure t)
;
;(use-package markdown-mode
;  :ensure t)

;(use-package projectile :ensure t)
;(use-package magit :ensure t)
;(use-package iedit :ensure t)
;(use-package helm :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil-visual-mark-mode)))
 '(show-paren-mode 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 
 ; http://www.raebear.net/computers/emacs-colors/
 
 '(rainbow-delimiters-depth-1-face ((t (:bold t :foreground "firebrick3"))))
 '(rainbow-delimiters-depth-2-face ((t (:bold nil :foreground "orange1"))))
 '(rainbow-delimiters-depth-3-face ((t (:bold nil :foreground "green"))))
 '(rainbow-delimiters-depth-4-face ((t (:bold t :foreground "cyan"))))
 '(rainbow-delimiters-depth-5-face ((t (:bold nil :foreground "DarkOrchid3"))))
 '(rainbow-delimiters-depth-6-face ((t (:bold t :foreground "firebrick3"))))
 '(rainbow-delimiters-depth-7-face ((t (:bold nil :foreground "orange1"))))
 '(rainbow-delimiters-depth-8-face ((t (:bold nil :foreground "green"))))
 '(rainbow-delimiters-depth-9-face ((t (:bold t :foreground "cyan"))))

 '(rainbow-delimiters-unmatched-face ((t (:bold t :foreground "white" :background "red"))))

 )

(set-default-font "Bitstream Vera Sans Mono-11")


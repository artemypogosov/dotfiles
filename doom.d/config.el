;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(require 'battery)
(require 'lsp)
(require 'lsp-haskell)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name    "Artemy Pogosov"
      user-mail-address "artemypogosov@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones: doom-font, doom-variable-pitch-font, doom-big-font
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

;; >>> FONTS
;; ‘doom-font’ – standard monospace font that is used for most things in Emacs.
;; ‘doom-variable-pitch-font’ – variable font which is useful in some Emacs plugins.
;; ‘doom-big-font’ – used in doom-big-font-mode; useful for presentations.
;; ‘font-lock-comment-face’ – for comments.
;; ‘font-lock-keyword-face’ – for keywords with special significance like ‘setq’ in elisp.

(setq doom-font (font-spec :family "Source Code Pro" :size  15)
      doom-variable-pitch-font (font-spec :family "UbuntuMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "Source Code Pro" :size 20))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
 '(font-lock-comment-face :slant italic)
 '(font-lock-keyword-face :slant italic))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-hide-emphasis-markers t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; PROJECTILE
(setq projectile-project-search-path '("~/IdeaProjects"))


;; OTHER
(setq undo-limit 80000000               ; Raise undo-limit to 80Mb
      evil-want-fine-undo t             ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t               ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"      ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil)        ; I can trust my computers ... can't I?

(display-time-mode 1)

; On laptops it's nice to know how much power you have
;(when (and (funcall battery-status-function)
;           (not (string-match-p "N/A" (battery-format "%B"(funcall battery-status-function)))))
;  (display-battery-mode 1))

;; (global-subword-mode 1)

;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)




;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

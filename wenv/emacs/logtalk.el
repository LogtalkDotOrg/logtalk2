;; logtalk.el -- font lock support for Logtalk (http://www.logtalk.org/)

;; Copyright (C) 2003 Paulo Moura

;; Author: Paulo Moura
;; Creation date: November 11, 2003
;; Last modification date: November 11, 2003
;; Version: 0.1

;; Installation:
;;
;; Add the following lines in your Emacs init file, for example
;; your ~/.emacs file.
;;
;;     (add-hook 'logtalk-mode-hook 'my-logtalk-mode-hook)
;;
;;     (defun my-logtalk-mode-hook ()
;;       (cond (window-system
;;              (require 'logtalk-font-lock)
;;              (turn-on-font-lock))))
;;
;;     (setq font-lock-maximum-decoration t)



;; setup 

(defvar logtalk-mode-hook nil)
(defvar logtalk-mode-map nil) 

(if logtalk-mode-map nil
	(setq logtalk-mode-map (make-keymap)) 


(add-to-list 'auto-mode-alist '("\\.lgt\\'" . logtalk-mode))



;; syntax highlighting 


(defvar logtalk-font-lock-keywords
	(list
		'("\\('\\info'\\)" . logtalk-operator-face))
		"Minimal highlighting expressions for WPDL mode")

 



;; indentation 

(defun logtalk-indent-line ()
	"Indent current line as Logtalk code"
	(beginning-of-line)

(if (bobp)
	(indent-line-to 0)



;; syntax table 

(defvar logtalk-mode-syntax-table nil) 

(defun logtalk-create-syntax-table ()
	(if logtalk-mode-syntax-table
		()
		(setq logtalk-mode-syntax-table (make-syntax-table)) 

(modify-syntax-entry ?_ "w" logtalk-mode-syntax-table)
(modify-syntax-entry ?_ "_" logtalk-mode-syntax-table)

(modify-syntax-entry ?/ ". 14" logtalk-mode-syntax-table)
(modify-syntax-entry ?* ". 23b" logtalk-mode-syntax-table)
(modify-syntax-entry ?% "<" logtalk-mode-syntax-table)
(modify-syntax-entry ?\n ">" logtalk-mode-syntax-table)


(set-syntax-table logtalk-mode-syntax-table)


;; entry function

(defun logtalk-mode ()
	"Major mode for editing Logtalk files"
	(interactive)
	(kill-all-local-variables)
	(logtalk-create-syntax-table)

(set (make-local-variable 'font-lock-defaults) '(logtalk-font-lock-keywords))

(set (make-local-variable 'indent-line-function) 'logtalk-indent-line)

(setq major-mode 'logtalk-mode)
(setq mode-name "Logtalk")
(run-hooks 'logtalk-mode-hook)

(provide 'logtalk-mode) 



;; create logtalk font-lock-faces

(make-face 'logtalk-directive-face)
(setq font-lock-face-attributes '((logtalk-directive-face "darkmagenta" nil nil nil nil))) 

(make-face 'logtalk-built-in-predicate-face)
(setq font-lock-face-attributes '((logtalk-built-in-predicate-face "magenta " nil nil nil nil))) 

(make-face 'logtalk-built-in-method-face)
(setq font-lock-face-attributes '((logtalk-built-in-method-face "magenta " nil nil nil nil))) 

(make-face 'logtalk-operator-face)
(setq font-lock-face-attributes '((logtalk-operator-face "blue" nil t nil nil))) 

(make-face 'logtalk-string-face)
(setq font-lock-face-attributes '((logtalk-string-face "brown " nil t nil nil))) 

(make-face 'logtalk-number-face)
(setq font-lock-face-attributes '((logtalk-number-face "blue" nil t nil nil))) 

(make-face 'logtalk-comment-face)
(setq font-lock-face-attributes '((logtalk-comment-face "green" nil nil t nil))) 


;; set the font-lock-comment-face to the logtalk-comment-face

(setq font-lock-comment-face 'logtalk-comment-face)



(defvar logtalk-font-lock-directives nil)





(defvar logtalk-font-lock-built-in-methods nil)


(defvar logtalk-font-lock-operators nil)

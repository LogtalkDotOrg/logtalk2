;; logtalk.el -- font lock support for Logtalk (http://www.logtalk.org/)

;; Copyright (C) 2003 Paulo Moura

;; Author: Paulo Moura
;; Creation date: November 15, 2003
;; Last modification date: November 15, 2003
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
;;
;; (autoload 'logtalk-mode "logtalk" "Major mode for editing Logtalk programs." t)
;; (setq auto-mode-alist (cons '("\\.lgt\\'" . logtalk-mode) auto-mode-alist))



;; setup 

(defvar logtalk-mode-version "0.1"
	"Logtalk mode version number")

(defvar logtalk-mode-hook nil)

(defvar logtalk-mode-map nil) 

(if logtalk-mode-map nil
	(setq logtalk-mode-map (make-keymap)))



(add-to-list 'auto-mode-alist '("\\.lgt\\'" . logtalk-mode))



;; syntax highlighting 

(defvar logtalk-font-lock-keywords nil)



;; syntax table 

(defvar logtalk-mode-syntax-table
	(let ((logtalk-mode-syntax-table (make-syntax-table)))
		(modify-syntax-entry ?_ "w" logtalk-mode-syntax-table)
		(modify-syntax-entry ?_ "_" logtalk-mode-syntax-table)
		(modify-syntax-entry ?/ ". 14b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?* ". 23b" logtalk-mode-syntax-table)
		(modify-syntax-entry ?% "<" logtalk-mode-syntax-table)
		(modify-syntax-entry ?\n ">" logtalk-mode-syntax-table)
		logtalk-mode-syntax-table)
	"Syntax table for logtalk-mode")



;; create logtalk font-lock-faces

(make-face 'logtalk-directive-face)
(set-face-foreground 'logtalk-directive-face "darkmagenta")

(make-face 'logtalk-built-in-predicate-face)
(set-face-foreground 'logtalk-built-in-predicate-face "magenta")

(make-face 'logtalk-built-in-method-face)
(set-face-foreground 'logtalk-built-in-method-face "magenta")

(make-face 'logtalk-operator-face)
(set-face-foreground 'logtalk-operator-face "blue")
(set-face-bold-p 'logtalk-operator-face t)

(make-face 'logtalk-string-face)
(set-face-foreground 'logtalk-string-face "brown")

(make-face 'logtalk-number-face)
(set-face-foreground 'logtalk-number-face "blue")

(make-face 'logtalk-comment-face)
(set-face-foreground 'logtalk-comment-face "forest green")


;; set the font-lock-comment-face to the logtalk-comment-face

(setq font-lock-comment-face 'logtalk-comment-face)



(setq logtalk-font-lock-directives
	'(
		("\\(end_\\(?:category\\|object\\|protocol\\)\\)" . 'logtalk-directive-face)
		("category\\|object\\|protocol" . 'logtalk-directive-face)
		("p\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)" . 'logtalk-directive-face)
		("calls\\|d\\(?:iscontiguous\\|ynamic\\)\\|in\\(?:fo\\|itialization\\)\\|m\\(?:\\(?:etapredicat\\|od\\)e\\)\\|op\\|uses" . 'logtalk-directive-face)
		("\\(?:extend\\|i\\(?:mp\\(?:\\(?:lemen\\|or\\)t\\)\\|nstantiate\\)\\|specialize\\)s" . 'logtalk-directive-face)
	))


(setq logtalk-font-lock-built-in-methods
	'(
		("parameter\\|se\\(?:lf\\|nder\\)\\|this" . 'logtalk-built-in-method-face)
		("current_predicate\\|predicate_property" . 'logtalk-built-in-method-face)
		("a\\(?:bolish\\|ssert[az]\\)\\|clause\\|retract\\(?:all\\)?" . 'logtalk-built-in-method-face)
		("bagof\\|f\\(?:\\(?:ind\\|or\\)all\\)\\|setof" . 'logtalk-built-in-method-face)
		("after\\|before" . 'logtalk-built-in-method-face)
		("phrase" . 'logtalk-built-in-method-face)
	))


(setq logtalk-font-lock-built-in-predicates
	'(
		("current_\\(?:category\\|object\\|protocol\\)" . 'logtalk-built-in-predicate-face)
		("create\\(?:_object\\|e_\\(?:category\\|protocol\\)\\)" . 'logtalk-built-in-predicate-face)
		("abolish_\\(?:category\\|object\\|protocol\\)" . 'logtalk-built-in-predicate-face)
		("\\(?:category\\|object\\|protocol\\)_property" . 'logtalk-built-in-predicate-face)
		("extends_\\(?:object\\|protocol\\)\\|i\\(?:mp\\(?:lements_protocol\\|orts_category\\)\\|nstantiates_object\\)\\|specializes_object" . 'logtalk-built-in-predicate-face)
		("abolish_events\\|current_event\\|define_events" . 'logtalk-built-in-predicate-face)
		("\\(?:curren\\|se\\)t_logtalk_flag" . 'logtalk-built-in-predicate-face)
		("logtalk_\\(?:compile\\|load\\|version\\)" . 'logtalk-built-in-predicate-face)
		("forall" . 'logtalk-built-in-predicate-face)
		("retractall" . 'logtalk-built-in-predicate-face)
		;; control constructs:
		("ca\\(?:ll\\|tch\\)\\|fail\\|t\\(?:hrow\\|rue\\)" . 'logtalk-built-in-predicate-face)
		;; logic and control:
		("\\\\\\+\\|once\\|repeat" . 'logtalk-built-in-predicate-face)
		;; term unification:
		("\\\\?=" . 'logtalk-built-in-predicate-face)
		;; term testing:
		("atom\\(?:ic\\)?\\|compound\\|float\\|\\(?:intege\\|n\\(?:onva\\|umbe\\)\\|va\\)r" . 'logtalk-built-in-predicate-face)
		;; term comparison:
		("==\\|@\\(?:=<\\|>=\\|[<>]\\)\\|\\\\==" . 'logtalk-built-in-predicate-face)
		;; term creation and decomposition:
		("=\\.\\.\\|arg\\|copy_term\\|functor" . 'logtalk-built-in-predicate-face)
		;; arithemtic evaluation:
		("is" . 'logtalk-built-in-predicate-face)
		;; arithemtic comparison:
		("=\\(?::=\\|[<\\]\\)\\|>=\\|[<>]" . 'logtalk-built-in-predicate-face)
		;; evaluable functors:
		
		("at_end_of_stream\\|c\\(?:lose\\|urrent_\\(?:\\(?:in\\|out\\)put\\)\\)\\|flush_output\\|open\\|s\\(?:et_\\(?:input\\|output\\|stream_position\\)\\|tream_property\\)" . 'logtalk-built-in-predicate-face)
		;; character input/output:
		("get_c\\(?:har\\|ode\\)\\|nl\\|p\\(?:eek_c\\(?:har\\|ode\\)\\|ut_c\\(?:har\\|ode\\)\\)" . 'logtalk-built-in-predicate-face)
		("\\(?:get\\|p\\(?:eek\\|ut\\)\\)_byte" . 'logtalk-built-in-predicate-face)
		;; term input/output:
		("c\\(?:har_conversion\\|urrent_\\(?:char_conversion\\|op\\)\\)\\|op\\|read\\(?:_term\\)?\\|write\\(?:_\\(?:canonical\\|term\\)\\|q\\)?" . 'logtalk-built-in-predicate-face)
		("\\(?:curren\\|se\\)t_prolog_flag\\|halt" . 'logtalk-built-in-predicate-face)
		("atom_\\(?:c\\(?:hars\\|o\\(?:des\\|ncat\\)\\)\\|length\\)\\|char_code\\|number_c\\(?:\\(?:har\\|ode\\)s\\)\\|sub_atom" . 'logtalk-built-in-predicate-face)
	))



(setq logtalk-font-lock-operators
	'(
		("::\\|\\[\\^\\]\\[\\^\\]\\|[{}]" . 'logtalk-operator-face)
	))



(setq logtalk-font-lock-keywords
	(append
		logtalk-font-lock-directives
		logtalk-font-lock-built-in-methods
		logtalk-font-lock-built-in-predicates
		logtalk-font-lock-operators
	))



;; entry function

(defun logtalk-mode ()
	"Major mode for editing Logtalk files"
	(interactive)
	(kill-all-local-variables)
	(set-syntax-table logtalk-mode-syntax-table)
	(set (make-local-variable 'font-lock-defaults) '(logtalk-font-lock-keywords))
	(turn-on-font-lock)
	(setq major-mode 'logtalk-mode)
	(setq mode-name "Logtalk")
	(run-hooks 'logtalk-mode-hook))

(provide 'logtalk-mode) 

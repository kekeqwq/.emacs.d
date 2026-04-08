EM ?= emacs
EE ?= $(EM) -Q --batch --eval "(progn (require 'ob-tangle) (setq org-confirm-babel-evaluate nil))"
all: init.org
	$(EE) --eval '(org-babel-tangle-publish t "$<" "$(@D)/")'

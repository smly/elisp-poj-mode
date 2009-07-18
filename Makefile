all: poj-mode.el
	emacs --no-init-file --batch -f batch-byte-compile poj-mode.el

test: poj-mode.el
	emacs --no-init-file --batch -f batch-byte-compile poj-mode.el
	rm poj-mode.elc

clean:
	rm poj-mode.elc

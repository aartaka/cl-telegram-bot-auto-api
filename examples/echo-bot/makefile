LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 1024 --no-userinit --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: echo-bot

clean:
	rm echo-bot

echo-bot:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load echo.asd --eval '(asdf:load-system :echo-bot)' --eval '(asdf:make :echo-bot)' --eval '(quit)'

install: echo-bot
	cp $^ $(DESTDIR)/

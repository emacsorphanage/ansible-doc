EMACS = emacs
EMACSFLAGS =

SRCS = ansible-doc.el
OBJS = $(SRCS:.el=.elc)

PHONY: compile

# Build targets
compile: $(OBJS)

# File targets
%.elc : %.el
	$(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

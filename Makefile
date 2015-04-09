EMACS = emacs
EMACSFLAGS =
EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

SRCS = ansible-doc.el
OBJS = $(SRCS:.el=.elc)

PHONY: compile test

# Build targets
compile: $(OBJS)

test: $(OBJS)
	$(EMACSBATCH) $(addprefix -l ,$(OBJS)) -l ansible-doc-test.el \
		-f ert-run-tests-batch-and-exit

# File targets
%.elc : %.el
	$(EMACSBATCH) -f batch-byte-compile $<

# -*- mode: makefile-gmake -*-

ELISP	= svn.el svn-blame.el svn-explorer.el svn-log.el svn-repository.el svn-update.el
TARGET	= ${ELISP:.el=.elc}

.PHONY: all build clean

all: build

build: ${TARGET}

clean:
	${RM} ${TARGET}

svn.elc: svn.el
	emacs --batch -f batch-byte-compile $<

svn-explorer.elc: svn-explorer.el
	emacs --batch -l svn.el -l folder-mode/folder-mode.el -l file-explorer/file-explorer.el -f batch-byte-compile $<

svn-repository.elc: svn-repository.el
	emacs --batch -l svn.el -l folder-mode/folder-mode.el -l file-explorer/file-explorer.el -f batch-byte-compile $<


%.elc: %.el
	emacs --batch -l svn.el -f batch-byte-compile $<

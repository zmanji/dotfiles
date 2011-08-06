SHELL := /bin/bash

all: link

link: .gemrc .gitconfig
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

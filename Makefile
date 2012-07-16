SHELL := /bin/bash

DOTFILES := .ackrc \
						.gemrc \
						.gitconfig .gitignore_global \
						.vim .vimrc .gvimrc \
						.osx .hushlogin \
						.inputrc .editrc \
						.tmux.conf \
						.zshenv .zshrc

all: submodules link

link: $(DOTFILES)
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

clean: $(DOTFILES)
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))

submodules:
	git submodule init
	git submodule update
	git submodule foreach git pull origin master

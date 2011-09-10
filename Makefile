SHELL := /bin/bash

DOTFILES := .gemrc .gitconfig .gitignore_global .vim .vimrc .gvimrc


all: link

link: $(DOTFILES)
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

clean: $(DOTFILES)
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))

rvm:
	bash < <(curl -s https://rvm.beginrescueend.com/install/rvm)

SHELL := /bin/bash

DOTFILES := .gemrc .gitconfig .gitignore_global .vim .vimrc .gvimrc


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

native: CommandT

CommandT:
	cd ~/.vim/bundle/Command-T/ruby/command-t && \
	ruby extconf.rb && \
	make

doc:
	rocco --language=VimL --comment-chars=\" .vimrc

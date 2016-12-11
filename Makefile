SHELL := /bin/bash

all: submodules shells osx git vim karabiner hammerspoon

git: .gitconfig .gitignore_global
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

vim: .vim .vimrc .gvimrc
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

shells: .editrc .gemrc .hushlogin .inputrc .zshenv .zshrc .zsh
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

osx: .osx
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )
	@echo Running .osx; \
	sh ~/.osx

hammerspoon: .hammerspoon
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

submodules:
	git submodule init
	git submodule update --recursive

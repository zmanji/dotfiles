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

shells: .editrc .gemrc .hushlogin .inputrc .profile
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

karabiner: karabiner.sh private.xml
	rm ~/Library/Application\ Support/Karabiner/private.xml
	ln -s $(CURDIR)/private.xml ~/Library/Application\ Support/Karabiner/private.xml
	sh ./karabiner.sh

hammerspoon: .hammerspoon
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

submodules:
	git submodule init
	git submodule update --recursive

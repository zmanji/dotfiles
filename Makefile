SHELL := /bin/bash

.PHONY: all
all: submodules shells osx git vim hammerspoon emacs hunspell bin

.PHONY: git
git: .gitconfig .gitignore_global
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: vim
vim: .vim .vimrc .gvimrc
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: emacs
emacs: .emacs.d
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: shells
shells: .editrc .gemrc .hushlogin .inputrc .zshenv .zshrc .zsh
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: osx
osx: .osx
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )
	@echo Running .osx; \
	sh ~/.osx

.PHONY: hammerspoon
hammerspoon: .hammerspoon
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: hunspell
hunspell: .hunspell_en_CA
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: bin
bin: git-squash

# Sub targets for bin
.PHONY: git-squash
git-squash: ~/bin/git-squash

~/bin/git-squash:
	mkdir -p ~/bin/
	wget -O ~/bin/git-squash https://github.com/zmanji/git-squash/releases/download/0.0.1/git-squash-0.0.1.pex
	chmod +x ~/bin/git-squash

submodules:
	git submodule init
	git submodule update --recursive

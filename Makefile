SHELL := /bin/bash

# TODO(zmanji): Replace this with an easier to understand shell script.

.PHONY: all

all: submodules shells osx git vim hammerspoon emacs emacs-bin hunspell bin karabiner iterm2 tmux idea

.PHONY: tmux
tmux: .tmux.conf
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: git
git: .gitconfig .gitignore_global
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: vim
vim: .vim .vimrc .gvimrc .surfingkeys.js
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

.PHONY: karabiner
karabiner: .config/karabiner
	$(shell mkdir -p ~/.config)
	@echo removing $^; \
	$(shell rm -rf ~/$^)
	@echo Installing $^; \
	$(shell ln -s $(CURDIR)/$^ ~/$^)

.PHONY: iterm2
iterm2: .config/iterm2
	$(shell mkdir -p ~/.config)
	@echo removing $^; \
	$(shell rm -rf ~/$^)
	@echo Installing $^; \
	$(shell ln -s $(CURDIR)/$^ ~/$^)

.PHONY: tridactyl
tridactyl: .config/tridactyl
	$(shell mkdir -p ~/.config)
	@echo removing $^; \
	$(shell rm -rf ~/$^)
	@echo Installing $^; \
	$(shell ln -s $(CURDIR)/$^ ~/$^)

.PHONY: hunspell
hunspell: .hunspell_en_CA
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: idea
idea: .ideavimrc
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )

.PHONY: bin
bin: git-squash emacs-bin git-bin

# Sub targets for bin
.PHONY: git-squash
git-squash: ~/bin/git-squash

.PHONY: git-bin
git-bin: bin/git-fb
	$(shell mkdir -p ~/bin)
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~/$(df); )

.PHONY: emacs-bin
emacs-bin: bin/README.emacs bin/ec bin/emacs bin/et
	$(shell mkdir -p ~/bin)
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~/$(df); )

.PHONY: ~/bin/git-squash
~/bin/git-squash:
	mkdir -p ~/bin/
	wget -O ~/bin/git-squash https://github.com/zmanji/git-squash-rs/releases/download/0.2.0/git-squash-rs.stable-x86_64-apple-darwin
	chmod +x ~/bin/git-squash

.PHONY: launchd
launchd: launchd/zmanji.emacs.plist
	mkdir -p ~/Library/LaunchAgents/
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/Library/LaunchAgents/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~/Library/LaunchAgents/$(notdir $(df)); )


submodules:
	git submodule init
	git submodule update --recursive

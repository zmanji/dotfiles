SHELL := /bin/bash

# TODO(zmanji): Replace this with an easier to understand shell script.

.PHONY: all

all: submodules shells osx git vim hammerspoon emacs emacs-bin hunspell bin karabiner iterm2 tmux idea tweak startpage ripgrep dicts

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

.PHONY: ripgrep
ripgrep: .ripgreprc
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

.PHONY: tweak
tweak: .tweak
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )
	@echo Installing Native Messenger
	.tweak/install_native.py

.PHONY: startpage
startpage: .config/startpage
	$(shell mkdir -p ~/.config)
	@echo removing $^; \
	$(shell rm -rf ~/$^)
	@echo Installing $^; \
	$(shell ln -s $(CURDIR)/$^ ~/$^)

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

.PHONY: kitty
kitty: .config/kitty
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

.PHONY: dicts
dicts: dicts/en_CA.aff dicts/en_CA.dic dicts/en_US.aff dicts/en_US.dic
	mkdir -p ~/Library/Spelling/
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/Library/Spelling/$$(basename $(df)));
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~/Library/Spelling/$(notdir $(df)); )

.PHONY: hunspell
hunspell: .hunspell_en_CA .hunspell_en_US
	@echo removing $^; \
	$(foreach df, $^, rm -f ~/$(df))
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~; )
	# Update vim spelling file from hunspell dicts
	/usr/local/bin/vim -u NONE -e -c "mkspell! ~/.vim/spell/en ~/Library/Spelling/en_CA ~/Library/Spelling/en_US" -c q
	# Update vim personal dict from hunspell dict
	/usr/local/bin/vim -U NONE -e -c "mkspell! ~/.vim/custom-dictionary.en.utf8.add" -c q

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
launchd: launchd/zmanji.emacs.plist launchd/zmanji.startpage.plist
	mkdir -p ~/Library/LaunchAgents/
	@echo removing $^; \
	$(foreach df, $^, rm -rf ~/Library/LaunchAgents/$$(basename $(df)));
	@echo Installing $^; \
	$(foreach df, $^, ln -s $(CURDIR)/$(df) ~/Library/LaunchAgents/$(notdir $(df)); )


submodules:
	git submodule init
	git submodule update --recursive

Zameer Manji's Configuration Files
==================================
The files here are my configuration files for vim, git and gem.

Installation
=============
Run: `make link` and it will create symbolic links to these files.

Features
=========
There are two large sets of configuration currently. One for git and one for vim.

The git configuration enables color for all command line arguments and sets aesthetically
pleasing defaults. The git configuration also enables useful aliases to save keystrokes for
common commands.

The vim configuration is more extensive. The goal of the configuration is to have a simple
set of defaults for a new vim user. I have avoided extensive customization and confusing
options. I have also documented every option in my vimrc. The vim configuration uses vundle
for plugin management to keep the .vim directory to an absolute minimum. Notable configuration
options are the aliasing of ';' to ':' in normal mode, visible whitespace characters, and the
use of the supertab vim plugin.

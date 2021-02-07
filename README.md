# Chong-Chong He's dotfiles

This repository contains cross-platform essential dotfiles for macOS and linux machines, including
- bashrc: a bash or zsh configuration script. Shared among all UNIX platforms
- emacs: my [Emacs dotfile](https://github.com/chongchonghe/emacs-dotfile). It's a standalone git repository. 
- vimrc: essential configuration file for vim
- tmux.conf: essential configuration for tmux
- vifmrc: essential configuration for vifm
- programs: some softwares that I wrote to boost up productivity
- bin: executables

## How to use these dotfiles in your machine?

I has the following script near the top of ~/.bashrc or ~/.zshrc on all my UNIX
machines (macOS, centOS, Redhat):

```shell
[[ -f ~/dotfiles/bashrc ]] && . ~/dotfiles/bashrc
```

I have the following script at the top of ~/.vimrc:

```vimrc
try
  source ~/dotfiles/vimrc
catch
  "~/dotfiles/vimrc not found. No problem; just ignore it."
endtry
```


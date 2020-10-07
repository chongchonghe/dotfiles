# system-essentials

Contains cross-platform essential configuration files, including
- bashrc: a minimum bash or zsh configuration script. Shared among all UNIX platforms
- vimrc: essential .vimrc shared on all platforms
- tmux.conf: essential .tmux.conf shared on all platforms
- .emacs: essential .emacs shared on platforms (TODO)

## How to use this shell script in all platforms

I has the following script on the end of .bashrc or .zshrc on all UNIX
systems (macOS, centOS, Redhat) to source bashrc.share:

```shell
if [ -f ~/system-essentials/bashrc.share ]; then 
	. ~/system-essentials/bashrc.share;
fi
```

and I have the following script on the end of .vimrc:

```vimrc
try
  source ~/system-essentials/vimrc.share
catch
  " No such file? No problem; just ignore it.
endtry
```

# system-essentials

Contains cross-platform essential configuration files, including
- sharerc: shared part in .bashrc and .zshrc
- .vimrc
- .emacs

## How to use this shell script in all platforms

I has the following script on the bottom of .bashrc or .zshrc on all UNIX
systems (macOS, centOS, Redhat) which sources sharerc:
  
  ```shell
  if [ -f ~/system-essentials/sharerc ]; then 
      . ~/system-essentials/sharerc;
  fi
  ```

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

#-------------------------- oh-my-zsh init -------------------------------------
export ZSH="/home/chongchong/.oh-my-zsh"
# ZSH_THEME="pygmalion"
# ZSH_THEME="pygmalion"
# ZSH_THEME="ys"
ZSH_THEME="pygmalion-virtualenv"
DISABLE_AUTO_TITLE="true"
plugins=(
  git
  virtualenv
  )
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
source $ZSH/oh-my-zsh.sh

if [ -f ~/dotfiles/bashrc ]; then
  . ~/dotfiles/bashrc;
fi

alias cdh="cd /startrek/chongchong"
alias cdh1='cd /startrek1/chongchong'
alias jabref="java -jar /startrek/chongchong/bin/JabRef-3.8.2.jar"
alias sublime="/startrek/chongchong/codes/sublime_text_3/sublime_text"
alias typora="/startrek/chongchong/usr/local/Typora-linux-x64/Typora --no-sandbox"
alias globus="${HOME}/usr/local/globusconnectpersonal-3.0.2/globusconnect"
alias jl="jupyter lab --ip=0.0.0.0 --port=8080"
alias jln="jupyter lab --ip=0.0.0.0 --no-browser --port=8080 --NotebookApp.token='' --NotebookApp.password=''"
alias jln1="jupyter lab --ip=0.0.0.1 --no-browser --port=8081 --NotebookApp.token='' --NotebookApp.password=''"
alias jln2="jupyter lab --ip=0.0.0.2 --no-browser --port=8082 --NotebookApp.token='' --NotebookApp.password=''"
alias mc="~/usr/local/mc/bin/mc"
alias cdsam="cd /startrek2nb/chongchong/Sam"
alias ca="conda activate"
alias cmtnow="git commit -m 'on st, at `date +"%Y-%m-%d %T"`'"

export home1="/startrek/chongchong"
export home2="/startrek2nb/chongchong"

export PATH="${PATH}:${HOME}/usr/local/bin"

# git. Disabled due to the error "fatal: Unable to find remote helper for 'https'"
#export PATH="${HOME}/usr/local/git/bin:${PATH}"

export PATH="${PATH}:${HOME}/usr/local/mc/bin"

# Enable Anaconda3
# export PATH="/startrek/chongchong/anaconda3/bin:$PATH"  # commented out by conda initialize

export EDITOR="vim"

#export PYTHONPATH=${HOME}/usr/local/pymses_4.0.0:$PYTHONPATH
#export PATH=$PATH:${HOME}/usr/local/pymses_4.0.0/bin

export PYTHONPATH=/startrek/chongchong/pymses/pymses/pymses_4.0.0:$PYTHONPATH
export PATH=$PATH:/startrek/chongchong/pymses/pymses/pymses_4.0.0/bin

#export PATH=${HOME}/usr/local/vim/bin:$PATH

# nnn config (plus that from system-essentials/bashrc
export NNN_BMS='u:~/;s:/startrek/chongchong/;S:/startrek2nb/chongchong/;r:/startrek2nb/chongchong/Sam/coding/'
export VISUAL="vim"

# dd: disk speed test
dd-late ()
{
  dd if=/dev/zero of=$1 bs=512 count=1000 oflag=direct
}
dd-speed ()
{
  dd if=/dev/zero of=$1 bs=1G count=1 oflag=direct
}

# keep in the end of this file
export PYENV_ROOT="/startrek/chongchong/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
## pyenv init
#if command -v pyenv 1>/dev/null 2>&1; then
#  eval "$(pyenv init -)"
#fi

export LD_LIBRARY_PATH=${HOME}/usr/local/libevent/lib/:${LD_LIBRARY_PATH}

alias timer="while true; do echo -ne '`date`\r'; done"

stty -ixon

export PATH="$PATH:$HOME/usr/local/vifm/bin"

[[ -s /home/chongchong/.autojump/etc/profile.d/autojump.sh ]] && source /home/chongchong/.autojump/etc/profile.d/autojump.sh

export PATH="$PATH:$HOME/system-essentials/bin"
alias icat="kitty +kitten icat"

export WO_ROOT="${HOME}/tools/programs/workon"
source ~/tools/programs/workon/workon.rc

export PATH="$PATH:$HOME/tools/bin"

# emacs
export PATH="/startrek/chongchong/local/emacs/bin:$PATH"

alias dif="kitty +kitten diff"
alias e="emacs"

export EDITOR=~/usr/local/bin/ec

# julia
export PATH="/startrek/chongchong/softwares/julia-1.7.3/bin:$PATH"

echo ".zshrc sourced"

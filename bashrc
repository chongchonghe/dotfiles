#; -*-Shell-script-*-
#------------------------------------------------------------------
# This is a set of bash/zsh init script that I share among all
# my UNIX machiens
#------------------------------------------------------------------

function patha () {
  export PATH=$PATH:$1
}
function pathp () {
  export PATH=$1:$PATH
}

#--------------------------------- Alias ---------------------------------------
alias ls="ls -GFh --color=auto"
alias lt="ls -GFhltr --color=auto"
alias ll="ls -GFhl --color=auto"
alias le="less -R"
alias ipythonpy="ipython --pylab"
# alias lt="ls -ltrh --color=auto" # linux
alias cctest="echo Yes sharerc is sourced"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias du="du -sh"
alias l1="ls -t1 | head -n 1"
alias l2="ls -t1 | head -n 2 | tail -n 1"
alias l3="ls -t1 | head -n 3 | tail -n 1"
# export lr="`ls -Art | tail -n 1`"
# alias grep="grep -d skip"
# alias grep="grep -r"
# alias vi="vim"
alias sizesort="du -s * | sort -n"
# alias tree="tree -N"
alias en="emacsclient -n"
alias debugf="gfortran -g -ffpe-trap=zero,invalid,overflow,underflow"
#alias icat="kitty +kitten icat"
alias ca="conda activate"
#alias t="kitty @ set-tab-title"
alias enw="emacsclient -nw"	# open a new window on existing server

### Commands alias
alias sshupdate="rsync -raz --progress"
alias sizesort="du -s * | sort -n"
alias tree="tree -N"
alias myrsync="rsync -larvh"
alias myrsyncL="rsync -Larvh"
alias myrsyncLu="rsync -Larvh --update"
alias myrsyncbar="rsync -larh --info=progress2"
alias vimode="set -o vi"
alias emacsmode="set -o emacs"
alias pdb="python -m pdb"
alias jn="jupyter notebook"
alias jl="jupyter lab"
alias jpcv="jupyter nbconvert"
# alias mpirun='TMPDIR=/var/tmp/mympi mpirun'
# alias sshupdate="rsync -raz --progress"
#alias kittydiff="kitty +kitten diff"
#alias kittygitdiff="git difftool --no-symlinks --dir-diff"
alias my-rsync="rsync -lhrtu"
alias my-rsync-bar="rsync -lhrtu --info=progress2"
alias my-rsyncL="rsync -Lhrtu"
alias my-rsyncL-bar="rsync -Lhrtu --info=progress2"
alias ch="cheat"

### set vi mode
set -o emacs

#---------------------  functions -------------------------------

### makecd
mkcd() {
  mkdir -p -- "$1" && cd -P -- "$1"
}

### git
function gitall() {
    git add .
    git commit -a -m "$1"
    git push origin $(current_branch)
}
function gitallnow() {
    git add .
    cmtnow
    git push origin $(current_branch)
}
alias add="git add"
alias pull="ggpull"
alias push="ggpush"
alias cmt="git commit"
alias gitadd="git commit --amend --no-edit"

function rc() {
  therc="$HOME/rc"
  if [[ $1 == list ]]; then
      if [[ $# -ge 2 ]]; then
	  file=$therc/${2}.list
	  if [[ -e $file ]]; then
	      zsh $file
	  fi
      else
	  (cd $therc ; ls *.rc)
      fi
  else
      source $therc/${1}.rc
  fi
}

### github
function gitinit() {
  git init
  git add .
  git commit -m "first commit"
  git remote add origin $1
  git push origin $(current_branch)
}

function cht() {
  curl "cht.sh/$1"
}

#---------------------------------  nnn ----------------------------------------
export NNN_USE_EDITOR=1                                 # use the $EDITOR when opening text files
# export NNN_SSHFS_OPTS="sshfs -o follow_symlinks"      # make sshfs follow symlinks on the remote
export NNN_COLORS="2567"                                # use a different color for each context
export NNN_FIFO=~/tmp/nnn.fifo
export NNN_PLUG='p:preview-tui'
n ()
{
    # Block nesting of nnn in subshells
    [ "${NNNLVL:-0}" -eq 0 ] || {
        echo "nnn is already running"
        return
    }

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
    # see. To cd on quit only on ^G, remove the "export" and make sure not to
    # use a custom path, i.e. set NNN_TMPFILE *exactly* as follows:
    #      NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    # The command builtin allows one to alias nnn to n, if desired, without
    # making an infinitely recursive alias
    command nnn "$@"

    [ ! -f "$NNN_TMPFILE" ] || {
        . "$NNN_TMPFILE"
        rm -f "$NNN_TMPFILE" > /dev/null
    }
}
nnn_cd()
{
    if ! [ -z "$NNN_PIPE" ]; then
        printf "%s\0" "0c${PWD}" > "${NNN_PIPE}" !&
    fi
}
trap nnn_cd EXIT

#-------------------------------   vifm   -------------------------------------
v () {
    # Block nesting of vifm in subshells
    # if [ -n "$VIFM" ] && [ -n "${VIFM:-0}" ]; then
    if [ -n "$VIFM" ]; then
	      echo "vifm is already running. Press C-d to quit current shell "
	      return
    fi
    local dst="$(command vifm --choose-dir - "$@")"
    if [ -z "$dst" ]
    then
        echo 'Directory picking cancelled/failed'
        return 1
    fi
    cd "$dst"
}

#-------------------------------   Other   -------------------------------------
export TERM=xterm-256color
export EDITOR=vim

# case `uname` in
#     Darwin)
# 	# commands for macOS go here
# 	;;
#     Linux)
# 	# commands for Linux go here
# 	alias ipython="ipython --pylab"
# 	;;
#     FreeBSD)
# 	# commands for FreeBSD go here
# 	;;
# esac

#-------------------------------   End   -------------------------------------
# echo "bashrc.share is sourced"
# echo "$0 is sourced"

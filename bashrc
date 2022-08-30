#; -*-Shell-script-*-
#------------------------------------------------------------------
# This is a set of bash/zsh init script that I share among all
# my UNIX machiens
#------------------------------------------------------------------

#--------------------------------- Alias ---------------------------------------
alias ls="ls -GFh --color=auto"
alias lt="ls -GFhltr --color=auto"
alias ll="ls -GFhl --color=auto"
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
alias myrsync="rsync -Larvh --update"
alias en="emacsclient -n"
alias debugf="gfortran -g -ffpe-trap=zero,invalid,overflow,underflow"
alias icat="kitty +kitten icat"
alias ca="conda activate"
alias t="kitty @ set-tab-title"
alias enw="emacsclient -nw"	# open a new window on existing server

### Commands alias
alias sshupdate="rsync -raz --progress"
alias sizesort="du -s * | sort -n"
alias tree="tree -N"
alias rsyncu="rsync -Larvh --update"
alias vimode="set -o vi"
alias emacsmode="set -o emacs"
alias pdb="python -m pdb"
alias jn="jupyter notebook"
alias jl="jupyter lab"
alias jpcv="jupyter nbconvert"
# alias mpirun='TMPDIR=/var/tmp/mympi mpirun'
# alias sshupdate="rsync -raz --progress"
alias kittydiff="kitty +kitten diff"
alias kittygitdiff="git difftool --no-symlinks --dir-diff"

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
alias add="git add"
alias pull="ggpull"
alias push="ggpush"
alias cmt="git commit"

function rc() {
  if [[ $1 == list ]]; then
      if [[ $# -ge 2 ]]; then
	  file=~/rc/${2}.list
	  if [[ -e $file ]]; then
	      zsh $file
	  fi
      else
	  (cd ~/rc ; ls *.rc)
      fi
  else
      source ~/rc/${1}.rc
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

#---------------------------------  nnn ----------------------------------------
export NNN_USE_EDITOR=1                                 # use the $EDITOR when opening text files
# export NNN_SSHFS_OPTS="sshfs -o follow_symlinks"      # make sshfs follow symlinks on the remote
export NNN_COLORS="2567"                                # use a different color for each context
export NNN_FIFO=~/tmp/nnn.fifo
export NNN_PLUG='p:preview-tui'
n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn -x "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

#-------------------------------   vifm   -------------------------------------
v () {
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
echo "$0 is sourced"

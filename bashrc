#; -*-Shell-script-*-
#------------------------------------------------------------------
# This is a set of bash/zsh init script that I share among all
# my UNIX machiens
#------------------------------------------------------------------

case `uname` in
    Darwin)
	# commands for macOS go here
  alias ls="ls -GFh"
  alias ll="ls -GFhl"
  alias lt="ls -GFhltr"
	alias ipython="ipython --pylab osx"
	;;
    Linux)
	# commands for Linux go here
  alias ls="ls --color=auto"
  alias ll="ls -lah --color=auto"
  alias lt="ls -ltrh --color=auto"
	alias ipython="ipython --pylab"
	;;
    FreeBSD)
	# commands for FreeBSD go here
	;;
esac

### Basics
alias cctest="echo Yes sharerc is sourced"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias du="du -sh"
alias l1="ls -t1 | head -n 1"
alias l2="ls -t1 | head -n 2 | tail -n 1"
alias l3="ls -t1 | head -n 3 | tail -n 1"
# alias grep="grep -d skip"
# alias grep="grep -r"

### Commands
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

### set vi mode
set -o vi

function gitall() {
    git add .
    git commit -a -m "$1"
    git push origin $(current_branch)
}

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

#-------------------------------   End   -------------------------------------
# echo "bashrc.share is sourced"
echo "$0 is sourced"

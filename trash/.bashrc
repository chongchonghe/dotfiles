echo ".bashrc is sourced"
# Sourced on Mac, will source system-essentials/sharerc

set -o vi

### alias
alias debugf="gfortran -g -ffpe-trap=zero,invalid,overflow,underflow"
# alias vi="vim"
# alias mymac="ifconfig en0 | grep ether"
# alias changemac="sudo ifconfig en0 ether"
# alias scp="scp -p"
# export lr="`ls -Art | tail -n 1`"
# alias ol="open `ls -t | head -n1`"
alias vlc="/Applications/VLC.app/Contents/MacOS/VLC"
alias xld="/Applications/XLD.app/Contents/MacOS/XLD"
alias wolfram="wolframscript"
alias ulysses="/Applications/Ulysses.app/Contents/MacOS/Ulysses"
alias mpirun='TMPDIR=/var/tmp/mympi mpirun'
alias seq="/Applications/Sequential.app/Contents/MacOS/Sequential"
alias jl="jupyter-lab"
alias dp2="ssh dp2"

#alias gcc='gcc-8'
#alias g++="g++-8"
# alias bib="cp $HOME/Dropbox/{Bib_bibdesk.bib,Bib_extra.bib} ."

##### Other application executables #####

#export TERM="xterm-color"
export PATH=$PATH:~/local/bin
# export PATH=$PATH:/Applications/Inkscape.app/Contents/Resources/bin

# gvim installed via brew
export PATH=/usr/local/Cellar/macvim/8.0-144_3/bin:$PATH

# brew install ffmpeg
export PATH=/usr/local/Cellar/ffmpeg/3.4.2/bin:$PATH

# glnemo2
export PATH="/Applications/glnemo2.app/Contents/MacOS:$PATH"

# added by ChongChogn He, Feb 5 2017. To install cmake command line
# PATH="/Applications/CMake.app/Contents/bin":"$PATH"

# solution to a mpi4py issue
export TMPDIR=/tmp

# The following line was added to solve the error:
#   Error in sitecustomize; set PYTHONVERBOSE for traceback:
#   KeyError: 'PYTHONPATH'
# Following http://www.agapow.net/programming/python/verbose/ which refers to
# https://github.com/conda/conda/issues/448
export PYTHONNOUSERSITE="$HOME/.local"
export PATH="/usr/local/opt/openssl/bin:$PATH"

export SPS_HOME="$HOME/local/fsps/"

export PATH="$PATH:/System/Library/Extensions/TMSafetyNet.kext/Contents/Helpers"

export ADS_API_TOKEN="Tmek3GUtNMBMeCUyPV1Z8Brwvs9ltmdIwlDq8jJw"

export PATH="/usr/local/opt/qt/bin:$PATH"

export LDFLAGS="-L/usr/local/opt/qt/lib"
export CPPFLAGS="-I/usr/local/opt/qt/include"
export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig"

export PATH="$PATH:/Applications/Julia-1.0.app/Contents/Resources/julia/bin"

# ?
export PATH=/usr/local/include:$PATH
export PATH=/usr/local/lib:/usr/local/sbin:$PATH

# 2020-02-11: my own nbody tools
# export PATH="$HOME/local/nbody:$PATH"

# 2020-02-11: Installed NEMO
# Moved to ~/Academics/Projects/Dynamics/modules
#source $HOME/local/nemo/nemo_start.sh

############### Load .sharerc ####################

if [ -f ~/system-essentials/bashrc.share ]; then
    . ~/system-essentials/bashrc.share;
fi

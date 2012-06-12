umask 022

if [ -f /etc/profile ]; then
    source /etc/profile
fi

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

if [ -f $HOME/.dir_colors ]; then
   eval `/opt/local/bin/gdircolors $HOME/.dir_colors`
fi

# functions I use in all POSIX platforms

function reload
{
  source /Users/yielding/.bash_profile
}

function ansi 
{
  export TERM=ansi
}

function vt100 
{
  export TERM=vt100
}

# functions I use in OSX
function ql()
{
  (qlmanage -p "$@" > /dev/null 2>&1 &
   local qlmanage_pid=$!
   read -sn 1
   kill ${qlmanage_pid}) > /dev/null 2>&1
}

pman()
{
  man -t "${1}" | open -f -a /Applications/Preview.app
}

# the rest of this file is commented out.

SVN_EDITOR=vim
#TERM=xterm
LANG="ko_KR.UTF-8"
USERNAME="yielding"
INPUTRC=$HOME/.inputrc

PATH=~/bin:~/opensource/mruby/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/usr/sbin:/sbin:$PATH

C_INCLUDE_PATH=.:/opt/local/include
CPLUS_INCLUDE_PATH=.:/opt/local/include:~/opensource/mruby/include
LIBRARY_PATH=$LIBRARY_PATH:/opt/local/lib

RUBYOPT=rubygems

PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u\[\e[37m\]@\[\e[36m\]\h \[\e[31m\]\w\[\e[0m\]\n\$ '

stty -istrip
set -o vi

export DYLD_FALLBACK_LIBRARY_PATH=/opt/local/lib:$DYLD_FALLBACK_LIBRARY_PATH
export HISTIGNORE="&:ls:[bf]g:exit"
export P2_DEP=~/project/solbrain/app/panther2/develop
export P2_DEV=~/project/solbrain/app/panther2
export DEV=~/develop

export MANPATH=/opt/local/share/man:$MANPATH

export PS1 USERNAME PATH LANG INPUTRC SVN_EDITOR C_INCLUDE_PATH CPLUS_INCLUDE_PATH LIBRARY_PATH 

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

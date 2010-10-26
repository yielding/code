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
TERM=xterm
LANG="ko_KR.UTF-8"
USERNAME="yielding"
OS=mac
INPUTRC=$HOME/.inputrc

PATH=$HOME/.rvm/bin
PATH=$PATH:$HOME/.rvm/gems/ruby-1.9.2-p0/bin
PATH=$PATH:$HOME/.rvm/gems/ruby-1.9.2-p0@global/bin
PATH=$PATH:$HOME/.rvm/rubies/ruby-1.9.2-p0/bin
PATH=$PATH:~/bin:/opt/local/bin:/usr/local/bin:/bin:/usr/bin:/usr/sbin:/sbin

CPLUS_INCLUDE_PATH=.:/opt/local/include
C_INCLUDE_PATH=.:/opt/local/include

C_INCLUDE_PATH=.:/opt/local/include
CPLUS_INCLUDE_PATH=.:/opt/local/include
LIBRARY_PATH=$LIBRARY_PATH:/opt/local/lib

RUBYOPT=rubygems

PS1='\[\e]0;\w\a\]\n\[\e[32m\]\u\[\e[37m\]@\[\e[36m\]\h \[\e[31m\]\w\[\e[0m\]\n\$ '

stty -istrip
set -o vi

export DYLD_FALLBACK_LIBRARY_PATH=/opt/local/lib:$DYLD_FALLBACK_LIBRARY_PATH
export HISTIGNORE="&:ls:[bf]g:exit"
export P2_DEP=~/develop/app/panther2/develop
export P2_DEV=~/develop/app/panther2
export MANPATH=/opt/local/share/man:$MANPATH

export PS1 USERNAME PATH LANG INPUTRC SVN_EDITOR C_INCLUDE_PATH CPLUS_INCLUDE_PATH LIBRARY_PATH 

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

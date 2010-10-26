#If not running interactively, don't do anything
#[ -z "$PS1" ] && return

shopt -s checkwinsize

bind '"\e[A"':history-search-backward
bind '"\e[B"':history-search-forward

if [ -f /etc/bashrc ]; then
   source /etc/bashrc
fi

if [ -f ~/.bash_aliases ]; then
  source ~/.bash_aliases
fi

if [ -f /etc/bash_completion ]; then
  source /etc/bash_completion
fi

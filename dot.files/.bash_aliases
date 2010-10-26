# some useful aliases
# vim:filetype=sh

alias vi='/opt/local/bin/vim'
alias aliases='vim ~/.bash_aliases && source ~/.bash_aliases'
alias profiles='vim ~/.bash_profile && source ~/.bash_profile'

# function which adds an alias to the current shell and to
# the ~/.bash_aliases file.
add-alias ()
{
  local name=$1 value="$2"
  echo "alias $name='$value'" >> ~/.bash_aliases
  eval "alias $name='$value'"
  alias $name
}


#######
# git #
#######
alias gl='git pull'
alias gp='git push'
alias gd='git diff'
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gb='git branch -v'

function gco {
  if [ -z "$1" ]; then
    git checkout master
  else
    git checkout $1
  fi
}

function st {
  if [ -d ".svn" ]; then
    svn status
  else
    git status
  fi
}

alias rm_out='find . -name *.out -print0 | xargs -0 rm -f'

#######
# SVN #
#######
alias sup='svn up' # trust me 3 chars makes a different
# alias sstu='svn st -u' # remote repository changes
# alias scom='svn commit' # commit
alias svnclear='find . -name .svn -print0 | xargs -0 rm -rf' # removes all .svn folders from
alias svnaddall='svn status | grep "^\?" | awk "{print \$2}" | xargs svn add' # adds all una
alias svnci='svn ci -m ""'


################################################################################
#
# Ruby
#
################################################################################
alias rv='ruby -v'

# use readline, completion and require rubygems by default for irb
# alias irb='irb --simple-prompt -r irb/completion -rubygems'

# really awesome function, use: cdgem <gem name>, cd's into your gems directory
# and opens gem that best matches the gem name provided

function cdgem 
{
  cd /opt/local/lib/ruby/gems/1.8/gems/
  cd `ls | grep $1 | sort | tail -1`
}

function gemdoc 
{
  firefox /opt/local/lib/ruby/gems/1.8/doc/`ls /opt/local/lib/ruby/gems/1.8/doc | grep $1 | sort | tail -1`/rdoc/index.html
}

alias qri='qri -w 106'
alias fri='fri -w 106'


#########
# RAILS #
#########

function railsapp 
{
  template=$1
  appname=$2
  shift 2
  rails $appname -m http://github.com/ryanb/rails-templates/raw/master/$template.rb $@
}

alias ss='script/server' # start up the beast
alias sr='kill -USR2 `cat tmp/pids/mongrel.pid`' # restart detached Mongrel
alias sst='kill `cat tmp/pids/mongrel.pid`' # restart detached Mongrel
alias sc='script/console'

# don't put duplicate lines in the history. See bash(1) for more options
# export HISTCONTROL=ignoredups
# export LANG=ko_KR.eucKR
# export LANG="ko_KR.UTF-8"

# enable color support of ls and also add handy aliases
export LS_OPTIONS='--color=auto'
alias ls='gls -v -FG $LS_OPTIONS'
alias dir='ls $LS_OPTIONS --format=vertical'
alias vi=vim

# some more ls aliases
alias ll='ls -l'
alias p='cd ..'
alias pp='p;p'
alias ppp='p;p;p'
alias pppp='p;p;p;p'
alias c='clear'
alias du='du -sh'
alias la='ls -A'
alias l='ls -F'
alias dir='ls -l'
alias cls=clear
alias www='cd /opt/local/apache2'
alias mda='cd /Users/yielding/opensource/mobiledeviceaccess'
alias research='cd /Users/yielding/Documents/research'
alias snmp='cd /opt/local/share/snmp'
alias p2='cd /Users/yielding/develop/app/panther2'
alias o2='cd /Users/yielding/develop/app/openeye2'
alias f2='cd /Users/yielding/develop/test'
alias embed='cd /Users/yielding/code/ruby/embed/rice/thread'
alias tmp='cd ~/tmp'
alias down='cd /Users/yielding/Downloads'
alias gowork='cd /Users/yielding/develop/app/iphone'


alias r='fc -e -'
alias h='history'
alias less='less -rf'
alias lynx='lynx -use_mouse'
alias duh='du -h --max-depth=10 ./'

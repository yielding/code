export HOME=$HOME

export RUBY_HOME=$HOME/.rubies/ruby-4.0.1
export DOTNET_ROOT=/usr/local/share/dotnet
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export RUBYLIB=$HOME/develop.ruby/lib

export PATH=$RUBY_HOME/bin:$HOME/.rubies/ruby-4.0.1/lib/ruby/gems/4.0.0/bin:$PATH
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH=$DOTNET_TOOLS_HOME:$PATH

export VIDEO_ENGINE="${HOME}/no.cloud/video"

export LDFLAGS="-L$RUBY_HOME/lib"

export PROJECT=$HOME/project/md.platform.infra

export NODE_OPTIONS="--no-experimental-webstorage"

#export TERM=xterm-256color
export TERMINFO=~/.terminfo
export COLORTERM="truecolor"
export NVIM_TUI_ENABLE_TRUE_COLOR=1

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

export GIT_TERMINAL_PROMPT=false
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="steeef"
ZSH_THEME="fino"

export MY_INSTALL_DIR=$HOME/.local
export PATH="$MY_INSTALL_DIR/bin:$PATH"

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'

export FZF_DEFAULT_OPTS='
  --height 80%
  --layout=reverse
  --border
  --info=inline
'

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

HIST_STAMPS="yyyy-mm-dd"

# Which plugins would you like to load?
plugins=(git
  history
  ruby
  python
  vi-mode
  docker
  fasd
  fzf
  git
  zsh-syntax-highlighting
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

bindkey -v

# User configuration

export EDITOR=nvim
export VISUAL=nvim

export NNN_OPTS="x"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_USE_EDITOR=1
export NNN_OPENER="${HOME}/.config/nnn/plugins/nuke"
export NNN_SEL="${HOME}/.config/nnn/.selection"
export NNN_PLUG='s:nvim-vsplit;l:lsdpreview;p:preview-tui'

mn() {
  session_name="${2:-'nnn'}";
  # if the session already exists, kill it
  tmux kill-session -t "$session_name" 2>/dev/null;
  tmux new-session -d -s "$session_name" "source ~/.zshrc && nnn -a -c -d -e -U $@";
  tmux attach-session -t "$session_name";
}

tx() {
  [ -z "$TMUX" ] && tmux new -As "${1:-blue}"
}

vf() {
  local file=$(fzf)
  [ -n "$file" ] && nvim "$file"
}

vd() {
  local dir=$(fd --type d | fzf)
  [ -n "$dir" ] && nvim "$dir"
}

export LANG=ko_KR.UTF-8

zstyle ':completion:*' rehash true

compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
eval "$(zoxide init zsh)"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

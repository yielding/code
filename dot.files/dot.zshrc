export HOME=/home/yielding 
export VIDEO_ENGINE=/data1/yielding/video

export RUBY_HOME=/home/yielding/.rubies/ruby-4.0.0
export OPENCV_HOME=/usr/local
export DOTNET_ROOT=/usr/local/share/dotnet
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export RUBYLIB=$HOME/develop/lib.ruby
export ORT_ROOT=$HOME/opensource/onnxruntime-linux-x64-1.17.1

export PATH=$RUBY_HOME/bin:$PATH
export PATH=$HOME/bin:/usr/local/bin:/usr/local/go/bin:$HOME/.cargo/bin:$PATH
export PATH=$HOME/.cargo/bin:$DOTNET_TOOLS_HOME:$PATH

#export CPLUS_INCLUDE_PATH=/usr/include/c++/v1:/usr/include:$OPENCV_HOME/include/opencv4:$CPLUS_INCLUDE_PATH

export CC=gcc
export CXX=g++
export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LIBRARY_PATH
export LD_LIBRARY_PATH=/usr/local/gcc-15.2/lib64:${LD_LIBRARY_PATH:-}
export LD_LIBRARY_PATH=$ORT_ROOT/lib:$LD_LIBRARY_PATH

export PROJECT=$HOME/project/md.platform.infra

export NODE_OPTIONS="--no-experimental-webstorage"

#export TERM=xterm-256color
export TERMINAL=tmux-256color
export TERMINFO=~/.terminfo
export COLORTERM="truecolor"
export NVIM_TUI_ENABLE_TRUE_COLOR=1

export HOMEBREW_NO_ENV_HINTS=1

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

export GIT_TERMINAL_PROMPT=false
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="miloshadzic"
#ZSH_THEME="refined"
#ZSH_THEME="dst"
ZSH_THEME="steeef"
ZSH_THEME="fino"

export MY_INSTALL_DIR=$HOME/.local
export PATH="$MY_INSTALL_DIR/bin:$PATH"

# Prepend lazy/fzf to PATH so the newer version is found before system fzf
if [[ ! "$PATH" == */home/yielding/.local/share/nvim/lazy/fzf/bin* ]]; then
  PATH="/home/yielding/.local/share/nvim/lazy/fzf/bin${PATH:+:${PATH}}"
fi

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND='fd --type d --hidden --follow --exclude .git'

export FZF_DEFAULT_OPTS='
  --height 80%
  --layout=reverse
  --border
  --info=inline
'

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.  DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="false"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git
  history
  macos
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

export LANG=ko_KR.UTF-8
export MANPATH="/usr/local/man:$MANPATH"

export EDITOR=nvim 
export VISUAL=nvim 

export NNN_OPTS="x"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_USE_EDITOR=1
export NNN_OPENER="${HOME}/.config/nnn/plugins/nvim-open"
export NNN_SEL="${HOME}/.config/nnn/.selection"
export NNN_PLUG='p:preview-tui;f:finder;s:nvim-vsplit;l:lsdpreview'

mn() {
  session_name="${2:-'nnn'}";
  # if the session already exists, kill it
  tmux kill-session -t "$session_name" 2>/dev/null;
  tmux new-session -d -s "$session_name" "source ~/.zshrc && nnn -a -c -d -e -U $@";
  tmux attach-session -t "$session_name";
}

zstyle ':completion:*' rehash true

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
eval "$(zoxide init zsh)"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f "/home/yielding/.ghcup/env" ] && . "/home/yielding/.ghcup/env" # ghcup-env

[ -z "$TMUX" ] && tmux new -As main

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#export SDKMAN_DIR="$HOME/.sdkman"
#[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

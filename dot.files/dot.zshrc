export HOME=/Users/yielding 

export RUBY_HOME=/opt/homebrew/opt/ruby 
export LLVM_HOME=/opt/homebrew/opt/llvm
export MINGW_HOME=/opt/homebrew/opt/mingw-w64
export OPENCV_HOME=/opt/homebrew/opt/opencv
export DOTNET_ROOT=/usr/local/share/dotnet
export DOTNET_TOOLS_HOME=$HOME/.dotnet/tools
export RUBYLIB=$HOME/develop/lib.ruby

export SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk

export PATH=$RUBY_HOME/bin:/opt/homebrew/lib/ruby/gems/3.4.0/bin:$PATH
export PATH=$HOME/bin:/opt/homebrew/bin:$PATH
export PATH=$LLVM_HOME/bin:$PATH
export PATH=$MINGW_HOME/bin:$PATH
export PATH=$DOTNET_TOOLS_HOME:$PATH

export LDFLAGS="-L$RUBY_HOME/lib -L/opt/homebrew/lib -lpthread"
export CPLUS_INCLUDE_PATH=/opt/homebrew/opt/llvm/include:/opt/homebrew/include:$OPENCV_HOME/include/opencv4:$CPLUS_INCLUDE_PATH
#export CPLUS_INCLUDE_PATH=/opt/homebrew/include:$OPENCV_HOME/include/opencv4:$CPLUS_INCLUDE_PATH

#export CXX="/opt/homebrew/bin/g++-14"
#export CC=/opt/homebrew/opt/llvm/bin/clang
export CXX=/opt/homebrew/opt/llvm/bin/clang++
export PROJECT=$HOME/project/md.platform.infra

#export TERM=xterm-256color
export TERMINFO=~/.terminfo
export COLORTERM="truecolor"
export NVIM_TUI_ENABLE_TRUE_COLOR=1

export HOMEBREW_NO_ENV_HINTS=1

export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-17.0.2.jdk/Contents/Home
export JDK_HOME=$JAVA_HOME
export CLASSPATH=.:/opt/homebrew/Cellar/antlr/4.13.2/antlr-4.13.2-complete.jar

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

#. /opt/homebrew/etc/profile.d/z.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
export EDITOR=nvim 
export VISUAL=nvim 

export NNN_OPTS="x"
export NNN_USE_EDITOR=1
export NNN_OPENER="${HOME}/.config/nnn/plugins/nvim-open"
export NNN_SEL="${HOME}/.config/nnn/.selection"
export NNN_PLUG='s:nvim-vsplit;c:cbcopy-mac;v:cbpaste-mac'

mn() {
  session_name="${2:-'nnn'}";
  # if the session already exists, kill it
  tmux kill-session -t "$session_name" 2>/dev/null;
  tmux new-session -d -s "$session_name" "source ~/.zshrc && nnn -a -c -d -e -U $@";
  tmux attach-session -t "$session_name";
}

source ~/scripts/mn.zsh

export LANG=ko_KR.UTF-8

zstyle ':completion:*' rehash true

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/Users/yielding/miniforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/Users/yielding/miniforge/etc/profile.d/conda.sh" ]; then
#         . "/Users/yielding/miniforge/etc/profile.d/conda.sh"
#     else
#         export PATH="/Users/yielding/miniforge/bin:$PATH"
#     fi
# fi
# unset __conda_setup
## <<< conda initialize <<<

compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
eval "$(zoxide init zsh)"

test -e /Users/yielding/.iterm2_shell_integration.zsh && source /Users/yielding/.iterm2_shell_integration.zsh || true

[ -f "/Users/yielding/.ghcup/env" ] && source "/Users/yielding/.ghcup/env" # ghcup-env
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#fastfetch
# If you come from bash you might have to change your $PATH.
export HOME=/Users/yielding 
export RUBY_HOME=/opt/homebrew/opt/ruby
export PATH=$RUBY_HOME/bin:/opt/homebrew/lib/ruby/gems/3.2.0/bin:$PATH
export PATH=$HOME/bin:/opt/homebrew/bin:$PATH
export PATH=/opt/homebrew/Cellar/llvm/14.0.6_1/bin:$PATH
export PATH=/opt/homebrew/Cellar/mingw-w64/10.0.0_4/bin/path/to/mingw/bin:$PATH
export PATH=$PATH:$HOME/.dotnet/tools
export BOOST_HOME=/usr/local/include
export OPENCV_HOME=/opt/homebrew/Cellar/opencv/4.7.0_1
export LDFLAGS="-L$RUBY_HOME/lib -L/opt/homebrew/lib -lpthread"
export CPLUS_INCLUDE_PATH=/opt/homebrew/include:$RUBY_HOME/include:$BOOST_HOME:$OPENCV_HOME/include/opencv4:$HOME/develop/include:$HOME/develop/vendor/include:$CPLUS_INCLUDE_PATH
export CPPFLAGS=" -std=c++23"
export CXX="/opt/homebrew/bin/g++-13"

export TERM=xterm-256color
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-17.0.2.jdk/Contents/Home
export JDK_HOME=$JAVA_HOME

# Path to your oh-my-zsh installation.
export ZSH="/Users/yielding/.oh-my-zsh"

export GIT_TERMINAL_PROMPT=false
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="miloshadzic"
#ZSH_THEME="refined"
#ZSH_THEME="dst"
ZSH_THEME="steeef"

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
# ENABLE_CORRECTION="true"

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
# HIST_STAMPS="mm/dd/yyyy"

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
  ripgrep
  zsh-syntax-highlighting
  zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

bindkey -v

. /opt/homebrew/etc/profile.d/z.sh


# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8
export LANG=ko_KR.UTF-8

export EDITOR=nvim 
# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/yielding/miniforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/yielding/miniforge/etc/profile.d/conda.sh" ]; then
        . "/Users/yielding/miniforge/etc/profile.d/conda.sh"
    else
        export PATH="/Users/yielding/miniforge/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION

test -e /Users/yielding/.iterm2_shell_integration.zsh && source /Users/yielding/.iterm2_shell_integration.zsh || true

[ -f "/Users/yielding/.ghcup/env" ] && source "/Users/yielding/.ghcup/env" # ghcup-env
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

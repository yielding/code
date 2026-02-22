alias v='nvim'
alias vi='nvim'
alias ngit='nvim -c "Neogit" -c "autocmd WinClosed * if winnr('$') == 1 | qa | endif"'

# alias nnn='nnn -acde'
alias n='nnn -acde'
alias l='ls -al'
alias ll='ls -al'
alias dir='ls -al'
alias sqlite3='sqlite3'

#alias ruby='ruby --jit'
alias ruby='$HOME/.rubies/ruby-4.0.1/bin/ruby --jit'

# dir long
alias cls='clear'
alias c='clear'
alias p='cd ..'
alias pp='cd ../..'
alias sz='source ~/.zshrc'

# git
alias gp='git pull'
alias gpr='git pull --recurse-submodules'

alias xxd='xxd -g 1'

# shortcuts
alias gonnn='cd ~/.config/nnn'
alias gonvim='cd ~/.config/nvim/lua'
alias gowork='cd ~/project/md.platform.infra'

alias aliases='vi ~/.oh-my-zsh/lib/aliases.zsh && source ~/.oh-my-zsh/lib/aliases.zsh'

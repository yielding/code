if [[ "$OSTYPE" == darwin* ]]; then
  alias v='/opt/homebrew/bin/nvim'
else
  alias v='/snap/bin/nvim'
fi

alias ngit='nvim -c "Neogit" -c "autocmd WinClosed * if winnr('$') == 1 | qa | endif"'
alias oo='cd ~/obsidian/notes'

# alias nnn='nnn -acde'
alias n='nnn -acde'
alias l='ls -al'
alias ll='ls -al'
alias dir='ls -al'
alias lst='tmux ls'

alias ruby="$HOME/.rubies/ruby-4.0.1/bin/ruby --yjit"

# dir long
alias cls='clear'
alias c='clear'
alias p='cd ..'
alias pp='cd ../..'
alias ppp='cd ../../..'
alias sz='source ~/.zshrc'
alias cop='conda activate etri'
alias col='conda activate lab'
alias cod='conda deactivate'

# git
alias gpr='git pull --recurse-submodules'

alias xxd='xxd -g 1'

# shortcurs
alias gonnn='cd ~/.config/nnn'
alias gonvim='cd ~/.config/nvim/lua'
alias gowork='cd ~/project/md.platform.infra'
alias gosqlite='cd ~/project/sqlite.reader/docs/fts'
alias gopsql='docker exec -it forensic-postgres-study psql -U forensic -d forensicdb'

# dotfiles
backup()  { backup.dot.files  && cd ~/code; }
restore() { restore.dot.files && cd ~/code; }

alias aliases='vi ~/.oh-my-zsh/lib/aliases.zsh && source ~/.oh-my-zsh/lib/aliases.zsh'

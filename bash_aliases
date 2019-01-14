
# Aliases
alias ls='ls --color=auto --human-readable --group-directories-first --classify'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias update='sudo apt update'
alias upgrade='sudo apt dist-upgrade'


alias untar='tar -xzvf'

alias :q='exit'
alias :e="$VIM_VERSION"
alias sync-downloads='rsync -avz spenser@68.45.30.169/home/spenser/Torrents/Complete /home/spenser/Torrents'
alias sync-downloads-home='rsync -avz spenser@192.168.1.224:/home/spenser/Torrents/Complete /home/spenser/Torrents'
alias qutebrowser="python3 -m qutebrowser"
alias ssd="cd /local-ssd/sbauman"
#alias config-env="eval `opam config env`"

alias c='pygmentize -O style=borland -f console -g'
alias tmux='TERM=xterm-256color tmux'
alias sbsb='sb -nodesktop -nosplash'
alias sync-gecks='rsync -avz $HOME/gecks $s'
alias sandboxes='mw -using Bmain sbs list'
alias rtags-index='nice -n 15 sbcpptags -mods-all'

# Create tags files for sandboxes
alias mktags='find . -iname "*.[ch]pp" -print | ctags --c++-kinds=+p --fields=+iaS --extra=+q -f ./ctags -L-'


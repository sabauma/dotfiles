
# Aliases
alias ls='ls --color=auto --human-readable --group-directories-first --classify'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

alias update='sudo apt update'
alias upgrade='sudo apt dist-upgrade'

alias switch-to-devel-farm='sbruntests -farm-management -add sbauman-deb9-64 -farm devel'
alias switch-to-cuda-farm='sbruntests -farm-management -add sbauman-deb9-64 -farm cuda'
alias sandbox-update='sbclone -backup'

alias untar='tar -xzvf'

alias :q='exit'
alias :e="$VIM_VERSION"
alias sync-downloads='rsync -avz spenser@68.45.30.169/home/spenser/Torrents/Complete /home/spenser/Torrents'
alias sync-downloads-home='rsync -avz spenser@192.168.1.224:/home/spenser/Torrents/Complete /home/spenser/Torrents'
alias qutebrowser='python3 -m qutebrowser'
alias ssd='cd /local-ssd/sbauman'

alias c='pygmentize -O style=borland -f console -g'
alias tmux='TERM=xterm-256color tmux'
alias sync-gecks='rsync -avz $HOME/gecks $s'
alias sandboxes='mw -using Bmain sbs list'
alias rtags-index='nice -n 15 sbcpptags -mods-all'

# Create tags files for sandboxes
alias mktags='find . -iregex ".*\.[ch]\(pp\)?" -print | ctags --sort=foldcase --c++-kinds=+p --fields=+iaS --extra=+q -f ./ctags -L-'

# sbtools
alias sbsb='sb -nodesktop -nosplash'
alias sbsubmit='sbsubmit -no-clickable-shell'

alias changelists='p4 opened | awk '\''{print $5}'\'' | sort -u'
alias touchall='find . -name "*.cpp" | xargs touch'

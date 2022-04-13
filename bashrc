# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

. /mathworks/hub/share/sbtools/bash_setup.bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#[[ $TMUX = "" ]] && export TERM=xterm-256color

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=2000
HISTFILESIZE=4000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
    else
    color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto --human-readable --group-directories-first --classify'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# prefer neovim if it exists, otherwise default to vim
if [ -x "$(command -v nvim)" ]; then
    VIM_VERSION="nvim"
    VIMDIFF_VERSION="nvim -d"
else
    VIM_VERSION="vim"
    VIMDIFF_VERSION="vimdiff"
fi

# Set bash to use vi based interaction mode
set -o vi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# For Cabal and Racket
export PATH=$HOME/.cabal/bin:$HOME/bin:$HOME/bin/racket/bin/:$PATH
# Local Install Dirs
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.ghcup/bin:$PATH
# The RTags tool
export PATH=$RTAGS_BIN_DIR:$PATH
# CGIR Debugging Tools (cgbug)
export PATH=/mathworks/hub/share/sbtools/apps/cgir_tools:$PATH
# Cargo
export PATH=$HOME/.cargo/bin:$PATH

TEXMF=/home/sbauman/.latex

export XDG_CURRENT_DESKTOP=Unity

# Various editor variables
export EDITOR="$VIM_VERSION"
export SVN_EDITOR="$VIM_VERSION"
export VISUAL="$VIM_VERSION"

#Arguments for P4MERGE
export P4MERGE="$VIMDIFF_VERSION"
export P4EDITOR="$VIM_VERSION"

# CCACHE settings
export CCACHE_COMPRESS=yes

# Set the XDG data directory to not be on a networked location.
if [ $(hostname) == "ah-sbauman-l" ]
then
    export XDG_DATA_HOME="/local-ssd/sbauman/.local"
fi

PS1='${debian_chroot:+($debian_chroot)}\[\033[01;34m\]\u@\[\033[01;32m\]\h: \w\n\[\033[0;36m\]\t $ \[\033[0;39m\]'

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'

    # Show the currently running command in the terminal title:
    # http://www.davidpashley.com/articles/xterm-titles-with-bash.html
    show_command_in_title_bar()
    {
        case "$BASH_COMMAND" in
            *\033]0*)
                # The command is trying to set the title bar as well;
                # this is most likely the execution of $PROMPT_COMMAND.
                # In any case nested escapes confuse the terminal, so don't
                # output them.
                ;;
            *)
                echo -ne "\033]0;${USER}@${HOSTNAME}: ${BASH_COMMAND}\007"
                ;;
        esac
    }
    trap show_command_in_title_bar DEBUG
    ;;
*)
    ;;
esac

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     [ -f ~/.fzf.bash ] && source ~/.fzf.bash;;
esac

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

source /home/sbauman/.config/broot/launcher/bash/br

eval "$(starship init bash)"

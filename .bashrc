# DO NOT REPLACE THIS FILE

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
if [ -n "$PS1" ]; then

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
    HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
    shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
    HISTSIZE=1000
    HISTFILESIZE=2000

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
        alias ls='ls --color=always'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

        alias grep='grep --color=always'
        alias fgrep='fgrep --color=always'
        alias egrep='egrep --color=always'
    fi

# some more ls aliases
    alias ll='ls -alF'
    alias la='ls -A'
    alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

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

    if [ -f /etc/profile ]; then
        . /etc/profile
    fi


    ####################################################################################################
    #####                                 These are my own modifications                           #####
    ####################################################################################################

    PS1='$(
if [[ -n "$(__git_ps1)" ]]; then
    DIR_NAME=$(readlink -f $(dirname $(__gitdir)))
    REPO=$(basename $DIR_NAME)
    CWD=$PWD
    git rev-parse --verify refs/stash > /dev/null 2>&1 && STATUS="S"
    git diff --quiet --no-ext-diff || STATUS="$STATUS*"
    git diff-index --cached --quiet --no-ext-diff HEAD > /dev/null 2>&1 || STATUS="$STATUS+"
    if [[ -n $(git ls-files --others --exclude-standard) ]]; then STATUS="$STATUS%"; fi
    if [[ -n "$STATUS" ]]; then STATUS=" $STATUS"; fi
    if [[ "x$CWD" == "x$DIR_NAME" ]]; then
       echo "\u@\h [\[\033[1;34m\]GIT $REPO\[\033[0m\]\[\033[1;30m\]$(__git_ps1)\[\033[0m\]$STATUS] \n\$ ";
    else
       echo "\u@\h [\[\033[1;34m\]GIT $REPO [${PWD##$DIR_NAME}]\[\033[0m\]\[\033[1;30m\]$(__git_ps1)\[\033[0m\]$STATUS] \n\$ ";
    fi
else
    echo "\u@\h [\w] \n$ "
fi
   )'
    if [ "x$JAVA_HOME" == "x" ]; then
        if which java > /dev/null 2>&1; then
            export JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac))))
        else
            export JAVA_HOME=$HOME/Downloads/jdk1.6.0_24
        fi
    fi
    export JAVA_FONTS=$HOME/.fonts/
    export PATH=/usr/local/MATLAB/R2011b/bin:$HOME/Downloads/scala-2.9.0.final/bin:$JAVA_HOME/bin:$PATH:$HOME/Documents/
    export SCALA_HOME=$HOME/Downloads/scala-2.8.1.final
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
    alias sbt='java -Xmx512M -jar ~/Downloads/sbt-launch-0.7.7.jar'
    alias ec='emacsclient --no-wait'
    alias go='gnome-open'
    alias gst="git status"
    alias redis="~/Downloads/redis-2.4.5/src/redis-cli"
    alias be='bundle exec'
    alias bi='bundle install'
    alias ack=ack-grep
    alias run_query=~/Documents/benchmark/one-tick-scripts/run_query.sh
    alias xclipc='xclip -selection clipboard'
    export EDITOR='emacsclient'

    # Set CDPATH to my Documents directory
    document=($HOME/Documents/*)
    IFS=':'
    document_with_colons="${document[*]}"
    unset IFS

    CDPATH=.:$document_with_colons

    # disable terminal xon/xoff to be able to search forward
    stty -ixon

    ####################################################################################################
    #####                                 End of my own modifications                           #####
    ####################################################################################################
fi

function millis_to_date {
    if [ $# -ne 1 ]; then
        echo "Usage: millis_to_date <milliseconds since epoc>"
        return 1
    fi
    millis=$(echo "$1 / 1000" | bc)
    date -d @$millis
    echo -n $(date -d @$millis) | xclipc
}

function date_to_millis {
    if [ $# -ne 1 ]; then
        echo "Usage: millis_to_date <YYYYMMDD [HH:[MM:[SS]]]>"
        return 1
    fi
    seconds=$(date -d "$1" +"%s")
    echo "${seconds}000"
    echo -n "${seconds}000" | xclipc
}

function print_header {
    screen_rows=$(tput lines)
    read header
    echo "$header"
    if [[ $? -ne 0 ]]; then
        echo "EOF reached while reading the header"
        return 1
    fi
    count=2
    while read line; do
        if [[ "$count" -eq "$screen_rows" ]]; then
            echo "$header"
            count=2
        fi
        echo "$line"
        count=$((count + 1))
    done
}

function repo_home {
    cd $(dirname $(__gitdir))
}

function get_lvc_data() {
    pushd $HOME/Documents/benchmark/cache-loader-ruby
    if [[ "$2" == "A_S_CDS" ]]; then product="ATTRIBUTION_SENSITIVITIES_CDS_CURVES"
    elif [[ "$2" == "A_S" ]]; then product="ATTRIBUTION_SENSITIVITIES"
    elif [[ "$2" == "A_S_IRS" ]]; then product="ATTRIBUTION_SENSITIVITIES_IRS"
    elif [[ "$2" == "A_R" ]]; then product="ATTRIBUTION_REALTIME"
    elif [[ "$2" == "A_R_IRS" ]]; then product="ATTRIBUTION_REALTIME_IRS"
    elif [[ "$2" == "A_R_CDS" ]]; then product="ATTRIBUTION_REALTIME_CDS_CURVES"
    else product=$2
    fi
    $HOME/Documents/benchmark/cache-loader-ruby/scripts/get_cached_values.rb $1 $product $3 $4
    popd
}
alias get_url=$HOME/Documents/benchmark/cache-loader-ruby/scripts/convert_log_to_url.rb

# Setup the less colors
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;31;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_us=$'\E[01;92;5;146m' # begin underline
export LESS_TERMCAP_ue=$'\E[0m'           # end underline

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

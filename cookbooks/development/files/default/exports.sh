if which javac 2>&1 >/dev/null; then
    if [[ "x$JAVA_HOME" == "x" ]]; then
        if which java > /dev/null 2>&1; then
            export JAVA_HOME=$(dirname $(dirname $(readlink -f $(which javac))))
        else
            export JAVA_HOME=$HOME/Downloads/jdk1.6.0_24
        fi
    fi
fi
shift_to_titlebar='\[\e]0;'
shift_to_tty='\a\]'
export XTERM_PS1="${shift_to_titlebar}\h:\w${shift_to_tty}"
if [[ ${EMACS} != 't' ]] ; then
    export PS1="${XTERM_PS1}${PS1}"
fi
export JAVA_FONTS=$HOME/.fonts/
# go language path
# android sdk and ndk
export SDK=~/Android/Sdk/
export NDK=$SDK/ndk-bundle
export PATH=$HOME/bin:/usr/local/MATLAB/R2011b/bin:$HOME/Downloads/scala-2.9.0.final/bin:$PATH:$NDK/
# custom emacs build
emacs_installation=$HOME/bin/emacs-27
export PATH=${emacs_installation}/bin:$PATH
if [[ ! -z $TMUX_PANE ]]; then
    export TERM=screen-256color
fi
# android path
PATH="$PATH:$SDK/tools/:$SDK/platform-tools"
# go path
export LD_LIBRARY_PATH="$RUSTCROOT/lib:$LD_LIBRARY_PATH"
export SCALA_HOME=$HOME/Downloads/scala-2.8.1.final
export MOSH_INSTALLATION='$HOME/mosh-installation'
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export EDITOR=ec
[ -f /home/jvshahid/.travis/travis.sh ] && source /home/jvshahid/.travis/travis.sh
export GOPATH=$HOME/codez/gocodez
export PATH=$PATH:$GOPATH/bin:/home/jvshahid/.cask/bin
export PATH=$PATH:$HOME/bin/git-crypt/bin
export PATH=/home/jvshahid/bin/java/bin:/home/jvshahid/bin/eclipse:$PATH
# Rust
export PATH=$HOME/.cargo/bin:$PATH
export MANPATH=$emacs_installation/share/man:$MANPATH
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
export GPG_AGENT_INFO=${HOME}/.gnupg/S.gpg-agent:0:1
export FZF_DEFAULT_OPTS='--height=20'
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "$HOME/.nvm/nvm.sh" ]] && source "$HOME/.nvm/nvm.sh"
[[ -s $HOME/.z.sh ]] && source $HOME/.z.sh

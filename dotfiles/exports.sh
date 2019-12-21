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
export ANDROID_HOME=$HOME/bin/android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export PATH=$PATH:$HOME/bin/android/bin
# custom emacs build
emacs_installation=$HOME/bin/emacs-27
export PATH=${emacs_installation}/bin:$PATH
if [[ ! -z $TMUX_PANE ]]; then
    export TERM=screen-256color
fi
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
export PATH=/home/jvshahid/bin/clj/bin:$PATH
# Rust
export PATH=$HOME/.cargo/bin:$PATH
export MANPATH=$emacs_installation/share/man:$MANPATH
export FZF_DEFAULT_OPTS='--height=20'
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "$HOME/.nvm/nvm.sh" ]] && source "$HOME/.nvm/nvm.sh"
[[ -s $HOME/.z.sh ]] && source $HOME/.z.sh

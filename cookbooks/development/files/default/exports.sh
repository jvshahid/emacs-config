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
# rust language path
export RUSTROOT=$HOME/bin/rust
export RUSTCROOT=$RUSTROOT/rustc
export CARGOROOT=$RUSTROOT/cargo
# android sdk and ndk
export SDK=~/Android/Sdk/
export NDK=$SDK/ndk-bundle
export PATH=$HOME/bin:/usr/local/MATLAB/R2011b/bin:$HOME/Downloads/scala-2.9.0.final/bin:$PATH:$NDK/
# custom emacs build
emacs_installation=$(ls -1d $HOME/bin/emacs*)
export PATH=${emacs_installation}/bin:$PATH
export TERM=screen-256color
# android path
PATH="$PATH:$SDK/tools/:$SDK/platform-tools"
# go path
PATH="$PATH:$RUSTCROOT/bin:$CARGOROOT/bin"
export LD_LIBRARY_PATH="$RUSTCROOT/lib:$LD_LIBRARY_PATH"
export SCALA_HOME=$HOME/Downloads/scala-2.8.1.final
export MOSH_INSTALLATION='$HOME/mosh-installation'
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export EDITOR='emacsclient'
[ -f /home/jvshahid/.travis/travis.sh ] && source /home/jvshahid/.travis/travis.sh
export GOPATH=$HOME/codez/gocodez
export PATH=$PATH:$GOPATH/bin:/home/jvshahid/.cask/bin
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
[[ -s "$HOME/.nvm/nvm.sh" ]] && source "$HOME/.nvm/nvm.sh"
[[ -s $HOME/.z.sh ]] && source $HOME/.z.sh

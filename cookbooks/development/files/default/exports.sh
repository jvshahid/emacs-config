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
export GOROOT=$HOME/bin/go
export RUSTROOT=$HOME/bin/rust
# android sdk and ndk
export SDK=~/bin/adt-bundle-linux/sdk/
export NDK=~/bin/android-ndk/
export PATH=/usr/local/MATLAB/R2011b/bin:$HOME/Downloads/scala-2.9.0.final/bin:$PATH:$HOME/bin:$NDK/
# android path
PATH="$PATH:$SDK/tools/:$SDK/platform-tools"
# go path
PATH="$PATH:$GOROOT/bin:$RUSTROOT/bin"
export SCALA_HOME=$HOME/Downloads/scala-2.8.1.final
export MOSH_INSTALLATION='$HOME/mosh-installation'
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
export EDITOR='emacsclient'
# added by travis gem

[ -f /home/jvshahid/.travis/travis.sh ] && source /home/jvshahid/.travis/travis.sh
[[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
export GOPATH=$HOME/codez/gocodez

alias sbt='java -Xmx512M -jar ~/Downloads/sbt-launch-0.7.7.jar'
alias ec_internal='emacsclient -t'
alias gno='gnome-open'
alias gst="git status"
alias be='bundle exec'
alias bi='bundle install'
alias bu='bundle update'
alias ack='ack-grep --no-group'
alias run_query=~/Documents/benchmark/one-tick-scripts/run_query.sh
alias xclipc='xclip -selection clipboard'
alias gen_tags="$HOME/Documents/generate_tags.sh"
alias mosh="$MOSH_INSTALLATION/bin/mosh --client=$MOSH_INSTALLATION/bin/mosh-client --server='$MOSH_INSTALLATION/bin/mosh-server'"
alias wdiff="wdiff -n -w $'\033[1;31m' -x $'\033[0m' -y $'\033[1;34m' -z $'\033[0m'"
alias ll='ls -l -a --color'
# dsh interactive, wait for new commands and execute them
alias dshi="dsh -i -c -M"
# if you want to filter by port number add 'port 80' to the end
alias tcpdump_print="tcpdump -nnXSs 0"
# show changes on this branch since it was forked from master
alias gdbr="git d \$(git merge-base HEAD master)"
alias sshtor='ssh -o "ProxyCommand nc -X 5 -x 127.0.0.1:9050 %h %p"'
alias RD="rdesktop ${HOST} -u Administrator -p '${PASS}' -r clipboard:CLIPBOARD -f -D -0 -g 1360x768 -x l -a 16"
alias sync_my_files='rsync -av --progress /home/jvshahid /media/jvshahid/backup'

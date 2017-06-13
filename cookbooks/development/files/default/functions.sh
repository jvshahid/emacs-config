function mount_optimus() {
    if [ "$#" -ne 1 ]; then
        echo "Usage: mount_optimus ip_address"
        return 1;
    fi
    sudo mount -t cifs -o uid=jvshahid,gid=jvshahid,forceuid,user=,password=,rw,nounix,noperm //$1/LG-NETWORKFOLDER /media/optimus/
}
# Set CDPATH to my Documents directory
document=($HOME/codez $HOME/codez/gocodez/src/github.com/influxdb)
IFS=':'
document_with_colons="${document[*]}"
unset IFS

CDPATH=.:$document_with_colons

# disable terminal xon/xoff to be able to search forward
stty -ixon

function is_zsh {
    if [[ "x$(ps -p $$ | tail -n1 | awk '{print $4}')" == "xzsh" ]]; then
        return 0
    else
        return 1
    fi
}

function ec {
    args="-ra"
    file_index=0
    line_index=1
    if is_zsh; then
        args="-rA"
        file_index=1
        line_index=2
    fi
    IFS=":" read $args file_and_linenumber <<< "$@"
    if [ ${#file_and_linenumber[@]} -eq 1 ]; then
        ec_internal ${file_and_linenumber[$file_index]}
    else
        echo "${file_and_linenumber[$file_index]}"
        ec_internal -e "(progn (find-file \"${file_and_linenumber[$file_index]}\") (goto-line ${file_and_linenumber[$line_index]}))"
    fi
}

function ed {
    pushd $HOME/.emacs.d
      cask emacs --daemon
    popd
}

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
        echo "Usage: date_to_millis <YYYYMMDD [HH:[MM:[SS]]]>"
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

function git_root_dir {
    git rev-parse --show-toplevel
}

function repo_home {
    cd $(git_root_dir)
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

function firefox32() {
    docker run "$@" -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0 jvshahid/firefox32
}

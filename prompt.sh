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

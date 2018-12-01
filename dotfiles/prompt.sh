percent="%"
if is_zsh; then
    percent="%%"
fi

function git_repo_info {
    REPO=""
    STATUS=""
    if git status > /dev/null 2>&1; then
        DIR_NAME=$(git rev-parse --show-toplevel)
        REPO=$(basename $DIR_NAME)
        CWD=$PWD
        BRANCH=$(git branch | grep '*' | head -n1 | cut -c3-)
        if [[ "x$BRANCH" == "x(no branch)" ]]; then
            BRANCH="(($(git rev-list --abbrev-commit --max-count=1 HEAD)...))"
        else
            BRANCH="($BRANCH)"
        fi
        git rev-parse --verify refs/stash > /dev/null 2>&1 && STATUS="S"
        git diff --quiet --no-ext-diff || STATUS="$STATUS*"
        git diff-index --cached --quiet --no-ext-diff HEAD > /dev/null 2>&1 || STATUS="$STATUS+"
        if [[ -n $(git ls-files --others --exclude-standard) ]]; then STATUS="$STATUS$percent"; fi
        if [[ -n "$STATUS" ]]; then STATUS=" $STATUS"; fi
    fi
}

if is_zsh; then
    # PROMPT="%K{blue}%n@%m%k %B%F{cyan}%(4~|...|)%3~%F{white} %# %b%f%k"
    function get_zsh_prompt {
        git_repo_info
        if [[ "x$REPO" == "x" ]]; then
            echo "%~"
        else
            if [[ "x$CWD" == "x$DIR_NAME" ]]; then
                echo "%B%F{blue}GIT $REPO%f%F{grey} $BRANCH%f%b$STATUS";
            else
                echo "%B%F{blue}GIT $REPO [${CWD##$DIR_NAME}]%f%F{grey} $BRANCH%f%b$STATUS";
            fi
        fi
    }
    function set_zsh_prompt {
        export PROMPT="%n@%m [$(get_zsh_prompt)]
$ "
    }
    add-zsh-hook precmd set_zsh_prompt
else
    function get_bash_prompt {
        git_repo_info
        if [[ "x$REPO" == "x" ]]; then
            echo "\w"
        else
            if [[ "x$CWD" == "x$DIR_NAME" ]]; then
                echo "\[\033[1;34m\]GIT $REPO\[\033[0m\]\[\033[1;30m\]$(__git_ps1)\[\033[0m\]$STATUS";
            else
                echo "\[\033[1;34m\]GIT $REPO [${CWD##$DIR_NAME}]\[\033[0m\]\[\033[1;30m\]$(__git_ps1)\[\033[0m\]$STATUS";
            fi
        fi
    }
    function set_bash_prompt {
        export PS1="\u@\h [$(get_bash_prompt)]\n$ "
    }
    PROMPT_COMMAND=set_bash_prompt
fi

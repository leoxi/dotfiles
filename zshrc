# path
export PATH=$HOME/bin:$PATH
export WORKON_HOME=~/.virtualenvs
export PYTHONIOENCODING=UTF-8

# editor
export EDITOR="emacsclient"
export USE_EDITOR=$EDITOR
export VISUAL=$EDITOR

# history
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

# alias
alias ls="ls -FG"
alias la="ls -A"
alias ll="ls -lh"
alias lla="ll -A"
alias h="fc -l 1"
alias del="fc -e - ls=rm"
alias ff="emacsclient"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias reload="source ~/.zshrc"
alias brews="brew list -1"
alias bubu="brew update && brew upgrade && brew cleanup"

# git prompt
function git_current_branch() {
    echo $(git symbolic-ref HEAD | cut -d'/' -f3-10 2> /dev/null)
}

function git_dirty() {
    if $(git diff-index --quiet HEAD --); then
        echo ""
    else
        echo "*"
    fi
}

function git_unknown_files() {
    lines=$(git status --porcelain | egrep '^\?\? ' | wc -l | tr -d ' ')
    if [ $lines != 0 ]; then
        echo "?"
    else
        echo ""
    fi
}

function git_prompt() {
    GITINFO=""
    if [ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = "true" ]; then
        GITINFO=" $(git_current_branch)$(git_dirty)$(git_unknown_files)"
    fi
    echo $GITINFO
}

function precmd() {
    PSVAR=`git_prompt`
}

PS1="λ %~%v "

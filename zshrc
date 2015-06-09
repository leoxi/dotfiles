export LANG=en_US.UTF-8
export PATH=$HOME/bin:$PATH
export WORKON_HOME=~/.virtualenvs
export PYTHONIOENCODING=UTF-8
export SSH_KEY_PATH="~/.ssh/dsa_id"

export ZSH=~/.oh-my-zsh

ZSH_THEME="kardan"

plugins=(brew celery git git-flow osx pip python virtualenvwrapper)

source $ZSH/oh-my-zsh.sh

HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

bindkey -e

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
alias bubu="brew update && brew upgrade --all && brew cleanup"

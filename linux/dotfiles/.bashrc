export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000


alias ll='ls -alth --color'
alias ls='ls --color'
alias less='less -R -S -i'
alias grep='grep --color=always'
alias uncolor='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'
alias emacscs='/usr/bin/emacs -nw'

#
# user-site PATH
#
export PATH=~/.local/bin:$PATH


#
# python relateds
#
export PIPENV_VENV_IN_PROJECT=1

#
# go relateds
#
if [[ ! -z $(command -v go) ]]; then
		export PATH=$(go env GOROOT)/bin:$(go env GOPATH)/bin:$PATH
fi

#
# vagrant relateds
#
if [[ -e $HOME/home2/vagrant.d ]]; then
		export VAGRANT_HOME=$HOME/home2/vagrant.d
fi

#
# emacs relateds
#
if [[ -e /usr/local/bin/emacs_launcher.sh  ]]; then
		alias emacs='/usr/local/bin/emacs_launcher.sh'
fi

# for emacs-libvterm 
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

export http_proxy=''
export https_proxy=''
export ftp_proxy=''
export socks_proxy=''
. "$HOME/.cargo/env"

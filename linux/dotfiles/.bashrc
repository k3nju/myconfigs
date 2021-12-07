export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000


alias ll='ls -alth --color'
alias ls='ls --color'
alias less='less -R -S -i'
alias grep='grep --color=always'
alias uncolor='sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g"'
alias emacscs='/usr/bin/emacs -nw'


if [[ -e /usr/local/bin/emacs_launcher.sh  ]]; then
		alias emacs='/usr/local/bin/emacs_launcher.sh'
fi


if [[ ! -z $(command -v go) ]]; then
		export PATH=$(go env GOROOT)/bin:$(go env GOPATH)/bin:$PATH
fi


if [[ -e $HOME/home2/vagrant.d ]]; then
		export VAGRANT_HOME=$HOME/home2/vagrant.d
fi


function wttr {
		curl -s wttr.in/tokyo | less -R
}


export http_proxy=''
export https_proxy=''
export ftp_proxy=''
export socks_proxy=''

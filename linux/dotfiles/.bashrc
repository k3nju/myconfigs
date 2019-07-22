export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export HISTFILESIZE=10000


alias ll='ls -alth --color'
alias ls='ls --color'
alias less='less -R -S -i'
alias grep='grep --color=auto'


if [[ -e /usr/local/bin/emacs_launcher.sh  ]]; then
		alias emacs='/usr/local/bin/emacs_launcher.sh'
fi


if [[ -e $HOME/goenv/ ]]; then
		export GOROOT=$HOME/goenv/go/
		export GOPATH=$HOME/goenv/workspace/
		export PATH=$GOROOT/bin:$GOPATH/bin:$PATH
fi


function wttr {
		curl -s wttr.in/tokyo | less -R
}




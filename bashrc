if [ `uname -s` == "Linux" ]; then
    alias ls="ls --color=auto"
    alias ll="ls --color -alh"
elif [ `uname -s` == "Darwin" ]; then
	alias ls="ls -G"
	alias ll="ls -G -alh"
else
    alias ll="ls -alh"
fi

alias svnchanges="svn status | grep \"^\?\" | awk '{print $2}' | xargs svn
add"

export PS1="\[\033[01;32m\]\u@\h \[\033[01;34m\]\W \$ \[\033[00m\]"

[ -f /etc/profile.d/bash-completion ] && . /etc/profile.d/bash-completion


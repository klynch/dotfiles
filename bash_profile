[ -f /etc/profile ] && . /etc/profile
[ -f ~/.bashrc ] && . ~/.bashrc
[ -f /etc/profile.d/bash-completion ] && . /etc/profile.d/bash-completion
PATH=/opt/local/bin:$HOME/local/bin:$PATH

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"

EDITOR=vim

umask 077

# Setting PATH for MacPython 2.5
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/Current/bin:${PATH}"
PATH="/usr/local/bin:/usr/local/sbin:${PATH}"
PATH="/usr/local/mysql/bin:${PATH}"
export PATH


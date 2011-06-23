# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#We only need to get the OS once
OS=$(uname -s)

###########################################################################
##### Shell Options
###########################################################################

# If this is set, an argument to the cd builtin command that
# is not a directory is assumed to be the name of a variable whose value is
# the directory to change to.
shopt -s cdable_vars

# If set, minor errors in the spelling of a directory component in a cd
# command will be corrected. The errors checked for are transposed characters,
# a missing character, and a character too many. If a correction is found, the
# corrected path is printed, and the command proceeds. This option is only used
# by interactive shells.
shopt -s cdspell

# If this is set, Bash checks that a command found in the hash table exists
# before trying to execute it. If a hashed command no longer exists, a normal
# path search is performed.
#shopt -s checkhash

# If set, Bash checks the window size after each command and, if necessary,
# updates the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, Bash attempts to save all lines of a multiple-line command in the
# same history entry. This allows easy re-editing of multi-line commands.
shopt -s cmdhist

# If set, Bash includes filenames beginning with a `.' in the results of filename
# expansion.
#shopt -s dotglob

# If this is set, a non-interactive shell will not exit if it cannot execute
# the file specified as an argument to the exec builtin command.
# An interactive shell does not exit if exec fails.
#shopt -s execfail

# If set, aliases are expanded. This option is enabled by default for interactive
# shells.
shopt -s expand_aliases

# If set, the extended pattern matching features described above are enabled.
shopt -s extglob

# If set, the history list is appended to the history file when the shell exits,
# rather than overwriting the history file.
#    shopt -s histappend
# To append every line to history individually set:
#    PROMPT_COMMAND='history -a'
# With these two settings, a new shell will get the history lines from all previous
# shells instead of the default 'last window closed'>history
#(the history file is named by the value of the HISTFILE variable)
shopt -s histappend

# If set, and Readline is being used, a user is given the opportunity to re-edit
# a failed history substitution.
shopt -s histreedit

# If set, and Readline is being used, the results of history substitution
# are not immediately passed to the shell parser. Instead, the resulting line
# is loaded into the Readline editing buffer, allowing further modification.
shopt -s histverify

# If set, and Readline is being used, Bash will attempt to perform hostname
# completion when a word containing a `@' is being completed.
# This option is enabled by default.
#shopt -s hostcomplete

# If set, Bash will send SIGHUP to all jobs when an interactive
# login shell exits.
#shopt -s huponexit

# Allow a word beginning with `#' to cause that word and all
# remaining characters on that line to be ignored in an interactive shell. This
# option is enabled by default.
shopt -s interactive_comments

# If enabled, and the cmdhist option is enabled, multi-line commands
# are saved to the history with embedded newlines rather than using semicolon
# separators where possible.
#shopt -s lithist

# If set, and a file that Bash is checking for mail has been accessed since
# the last time it was checked, the message "The mail in mailfile
# has been read" is displayed.
#shopt -s mailwarn

# If set, and Readline is being used, Bash will not attempt to search the
# PATH for possible completions when completion is attempted on
# an empty line.
#shopt -s no_empty_cmd_completion

# If set, Bash matches filenames in a case-insensitive fashion when performing
# filename expansion.
#shopt -s nocaseglob

# If set, Bash allows filename patterns which match no files to expand to
# a null string, rather than themselves.
#shopt -s nullglob

# If set, the programmable completion facilities are enabled. This option
# is enabled by default.
shopt -s progcomp

# If set, prompt strings undergo variable and parameter expansion after being
# expanded. This option is enabled by default.
shopt -s promptvars

# The shell sets this option if it is started in restricted mode. The value
# may not be changed. This is not reset when the startup files are executed,
# allowing the startup files to discover whether or not a shell is restricted.
#shopt -s restricted_shell

# If this is set, the shift builtin prints an error message when
# the shift count exceeds the number of positional parameters.
#shopt -s shift_verbose

# If set, the source builtin uses the value of PATH
# to find the directory containing the file supplied as an argument. This option
# is enabled by default.
#shopt -s sourcepath

# If set, the echo builtin expands backslash-escape sequences
# by default.
#shopt -s xpg_echo


###########################################################################
##### HISTORY
###########################################################################

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# suppresses duplicate commands, the simple invocation of 'ls' without any
# arguments, and the shell built-ins bg, fg, and exit:
HISTIGNORE="&:cd:ls:ll:bg:fg:exit"

shopt -s cmdhist

# append to the history file, don't overwrite it
shopt -s histappend histreedit histverify

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000


###########################################################################
##### PS1
###########################################################################

function set_PS1 {
    PS1='\u@\h:\w\$ '
}

function set_PS1_color {
    local reset='\[\033[0m\]'				 # Reset Text
    local bhi_red='\[\033[1;91m\]'    # Bold High Intensity Red
    local hi_blk='\[\033[0;90m\]'         # High Intensity Black
    local bhi_ylw='\[\033[1;93m\]'    # Bold High Intensity Yellow
    local bhi_grn='\[\033[1;92m\]'    # Bold High Intensity Green
    local bhi_wht='\[\033[1;97m\]'    # Bold High Intensity White
    local bhi_blu='\[\033[1;94m\]'    # Bold High Intensity Blue

    if [ ${EUID} = 0 ] ; then
        PS1="${reset}${bhi_red}\u@\h${hi_blk}:${bhi_ylw}\w${hi_blk}\$${reset} "
    else
        PS1="${reset}${bhi_grn}\u@\h${bhi_wht}:${bhi_blu}\w${bhi_wht}\$${reset} "
    fi
}

###########################################################################
##### COLORS
###########################################################################

function has_color_support {
    [ -x /usr/bin/tput ] && /usr/bin/tput setaf 1 >&/dev/null
}

function has_color_term {
    case "$TERM" in
        xterm*) return 0 ;;
        screen-256color-bce) return 0 ;;
    esac
    return 1
}

function set_linux_colors {
	  if [ -x /usr/bin/dircolors ]; then
		    [ -r ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
		    alias ls='ls --color=auto'
		    alias dir='dir --color=auto'
		    alias vdir='vdir --color=auto'
	  fi
}

function set_bsd_colors {
    export CLICOLOR=yes
}


if has_color_term && has_color_support; then
    color=yes
    set_PS1_color
else
    color=no
    set_PS1
fi

if [ $color = yes ]; then
    case "$OS" in
        Linux) set_linux_colors ;;
        Darwin) set_bsd_colors ;;
    esac

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi



if [ -f .bash_functions ]; then
	  source .bash_functions
fi

sourceconf .bash_aliases



###########################################################################
##### MISC
###########################################################################

export VISUAL=vim
export EDITOR=vim
export PAGER=less

umask 077

stty erase ^?

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
#export LESSOPEN='| /opt/local/bin/lesspipe.sh %s'
#export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
#export LESS=' -R '
#http://linux-tips.org/article/78/syntax-highlighting-in-less

###########################################################################
##### CDPATH
###########################################################################

load_cdpath

###########################################################################
##### PATH VARIABLES
###########################################################################

pathprepend ${HOME}/.local/bin
#pathprepend ${HOME}/.local/grade/bin
pathprepend ${HOME}/bin

if is_osx; then
    HOMEBREW=/opt/homebrew
    pathprepend /Developer/usr/bin
    pathprepend /Developer/usr/sbin

    pathprepend ${HOMEBREW}/bin
    pathprepend ${HOMEBREW}/sbin
    sourceconf-noposix ${HOMEBREW}/etc/bash_completion
    sourceconf-noposix ${HOMEBREW}/Library/Contributions/brew_bash_completion.sh

	pathprepend ${HOMEBREW}/share/python

    #requires `gem install brewbygems`
    export GEM_HOME=${HOMEBREW}/Cellar/gems/1.8
fi

sourceconf-noposix /opt/local/etc/bash_completion
sourceconf-noposix /etc/bash_completion

###########################################################################
##### PYTHON
###########################################################################

#export WORKON_HOME=$HOME/.virtualenvs
hash virtualenvwrapper.sh 2>/dev/null && sourceconf-noposix $(which virtualenvwrapper.sh)

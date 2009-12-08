#Set paths and env variables
## think about setting path,cdpath,manpath & fpath in .zshenv
MATLAB_APP=/Applications/MATLAB_R2009a.app
EMACS_APP=/Applications/MacPorts/Emacs.app

typeset -T INFOPATH infopath

path=(/bin $path)
path=(/usr/bin $path)
path=(/usr/local/bin $path)

manpath=(/usr/share/man $manpath)
manpath=(/usr/man $manpath)
manpath=(/usr/local/man $manpath)

## for root add sbin dirs to path
if (( EUID == 0 )); then
	  path=(/sbin $path)
    path=(/usr/sbin $path)
    path=(/usr/local/sbin $path)
    path=(/opt/local/sbin $path)
fi

path=(/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin $path)

path=(/opt/local/bin $path)
manpath=(/opt/local/man $manpath)
infopath=(/opt/local/share/info $infopath)

path=(/Developer/usr/bin $path)
manpath=(/Developer/usr/share/man $manpath)

path=(${EMACS_APP}/Contents/MacOS/bin $path)
infopath=(${EMACS_APP}/Contents/Resources/info $infopath)

## remove duplicate entries from path & manpath
typeset -U path manpath infopath

#typeset -T FPATH fpath
fpath=(~/.zfunc.d $fpath)
typeset -U fpath

autoload ~/.zfunc.d/*(N.)

alias matlab='matlab -nodesktop -nodisplay -nosplash'

EMACS=${EMACS_APP}/Contents/MacOS/Emacs

function emacs-compile() {
    ${EMACS} -batch -f batch-byte-compile $*
}

#TODO make this emacsclient if emacs-server is not running
function emacs () {
    ${EMACS} $*&
}

function psg () {
    ps aux | grep $1
}

########################################
#######################################

#######################################
########################################

#Set all zsh options

setopt nobeep                  # i hate beeps
setopt noautomenu              # don't cycle completions
setopt autocd                  # change to dirs without cd
setopt pushdignoredups         # and don't duplicate them
setopt cdablevars              # avoid the need for an explicit $
setopt nocheckjobs             # don't warn me about bg processes when exiting
setopt nohup                   # and don't kill them, either
setopt nolisttypes             # show types in completion
setopt extendedglob            # weird & wacky pattern matching - yay zsh!
setopt completeinword          # not just at the end
setopt alwaystoend             # when complete from middle, move cursor
setopt correct                 # spelling correction
setopt nopromptcr              # don't add \n which overwrites cmds with no \n
setopt histverify              # when using ! cmds, confirm first
setopt interactivecomments     # escape commands so i can use them later
setopt printexitvalue          # alert me if something's failed
setopt hist_ignore_dups        # ignore same commands run twice+
setopt appendhistory           # don't overwrite history

###################################

## The file to save the history in when an interactive shell exits.
## If unset, the history is not saved.
HISTFILE=${HOME}/.zsh_history

## The maximum number of events stored in the internal history list.
HISTSIZE=1000

## The maximum number of history events to save in the history file.
SAVEHIST=1000

## maximum size of the directory stack.
DIRSTACKSIZE=10

## This set of functions implements a sort of magic history searching.
## After predict-on, typing characters causes the editor to look backward
## in the history for the first line beginning with what you have typed so
## far.  After predict-off, editing returns to normal for the line found.
## In fact, you often don't even need to use predict-off, because if the
## line doesn't match something in the history, adding a key performs
## standard completion - though editing in the middle is liable to delete
## the rest of the line.
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey "^X^Z" predict-on ## C-x C-z
bindkey "^Z" predict-off ## C-z

## This allows incremental completion of a word.
## After starting this command, a list of completion
## choices can be shown after every character you
## type, which you can delete with ^h or DEL.
## RET will accept the completion so far.
## You can hit TAB to do normal completion, ^g to
## abort back to the state when you started, and ^d to list the matches.
autoload -U incremental-complete-word
zle -N incremental-complete-word
bindkey "^Xi" incremental-complete-word ## C-x-i

#HOME and END key should behave as expected
# ctrl-a and ctrl-e will also work (emacs style)
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
case $TERM in (xterm*)
	  bindkey '\e[H' beginning-of-line
	  bindkey '\e[F' end-of-line ;;
esac

########################################
#######################################

#######################################
########################################

#Set Look and Feel (PS1, Prompt, aliases, exports, CFLAGS)

# If running interactively, then:
if [ "$PS1" ]; then
    PS1='%n@%M:%c [%h | %j]%# '
    export GREP_OPTIONS='--color=auto'

    # colour ls
	  if [[ $OSTYPE = (darwin*|solaris*) ]]; then
		    alias ls='ls -G'
	  else
		    eval `dircolors`
		    alias ls='ls --color=auto'
	  fi

    # anti aliasing in the two toolkits
    export QT_XFT=1
    export GDK_USE_XFT=1

#
# completion tweaking
#
	  autoload -U compinit; compinit
    zstyle ':completion:*' use-cache on
    zstyle ':completion:*' cache-path ~/.zsh.d/cache

# complete hostnames out of ssh's ~/.ssh/known_hosts
    zstyle ':completion:*' users resolve
    hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*})
    zstyle ':completion:*:hosts' hosts $hosts

	# use dircolours in completion listings
    zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

	# allow approximate matching
    zstyle ':completion:*' completer _complete _match _approximate
    zstyle ':completion:*:match:*' original only
    zstyle ':completion:*:approximate:*' max-errors 1 numeric
    zstyle ':completion:*' auto-description 'Specify: %d'
    zstyle ':completion:*' format 'Completing %d'
    zstyle ':completion:*' verbose true
    zstyle ':completion:*:functions' ignored-patterns '_*'
    zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns \
        '*?.(o|c~|zwc)' '*?~'



    #Completing process IDs with menu selection:
    zstyle ':completion:*:*:kill:*' menu yes select
    zstyle ':completion:*:kill:*'   force-list always



	  bindkey -e
    bindkey "\C-w" kill-region
fi


#### ALIAS ####
#general commands
alias l='ls'
alias la='ls -A'
alias ll='ls -lh'
alias lla='ls -Alh'
alias lsdir='ls -d */'
alias duc='du -chs'
alias sizeof='duc'
alias df='df -h'
alias free='free -m'
alias ttop='top -ocpu -R -F -s 2 -n30'

alias gcc_macros='gcc -E -dM - </dev/null | sort'

#font sizes
alias hide='echo -en "\033]50;nil2\007"'
alias tiny='echo -en "\033]50;-misc-fixed-medium-r-normal--8-80-75-75-c-50-iso10646-1\007"'
alias small='echo -en "\033]50;6x10\007"'
alias default='echo -e "\033]50;-misc-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso10646-1\007"'
alias medium='echo -en "\033]50;-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1\007"'
alias large='echo -en "\033]50;-misc-fixed-medium-*-*-*-15-*-*-*-*-*-iso10646-1\007"'
# This is a large font that has a corresponding double-width font for
# CJK and other characters, useful for full-on utf-8 goodness.
alias larger='echo -en "\033]50;-misc-fixed-medium-r-normal--18-*-*-*-*-*-iso10646-1\007"'
alias huge='echo -en "\033]50;-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1\007"'

#### PROMPT ####
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_${color}='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_${color}='%{$fg[${(L)color}]%}'
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

case $TERM in
	  screen|xterm*|rxvt|linux)
		    PROMPT="$PR_BLUE%n$PR_WHITE$@$PR_LIGHT_BLUE$%m$PR_WHITE:$PR_GREEN$%c [%h | %j]%# $PR_NO_COLOR"
		    ;;
	  *)
		    PROMPT="[FIX PROMPT CASE] $PR_BLUE%n$PR_WHITE$@$PR_LIGHT_BLUE$%m$PR_WHITE:$PR_GREEN$%c [%h | %j]%~ $PR_NO_COLOR"
esac


#### EXTRA ####

## functions
## restore all .bak files
function restore_bak () {
	  autoload -U zmv
	  zmv '(**/)(*).bak' '$1$2'
}

function bu () {
    cp $1 `basename $1`-`date +%Y%m%d%H%M`.backup ;
}

# TODO mount *.dmg, *.iso, FUSE, Dropbox
#call it fmount? fumount?
function mount_file () {
    if [ -f $1 ] ; then
        case $1 in
            *.dmg)    hdiutil attach $1    ;; #OSX only
            *.iso)    hdiutil attach $1    ;; #OSX only
        esac
    fi
}

function umount_file () {
    if [ -f $1 ] ; then
        case $1 in
            #TODO
        esac
    fi
}

function extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.bz2)        bunzip2 $1       ;;
            *.rar)        unrar e -ad $1   ;;
            *.gz)         gunzip $1        ;;
            *.zip)        unzip $1         ;;
            *.tar)        tar xf $1        ;;
            *.tar.bz2)    tar xjf $1       ;;
            *.tbz2)       tar xjf $1       ;;
            *.tar.gz)     tar xzf $1       ;;
            *.tgz)        tar xzf $1       ;;
            *.Z)          uncompress $1    ;;
            *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

function list () {
    if [ -f $1 ] ; then
        case $1 in
            *.bz2)        echo "NO"        ;;
            *.rar)        echo "NO"        ;;
            *.gz)         echo "NO"        ;;
            *.zip)        echo "NO"        ;;
            *.tar)        tar tf $1        ;;
            *.tar.bz2)    tar tjf $1       ;;
            *.tbz2)       tar tjf $1       ;;
            *.tar.gz)     tar tzf $1       ;;
            *.tgz)        tar tzf $1       ;;
            *.Z)          echo "NO"        ;;
            *) echo "'$1' cannot be listed via list()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

function s () {
    echo "This is supposed to print the size of files passed in"
}

function launch() {
    eval "$@" &> /dev/null &!
}

function clearflags() {
 	  export CFLAGS="-g -Wall --std=c99"
	  export CXXFLAGS="-g -Wall"
}

function title() {
	# escape '%' chars in $1, make nonprintables visible
	  a=${(V)1//\%/\%\%}

	# Truncate command, and join lines.
	  a=$(print -Pn "%40>...>$a" | tr -d "\n")

	  case $TERM in
	      screen)
            print -Pn "\e]2;$a @ $2\a" # plain xterm title
            print -Pn "\ek$a\e\\"      # screen title (in ^A")
            print -Pn "\e_$2   \e\\"   # screen location
            ;;
        xterm*|rxvt)
            print -Pn "\e]2;$a @ $2\a" # plain xterm title
            ;;
    esac
}

#precmd is called just before the prompt is printed
function precmd() {
	  title "zsh" "%m(%55<...<%~)"
}

# preexec is called just before any command line is executed
function preexec() {
	  title "$1" "%m(%35<...<%~)"
}

_rake_does_task_list_need_generating () {
    if [ ! -f .rake_tasks ]; then return 0;
    else
        accurate=$(stat -f%m .rake_tasks)
        changed=$(stat -f%m Rakefile)
        return $(expr $accurate '>=' $changed)
    fi
}

_rake () {
    if [ -f Rakefile ]; then
        if _rake_does_task_list_need_generating; then
            echo "\nGenerating .rake_tasks..." > /dev/stderr
            rake --silent --tasks | cut -d " " -f 2 > .rake_tasks
        fi
        compadd `cat .rake_tasks`
    fi
}

compdef _rake rake

source ~/.zsh.d/git
#source ~/.zsh.d/port

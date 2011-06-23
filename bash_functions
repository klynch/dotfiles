#!/usr/bin/env bash

# Functions to help us manage paths.  Second argument is the name of the
# path variable to be modified (default: PATH)
function pathremove {
    local IFS=':'
    local NEWPATH
    local DIR
    local PATHVARIABLE=${2:-PATH}
    for DIR in ${!PATHVARIABLE} ; do
        if [ "$DIR" != "$1" ] ; then
            NEWPATH=${NEWPATH:+$NEWPATH:}$DIR
        fi
    done
    export $PATHVARIABLE="$NEWPATH"
}

function pathprepend {
    pathremove "$1" "$2"
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="$1${!PATHVARIABLE:+:${!PATHVARIABLE}}"
}

function pathappend {
    pathremove "$1" "$2"
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="${!PATHVARIABLE:+${!PATHVARIABLE}:}$1"
}

function load_cdpath {
    local CDPATH_CONF=.cdpath
    CDPATH=.
    if [ -f ${CDPATH_CONF} ]; then
        while read dir; do
            pathappend "${dir}" CDPATH
        done < ${CDPATH_CONF}
    fi
    export CDPATH
}

function pathlist {
    local IFS=':'
    local DIR
    local PATHVARIABLE=${2:-PATH}
    for DIR in ${!PATHVARIABLE} ; do
        echo "${DIR}"
    done
}


# Functions to help us source files in bash. Not really necessary, but it
# streamlines some branches
function sourceconf {
    if [ -f "$1" ]; then
        source "$1"
    fi
}

function sourceconf-noposix {
    if [ -f "$1" ] && ! shopt -oq posix; then
        source "$1"
    fi
}



# What am i???? What have I become???
function is_osx {
    test "$OS" = Darwin
}

function is_linux {
    test "$OS" = Linux
}

if is_osx; then
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
#alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# # Display files in Quick Look
# function ql () {
# 	(qlmanage -p “$@” > /dev/null 2>&1 &
# 	local ql_pid=$!
# 	read -sn 1
# 	kill ${ql_pid}) > /dev/null 2>&1
# }

# # Display any filetype as plain text
# function qlt () {
# 	(qlmanage -p -c public.plain-text “$@” > /dev/null 2>&1 &
# 	local ql_pid=$!
# 	read -sn 1
# 	kill ${ql_pid}) > /dev/null 2>&1
# }

    function unquarantine {
	      xattr -d -r -v com.apple.quarantine "$@"
    }

    function addgroup {
        if [ $# -ne 2 ]; then
            echo -e "Adds user to group.\n\tUsage: addgroup USER GROUP"
            return 1
        fi

        /usr/sbin/dseditgroup -o edit -a $1 -t user $2
    }

    function removegroup {
        if [ $# -ne 2 ]; then
            echo -e "Removes user from group.\n\tUsage: addgroup USER GROUP"
            return 1
        fi

        /usr/sbin/dseditgroup -o edit -d $1 -t user $2
    }
fi

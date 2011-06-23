#!/usr/bin/env bash

alias ll='ls -Al'
alias la='ls -A'
alias l='ls -C'

if is_linux; then
	  alias notify='notify-send'
fi

if is_osx; then
	  alias notify='growlnotify'

		alias brwe='brew'
		alias brw='brew'
fi

export VISUAL=vim
export EDITOR=vim
export PAGER=less

umask 077

stty erase 

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

#
# Your previous .profile  (if any) is saved as .profile.dpsaved
# Setting the path for DarwinPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# Display files in Quick Look
function ql ()
{
	(qlmanage -p “$@” > /dev/null 2>&1 &
	local ql_pid=$!
	read -sn 1
	kill ${ql_pid}) > /dev/null 2>&1
}
# Display any filetype as plain text
function qlt ()
{
	(qlmanage -p -c public.plain-text “$@” > /dev/null 2>&1 &
	local ql_pid=$!
	read -sn 1
	kill ${ql_pid}) > /dev/null 2>&1
}

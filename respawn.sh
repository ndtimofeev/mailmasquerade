#!/usr/bin/env bash
#
# Run a (leaky) application under a memory ulimit and restart it whenever it
# crashes. A normal exit -- e.g. the user closed the Qt main window, so
# QApplication::exec() returned 0 -- ends this script as well.
#
# Usage: respawn.sh [options] [--] program [args...]
# Run with -h for the list of options. Example, e.g. for a .desktop file:
#   Exec=/path/to/respawn.sh -m 3000 /opt/myapp/bin/myapp %U

set -u

mem_mb=2048     # -m: memory limit, MiB
kind=v          # -k: v = ulimit -v (address space), d = ulimit -d (heap/data)
ok_codes=0      # -e: exit codes that mean "closed normally", comma separated
delay=2         # -d: pause before a restart, seconds
max_fast=5      # -n: give up after this many quick crashes in a row...
min_uptime=30   # -u: ...where "quick" means dying within this many seconds

me=${0##*/}

usage() {
	cat <<EOF
Usage: $me [options] [--] program [args...]

Runs program with a memory ulimit and restarts it if it crashes.
Exits when the program exits normally (window closed, code 0) or when
this script gets SIGTERM/SIGINT/SIGHUP (the program is terminated too).

Options:
  -m MB     memory limit in MiB (default: $mem_mb)
  -k v|d    v: limit virtual memory, ulimit -v (default)
            d: limit data segment/heap, ulimit -d; use this if the program
               fails to start even with a generous -v limit (QtWebEngine,
               some GPU drivers reserve huge amounts of address space)
  -e CODES  comma separated exit codes that mean a normal exit (default: $ok_codes)
  -d SEC    delay before restarting (default: $delay)
  -n N      give up after N crashes in a row, each happening less than
            -u seconds after start (default: $max_fast)
  -u SEC    see -n (default: $min_uptime)
  -h        show this help

The program being killed by SIGTERM/SIGINT/SIGHUP is treated as a normal
exit too: somebody asked it to quit, it did not crash.
EOF
}

die() { printf '%s: %s\n' "$me" "$*" >&2; exit 2; }
log() { printf '%s[%d] %(%F %T)T: %s\n' "$me" "$$" -1 "$*" >&2; }

is_uint() { [[ $1 =~ ^(0|[1-9][0-9]*)$ ]]; }
is_posint() { [[ $1 =~ ^[1-9][0-9]*$ ]]; }

while getopts 'm:k:e:d:n:u:h' opt; do
	case $opt in
	m) mem_mb=$OPTARG ;;
	k) kind=$OPTARG ;;
	e) ok_codes=$OPTARG ;;
	d) delay=$OPTARG ;;
	n) max_fast=$OPTARG ;;
	u) min_uptime=$OPTARG ;;
	h) usage; exit 0 ;;
	*) usage >&2; exit 2 ;;
	esac
done
shift $((OPTIND - 1))

(( $# )) || { usage >&2; exit 2; }
is_posint "$mem_mb" || die "-m: bad memory limit '$mem_mb'"
[[ $kind == [vd] ]] || die "-k: expected v or d, got '$kind'"
[[ $ok_codes =~ ^[0-9]+(,[0-9]+)*$ ]] || die "-e: bad exit code list '$ok_codes'"
is_uint "$delay" || die "-d: bad delay '$delay'"
is_posint "$max_fast" || die "-n: bad count '$max_fast'"
is_uint "$min_uptime" || die "-u: bad uptime '$min_uptime'"
command -v -- "$1" >/dev/null || die "$1: command not found"

limit_kb=$((mem_mb * 1024))
# Check once here, so that an impossible limit (e.g. above the hard limit)
# is reported instead of turning into an endless crash loop.
(ulimit -"$kind" "$limit_kb") 2>/dev/null ||
	die "cannot set ulimit -$kind $limit_kb KiB (hard limit: $(ulimit -H"$kind") KiB)"

app=${1##*/}
child=
stop_sig=

on_signal() {
	stop_sig=$1
	[[ -n $child ]] && kill -TERM "$child" 2>/dev/null
}
for sig in HUP INT TERM; do
	# shellcheck disable=SC2064 # expand $sig now
	trap "on_signal $sig" "$sig"
done

# Start "$@" in the background and wait for it; the result goes to $status.
# Waiting on a background job (instead of running it in the foreground) lets
# the traps above fire immediately rather than after the program exits.
run() {
	"$@" &
	child=$!
	[[ -n $stop_sig ]] && kill -TERM "$child" 2>/dev/null
	# 2>/dev/null hides bash's own "Aborted ..." job notice, we log it ourselves
	wait "$child" 2>/dev/null
	status=$?
	# wait returns early when a trapped signal arrives; the trap has sent
	# SIGTERM to the child, so wait for it to actually go away.
	while kill -0 "$child" 2>/dev/null; do
		wait "$child" 2>/dev/null
		status=$?
	done
	child=
}

start_app() {
	ulimit -"$kind" "$limit_kb" || exit 127
	exec "$@"
}

describe() {
	if (( $1 > 128 )); then
		local name
		name=$(kill -l "$(( $1 - 128 ))" 2>/dev/null) || name=$(( $1 - 128 ))
		printf 'killed by SIG%s' "$name"
	else
		printf 'exited with code %d' "$1"
	fi
}

is_normal_exit() {
	local code
	case $1 in
	129 | 130 | 143) return 0 ;; # SIGHUP, SIGINT, SIGTERM
	esac
	for code in ${ok_codes//,/ }; do
		(( $1 == code )) && return 0
	done
	return 1
}

duration() {
	printf '%dh%02dm%02ds' $(($1 / 3600)) $(($1 % 3600 / 60)) $(($1 % 60))
}

stop() {
	log "got SIG$stop_sig, $*, not restarting"
	exit $((128 + $(kill -l "$stop_sig")))
}

fast_fails=0
log "starting $app with ulimit -$kind ${mem_mb}M"
while :; do
	started=$SECONDS
	run start_app "$@"
	uptime=$((SECONDS - started))

	[[ -n $stop_sig ]] && stop "$app $(describe "$status")"
	if is_normal_exit "$status"; then
		log "$app $(describe "$status") after $(duration "$uptime"), not restarting"
		exit "$status"
	fi

	if (( uptime < min_uptime )); then
		fast_fails=$((fast_fails + 1))
	else
		fast_fails=0
	fi
	if (( fast_fails >= max_fast )); then
		log "$app $(describe "$status") after $(duration "$uptime")," \
			"$fast_fails quick crashes in a row, giving up"
		exit "$status"
	fi

	log "$app $(describe "$status") after $(duration "$uptime"), restarting in ${delay}s"
	run sleep "$delay"
	[[ -n $stop_sig ]] && stop "$app stays down"
done

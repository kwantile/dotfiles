#!/bin/sh
# LICENSE: PUBLIC DOMAIN
# switch between my layouts

# If an explicit layout is provided as an argument, use it. Otherwise, select the next layout from
# the set [us, it, fr].

if [[ -n "$1" ]]; then
    setxkbmap $1
else
    layout=$(setxkbmap -query | grep layout | awk 'END{print $2}')
    case $layout in
        'us')
                setxkbmap -layout ru
				LANG='ru'
            ;;
        *)
                setxkbmap -layout us
				LANG='us'
            ;;
    esac
fi

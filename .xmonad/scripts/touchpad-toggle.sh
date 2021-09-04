#!/bin/sh

enabled=$(xinput --list-props "SynPS/2 Synaptics TouchPad" | grep "Device Enabled" | awk 'END{print $4}')
id_device=$(xinput --list | grep "SynPS/2 Synaptics TouchPad" | awk 'END{print $6}' | awk -F= 'END{print $2}' )
case $enabled in
	1) xinput set-prop $id_device "Device Enabled" 0
		;;
	0) xinput set-prop $id_device "Device Enabled" 1
		;;
esac

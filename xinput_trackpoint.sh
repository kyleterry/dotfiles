#!/bin/bash

# Scroll with the middle button:
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 8 200

# Middle click by using both the left and right mouse buttons:
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Middle Button Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Middle Button Timeout" 8 50

# Disable access control
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Fallback cursor
xsetroot -cursor_name left_ptr

# Keyboard repeat rate
xset r rate 200 60

cinnamon-screensaver &

synclient TouchpadOff=1

dropbox start

xrdb -merge ~/.Xresources

xset m 2 0

# Start Emacs
# exec dbus-launch --exit-with-session emacs
exec emacs

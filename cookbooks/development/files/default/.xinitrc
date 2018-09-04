# Disable access control
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Fallback cursor
xsetroot -cursor_name left_ptr

xset s 300                      # sleep after 5 minutes

xset r rate 200 60              # Keyboard repeat rate

synclient TouchpadOff=1

dropbox start

xrdb -merge ~/.Xresources

xss-lock -- i3lock -c 000000 --nofork & # automatically run screensave on resume

xset m 2 0

# Start Emacs
# exec dbus-launch --exit-with-session emacs
exec emacs

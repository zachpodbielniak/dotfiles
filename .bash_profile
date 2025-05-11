# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# User specific environment and startup programs
if [ -f "$HOME/.cargo/env" ]; then . "$HOME/.cargo/env"; fi

# Theme
export GTK_THEME=WhiteSur-Dark
export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus


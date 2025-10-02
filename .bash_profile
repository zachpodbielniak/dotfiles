# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

# User specific environment and startup programs
if [ -f "$HOME/.cargo/env" ]; then . "$HOME/.cargo/env"; fi

# Theme
if [[ "${HOSTNAME}" == "lt-zach" ]] || [[ "${HOSTNAME}" == "hacbook" ]]
then
    export GTK_THEME=WhiteSur-Dark
fi

export XMODIFIERS=@im=ibus
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export GLFW_IM_MODULE=ibus # for kitty


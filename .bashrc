# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
if [ -d ~/.bashrc.d ]; then
    for rc in ~/.bashrc.d/*; do
        if [ -f "$rc" ]; then
            . "$rc"
        fi
    done
fi

unset rc

set -o vi

export EDITOR=nvim
export REGISTRY_AUTH_FILE="/var/home/$(whoami)/.config/containers/auth.json"

source ~/.bashrc-functions
source ~/.bashrc-secrets

alias dc="distrobox create"
alias de="distrobox enter"
alias dl="distrobox list"
alias dr="distrobox rm"

alias flatpak="flatpak --user"
alias vim="nvim"

export PATH="/var/home/zach/bin/scripts:$PATH"
[ "$ORIG_PATH" == "" ] && export ORIG_PATH="$PATH"

# Export only to main system not distrobox containers
# or any other container that gets ~ mapped
if [ ! -f /run/.containerenv ]; then
    export PATH="/var/home/zach/bin/export:$PATH"
else
    export PATH="$ORIG_PATH"
fi

# PS1 Config -- No longer needed due to usage of starship
#if [ -f /run/.containerenv ]
#then
#	export PS1="[\u@\h \W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
#else
#	export PS1="[\W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
#fi

# Container specific setups
[ "dev" == "$(get_current_container_name)" ] &&
    source /usr/share/bash-completion/bash_completion &&
    source <(kubectl completion bash) &&
    source <(_HASS_CLI_COMPLETE=bash_source hass-cli) &&
    source "$HOME/.cargo/env"

eval "$($HOME/bin/starship/starship init bash)"
# WORKSTATION_BASHRC
source ~/.bashrc-workstation

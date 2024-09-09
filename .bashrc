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
export REGISTRY_AUTH_FILE="$HOME/.config/containers/auth.json"

source ~/.bashrc-functions
if [ -f ~/.bashrc-secrets ]; then source ~/.bashrc-secrets; fi
if [ -f /usr/immutablue/scripts/common.sh ]; then source /usr/immutablue/scripts/common.sh; fi


# Aliases
alias dc="distrobox create --additional-flags '-v /var/home/linuxbrew:/var/home/linuxbrew -v /home/linuxbrew:/home/linuxbrew'"
alias de="distrobox enter"
alias dl="distrobox list"
alias dr="distrobox rm"

alias flatpak="flatpak --user"
alias vim="nvim"
alias gpia="curl https://icanhazip.com"
alias k="kubectl"
alias cat="bat --theme='Catppuccin Mocha' --paging=never"

alias cdd="cd $HOME/.dotfiles"
alias cdp="cd $HOME/Source/Projects"
alias cdi="cd $HOME/Source/Projects/immutablue"
alias cdh="cd $HOME/Source/Projects/hyacinth-macaw"

alias weather="wthrr toledo,oh"
alias forecast="wthrr --forecast d,w toledo,oh"


# Only for arm64 machines
if [ "$(uname -m)" == "aarch64" ]
then
    alias box64="BOX64_LD_LIBRARY_PATH=\"$HOME/bin/libs/x64\" box64.asahi"
fi


# Exports
export PATH="$HOME/bin/scripts:$PATH"
export PATH="$HOME/bin/export:$PATH"
export PATH="$HOME/../linuxbrew/.linuxbrew/bin:$PATH"

# [ "$ORIG_PATH" == "" ] && export ORIG_PATH="$PATH"
# Export only to main system not distrobox containers
# or any other container that gets ~ mapped
# if [ ! -f /run/.containerenv ]
# then 
# 	export PATH="$HOME/../linuxbrew/.linuxbrew/bin:$HOME/bin/export:$PATH"
# else
# 	export PATH="$ORIG_PATH"
# fi

if [ -f /run/.containerenv ]
then
	export PS1="[\u@\h \W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
else
	export PS1="[\W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
fi




# Settings
# export FZF_DEFAULT_COMMAND='find . -type f ! -path "*git*"'
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
export FZF_DEFAULT_OPTS='-i --height=50% --preview="bat {}" --tmux center'



# Bash completion stuff
if [ -f /usr/share/bash-completion/bash_completion ]; then source /usr/share/bash-completion/bash_completion; fi

type kubectl 2>/dev/null >/dev/null 
if [ $? -eq 0 ]; then source <(kubectl completion bash); fi

# type hass-cli 2>/dev/null >/dev/null
# if [ $? -eq 0 ] && [ ! -f /run/.containerenv ]; then source <(_HASS_CLI_COMPLETE=bash_source hass-cli); fi

if [ -f "$HOME/.cargo/env" ]; then source "$HOME/.cargo/env"; fi

type fzf 2>/dev/null >/dev/null
if [ $? -eq 0 ]; then eval "$(fzf --bash)"; fi

type himalaya 2>/dev/null >/dev/null 
if [ $? -eq 0 ]; then eval "$(himalaya completion bash)"; fi


# Keybinds
bind -x '"\C-o":vim $(fzf)'


# Starship
eval "$($HOME/bin/starship/starship init bash)"

fastfetch

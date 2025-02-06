#    _  _       _               _              
#  _| || |_    | |__   __ _ ___| |__  _ __ ___ 
# |_  ..  _|   | '_ \ / _` / __| '_ \| '__/ __|
# |_      _|  _| |_) | (_| \__ \ | | | | | (__ 
#   |_||_|   (_)_.__/ \__,_|___/_| |_|_|  \___|
                                             


# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


# check to see if we have a program
_have() { type "${1}" &>/dev/null; }

# source only if we have the file
_source() { if [[ -f "${1}" ]]; then source "${1}"; fi; }

# append to the path, last is the last in the $PATH
_path_append() {
	for arg in "$@"; do
		[[ -d "${arg}" ]] || continue
		PATH=${PATH//":${arg}:"/:} # remove if present
		PATH=${PATH/#"${arg}:"/} # remove if at beginning
		PATH=${PATH/%":${arg}"/} # remove if at end
		export PATH="${PATH:+"$PATH:"}${arg}"
	done
}

# prepend to the path, last arg passed is first in the $PATH
_path_prepend() {
	for arg in "$@"; do
        [[ -d "${arg}" ]] || continue
		PATH=${PATH//:"${arg}:"/:} # remove if present
		PATH=${PATH/#"${arg}:"/} # remmove if at beginning
		PATH=${PATH/%":${arg}"/} # remove if at end
		export PATH="${arg}${PATH:+":${PATH}"}"
	done
}

# helper function for setting the editor correctly
_set_editor() {
    export EDITOR="${1}"
    export VISUAL="${1}"
    export GIT_EDITOR="${1}"
    alias vim="${EDITOR}"
    alias vi="${EDITOR}"
    alias v="${EDITOR}"
    alias nano="${EDITOR}"
}


# Bottom is the first
_path_prepend \
    "${HOME}/.local/bin" \
    "${HOME}/.cargo/bin" \
    "/var/home/linuxbrew/.linuxbrew/bin" \
    "${HOME}/perl5/bin" \
    "${HOME}/bin" \
    "${HOME}/bin/scripts" \
    "${HOME}/bin/export"


_path_append \
    "/usr/local/bin" \
    "/usr/local/sbin" \
    "/usr/sbin" \
    "/usr/bin" 




# fall-through for portability
_have vi && _set_editor "vi"
_have vim && _set_editor "vim"
_have nvim && _set_editor "nvim"

# source the files if we have them
_source "${HOME}/.bashrc-functions"
_source "${HOME}/.bashrc-secrets"
_source "/usr/immutablue/scripts/common.sh"
_source "/usr/libexec/immutablue/immutablue-header.sh"


# cd configuration / lookup for fast dir changes
export CDPATH=".:${HOME}/Documents:${HOME}:${HOME}/Documents/notes:${HOME}/Source/Projects:${HOME}/Source/Public"


# bash options
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s globstar
shopt -s extglob


# Uncomment the following line if you don't like systemctl's auto-paging feature:
export SYSTEMD_PAGER=""
export REGISTRY_AUTH_FILE="${HOME}/.config/containers/auth.json"
_have nvim && export MANPAGER="nvim +Man!"
# history config
export HISTCONTROL=ignoreboth
export HISTSIZE=5000
export HISTFILESIZE=10000
shopt -s histappend

# vi mode
set -o vi


# Aliases

# distrobox
if [[ -d "${HOME}../linuxbrew/.linuxbrew" ]] && [[ "$(uname -m)" == "x86_64" ]]
then 
    alias dc="distrobox create --additional-flags '-v /var/home/linuxbrew:/var/home/linuxbrew -v /home/linuxbrew:/home/linuxbrew'"
else 
    alias dc="distrobox create"
fi
alias de="distrobox enter"
alias dl="distrobox list"
alias dr="distrobox rm"

# ovrerides, must be careful!
_have nvim && alias less="nvim +Man!"
_have bat && alias cat="bat --theme='Catppuccin Mocha' --paging=never"

# common things
alias flatpak="flatpak --user"
alias gpia="curl https://icanhazip.com"
alias k="kubectl"
alias fcd='cd $(find * -type d | fzf)'
alias y="yazi"

alias cdd="cd ${HOME}/.dotfiles"
alias cdp="cd ${HOME}/Source/Projects"
alias cdi="cd ${HOME}/Source/Projects/immutablue"
alias cdh="cd ${HOME}/Source/Projects/hyacinth-macaw"
alias cdk="cd ${HOME}/Source/Projects/kuberblue"

alias cdnas="cd /var/mnt/NAS"
alias cdnasme="cd /var/mnt/NAS/Media"
alias cdnasmu="cd /var/mnt/NAS/Media/Music"
alias cdnasmv="cd /var/mnt/NAS/Media/Movies"
alias cdnastv="cd /var/mnt/NAS/Media/TV\ Series/"

alias nn="new_note"
alias na="note_append"
alias on="open_note"
alias nj="new_journal"
alias goals="bash -c 'cd ${HOME}/Documents/notes && nvim 02_areas/goals.norg'"
alias notes="bash -c 'cd ${HOME}/Documents/notes && nvim 00_index.norg'"
alias ninbox="bash -c 'cd ${HOME}/Documents/notes && nvim ./00_inbox/00_index.norg'"
alias nprojects="bash -c 'cd ${HOME}/Documents/notes && nvim ./01_projects/00_index.norg'"
alias nareas="bash -c 'cd ${HOME}/Documents/notes && nvim ./02_areas/00_index.norg'"
alias nresources="bash -c 'cd ${HOME}/Documents/notes && nvim ./03_resources/00_index.norg'"
alias narchives="bash -c 'cd ${HOME}/Documents/notes && nvim ./04_archives/00_index.norg'"
alias cdn="cd ${HOME}/Documents/notes/"

alias journal="bash -c 'cd ${HOME}/Documents/nodes && nvim ./02_areas/personal/journal/00_index.norg"

alias qw="qwik"
alias weather="wthrr toledo,oh"
alias forecast="wthrr --forecast d,w toledo,oh"
! _have calibre && alias calibre="flatpak run com.calibre_ebook.calibre"


# Only for arm64 machines
if [ "$(uname -m)" == "aarch64" ]
then
    alias box64="BOX64_LD_LIBRARY_PATH=\"${HOME}/bin/libs/x64\" box64.asahi"
fi



if [ -f /run/.containerenv ]
then
	export PS1="[\u@\h \W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
else
	export PS1="[\W]\`parse_git_branch\`\[\e[37m\]\`nonzero_return\`\[\e[m\]\\$ "
fi




# Settings
# export FZF_DEFAULT_COMMAND='find . -type f ! -path "*git*"'
# export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git" --preview-window=wrap'
export FZF_DEFAULT_COMMAND='rg --files --hidden --glob "!.git"'
# export FZF_DEFAULT_OPTS='-i --height=50% --preview="bat {}" --tmux center'
# https://github.com/catppuccin/fzf
export FZF_DEFAULT_OPTS=" \
--color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
--color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
--color=marker:#b4befe,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8 \
--color=selected-bg:#45475a \
--multi \
-i --height=50% \
--preview=\"if [ -f "{}" ]; then bat {}; else echo "{}"; fi\" \
--tmux center --preview-window=wrap"


# Bash completion stuff
_source "/usr/share/bash-completion/bash_completion"

# source programs we have
_have kubectl && source <(kubectl completion bash)
_have fzf && eval "$(fzf --bash)"
_have himalaya && eval "$(himalaya completion bash)"
_have glab && eval "$(glab completion -s bash)"


# Keybinds
bind -x '"\C-o":vim $(fzf)'


# Starship
if [[ $(_have starship) ]]
then 
    eval "$(starship init bash)"
else
    [[ -f "${HOME}/bin/starship/starship" ]] && eval "$(${HOME}/bin/starship/starship init bash)"
fi


# I manually set this in the path functions above
# PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;


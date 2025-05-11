stow: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile --ignore=CLAUDE.md --ignore=tests .


stow_alt: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile --ignore=CLAUDE.md --ignore=tests --ignore=.gitconfig .


unstow:
    #!/bin/bash 
    set -euxo pipefail

    stow -D .


dry: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile --simulate -v .


test:
    #!/bin/bash 
    set -euxo pipefail

    qtile check


dep_dirs:
    #!/bin/bash 
    set -euxo pipefail

    mkdir -p $HOME/bin 
    mkdir -p $HOME/bin/scripts 
    mkdir -p $HOME/bin/export

    mkdir -p $HOME/.config
    mkdir -p $HOME/.config/btop
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/rofi
    mkdir -p $HOME/.config/qtile
    mkdir -p $HOME/.config/systemd/user
    mkdir -p $HOME/.config/containers/systemd

    mkdir -p $HOME/.config/ncmpcpp
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/tmux/plugins


# install deps
bootstrap:
    #!/bin/bash
    set -euxo pipefail 
    
    if [[ -f "${HOME}/.config/.dotfiles_init" ]]
    then 
        echo "dotfiles_init file already exists...exiting"
        exit 0
    fi

    # needed for $(pomo)
    cpan install YAML::XS < <(yes)
    # init pomo so it has a state file
    bash -c "source ${HOME}/.bashrc && pomo -s && pomo -S"


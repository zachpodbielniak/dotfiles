stow: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile .


stow_alt: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile --ignore=.gitconfig .


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
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/rofi
    mkdir -p $HOME/.config/qtile
    mkdir -p $HOME/.config/systemd/user
    mkdir -p $HOME/.config/containers/systemd

    mkdir -p $HOME/.config/ncmpcpp
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/tmux/plugins


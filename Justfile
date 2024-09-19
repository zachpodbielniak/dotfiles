stow: dep_dirs
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile .


unstow:
    #!/bin/bash 
    set -euxo pipefail

    stow -D .


test: dep_dirs 
    #!/bin/bash 
    set -euxo pipefail

    stow --ignore=Justfile --simulate -v .


dep_dirs:
    #!/bin/bash 
    set -euxo pipefail

    mkdir -p $HOME/bin 
    mkdir -p $HOME/bin/scripts 
    mkdir -p $HOME/bin/export

    mkdir -p $HOME/.config
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/systemd/user
    mkdir -p $HOME/.config/containers/systemd

    mkdir -p $HOME/.config/ncmpcpp
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/tmux/plugins


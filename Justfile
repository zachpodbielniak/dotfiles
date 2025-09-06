# normal stow operation
stow: dep_dirs
    #!/usr/bin/env bash 
    set -euxo pipefail

    stow \
        --ignore=LICENSE \
        --ignore=Justfile \
        --ignore=CLAUDE.md \
        --ignore=tests \
        --ignore=Containerfile \
        --ignore=requirements.txt \
        --ignore=trees \
        .


# modified stow operation for other devices
stow_alt: dep_dirs
    #!/usr/bin/env bash 
    set -euxo pipefail

    stow \
        --ignore=LICENSE \
        --ignore=Justfile \
        --ignore=CLAUDE.md \
        --ignore=tests \
        --ignore=Containerfile \
        --ignore=requirements.txt \
        --ignore=trees \
        --ignore=.gitconfig \
        .


# unstow
unstow:
    #!/usr/bin/env bash 
    set -euxo pipefail

    stow -D .


# dry-run
dry: dep_dirs
    #!/usr/bin/env bash 
    set -euxo pipefail

    stow --ignore=Justfile --simulate -v .


# test on the whole repo
test:
    #!/usr/bin/env bash 
    set -euxo pipefail

    qtile check


# create depedendent dirs so we don't end up symlinking the dirs here
dep_dirs:
    #!/usr/bin/env bash 
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
    #!/usr/bin/env bash
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


# create git-worktree
tree branch="" parent="master":
    #!/usr/bin/env bash 
    set -euo pipefail 
    
    mkdir -p ./trees
    git worktree add -b "{{branch}}" "./trees/{{branch}}" "{{parent}}"


# remove git-worktree and optionally delete branch with it
rm_tree branch="" rm_branch="false":
    #!/usr/bin/env bash 
    set -euo pipefail 
    
    git worktree remove "./trees/{{branch}}"
    if [[ "true" == "{{rm_branch}}" ]]
    then 
        git branch -D "{{branch}}"
    fi

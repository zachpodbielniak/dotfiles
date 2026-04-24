# normal stow operation
# Pass quadlets=false to skip Podman quadlet units (.config/containers/systemd/*.container)
stow quadlets="true": dep_dirs
    #!/usr/bin/env bash
    set -euxo pipefail

    # build ignore flags for all known hostnames except the current host
    CURRENT_HOST=$(hostname -s)
    KNOWN_HOSTS=(lt-zach hacbook libreclaw-00 srv-zach)
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            IGNORE_FLAGS+=("--ignore=${h}")
            # srv-zach also appears without hyphen in one mount filename
            if [[ "$h" == "srv-zach" ]]; then
                IGNORE_FLAGS+=("--ignore=srvzach")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{quadlets}}" != "true" ]; then
        IGNORE_FLAGS+=("--ignore=\\.container$")
    fi

    stow \
        --ignore=LICENSE \
        --ignore=Justfile \
        --ignore=CLAUDE.md \
        --ignore=tests \
        --ignore=Containerfile \
        --ignore=requirements.txt \
        --ignore=trees \
        --ignore=share \
        --ignore=.gitconfig \
        --ignore=deps \
        "${IGNORE_FLAGS[@]}" \
        .

    # stow vimban submodule as its own package
    stow \
        -d deps \
        -t $HOME \
        --ignore=LICENSE \
        --ignore=README.md \
        --ignore=Makefile \
        --ignore=requirements.txt \
        --ignore=docs \
        --ignore=examples \
        --ignore=share \
        --ignore=.gitignore \
        vimban


# modified stow operation for other devices
# Pass quadlets=false to skip Podman quadlet units (.config/containers/systemd/*.container)
stow_alt quadlets="true": dep_dirs
    #!/usr/bin/env bash
    set -euxo pipefail

    # build ignore flags for all known hostnames except the current host
    CURRENT_HOST=$(hostname -s)
    KNOWN_HOSTS=(lt-zach hacbook libreclaw-00 srv-zach)
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            IGNORE_FLAGS+=("--ignore=${h}")
            # srv-zach also appears without hyphen in one mount filename
            if [[ "$h" == "srv-zach" ]]; then
                IGNORE_FLAGS+=("--ignore=srvzach")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{quadlets}}" != "true" ]; then
        IGNORE_FLAGS+=("--ignore=\\.container$")
    fi

    stow \
        --ignore=LICENSE \
        --ignore=Justfile \
        --ignore=CLAUDE.md \
        --ignore=tests \
        --ignore=Containerfile \
        --ignore=requirements.txt \
        --ignore=trees \
        --ignore=share \
        --ignore=.gitconfig \
        --ignore=deps \
        "${IGNORE_FLAGS[@]}" \
        .

    # stow vimban submodule as its own package
    stow \
        -d deps \
        -t $HOME \
        --ignore=LICENSE \
        --ignore=README.md \
        --ignore=Makefile \
        --ignore=requirements.txt \
        --ignore=docs \
        --ignore=examples \
        --ignore=share \
        --ignore=.gitignore \
        vimban


# unstow
unstow:
    #!/usr/bin/env bash
    set -euxo pipefail

    stow -D -d deps -t $HOME vimban
    stow -D .


# dry-run
# Pass quadlets=false to skip Podman quadlet units (.config/containers/systemd/*.container)
dry quadlets="true": dep_dirs
    #!/usr/bin/env bash
    set -euxo pipefail

    # build ignore flags for all known hostnames except the current host
    CURRENT_HOST=$(hostname -s)
    KNOWN_HOSTS=(lt-zach hacbook libreclaw-00 srv-zach)
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            IGNORE_FLAGS+=("--ignore=${h}")
            if [[ "$h" == "srv-zach" ]]; then
                IGNORE_FLAGS+=("--ignore=srvzach")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{quadlets}}" != "true" ]; then
        IGNORE_FLAGS+=("--ignore=\\.container$")
    fi

    stow --ignore=Justfile --simulate -v "${IGNORE_FLAGS[@]}" .


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
    mkdir -p $HOME/.config/autostart
    mkdir -p $HOME/.config/btop
    mkdir -p $HOME/.config/gst
    mkdir -p $HOME/.config/gowl
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/rofi
    mkdir -p $HOME/.config/qtile
    mkdir -p $HOME/.config/systemd/user
    mkdir -p $HOME/.config/containers/systemd

    mkdir -p $HOME/.config/ncmpcpp
    mkdir -p $HOME/.config/neomutt
    mkdir -p $HOME/.config/mpd
    mkdir -p $HOME/.config/tmux/plugins
    mkdir -p $HOME/.config/vimban
    
    mkdir -p $HOME/.claude/agents
    mkdir -p $HOME/.claude/agent-memory
    mkdir -p $HOME/.opencode/skills

    mkdir -p $HOME/.librewolf/native-messaging-hosts
    mkdir -p $HOME/.config/emacs/.local/cache/auto-save

    mkdir -p $HOME/.cache/docling/models


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

    # Pre-fetch docling models into the user cache so the `rag` tool's
    # Docling parser can OCR on first use without trying to write into
    # root-owned site-packages. Idempotent (safe to run again). Downloads
    # the default set (layout, tableformer, etc.) plus rapidocr. Total
    # ~200-400 MB. If docling isn't installed yet this fails silently —
    # the models can be fetched later via `just rag-models`.
    if distrobox enter dev -- command -v docling-tools >/dev/null 2>&1; then
        distrobox enter dev -- docling-tools models download || true
        distrobox enter dev -- docling-tools models download rapidocr || true
    fi


# fetch docling models into the user cache (idempotent)
rag-models:
    #!/usr/bin/env bash
    set -euo pipefail
    distrobox enter dev -- docling-tools models download
    distrobox enter dev -- docling-tools models download rapidocr


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


# generate self-signed SSL certificate for nginx-private (for testing)
# for production, use: certbot_helper --nginx-private -d localhost.podbielniak.com
generate-nginx-private-ssl-selfsigned:
    #!/usr/bin/env bash
    set -euo pipefail

    ssl_dir="${HOME}/.config/nginx-private/ssl"
    mkdir -p "${ssl_dir}"

    # Get tailscale IP if available
    tailscale_ip=""
    if command -v tailscale &>/dev/null
    then
        tailscale_ip=$(tailscale ip -4 2>/dev/null || true)
    fi

    # Build SAN extension
    san="DNS:localhost,DNS:localhost.podbielniak.com,IP:127.0.0.1"
    if [[ -n "${tailscale_ip}" ]]
    then
        san="${san},IP:${tailscale_ip}"
        echo "Including tailscale IP: ${tailscale_ip}"
    fi

    openssl req -x509 -nodes -days 3650 -newkey rsa:2048 \
        -keyout "${ssl_dir}/private.key" \
        -out "${ssl_dir}/private.crt" \
        -subj "/CN=localhost.podbielniak.com" \
        -addext "subjectAltName=${san}"

    chmod 600 "${ssl_dir}/private.key"
    chmod 644 "${ssl_dir}/private.crt"

    echo "Self-signed SSL certificate generated at ${ssl_dir}"
    echo "Valid for 10 years"


# generate Let's Encrypt SSL certificate for nginx-private via certbot
generate-nginx-private-ssl email="":
    #!/usr/bin/env bash
    set -euo pipefail

    email_arg=""
    if [[ -n "{{email}}" ]]
    then
        email_arg="-e {{email}}"
    elif [[ -n "${CERTBOT_EMAIL:-}" ]]
    then
        email_arg="-e ${CERTBOT_EMAIL}"
    else
        echo "Error: Email required. Use 'just generate-nginx-private-ssl email@example.com'"
        echo "       or set CERTBOT_EMAIL environment variable"
        exit 1
    fi

    certbot_helper --nginx-private -d localhost.podbielniak.com ${email_arg}

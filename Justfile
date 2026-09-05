# hosts that own host-specific files in this repo (unit names contain the
# hostname, e.g. postgres-lt-zach.container). Used to keep another host's
# files from being stowed here. Add new hosts HERE only.

known_hosts := "lt-zach mob-zach hacbook libreclaw-00 srv-zach"

# normal stow operation

# Pass quadlets=false to skip Podman quadlet units (.config/containers/systemd/*.container)
stow quadlets="true": dep_dirs
    #!/usr/bin/env bash
    set -euxo pipefail

    # build ignore flags for all known hostnames except the current host
    CURRENT_HOST=$(hostname -s)
    KNOWN_HOSTS=({{ known_hosts }})
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            # stow anchors --ignore regexes to the ENTIRE basename, so a bare
            # "lt-zach" never matches postgres-lt-zach.container and the file
            # gets stowed anyway. The .* wrappers are what make this work.
            IGNORE_FLAGS+=("--ignore=.*${h}.*")
            # some filenames drop the hyphen (var-home-zach-data-mnt-srvzach.mount)
            hn="${h//-/}"
            if [[ "$hn" != "$h" ]]; then
                IGNORE_FLAGS+=("--ignore=.*${hn}.*")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{ quadlets }}" != "true" ]; then
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
        --ignore=.claude \
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
        --ignore=.claude \
        vimban

# modified stow operation for other devices

# Pass quadlets=false to skip Podman quadlet units (.config/containers/systemd/*.container)
stow_alt quadlets="true": dep_dirs
    #!/usr/bin/env bash
    set -euxo pipefail

    # build ignore flags for all known hostnames except the current host
    CURRENT_HOST=$(hostname -s)
    KNOWN_HOSTS=({{ known_hosts }})
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            # stow anchors --ignore regexes to the ENTIRE basename, so a bare
            # "lt-zach" never matches postgres-lt-zach.container and the file
            # gets stowed anyway. The .* wrappers are what make this work.
            IGNORE_FLAGS+=("--ignore=.*${h}.*")
            # some filenames drop the hyphen (var-home-zach-data-mnt-srvzach.mount)
            hn="${h//-/}"
            if [[ "$hn" != "$h" ]]; then
                IGNORE_FLAGS+=("--ignore=.*${hn}.*")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{ quadlets }}" != "true" ]; then
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
        --ignore=.claude \
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
        --ignore=.claude \
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
    KNOWN_HOSTS=({{ known_hosts }})
    IGNORE_FLAGS=()
    for h in "${KNOWN_HOSTS[@]}"; do
        if [[ "$h" != "$CURRENT_HOST" ]]; then
            # stow anchors --ignore regexes to the ENTIRE basename, so a bare
            # "lt-zach" never matches postgres-lt-zach.container and the file
            # gets stowed anyway. The .* wrappers are what make this work.
            IGNORE_FLAGS+=("--ignore=.*${h}.*")
            # some filenames drop the hyphen (var-home-zach-data-mnt-srvzach.mount)
            hn="${h//-/}"
            if [[ "$hn" != "$h" ]]; then
                IGNORE_FLAGS+=("--ignore=.*${hn}.*")
            fi
        fi
    done

    # optionally skip Podman quadlet units on hosts that don't run containers
    if [ "{{ quadlets }}" != "true" ]; then
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

    mkdir -p $HOME/.cache/docling/models

    # bind-mount sources for the Podman quadlets; podman errors out with
    # "statfs ...: no such file or directory" rather than creating them
    mkdir -p $HOME/.data/postgres
    mkdir -p $HOME/.ollama

# install deps
bootstrap:
    #!/usr/bin/env bash
    set -euxo pipefail

    if [[ -f "${HOME}/.config/.dotfiles_init" ]]
    then
        echo "dotfiles_init file already exists...exiting"
        exit 0
    fi

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
    git worktree add -b "{{ branch }}" "./trees/{{ branch }}" "{{ parent }}"

# remove git-worktree and optionally delete branch with it
rm_tree branch="" rm_branch="false":
    #!/usr/bin/env bash
    set -euo pipefail

    git worktree remove "./trees/{{ branch }}"
    if [[ "true" == "{{ rm_branch }}" ]]
    then
        git branch -D "{{ branch }}"
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
    if [[ -n "{{ email }}" ]]
    then
        email_arg="-e {{ email }}"
    elif [[ -n "${CERTBOT_EMAIL:-}" ]]
    then
        email_arg="-e ${CERTBOT_EMAIL}"
    else
        echo "Error: Email required. Use 'just generate-nginx-private-ssl email@example.com'"
        echo "       or set CERTBOT_EMAIL environment variable"
        exit 1
    fi

    certbot_helper --nginx-private -d localhost.podbielniak.com ${email_arg}

install-skills:
    #!/usr/bin/env bash
    set -euo pipefail

    SKILLS=(
        https://github.com/Leonxlnx/unlazy
        https://github.com/Leonxlnx/taste-skill
        https://github.com/petergyang/no-ai-slop
        https://github.com/mvanhorn/last30days-skill
    )

    for skill in "${SKILLS[@]}"
    do
        npx skills add "${skill}" -g
    done

# Element (im.riot.Riot) ships no `org.freedesktop.secrets=talk` in its
# session bus policy, so Electron's safeStorage can't reach gnome-keyring
# and the app opens with "Your system has a supported keyring but
# encryption is not available." Other keyring-using flatpaks here
# (Bitwarden, Proton Mail, Proton Bridge) already grant it upstream.
#
# Deskflow (org.deskflow.deskflow) implements Wayland clipboard sharing by
# shelling out to the wl-clipboard CLI (`wl-paste --list-types`, `wl-paste -n
# -t <type>`, `wl-copy`), resolved through a plain PATH lookup. The flathub
# build ships only deskflow and deskflow-core in /app/bin and org.kde.Platform
# carries no wl-clipboard, so the sandbox PATH (/app/bin:/usr/bin) has neither
# tool: deskflow logs "wl-clipboard tools not found, clipboard functionality
# disabled" and the clipboard silently never syncs to any client. Exposing the
# host /usr read-only and appending /run/host/usr/bin to PATH lets it find the
# host binaries, which run fine under the runtime's glibc. Not a permissions
# problem - mutter's security-context socket serves the sandbox clipboard
# happily once the tools are reachable.
#
# Idempotent. Quit the app fully (including tray) before it takes effect.
# Undo a single app with: flatpak override --user --reset <app-id>
#

# apply flatpak sandbox overrides that upstream manifests are missing
flatpak-overrides:
    #!/usr/bin/env bash
    set -euo pipefail

    # apps needing D-Bus access to the Secret Service (gnome-keyring)
    secrets_apps=(im.riot.Riot)

    for app in "${secrets_apps[@]}"
    do
        if ! flatpak info "${app}" &>/dev/null
        then
            echo "skipping ${app}: not installed"
            continue
        fi

        flatpak override --user --talk-name=org.freedesktop.secrets "${app}"
        echo "granted org.freedesktop.secrets to ${app}"
    done

    # apps that shell out to host wl-clipboard for Wayland clipboard support
    wl_clipboard_apps=(org.deskflow.deskflow)

    for app in "${wl_clipboard_apps[@]}"
    do
        if ! flatpak info "${app}" &>/dev/null
        then
            echo "skipping ${app}: not installed"
            continue
        fi

        if [[ ! -x /usr/bin/wl-copy ]] || [[ ! -x /usr/bin/wl-paste ]]
        then
            echo "warning: ${app} override applied but wl-clipboard is missing from the host"
        fi

        flatpak override --user \
            --filesystem=host-os:ro \
            --env=PATH=/app/bin:/usr/bin:/run/host/usr/bin \
            "${app}"

        echo "exposed host wl-clipboard to ${app}"
    done

# Neither the official ollama image (CPU/CUDA only) nor Intel's ipex-llm image
# can drive this machine's Intel Arc B390 (Panther Lake): ipex-llm pins a
# Jan-2025 Level Zero runtime that predates Xe3 and silently falls back to CPU.
# This image pairs upstream ollama's Vulkan backend with a Mesa new enough for
# Xe3. Required by ollama-mob-zach.container, which references it by name.
#

# build the local ollama Vulkan image for Intel Arc (mob-zach)
build-ollama-vulkan:
    #!/usr/bin/env bash
    set -euo pipefail

    podman build -t localhost/ollama-vulkan:latest \
        "{{ justfile_directory() }}/.supporting_files/ollama_vulkan"

    echo
    echo "built localhost/ollama-vulkan:latest"
    echo "verify GPU detection with:"
    echo "  podman run --rm --device /dev/dri --entrypoint bash \\"
    echo "    localhost/ollama-vulkan:latest -c 'ollama serve 2>&1 | grep \"inference compute\"'"

# The Podman quadlets are NOT listed here - the quadlet generator wires them
# into default.target itself, so they need no `systemctl --user enable`. These
# units do. Idempotent; safe to re-run after any stow.
#
# var-home-zach-data-mnt-srvzach.mount is deliberately omitted (it mounts
# srv-zach's home; enable it by hand if a given host actually wants it).
#

# enable the user units that need an explicit systemctl enable (start=true to also start)
enable-units start="false":
    #!/usr/bin/env bash
    set -euo pipefail

    UNITS=(
        var-home-zach-data-mnt-nas-apool.mount
        var-home-zach-data-mnt-nas-dpool.mount
        var-home-zach-data-mnt-nas-ypool.mount
        nas-sleep-guard.service
    )

    systemctl --user daemon-reload

    enabled=()
    for u in "${UNITS[@]}"
    do
        if ! systemctl --user cat "${u}" &>/dev/null
        then
            echo "skipping ${u}: not installed (run 'just stow' first)"
            continue
        fi
        systemctl --user enable "${u}" 2>&1 | grep -v '^Created symlink' || true
        enabled+=("${u}")
    done

    # starting the sshfs mounts needs the NAS reachable; each has TimeoutSec=90,
    # so this is opt-in rather than the default
    if [ "{{ start }}" = "true" ]
    then
        for u in "${enabled[@]}"
        do
            echo "starting ${u}"
            systemctl --user start "${u}" || echo "  failed to start ${u} (NAS unreachable?)"
        done
    fi

    echo
    for u in "${enabled[@]}"
    do
        printf '%-42s enabled=%-9s active=%s\n' "${u}" \
            "$(systemctl --user is-enabled "${u}" 2>&1)" \
            "$(systemctl --user is-active "${u}" 2>&1)"
    done

ARG FEDORA_IMAGE=${FEDORA_IMAGE:-registry.fedoraproject.org/fedora:43}
FROM ${FEDORA_IMAGE}

RUN set -eux && \
    useradd -m -g wheel -s /bin/bash zach && \
    dnf5 update -y && \
    dnf install -y make stow just git neovim python3 python3-pip && \ 
  	curl -Lo "/tmp/starship_installer.sh" https://starship.rs/install.sh


COPY . /home/zach/.dotfiles/
RUN set -eux && \
    sudo pip3 install -r /home/zach/.dotfiles/requirements.txt


USER zach
WORKDIR /home/zach/.dotfiles/
RUN set -eux && \
    rm ../.bashrc ../.bash_profile && \
    mkdir -p /home/zach/bin/starship && \
    sh "/tmp/starship_installer.sh" -y -b "/home/zach/bin/starship/" && \
    just stow

WORKDIR /home/zach/
ENV NO_DBOX_CHECK=1
ENTRYPOINT [ "/bin/bash", "-i" ]


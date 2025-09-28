# Mirror Container

This container serves package repository mirrors created by the `make_mirror` script via nginx.

## Directory Structure

The container expects the mirror data to be mounted at `/var/www/mirror` inside the container. The `make_mirror` script creates the following structure:

```
/var/www/mirror/
├── arch/          # Arch Linux packages and ISOs
├── fedora/        # Fedora packages and ISOs
├── ubuntu/        # Ubuntu packages and ISOs
├── freebsd/       # FreeBSD packages and ISOs
├── almalinux/     # AlmaLinux packages and ISOs
└── fdroid/        # F-Droid Android packages
```

## Building the Container

### Using Podman directly:
```bash
podman build -t mirror-server .
```

### Using Podman Compose:
```bash
# Set MIRROR_ROOT environment variable to point to your mirror directory
# Default: ~/data/mirror
# For NAS systems: /var/mnt/dpool/software/mirror
export MIRROR_ROOT="$HOME/data/mirror"

# Build and start
podman-compose up -d

# View logs
podman-compose logs -f mirror-server

# Stop the service
podman-compose down

# Rebuild after changes
podman-compose up -d --build
```

## Running the Container

The container needs to mount your mirror directory. The path depends on your hostname:

- **Default**: `~/data/mirror`
- **NAS systems** (hostname matches `nas-(main|backup)`): `/var/mnt/dpool/software/mirror`

You can override the mirror path by setting the `MIRROR_ROOT` environment variable before running the container.

**Note**: Volume mounts include SELinux options (`:ro,z`) for proper container access on SELinux-enabled systems.

### Example for default location:

```bash
podman run -d \
  --name mirror-server \
  -p 8680:80 \
  -v ~/data/mirror:/var/www/mirror:ro,z \
  mirror-server
```

### Example for NAS:

```bash
podman run -d \
  --name mirror-server \
  -p 8680:80 \
  -v /var/mnt/dpool/software/mirror:/var/www/mirror:ro,z \
  mirror-server
```

## Accessing the Mirror

Once running, access the mirror at `http://localhost:8680/`

- Browse directories: `http://localhost:8680/`
- Arch packages: `http://localhost:8680/arch/`
- Fedora packages: `http://localhost:8680/fedora/`
- Ubuntu packages: `http://localhost:8680/ubuntu/`
- etc.

## Health Check

The container includes a health check endpoint at `http://localhost:8680/health`

## Configuring Package Managers

Once your mirror server is running, you can configure your Linux distributions to use it instead of official repositories. Replace `your-server-ip` with your server's IP address or hostname.

### Arch Linux

Edit `/etc/pacman.conf` and add your mirror at the top of the file:

```
[options]
...
Include = /etc/pacman.d/mirrorlist

[mirror]
Server = http://your-server-ip:8680/arch/$repo/os/$arch
```

Or create a custom mirrorlist in `/etc/pacman.d/mirrorlist`:

```
Server = http://your-server-ip:8680/arch/$repo/os/$arch
```

### Fedora

Create or edit repository files in `/etc/yum.repos.d/`. For example, create `local-mirror.repo`:

```
[fedora]
name=Fedora $releasever - $basearch
baseurl=http://your-server-ip:8680/fedora/releases/$releasever/Everything/$basearch/os/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-$releasever-$basearch

[fedora-updates]
name=Fedora $releasever - $basearch - Updates
baseurl=http://your-server-ip:8680/fedora/updates/$releasever/Everything/$basearch/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-$releasever-$basearch
```

For rawhide (development):

```
[fedora-rawhide]
name=Fedora - Rawhide - Developmental packages for the next Fedora release
baseurl=http://your-server-ip:8680/fedora/development/rawhide/Everything/$basearch/os/
enabled=0
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-fedora-rawhide
```

### Ubuntu

Edit `/etc/apt/sources.list` or create a new file in `/etc/apt/sources.list.d/local-mirror.list`:

```
deb http://your-server-ip:8680/ubuntu noble main restricted universe multiverse
deb http://your-server-ip:8680/ubuntu noble-updates main restricted universe multiverse
deb http://your-server-ip:8680/ubuntu noble-security main restricted universe multiverse
```

Replace `noble` with your Ubuntu codename (e.g., `jammy` for 22.04, `focal` for 20.04).

### AlmaLinux

Create or edit repository files in `/etc/yum.repos.d/`. For example, create `local-almalinux.repo`:

```
[baseos]
name=AlmaLinux $releasever - BaseOS
baseurl=http://your-server-ip:8680/almalinux/$releasever/BaseOS/$basearch/os/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-AlmaLinux-$releasever

[appstream]
name=AlmaLinux $releasever - AppStream
baseurl=http://your-server-ip:8680/almalinux/$releasever/AppStream/$basearch/os/
enabled=1
gpgcheck=1
gpgkey=file:///etc/pki/rpm-gpg/RPM-GPG-KEY-AlmaLinux-$releasever
```

### FreeBSD

Edit `/usr/local/etc/pkg/repos/FreeBSD.conf`:

```
FreeBSD: {
  url: "http://your-server-ip:8680/freebsd/packages/FreeBSD:${ABI}/latest",
  enabled: yes
}
```

Or for quarterly packages:

```
FreeBSD: {
  url: "http://your-server-ip:8680/freebsd/packages/FreeBSD:${ABI}/quarterly",
  enabled: yes
}
```

### F-Droid (Android)

In the F-Droid app:
1. Go to Settings → Repositories
2. Add a new repository
3. Enter: `http://your-server-ip:8680/fdroid/repo`
4. Add the repository

**Note**: After configuring any package manager, update your package lists:
- Arch: `pacman -Syy`
- Fedora/AlmaLinux: `dnf clean all && dnf makecache`
- Ubuntu: `apt update`
- FreeBSD: `pkg update`
- F-Droid: Refresh repositories in the app

## Configuration

- **Port**: 80 inside container, mapped to 8680 externally
- **Caching**: Configured for optimal package repository performance
- **Autoindex**: Directory browsing enabled for easy navigation
- **Security**: Basic security headers included
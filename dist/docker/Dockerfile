# syntax=docker/dockerfile:1
#
# Reproduces the Linux build environment from .github/workflows/build.yml /
# .github/actions/setup-lazarus for local development against Trndi's
# `develop` branch. Supports both amd64 and arm64 (picking the matching CI
# matrix entry's Lazarus/FPC packages for each). This is a dev convenience
# image, not something CI itself uses.
#
# Build:
#   docker build -t trndi-dev -f dist/docker/Dockerfile .
#
# Run (entrypoint clones/updates `develop`, builds it via the repo's own
# Makefile, then hands off to a shell inside the checkout):
#   docker run -it --rm trndi-dev
#
# Persist the checkout across runs with
# a named volume, so repeat runs only need to fetch changed sources:
#   docker run -it --rm -v trndi-checkout:/root/trndi trndi-dev

FROM ubuntu:26.04

ENV DEBIAN_FRONTEND=noninteractive

# Same package set as .github/actions/setup-lazarus (qt6-base-dev/libqt6pas*
# need a fairly recent Ubuntu archive), plus git/make/build-essential/
# ca-certificates which the GitHub-hosted runner image provides for free but a
# bare container does not.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates curl p7zip-full pkg-config git make build-essential cmake ninja-build \
      qt6-base-dev qt6-tools-dev qt6-base-dev-tools \
      libqt6pas6 libqt6pas-dev \
      libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Some Ubuntu versions need this symlink for FPC's linker to find -lgcc (see
# guides/BUILDING.md's "Ubuntu notice"). Best-effort: skip quietly if the
# expected file layout isn't there.
RUN gcclib="$(find /usr/lib/gcc -maxdepth 3 -name 'libgcc.a' 2>/dev/null | head -n1)"; \
    if [ -n "$gcclib" ] && [ ! -e /usr/lib/libgcc.a ]; then ln -s "$gcclib" /usr/lib/libgcc.a; fi

# Lazarus/FPC toolchain: the same .deb packages CI uses, per architecture (see
# the linux-amd64/linux-arm64 matrix entries in .github/workflows/build.yml).
# The architecture comes from `dpkg --print-architecture` (i.e. what this image
# actually is) rather than BuildKit's TARGETARCH, which is empty under the
# classic builder and can otherwise disagree with the running container -
# installing amd64 debs into an arm64 image leaves no lazbuild and a confusing
# failure much later, at `docker run` time.
ARG FPC_LAZ_URL_AMD64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/fpc-laz_3.2.2-210709_amd64.deb/download"
ARG FPC_SRC_URL_AMD64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/fpc-src_3.2.2-210709_amd64.deb/download"
ARG LAZ_URL_AMD64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/lazarus-project_4.8.0-0_amd64.deb/download"
ARG FPC_LAZ_URL_ARM64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20arm64%20DEB/Lazarus%204.8/fpc-laz_3.2.3-240813_arm64.deb/download"
ARG FPC_SRC_URL_ARM64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20arm64%20DEB/Lazarus%204.8/fpc-src_3.2.3-240813_arm64.deb/download"
ARG LAZ_URL_ARM64="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20arm64%20DEB/Lazarus%204.8/lazarus-project_4.8.0-0_arm64.deb/download"

RUN set -eu; \
    arch="$(dpkg --print-architecture)"; \
    echo "==> Installing Lazarus/FPC for architecture: $arch"; \
    case "$arch" in \
      amd64) fpc_laz="$FPC_LAZ_URL_AMD64"; fpc_src="$FPC_SRC_URL_AMD64"; laz="$LAZ_URL_AMD64" ;; \
      arm64) fpc_laz="$FPC_LAZ_URL_ARM64"; fpc_src="$FPC_SRC_URL_ARM64"; laz="$LAZ_URL_ARM64" ;; \
      *) echo "Unsupported architecture: $arch" >&2; exit 1 ;; \
    esac; \
    curl -fsSL -o /tmp/fpc-laz.deb "$fpc_laz"; \
    curl -fsSL -o /tmp/fpc-src.deb "$fpc_src"; \
    curl -fsSL -o /tmp/lazarus.deb "$laz"; \
    dpkg -i /tmp/fpc-laz.deb /tmp/fpc-src.deb /tmp/lazarus.deb || true; \
    apt-get update && apt-get -f install -y --no-install-recommends; \
    rm -f /tmp/*.deb && rm -rf /var/lib/apt/lists/*; \
    command -v lazbuild >/dev/null 2>&1 || { echo "lazbuild not found after install (wrong arch or broken .deb?)" >&2; exit 1; }; \
    echo "==> lazbuild installed at $(command -v lazbuild)"

ENV TRNDI_REPO="https://github.com/slicke/trndi.git" \
    TRNDI_BRANCH="develop" \
    TRNDI_DIR="/root/trndi"

COPY dist/docker/entrypoint.sh /usr/local/bin/trndi-entrypoint.sh
RUN chmod +x /usr/local/bin/trndi-entrypoint.sh

WORKDIR /root/trndi
ENTRYPOINT ["/usr/local/bin/trndi-entrypoint.sh"]
CMD ["bash"]

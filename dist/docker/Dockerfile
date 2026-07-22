# syntax=docker/dockerfile:1
#
# Reproduces the Linux (amd64) build environment from
# .github/workflows/build.yml / .github/actions/setup-lazarus for local
# development against Trndi's `develop` branch. This is a dev convenience
# image, not something CI itself uses.
#
# Build:
#   docker build -t trndi-dev -f dist/docker/Dockerfile .
#
# Run (entrypoint clones/updates `develop`, builds it via the repo's own
# Makefile, then hands off to a shell inside the checkout):
#   docker run -it --rm trndi-dev
#
# Persist the checkout (and the fetched mORMot2/static deps) across runs with
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
RUN gcclib="$(find /usr/lib/gcc/x86_64-linux-gnu -maxdepth 2 -name 'libgcc.a' 2>/dev/null | head -n1)"; \
    if [ -n "$gcclib" ] && [ ! -e /usr/lib/libgcc.a ]; then ln -s "$gcclib" /usr/lib/libgcc.a; fi

# Lazarus/FPC toolchain: the same .deb packages as the CI linux-amd64 matrix
# entry (.github/workflows/build.yml). Override with --build-arg if those URLs
# move to a newer Lazarus release.
ARG FPC_LAZ_URL="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/fpc-laz_3.2.2-210709_amd64.deb/download"
ARG FPC_SRC_URL="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/fpc-src_3.2.2-210709_amd64.deb/download"
ARG LAZ_URL="https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%204.8/lazarus-project_4.8.0-0_amd64.deb/download"

RUN curl -fsSL -o /tmp/fpc-laz.deb "$FPC_LAZ_URL" \
    && curl -fsSL -o /tmp/fpc-src.deb "$FPC_SRC_URL" \
    && curl -fsSL -o /tmp/lazarus.deb "$LAZ_URL" \
    && (dpkg -i /tmp/fpc-laz.deb /tmp/fpc-src.deb /tmp/lazarus.deb || true) \
    && apt-get update && apt-get -f install -y --no-install-recommends \
    && rm -f /tmp/*.deb && rm -rf /var/lib/apt/lists/*

ENV TRNDI_REPO="https://github.com/slicke/trndi.git" \
    TRNDI_BRANCH="develop" \
    TRNDI_DIR="/root/trndi"

COPY dist/docker/entrypoint.sh /usr/local/bin/trndi-entrypoint.sh
RUN chmod +x /usr/local/bin/trndi-entrypoint.sh

WORKDIR /root/trndi
ENTRYPOINT ["/usr/local/bin/trndi-entrypoint.sh"]
CMD ["bash"]

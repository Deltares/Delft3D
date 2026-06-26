# Devcontainer image for the nvfortran (NVHPC) proof-of-concept build of D-Flow FM.
#
# Bases on the minimal nvfortran third-party-libs image. Build that first, e.g.:
#   docker build -f ci/dockerfiles/linux/third-party-libs-nvhpc.Dockerfile \
#                -t delft3d-third-party-libs:nvhpc .
# then (re)open this devcontainer.
#
# The base is Ubuntu (the NVHPC SDK image), so package management is apt, not dnf.

ARG THIRD_PARTY_LIBS_IMAGE=delft3d-third-party-libs:nvhpc
FROM ${THIRD_PARTY_LIBS_IMAGE} AS base

ARG USER_UID=1000
ENV DEBIAN_FRONTEND=noninteractive
SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

# Developer convenience tooling.
RUN <<"EOF"
apt-get update
apt-get install -y --no-install-recommends \
    sudo dos2unix tree strace git curl ca-certificates less
rm -rf /var/lib/apt/lists/*
EOF

# Create non-root user with passwordless sudo.
RUN <<"EOF"
useradd --uid ${USER_UID} -ms /bin/bash dev
echo "dev ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/dev
chmod 0440 /etc/sudoers.d/dev
EOF

USER dev
WORKDIR /home/dev

# Mount points for bash history, MinIO credentials and Conan.
RUN <<"EOF"
mkdir -p ~/.history ~/.aws ~/.conan2
touch ~/.history/.bash_history
EOF

# Install uv and a couple of Fortran dev tools (mirrors the Intel devcontainer).
ENV PATH=/home/dev/.local/bin:$PATH UV_LINK_MODE=copy
RUN <<"EOF"
curl -LsSf https://astral.sh/uv/install.sh | sh
export UV_PYTHON_DOWNLOADS=never
uv tool install fortls || true
uv tool install fprettify || true
EOF

# Interactive bash environment + toolchain exports for the nvfortran PoC.
RUN cat >> /home/dev/.bashrc <<'EOF'
# Bash history
export PROMPT_COMMAND='history -a'
export HISTFILE=/home/dev/.history/.bash_history

# nvfortran PoC toolchain (C/C++ via GNU, Fortran via nvfortran, MPI via bundled OpenMPI).
export OMPI_CC=gcc OMPI_CXX=g++ OMPI_FC=nvfortran
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH

alias ls='ls --color=auto'
export PS1='[\u@\h \w]\n\$ '
EOF

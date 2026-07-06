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
    sudo dos2unix tree strace git curl ca-certificates less unzip
rm -rf /var/lib/apt/lists/*
EOF

# Create non-root user with passwordless sudo.
RUN <<"EOF"
useradd --uid ${USER_UID} -ms /bin/bash dev
echo "dev ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/dev
chmod 0440 /etc/sudoers.d/dev
EOF

# --- ninja build tool --------------------------------------------------------
FROM base AS ninja

RUN --mount=type=cache,target=/var/cache/ninja <<"EOF"
set -eo pipefail

URL=https://github.com/ninja-build/ninja/releases/download/v1.12.1/ninja-linux.zip
INSTALLER_DIR=/var/cache/ninja/$(basename $(dirname "$URL"))
if [[ -d "$INSTALLER_DIR" ]]; then
    echo "CACHED $INSTALLER_DIR"
else
    mkdir -p $INSTALLER_DIR
    wget --quiet --output-document="${INSTALLER_DIR}/ninja-linux.zip" $URL
fi

pushd $INSTALLER_DIR
unzip ninja-linux.zip
chmod +x ninja
mv ninja /usr/bin/
popd
EOF

# --- CMake build tool --------------------------------------------------------
FROM ninja AS cmake

RUN --mount=type=cache,target=/var/cache/cmake <<"EOF"
set -eo pipefail

URL=https://github.com/Kitware/CMake/releases/download/v4.2.3/cmake-4.2.3-linux-x86_64.sh
INSTALLER_DIR=/var/cache/cmake/$(basename -s '.sh' "$URL")
if [[ -d "$INSTALLER_DIR" ]]; then
    echo "CACHED $INSTALLER_DIR"
else
    mkdir -p $INSTALLER_DIR
    wget --quiet --output-document="${INSTALLER_DIR}/install_cmake.sh" $URL
fi

pushd $INSTALLER_DIR
sh install_cmake.sh --skip-license --prefix=/usr
popd
EOF

RUN --mount=type=cache,target=/var/cache/src/,id=xerces-c-nvhpc <<"EOF-xerces-c"
set -eo pipefail

URL='https://github.com/apache/xerces-c/archive/refs/tags/v3.2.5.tar.gz'
BASEDIR='xerces-c-3.2.5'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

mkdir --parents "/var/cache/src/${BASEDIR}/build"
pushd "/var/cache/src/${BASEDIR}/build"
if [[ "$DEBUG" = "0" ]]; then
    cmake .. -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ \
        -DCMAKE_C_FLAGS="-O3 -DNDEBUG -fPIC" -DCMAKE_CXX_FLAGS="-O3 -DNDEBUG -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Release
else
    cmake .. -DCMAKE_C_COMPILER=gcc -DCMAKE_CXX_COMPILER=g++ \
        -DCMAKE_C_FLAGS="-g -O0 -fPIC" -DCMAKE_CXX_FLAGS="-g -O0 -fPIC" \
        -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_BUILD_TYPE=Debug
fi
make --jobs=$(nproc)
make install
popd
EOF-xerces-c


RUN --mount=type=cache,target=/var/cache/src/,id=pugixml-nvhpc <<"EOF-pugixml"
set -eo pipefail

URL='https://github.com/zeux/pugixml/releases/download/v1.15/pugixml-1.15.tar.gz'
BASEDIR='pugixml-1.15'
if [[ -d "/var/cache/src/${BASEDIR}" ]]; then
    echo "CACHED ${BASEDIR}"
else
    echo "Fetching ${URL}..."
    wget --quiet --output-document=- "$URL" | tar --extract --gzip --file=- --directory='/var/cache/src'
fi

pushd "/var/cache/src/${BASEDIR}"

[[ $DEBUG = "0" ]] && BUILD_TYPE="Release" || BUILD_TYPE="Debug"

cmake -S . -B build \
    -D CMAKE_BUILD_TYPE=$BUILD_TYPE \
    -D BUILD_SHARED_LIBS=ON \
    -D CMAKE_INSTALL_PREFIX=/usr/local \
    -D CMAKE_INSTALL_LIBDIR=lib

cmake --build build --parallel $(nproc)
cmake --install build
popd
EOF-pugixml


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
export UV_PYTHON_DOWNLOADS=automatic

# Pin the Python runtime used by build/test tasks and ensure python3.14 resolves.
uv python install 3.14
ln -sf "$(uv python find 3.14)" /home/dev/.local/bin/python3.14

# Install build tools from uv-managed environments so versions stay reproducible.
uv tool install --python 3.14 conan
uv tool install --python 3.14 cmake

# Nice-to-have developer tooling.
uv tool install --python 3.14 fortls || true
uv tool install --python 3.14 fprettify || true
EOF

# Interactive bash environment + toolchain exports for the nvfortran PoC.
RUN cat >> /home/dev/.bashrc <<'EOF'
# Bash history
export PROMPT_COMMAND='history -a'
export HISTFILE=/home/dev/.history/.bash_history

# nvfortran PoC toolchain (C/C++ via GNU, Fortran via nvfortran, MPI via bundled OpenMPI).
export OMPI_CC=gcc OMPI_CXX=g++ OMPI_FC=nvfortran
export DELFT3D_CONAN_PROFILE=delft3d_ubuntu22_nvhpc
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH

alias ls='ls --color=auto'
export PS1='[\u@\h \w]\n\$ '
EOF

ARG THIRD_PARTY_LIBS_TAG=oneapi-2024-ifx-release

FROM containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:${THIRD_PARTY_LIBS_TAG} AS base

ARG USER_UID=1000

ARG THIRD_PARTY_LIBS_TAG
ARG ENABLE_JAVA=0
ARG CACHE_SUFFIX=cache-${THIRD_PARTY_LIBS_TAG}-java=${ENABLE_JAVA}

# Set bash as the default shell for RUN commands.
SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

# Install some tools from the almalinux package manager
RUN --mount=type=cache,target=/var/cache/dnf,sharing=locked,id=dnf-install-${CACHE_SUFFIX} <<"EOF"
PACKAGES=( \
    sudo strace tree netcdf dos2unix gh jq \
    libxml2 xmlstarlet ripgrep fd-find bat \
)
[[ "${ENABLE_JAVA}" == "0" ]] || PACKAGES+=(java-17-openjdk java-17-openjdk-devel maven)

dnf config-manager --add-repo https://cli.github.com/packages/rpm/gh-cli.repo
dnf install -y "${PACKAGES[@]}"

if [[ "${ENABLE_JAVA}" != "0" ]]; then
    update-alternatives --set java java-17-openjdk.x86_64
    echo 'JAVA_HOME=/usr/lib/jvm/java-17-openjdk' >> /etc/bashrc
fi
EOF

# Create non-root user and grant it sudo privileges:
# See: https://code.visualstudio.com/remote/advancedcontainers/add-nonroot-user
RUN <<"EOF"
useradd --uid $USER_UID -ms /bin/bash dev
echo "dev ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/dev
chmod 0440 /etc/sudoers.d/dev
EOF

# Change user from 'root' to 'dev', and set 'dev's home directory as the image workdir.
USER dev
WORKDIR /home/dev

# Create mount points for bash history, MinIO credentials, and Conan in the user's home directory.
RUN <<"EOF"
mkdir ~/.history ~/.aws ~/.conan2 ~/.config
touch ~/.history/.bash_history
EOF

# Add `uv` binaries in `dev` home folder to the PATH
ENV PATH=/home/dev/.local/bin:$PATH UV_LINK_MODE=copy

# Use `uv` to install some tools.
RUN <<"EOF"
export UV_PYTHON_DOWNLOADS=never
uv tool install fortls
uv tool install fprettify
EOF

# Install `teamcity` command line tool.
RUN curl -fsSL https://jb.gg/tc/install | bash -s -- v1.3.0 /home/dev/.local/bin

# Configure interactive bash environment.
RUN cat >> /home/dev/.bashrc <<EOF
# Bash history
export PROMPT_COMMAND='history -a'
export HISTFILE=/home/dev/.history/.bash_history

# Git completion and prompt support.
source /usr/share/bash-completion/completions/git
source /usr/share/git-core/contrib/completion/git-prompt.sh

# Aliases
alias ls='ls --color=auto'

# Set bash prompt.
export PS1='[\u@\h \w\$(__git_ps1 " (%s)")]\n\\$ '
EOF

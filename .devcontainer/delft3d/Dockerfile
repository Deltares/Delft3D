FROM containers.deltares.nl/delft3d-dev/delft3d-third-party-libs:oneapi-2024-ifx-release AS base

# Set bash as the default shell for RUN commands.
SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

# Install some tools from the almalinux package manager
RUN --mount=type=cache,target=/var/cache/dnf,sharing=locked <<"EOF"
dnf install -y sudo git dos2unix which less tree \
    java-17-openjdk java-17-openjdk-devel maven
update-alternatives --set java java-17-openjdk.x86_64
EOF

ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk

# Create 'dev' user and grant it sudo privileges
RUN <<"EOF"
useradd -ms /bin/bash dev
echo "dev ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/dev
chmod 0440 /etc/sudoers.d/dev
EOF

# Switch to user 'dev'
WORKDIR /home/dev
USER dev

# Install uv, python, and python tools.
RUN curl -LsSf https://astral.sh/uv/0.8.4/install.sh | sh
ENV PATH=/home/dev/.local/bin:$PATH
RUN uv python install 3.12
RUN uv tool install fortls

# Install git prompt and completion scripts.
ADD --chmod=755 --chown=dev:dev \
    https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash \
    https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh \
    /home/dev/.local/bin/

# Configure interactive bash environment.
RUN cat >> /home/dev/.bashrc <<"EOF"
# Source Intel OneApi 
source /opt/intel/oneapi/setvars.sh

# Set Compilers and library path
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH
export FC=mpiifx
export CXX=mpicxx
export CC=mpiicx

# Aliases
alias ls='ls --color=auto'

# Git completion and prompt support.
source /home/dev/.local/bin/git-completion.bash
source /home/dev/.local/bin/git-prompt.sh

# Set bash prompt.
export PS1='[\u@\h \w$(__git_ps1 " (%s)")]\n\$ '
EOF


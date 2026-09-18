# syntax=containers.deltares.nl/docker-proxy/docker/dockerfile:1.4

# note that although the BASE_IMAGE_URL argument allows you to easily change the base image,
# all the following code assumes that you're running an Alma Linux or compatible environment.
ARG BASE_IMAGE_URL=containers.deltares.nl/docker-proxy/almalinux/8-base:latest

FROM ${BASE_IMAGE_URL}

ARG PYTHON_VERSION=3.12

RUN curl -LsSf https://astral.sh/uv/0.11.19/install.sh | UV_INSTALL_DIR=/usr/bin sh

RUN <<EOF
set -eo pipefail

# Configure `uv` to install python and tools in the `/opt/uv` directory. So they are shared with all users.
export UV_PYTHON_INSTALL_DIR=/opt/uv/share/uv/python
export UV_PYTHON_BIN_DIR=/opt/uv/bin

# Install python and python tools.
uv python install ${PYTHON_VERSION} --default
EOF

# Add python and uv tools to PATH for all users.
ENV PATH=/opt/uv/bin:$PATH

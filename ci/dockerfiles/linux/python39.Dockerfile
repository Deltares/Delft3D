FROM containers.deltares.nl/base_linux_containers/8-base:latest

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Install system dependencies and clean up packages afterwards
RUN dnf update --assumeyes && \
    dnf install --assumeyes python39 && \
    dnf clean all && \
    rm --recursive --force /var/cache/dnf

# Install pip
RUN set -eo pipefail && \
    curl https://bootstrap.pypa.io/get-pip.py | python3.9 - && \
    python3.9 -m pip install --upgrade pip

CMD [ "python3.9" ]

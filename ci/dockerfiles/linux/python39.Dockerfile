FROM containers.deltares.nl/base_linux_containers/8-base:latest

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Install system dependencies and clean up packages afterwards
RUN <<"EOF"
    dnf update --assumeyes
    dnf install --assumeyes python39
    dnf clean all
    rm --recursive --force /var/cache/dnf
EOF

# Install pip
RUN <<"EOF"
    curl https://bootstrap.pypa.io/get-pip.py | python3.9 -
    python3.9 --module pip install --upgrade pip
EOF

CMD [ "python3.9" ]

FROM containers.deltares.nl/delft3d-dev/almalinux:8.10

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Install system dependencies and clean up packages afterwards
RUN <<"EOF"
    dnf update -y 
    dnf install -y python39
    dnf clean all
    rm -rf /var/cache/dnf
EOF

# Install pip
RUN <<"EOF" 
    curl https://bootstrap.pypa.io/get-pip.py | python3.9 -
    python3.9 -m pip install --upgrade pip
EOF

CMD [ "python3.9" ]
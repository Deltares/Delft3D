FROM dimrset

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/
ADD test/deltares_testbench/pip/lnx-requirements.txt /tmp/lnx-requirements.txt

# Install system dependencies and clean up packages afterwards
RUN <<"EOF"
    dnf update --assumeyes
    dnf install --assumeyes python39 expect
    dnf clean all
    rm --recursive --force /var/cache/dnf
EOF

# Install pip
RUN <<"EOF"
    curl https://bootstrap.pypa.io/get-pip.py | python3.9 -
    python3.9 --module pip install --upgrade pip
    python3.9 --module pip install --requirement "/tmp/lnx-requirements.txt"
    rm --verbose "/tmp/lnx-requirements.txt"
EOF

CMD [ "python3.9" ]

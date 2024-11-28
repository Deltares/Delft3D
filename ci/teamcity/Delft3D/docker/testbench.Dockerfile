FROM dimrset

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/
ADD test/deltares_testbench/pip/lnx-requirements.txt /tmp/lnx-requirements.txt

# Install system dependencies and clean up packages afterwards
RUN <<"EOF"
    dnf update -y 
    dnf install -y python39 expect
    dnf clean all
    rm -rf /var/cache/dnf
EOF

# Install pip
RUN <<"EOF" 
    curl https://bootstrap.pypa.io/get-pip.py | python3.9 -
    python3.9 -m pip install --upgrade pip
    python3.9 -m pip install -r "/tmp/lnx-requirements.txt"
    rm -v "/tmp/lnx-requirements.txt"
EOF

CMD [ "python3.9" ]
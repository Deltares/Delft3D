FROM runtime-container

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1
# Unset variable value because of libcurl version conflict.
ENV LD_LIBRARY_PATH=

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/
ADD test/deltares_testbench/pip/lnx-requirements.txt /tmp/lnx-requirements.txt

# Install system dependencies and clean up packages afterwards
RUN set -eo pipefail && \
    dnf update --assumeyes && \
    # Install Python (try 3.12 first, then 3.11, then fallback to default python3)
    (dnf install --assumeyes python3.12 expect && ln -sf /usr/bin/python3.12 /usr/bin/python3) || \
    (dnf install --assumeyes python3.11 expect && ln -sf /usr/bin/python3.11 /usr/bin/python3) || \
    dnf install --assumeyes python3 expect && \
    dnf clean all && \
    rm --recursive --force /var/cache/dnf

# Install pip
RUN set -eo pipefail && \
    curl https://bootstrap.pypa.io/get-pip.py | python3 - && \
    python3 -m pip install --upgrade pip && \
    python3 -m pip install --requirement "/tmp/lnx-requirements.txt" && \
    rm --verbose "/tmp/lnx-requirements.txt"

ENV LD_LIBRARY_PATH=/opt/dimrset/lib

CMD [ "python3" ]

FROM runtime-container

COPY --from=ghcr.io/astral-sh/uv:0.13.4 /uv /usr/local/bin/uv

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1
ENV UV_PYTHON_INSTALL_DIR=/opt/python
ENV PATH=/usr/local/bin:$PATH
# Unset variable value because of libcurl version conflict.
ENV LD_LIBRARY_PATH=

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/
ADD test/deltares_testbench/pip/lnx-requirements.txt /tmp/lnx-requirements.txt

# Install system dependencies and clean up packages afterwards
RUN set -eo pipefail && \
    dnf update --assumeyes && \
    dnf install --assumeyes ca-certificates expect && \
    dnf clean all && \
    rm --recursive --force /var/cache/dnf

# AlmaLinux 8 repositories do not provide Python 3.14.
RUN set -eo pipefail && \
    uv python install 3.14 && \
    ln -s "$(uv python find --managed-python 3.14)" /usr/local/bin/python3 && \
    python3 -c 'import sys; assert sys.version_info[:2] == (3, 14), sys.version'

# Install pip
RUN set -eo pipefail && \
    python3 -m ensurepip --upgrade && \
    python3 -m pip install --upgrade pip --break-system-packages && \
    python3 -m pip install --requirement "/tmp/lnx-requirements.txt" --break-system-packages --ignore-installed && \
    rm --verbose "/tmp/lnx-requirements.txt"

ENV LD_LIBRARY_PATH=/opt/dimrset/lib

CMD [ "python3" ]

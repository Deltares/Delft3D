FROM runtime-container

COPY --from=ghcr.io/astral-sh/uv:0.13.4 /uv /usr/local/bin/uv

ENV UV_NO_CACHE=1
ENV UV_PYTHON_INSTALL_DIR=/opt/python
ENV UV_PYTHON_BIN_DIR=/usr/local/bin
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
    uv python install 3.14 --default && \
    python3 -c 'import sys; assert sys.version_info[:2] == (3, 14), sys.version'

# Install directly into the managed interpreter in this dedicated test container.
RUN set -eo pipefail && \
    uv pip install --python /usr/local/bin/python3 --system --break-system-packages \
        --requirement "/tmp/lnx-requirements.txt" && \
    rm --verbose "/tmp/lnx-requirements.txt"

ENV LD_LIBRARY_PATH=/opt/dimrset/lib

CMD [ "python3" ]

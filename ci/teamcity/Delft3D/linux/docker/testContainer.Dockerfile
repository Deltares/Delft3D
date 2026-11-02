FROM runtime-container

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1
# Unset variable value because of libcurl version conflict.
ENV LD_LIBRARY_PATH=

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/
ADD test/deltares_testbench/pyproject.toml test/deltares_testbench/uv.lock /tmp/testbench/

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
    python3 -m pip install --upgrade pip --break-system-packages && \
    # Export the locked testbench dependencies (uv.lock) to a requirements file, install them with pip,
    # then remove uv again.
    python3 -m pip install uv==0.13.6 --break-system-packages && \
    uv export --project /tmp/testbench --frozen --no-hashes --no-emit-project --no-header --no-annotate --format requirements-txt --output-file /tmp/requirements.txt && \
    python3 -m pip install --requirement "/tmp/requirements.txt" --break-system-packages --ignore-installed && \
    python3 -m pip uninstall --yes uv --break-system-packages && \
    rm --recursive --force --verbose /tmp/testbench /tmp/requirements.txt

ENV LD_LIBRARY_PATH=/opt/dimrset/lib

CMD [ "python3" ]

ARG PYTHON_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-python
ARG PYTHON_IMAGE_TAG=alma8-python3.14

FROM ${PYTHON_IMAGE_URL}:${PYTHON_IMAGE_TAG} AS python

ARG DELTARES_TESTBENCH_VERSION=4.0.0
ARG PYPI_INDEX_URL=https://internal-artifacts.deltares.nl/repository/python-internal/simple/

# `tcl` is required for the mormerge testbench
RUN --mount=type=cache,id=dnf-cache-${PYTHON_IMAGE_TAG},target=/var/cache/dnf \
    dnf update --assumeyes && \
    dnf install --assumeyes tcl

ENV UV_TOOL_DIR=/opt/uv/share/uv/tools
ENV UV_TOOL_BIN_DIR=/opt/uv/bin

# Install testbench from the custom pypi index.
RUN --mount=type=secret,id=pypi_username,env=UV_INDEX_DELTARES_USERNAME,required=true \
    --mount=type=secret,id=pypi_password,env=UV_INDEX_DELTARES_PASSWORD,required=true \
    --mount=type=cache,id=uv-cache,target=/root/.cache/uv \
    uv tool install "deltares-testbench==${DELTARES_TESTBENCH_VERSION}" \
        --index "deltares=${DELTARES_PYPI_INDEX_URL}"
        # should be --default-index instead of --index.

ENTRYPOINT [ "testbench" ]


# temporary build command:
# docker build -f testbench.Dockerfile . --secret id=deltares_pypi_username,env=UV_INDEX_DELTARES_USERNAME --secret id=deltares_pypi_password,env=UV_INDEX_DELTARES_PASSWORD --build-arg=DELTARES_TESTBENCH_VERSION=4.0.0rc2 --build-arg=DELTARES_PYPI_INDEX_URL=https://internal-artifacts.deltares.nl/repository/delft3d-python-dev/simple/ --tag localhost/testbench:4.0.0rc2
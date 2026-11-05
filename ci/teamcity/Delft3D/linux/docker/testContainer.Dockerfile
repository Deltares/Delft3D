ARG PYTHON_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-python
ARG PYTHON_IMAGE_TAG=alma8-python3.12

FROM ${PYTHON_IMAGE_URL}:${PYTHON_IMAGE_TAG} AS python

COPY test/deltares_testbench/pyproject.toml \
     test/deltares_testbench/uv.lock \
     /opt/testbench/

ENV UV_PROJECT_ENVIRONMENT=/opt/testbench-venv
ENV UV_LINK_MODE=copy

RUN uv sync \
    --locked \
    --project /opt/testbench/ \
    --no-dev \
    --no-install-project \
    --no-python-downloads

FROM runtime-container

# Used for (integration) testbench only
ADD wanda/bin/* /opt/dimrset/bin/
ADD wanda/lib/* /opt/dimrset/lib/

COPY --from=python /opt/uv/ /opt/uv/
COPY --from=python /opt/testbench-venv/ /opt/testbench-venv/

ENV PATH=/opt/testbench-venv/bin:/opt/uv/bin:$PATH
ENV LD_LIBRARY_PATH=/opt/dimrset/lib

CMD [ "python" ]

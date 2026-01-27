# syntax=containers.deltares.nl/docker-proxy/docker/dockerfile:1

FROM containers.deltares.nl/base_linux_containers/8-base:latest AS base

WORKDIR /testbench
RUN --mount=type=cache,target=/var/cache/dnf \
    dnf upgrade --assumeyes
RUN curl -sSL https://astral.sh/uv/0.7.3/install.sh | bash -
ENV PATH=/venv/bin:/root/.local/bin:$PATH
RUN uv venv --python=3.12 /venv

FROM base AS dev

RUN --mount=type=bind,source=pip/lnx-dev-requirements.txt,target=/testbench/pip/lnx-dev-requirements.txt \
    --mount=type=cache,target=/root/.cache/uv,id=uv-cache-dev \
    uv pip sync pip/lnx-dev-requirements.txt

COPY src ./src/
COPY test ./test/
COPY tools ./tools/
COPY configs/xsd/deltaresTestbench.xsd ./configs/xsd/
COPY TestBench.py pyproject.toml ./

FROM dev AS validate

RUN mkdir report
RUN ruff format --diff . > report/ruff_format.patch \
    || echo "The code is not formatted properly. Please inspect the patch file at 'report/ruff_format.patch'."
RUN ruff check \
    --select F4,F5,F6,F7,W,I \
    --output-format=junit \
    --output-file=report/ruff_check.xml \
    || echo "The linter found issues. Please inspect the report at 'report/ruff_check.xml'."
RUN pytest \
    --junitxml=report/pytest.xml \
    --cov-report=html:report/htmlcov \
    --cov=. \
    || echo "The unit tests failed. Please inspect the report at 'report/pytest.xml'."

FROM scratch AS report
COPY --from=validate /testbench/report/ ./

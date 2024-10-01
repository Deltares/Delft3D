ARG DIMRSET_TAG=oneapi-2024-ifort-release
FROM containers.deltares.nl/delft3d-dev/delft3d-dimrset:${DIMRSET_TAG} AS dimrset

FROM containers.deltares.nl/delft3d-dev/python:3.9

WORKDIR /testbench

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

COPY ./pip/lnx-requirements.txt ./pip/lnx-requirements.txt
RUN python3 -m pip install -r ./pip/lnx-requirements.txt

COPY --from=dimrset /delft3d/ ./data/engines/teamcity_artifacts/lnx64/
COPY configs/ ./configs/
COPY src/ ./src/
COPY thirdparty/ ./thirdparty/
COPY pyproject.toml TestBench.py ./

ENTRYPOINT [ "python3", "TestBench.py" ]
CMD ["--help"]
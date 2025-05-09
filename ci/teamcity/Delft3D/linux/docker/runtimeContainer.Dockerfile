ARG INTEL_ONEAPI_VERSION=2024
ARG BUILDTOOLS_IMAGE_URL=containers.deltares.nl/delft3d-dev/delft3d-buildtools
ARG BUILDTOOLS_IMAGE_TAG=oneapi-${INTEL_ONEAPI_VERSION}

FROM ${BUILDTOOLS_IMAGE_URL}:${BUILDTOOLS_IMAGE_TAG} as buildtools

FROM almalinux:8

# We have opted to not add any mpi executables to the released installers, but we do ship the mpi libraries in the installers (done by CMake).
# This choice was made since linux HPC clusters often offer their own specialized MPI executables.
# Therefore, we need to add the intel mpi executables to the container image that needs to run the tests.
ARG INTEL_MPI_PATH=/opt/intel/oneapi/mpi/latest/bin
COPY --from=buildtools ${INTEL_MPI_PATH}/hydra_* ${INTEL_MPI_PATH}/mpiexec* ${INTEL_MPI_PATH}/mpirun /opt/intel/mpi/bin/

ADD dimrset /opt/dimrset
ADD example /example

# For backwards compatibility with old docker scripts.
RUN mkdir -p /opt/delft3dfm_latest
RUN ln -s /opt/dimrset /opt/delft3dfm_latest/lnx64
ADD delft3dfm_latest_readme.txt /opt

RUN dnf --assumeyes update \
  && dnf --assumeyes install libgomp libfabric \
  && dnf clean all

ENV LD_LIBRARY_PATH=/opt/dimrset/lib
ENV PATH=/opt/dimrset/bin:/opt/intel/mpi/bin:$PATH
ENV PROC_DEF_DIR=/opt/dimrset/share/delft3d
ENV OMP_NUM_THREADS=1

ARG GIT_COMMIT=unknown
ARG GIT_BRANCH=unknown
LABEL delft3d-git-commit=$GIT_COMMIT
LABEL delft3d-git-branch=$GIT_BRANCH

# For backwards compatibility with old docker scripts.
RUN mkdir /data
WORKDIR /data
ADD readme.txt /data
CMD ["cat", "/data/readme.txt"]

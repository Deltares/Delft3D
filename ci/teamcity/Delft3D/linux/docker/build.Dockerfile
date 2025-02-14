FROM almalinux:8

ADD intel /opt/intel

ADD dimrset /opt/dimrset

RUN dnf --assumeyes update \
  && dnf --assumeyes install libgomp libfabric \
  && dnf clean all

ENV LD_LIBRARY_PATH=/opt/dimrset/lib:/opt/intel/mpi/lib
ENV PATH=/opt/dimrset/bin:/opt/intel/mpi/bin:$PATH
ENV OMP_NUM_THREADS=1

ARG GIT_COMMIT=unknown
ARG GIT_BRANCH=unknown
LABEL delft3d-git-commit=$GIT_COMMIT
LABEL delft3d-git-branch=$GIT_BRANCH

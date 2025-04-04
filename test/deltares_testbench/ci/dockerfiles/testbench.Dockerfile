FROM containers.deltares.nl/delft3d/test/python:3.9

WORKDIR /data

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1


# Copy the requirements.txt file into the container at /data
COPY test/deltares_testbench/pip/lnx-dev-requirements.txt /data/pip/

# Install Python dependencies
RUN python3.9 -m pip install -r pip/lnx-dev-requirements.txt

# Copy code and config.
COPY test/deltares_testbench/configs/ /data/configs/
COPY test/deltares_testbench/src/ /data/src/
COPY test/deltares_testbench/test/ /data/test/
COPY test/deltares_testbench/tools/ /data/tools/
COPY test/deltares_testbench/thirdparty/ /data/thirdparty/
COPY test/deltares_testbench/pyproject.toml test/deltares_testbench/TestBench.py /data/

# Set the default command to run pytest
CMD python3.9 -m pytest

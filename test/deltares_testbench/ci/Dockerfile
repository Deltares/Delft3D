FROM python:3.10.13

WORKDIR /data

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Install system dependencies and clean up packages afterwards
RUN apt-get update \
    && apt-get install -y libnetcdf-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy the requirements.txt file into the container at /data
COPY pip/lnx-dev-requirements.txt /data/pip/

# Install Python dependencies
RUN python3 -m pip install --upgrade pip \
    && pip3 install -r pip/lnx-dev-requirements.txt

# Copy code and config.
COPY configs/xsd/ /data/configs/xsd/
COPY src/ /data/src/
COPY test/ /data/test/
COPY tools/ /data/tools/
COPY pyproject.toml TestBench.py /data/

# Set the default command to run pytest
CMD python -m pytest

FROM almalinux:8

WORKDIR /data

ENV PIP_ROOT_USER_ACTION=ignore
ENV PIP_NO_CACHE_DIR=1

# Install system dependencies and clean up packages afterwards
RUN dnf update -y \
    && dnf install -y python39 \
    && rm -rf /var/lib/apt/lists/*

# Copy the requirements.txt file into the container at /data
COPY pip/lnx-dev-requirements.txt /data/pip/

# Install pip
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py \
    && python3.9 get-pip.py \
    && rm -f get-pip.py

# Install Python dependencies
RUN python3.9 -m pip install --upgrade pip \
    && python3.9 -m pip install -r pip/lnx-dev-requirements.txt

# Copy code and config.
COPY configs/ /data/configs/
COPY src/ /data/src/
COPY test/ /data/test/
COPY tools/ /data/tools/
COPY thirdparty/ /data/thirdparty/
COPY pyproject.toml TestBench.py /data/

# Set the default command to run pytest
CMD python3.9 -m pytest

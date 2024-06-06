FROM python:3.10.13

WORKDIR /data

# Install system dependencies and clean up packages afterwards
RUN apt-get update \
    && apt-get install -y libnetcdf-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy the requirements.txt file into the container at /data
COPY requirements.txt /data/

# Install Python dependencies
RUN python3 -m pip install --upgrade pip \
    && pip3 install -r requirements.txt

# Copy pytest configuration file
COPY pyproject.toml /data/

# Set the default command to run pytest
CMD python -m pytest

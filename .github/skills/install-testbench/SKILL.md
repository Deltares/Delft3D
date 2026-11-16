---
name: install-testbench
description: 'Perform all of the steps to get `TestBench.py` running.'
---

# Install `TestBench.py`

## When to use

- "Install the testbench"
- "Check if my testbench installation is up-to-date"

## What this skill does

It does all the set-up required to run `TestBench.py`:
- Creates the "venv" and installs the python dependencies in it.
- Makes sure the credentials for the MinIO bucket are in place.
- Sets up the symbolic link to the Delft3D binaries used to run the test cases.

## Steps

1. Create the "venv" in `/test/deltares_testbench`. Always create it in this folder.
If `uv` is installed you can use the following command with `/test/deltares_testbench`
as the working directory: `uv venv`. If `uv` is not installed, use:
`python -m venv .venv`

2. Install the dependencies. The exact (pinned) versions of all packages, for Windows and Linux, are stored in
`uv.lock` next to `pyproject.toml`.

If `uv` is available, run the following with `/test/deltares_testbench` as the working directory. It creates the
`.venv` itself if it does not exist yet, so activating the virtual environment is not needed for this step:
```bash
uv sync --locked --extra dev
```
`--locked` makes the command fail instead of silently changing `uv.lock` when it is out of date with `pyproject.toml`.
Leave out `--extra dev` if the development tools (pytest, ruff, mypy) are not needed.

If `uv` is not available, activate the virtual environment and use `pip`. Note that this does *not* use the pinned
versions, `pip` resolves the version ranges from `pyproject.toml` itself:

On Windows:
```powershell
.venv/Scripts/activate
pip install -e ".[dev]"
```
On Linux:
```bash
source .venv/bin/activate
pip install -e ".[dev]"
```

3. Make sure the credentials to access the _test case data_ are in place.
The _test case data_ is hosted in our [MinIO environment](https://s3-console.deltares.nl/).
It is an "S3-compatible" storage, but the _test case data_ is accessed through a 
third-party tool called [DVC](https://dvc.org). It needs credentials to be able
to access our bucket in MinIO: `delft3d-testbench`. Deltares employees can create
access keys on [this page](https://s3-console.deltares.nl/access-keys). 
The access keys should be stored in the home directory: `~/.aws/credentials`. If this
file does not exist yet, guide the user to create one. 
The content of this file should be as follows (`<access-key-id>` and `<secret-access-key>` need to be replaced):
```
[default]
aws_access_key_id=<access-key-id>
aws_secret_access_key=<secret-access-key>
```

4. Make sure there is a symbolic link in the _artifacts directory_. 
The _artifacts directory_ is `/test/deltares_testbench/data/engines/teamcity_artifacts`.
Create this directory if it does not yet exist.
`TestBench.py` searches for the binaries in this directory. The "name" and "target"
of the symbolic link depend on the platform. The target folder is the 
_install directory_ of the build (See the `build-delft3d` skill). On Windows the
install directory is `/install_fm-suite`. On Linux it is 
`/build_fm-suite_release/install/bin` (This one has the "Release" binaries)

To create the symbolic link: (Please use absolute paths for both `$INSTALL_DIR`
and `$ARTIFACTS_DIR`)

On Linux:
```bash
ln -s -T $INSTALL_DIR $ARTIFACTS_DIR/lnx64
```

On Windows
```powershell
new-item -target $INSTALL_DIR -itemtype SymbolicLink -Path $ARTIFACTS_DIR/x64  -force
```

Be aware that on Windows administrator privileges are required to create symbolic link.
Please have the user copy-paste the windows command in a powershell session with admin
permissions.
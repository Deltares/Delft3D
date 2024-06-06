# Deltares Testbench

## Installation
Before you can use the testbench you need to install the dependencies. You will need to have
python installed. At the time of this writing we require python with version greater than or
equal to '3.9'. Please consult the `requires-python` setting in `pyproject.toml` for the
latest version requirement of python. You can check which versions of python are in your path
and their versions using the following

Linux: 
```bash
which -a python python3  # To get a list of all versions of python in your current $PATH
python -V  # Prints the version of the first 'python' found in your $PATH
```

Windows:
```powershell
where.exe python  # Lists versions of python in your path. (.exe suffix required for powershell)
python -V
```

### Virtual environments (venv)
Each versions of python installed on your system has its own directory with installed
python packages. It is possible to install the dependencies of the testbench directly
to the package directory of your python installation. But this is not what we recommend.
If you have installed other software (and its dependencies) in that python installation
there is a chance you will run into package conflicts at some point. These conflicts can 
be difficult or even impossible to resolve without breaking something. Therefore we strongly
encourage the use of 'virtual environment' to keep the testbench dependencies separate
from the packages installed in your 'system' python environment.

Your virtual environment stores python packages in some directory on your computer. You
can store it anywhere you like. If you do not have a personal preference, you can
store the virtual environment in the same directory as this `README`. The following
command creates a virtual environment called `.venv` in this directory. The name
`.venv` is recommended because it is ignored in our git repository. You should not
commit your virtual environment to source control.

```bash
python -m venv .venv  # Create a virtual environment named `.venv`.
```

Before installing the requirements, you need to make sure you have 'activated' the
virtual environment. You can do so with the following command:

Linux: 
```bash
source .venv/bin/activate
```

Windows:
```powershell
.venv\Scripts\activate.ps1  # Powershell
.venv\Scripts\activate.bat  # cmd.exe
```

You will notice that this prepends the name of your virtual environment to your command line
prompt, to indicate that your virtual environment is active. It should look similar to
this:
```
(.venv) [<username>@<hostname> <cwd>] $ 
```

Your code editor of choice most likely has some functionality to automatically activate
virtual environments when opening a terminal window, so you don't have to do this
manually every time. Here's an example using vscode:

```json
{  // .vscode/settings.json
    "python.defaultInterpreterPath": "${workspaceFolder}/.venv/bin/python",
    "python.terminal.activateEnvironment": true,
    // Other settings...
}
```

### Installing the dependencies using `pip`
After activating your virtual environment, you're ready to install the dependencies of the
testbench:
```bash
pip install -r requirements.txt
```

Currently, the `requirements.txt` contains the dependencies needed for running the testbench,
but also for running the unit tests of the testbench. You can try running them to verify that
the dependencies have been installed:

```bash
python -m pytest
```

### Installing and pinning the dependencies using `uv`
`uv` is a tool that can generate a file listing 'pinned versions' of python packages without
actually installing any packages. It has separate steps for 'resolving' of package versions
and 'installing' the packages. The 'pinned' versions of packages can be saved in a file and
committed to source control. You can see some examples in the `pip` directory.
To install `uv`, follow the installation steps here: https://github.com/astral-sh/uv.
`uv` provides a similar command line interface as `pip-tools`. But it is not written in
python, so it's much faster.

The dependencies listed in `pyproject.toml` can be used by `uv` to generate the pinned
package versions. You only need to do this after you've adding a new python package to the 
`pyproject.toml` or updated the version constraints of the python packages
that are already there. If you just want to install the pinned python packages, you can
skip this step. Use `uv pip compile`:
```bash
# Resolve the python package versions listed in 'pyproject.toml' and save the result.
uv pip compile pyproject.toml --upgrade --output-file pip/lnx-requirements.txt
# Resolve the dependencies including the 'development dependencies' (linters, etc.)
uv pip compile pyproject.toml --upgrade --extra dev --output-file pip/lnx-dev-requirements.txt
```
Note: The pinned package versions are not entirely platform agnostic. For instance, 
sometimes Windows requires extra packages.

Use `uv pip sync` command to install the pinned dependencies:
```bash
uv pip sync pip/lnx-requirements.txt  # I don't need the development dependencies
uv pip sync pip/lnx-dev-requirements.txt  # Install all the packages.
```

Technically you can also use `pip install` to install the pinned dependencies. But `pip`
will also try to resolve the package versions, which is not necessary anymore because
`uv` took care of it in the `compile` command already.


### Why separate the 'dependencies' from 'development dependencies'
Developers need to do more than just run the testbench. They use tools to help write and
maintain the quality of the software, run unit tests, generate documentation, etc.
They should all be using the same tools with the same configuration. If, for instance,
developers have different settings for the amount of spaces in a tab, the
code will become a mess despite using automatic formatting tools.
The tools can also be used to automatically enforce coding style rules in the CI/CD
pipelines. This ensures that all the code committed to main adheres to the the rules.

People using or just testing the software don't need any of these tools to
run the software. So there's no point for them to install the development tools.
TeamCity build servers that are running the testbench also don't need to have
these tools installed.

### Why use version constraints?
Over time the python packages listed in our `requirements.txt` receive updates. We 
distinguish between three kinds of updates:

1. `PATCH`: Bugfixes, security updates
2. `MINOR`: New functionality, features
3. `MAJOR`: Breaking changes (changing or removing existing functionality)

Many python packages implement [semantic versioning](https://semver.org/). Where versions
are broken up into three numbers `<MAJOR>.<MINOR>.<PATCH>`. We would like to benefit 
automatically from the occasional bugfix or new feature of libraries that we use. Major
version updates cannot be done automatically. It requires us to verify
that we're still using the library correctly by going through a migration guide. 

Version constraints can be used to allow patches and minor version upgrades automatically,
but disallow automatic major version upgrades. The constraints are currently only in the 
`dependencies` section of our `pyproject.toml`.

### Why use version pinning?
Version pinning is tracking the exact versions of all the installed dependencies, including
sub-dependencies, in version control. This makes the installation repeatable. You can go
back to an earlier commit, reinstall the pinned dependencies, and the software should work
exactly as it did at the time of the commit. Version pinning eliminates problems
arising from differences in the versions of installed packages. These kind of problems can
be hard to solve.

The tools we use to pin the versions of packages can't produce a 'platform-independent' 
version of the pinned dependencies. Depending on the platform (Windows, Linux, or even
python version) you can get slightly different packages. Because it's important that
the testbench runs on both Windows and Linux, we produce a separate set of pinned
packages for each platform.

## Configuration tools in VSCode
Most IDEs have some kind of plugin system that can run our python tools automatically when
saving files containing python code.This requires you to install the 'development 
dependencies' in your virtual environment. You will need to install some additional plugins 
in VSCode:

- Python (Microsoft)
- Mypy Type Checker (Microsoft)
- Ruff (Astral Software)

`ruff`, `mypy` and `pytest` will read their configuration directly from the `pyproject.toml` 
file, so the VSCode plugins should require only minimal configuration.
Here's an example `.vscode/settings.json` that enables linting and formatting, test 
discovery and type checking. 


```json
{ // `.vscode/settings.json` in the `test/deltares_testbench` directory.
    "python.defaultInterpreterPath": "${workspaceFolder}/.venv/bin/python",
    "python.terminal.activateEnvironment": true,
    "python.testing.pytestEnabled": true,
    "python.testing.autoTestDiscoverOnSaveEnabled": true,
    "[python]": {
        "editor.formatOnSave": true,
        // Requires ruff, and the `Ruff` vscode  plugin
        "editor.defaultFormatter": "charliermarsh.ruff", 
        "editor.codeActionsOnSave": {
            "source.organizeImports": "always"
        }
    },
}
```
# Python CI tools

A Python package that contains Python scripts used in the Delft3D TeamCity project.

## Installation with uv (recommended)

If you don't have [`uv`](https://docs.astral.sh/uv/) installed, you can go 
[here](https://docs.astral.sh/uv/getting-started/installation/) 
and select your preferred method to install it. 
```bash
# From this directory
uv sync --extra all
```
The command above installs all dependencies, including the "development" dependencies
that you need during development. It includes the testing framework, linter, formatter
and type checker.
Unfortunately, the `--extra all` is necessary because we make use of the
`[project.optional-dependencies]` section in `pyproject.toml`, which `pip` also understands.

`uv` can manage your installed Python versions and virtual environments for you. If you don't
have at least (the required) Python 3.11 installed, you can install it with 
`uv python install 3.11`. You can
see which versions of python you have installed on your system with `uv python list`.
`uv` also creates a virtual environment or "venv" for you in this directory. You can either
activate it as usual like this:
```bash
source .venv/bin/activate  # On Linux
.\venv\Scripts\activate  # On Windows
```
Or you can run any command you want through the `uv` entrypoint like this:
```bash
uv run python -V
```
The above command runs `python -V` in `uv`'s virtual environment.

## Installation with pip

If you have the Delft3D git repository checked out, you can install this package with pip.
You need to have at least Python 3.11 installed on your system.
You can do this by passing it the path to the `ci/python` directory (which also contains
this README). If you're going to help develop the scripts inside this package, you should
install the package like this:

```bash
# From the Delft3D git repository root:
pip install --editable ./ci/python[all]
```

The `[all]` part in the above command instructs pip to install all of the optional dependencies.
You can see exactly what dependencies by reading the `[project.optional-dependencies]` section
in `pyproject.toml`. It includes the dependencies of all python scripts, including the 
"development" dependencies that you need to run the unit tests, the linter, type checker and
the formatter.

## Running tools

In our CI we enforce automatic quality control checks for the Python code in this package. 
We use `ruff`, a formatter and linter. `mypy`, a type checker. Finally we write unit tests 
for our code, which we run using `pytest`. All of these tools can read their 
options from the `pyproject.toml` file in this directory. To use them from the command line,
navigate to the `ci/python` directory and do one of the following:
- Activate the virtual environment where you installed the "development" dependencies and run
  the commands below.
- Prefix all of the commands below with `uv run` (If you have `uv` installed).
```bash
# Auto format all the files in this directory
ruff format

# Run the linter
ruff check

# Run the type checker (on all python files in the current working directory)
mypy .

# Run the unit tests
pytest
```

## Configuring your editor
I highly recommended you configure your editor to format your code when saving Python
files. And to highlight linter warnings and type errors. This should be possible with any 
modern editor. But of course you only have room for one of
those in your life at any given time. Currently, mine is VS Code, so I can only tell you how to 
configure that one.

Beware that these kind of instructions are prone to get outdated, so let us know if you run
into any problems. At some point in the future we can figure out how to set up development
containers, store all of the configuration in git and all of this stuff will be taken care off 
automatically. But until then these steps should help you on your way.

### Configuring VS code
You should download the recommended Python extensions from Microsoft, the 
"mypy-type-checker" extension from Microsoft, and the 'Ruff' extension from 'Charliermarsh'.
Then you can use the following template for your
`.vscode/settings.json`:

```json
{
    "python.defaultInterpreterPath": "<path-to-repo>/ci/python/.venv/bin/python",
    "python.testing.pytestEnabled": true,
    "[python]": {
        "editor.formatOnSave": true,
        "editor.defaultFormatter": "charliermarsh.ruff",
        "editor.codeActionsOnSave": {
            "source.organizeImports": "always"
        }
    },
}
```

Depending on where you store your `.vscode/settings.json` and where you store your virtual
environments, you may need to customize this file. I recommend you to create your `.venv`
and a `.vscode/settings.json` in the `ci` directory. If you're not already using
VS Code "workspaces", I recommend selecting the `ci` directory as a separate
workspace. That way, all the settings in the `.vscode/settings.json` in the `ci`
apply only to that directory. 
[Check out VS code workspace here](https://code.visualstudio.com/docs/editor/workspaces/workspaces).

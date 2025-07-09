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
have Python 3.11 or higher installed, you can install it with 
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
ruff format .

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


## Included tools

### MinIO synchronizer

The MinIO synchronizer program (`minio-sync`) is a command line program inspired
by the `aws s3 sync` subcommand of the AWS command line program. It 'synchronizes'
a local directory with files with objects in a MinIO bucket. `minio-sync` has two
required command line arguments: The `--source` and the `--destination`. You can
run it like this:
```bash
minio-sync --source ./path/to/files/ --destination s3://my-bucket/path/to/objects/
```
The command above will copy the files in `./path/to/files` to the `my-bucket` MinIO
bucket, under the prefix `/path/to/objects`. If the `./path/to/files` directory
contains subdirecties, `minio-sync` will keep the directory structure intact. So for 
example: It will upload `./path/to/files/sub/dir/README.md` as 
`s3://my-bucket/path/to/objects/sub/dir/README.md`. When `minio-sync` is done,
the contents of the source and destination should be the same.

The previous example used a local directory as the source and a MinIO prefix as the
destination. This will upload files from your computer to MinIO. If instead you
specify a local directory as the destination and a MinIO prefix as the source, the
objects in MinIO will be downloaded to the local directory:
```bash
minio-sync --source s3://my-bucket/path/to/objects/ --destination ./path/to/files/
```
Again, `minio-sync` will try to keep the directory structure intact. So it will
create subdirectories in `./path/to/files` if necessary.

In addition, if the source is a prefix in MinIO (so you're downloading the files), 
you can specify a `timestamp` to recover the files in MinIO from a specific point of 
time in the past. This is called "point in time recovery". This will only work for 
MinIO buckets that have versioning enabled:
```bash
minio-sync --source s3://my-bucket/objects --destination ./files --timestamp 2025-05-10
```
The timestamp should be an [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) formatted
timestamp. You can specify just the date, or a specific time of the day as well. For
instance `2025-05-10T12:34:56.789` specifies the timestamp to the millisecond. The
timestamp will be interpreted as being in the timezone of your computer, unless
explicitly specified otherwise. A UTC timestamp can be passed as well like this:
`2025-05-10T12:34:56Z`.

You can limit what kind of operations `minio-sync` will do to modify the files at the
destination. For example, if a file exists at the destination, but not at the source,
a faithful synchronization would remove the file at the destination. This can be a
destructive operation, so if you make a mistake you might lose data. These four command
line switches control the "operation mode":
- `--create-only`: Only create files that exist at the source but not at the destination.
- `--update-only`: Only update files that exist at both source and destination, but have different content
- `--no-delete`: The default mode. Perform "creations" and "updates", but do not delete
  files that exist at the destination but not at the source.
- `--delete`: Perform "creations", "updates" and "deletions". This mode provides a
  complete synchronization between the source and destination.

In addition you can specify "filters", so `minio-sync` will only operate on a subset
of the files at the source. Currently, you can specify filters using "glob" patterns
or "regex" patterns. These patterns will be applied to the "relative paths" of objects.
For example:
```bash
minio-sync --source s3://my-bucket/objects --destination ./files --glob '*.png'
```
Suppose the MinIO bucket contains the object `s3://my-bucket/object/cat_pictures/meow.png`.
The command above will apply the `*.png` glob pattern on `cat_pictures/meow.png`. It matches, 
so the file is downloaded to `./files/cat_pictures/meow.png`.

*NOTE*: Please always surround your glob patterns with single quotes (').
Otherwise your shell might try to 'expand' the glob pattern instead of passing it
to the program as is. For example:
```bash
minio-sync --source s3://my-bucket/objects --destination . --glob *.png
```
Might be expanded to...
```bash
minio-sync --source s3://my-bucket/objects --destination . --glob ape.png nut.png mies.png
```
...provided that you have those three PNG files in your current directory.
Surrounding the patterns in single quotes will prevent this.


It is possible to specify multiple filters. In that case, files will match if at least
one of the filters match. For example: The following command downloads all PNG files
_and_ JPEG files:
```bash
minio-sync --source s3://my-bucket/objects --destination ./files --glob '*.png' --regex '[.]jpe?g'
```
Note that we used a "regex" pattern to match JPEG files. So both `.jpeg` and `.jpg` files
will match.

Finally it's possible to "exclude" files based on a filter. For example if I'm not interested
in downloading NetCDF files. I can do the following:
```bash
minio-sync --source s3://my-bucket/objects --destination ./files --exclude-glob '*.nc'
```

You can specify any amount of "include" and "exclude" filters. The files will match if
they match _any_ include filter and _none_ of the exclude filters.

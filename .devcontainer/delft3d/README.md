# The Delft3D development container

## What is this?
[Development containers](https://containers.dev/) is an open standard 
allowing a [container](https://www.docker.com/resources/what-container/) to be 
used as a fully featured development environment. IDEs supporting this standard
can open the Delft3D source code project "inside a devcontainer". All of the tools 
you need for development can be installed in this container, so ideally you shouldn't need
to install anything else. These tools include but are not limited to:
- Compilers
- Build systems
- Debuggers
- Package managers
- Language servers
- Linters
- Formatters
- Profilers
- Version control tools

The [`devcontainer.json`](https://containers.dev/implementors/json_reference/) 
is a configuration file that your IDE should read to initialize the devcontainer.
It can either "pull" a ready-made container from a container registry, or build a 
container image with build instructions from a `Dockerfile`. The configuration file
may also contain a section with IDE extenions or plugins to install.

IDE extensions are usually a shell around the tools that add some integration with 
your IDE or add some visual annotations. Most importantly, extensions may add 
integration with your IDE's debugging and language server capabilities. 
Enabling powerful IDE features such as visual debugging, code navigation
and refactoring. There are also extensions that draw yellow or red 
"squiggly lines" under code where the linter found a problem, or that
enable syntax highlighting for certain source files. The extensions declared in 
`devcontainer.json` are isolated to your 'devcontainer' environment, and should be
installed automatically when opening the project in the devcontainer.

We want to make it easier for people to set up a development environment for Delft3D
on Linux. The large amount of required third-party libraries have made this difficult in
the past. Now that we're already using "build containers" in our CI pipelines to do our
builds, we can reuse them to make "development containers" as well. Any change made to the
build container can automatically be applied to the development container (provided
that a clean pull/rebuild of the development container is performed). This ensures that the
CI environment and the development environment are kept in sync. All of the dockerfiles
and devcontainer configuration should be tracked in this repository. So we can easily
plan, perform, review and share changes.

## Prerequisites

You need [Docker](https://docker.com) to build and run the devcontainer, and an
IDE that supports opening a project in the devcontainer. For supported IDEs you
can visit [containers.dev](https://containers.dev/supporting#editors) for an
up-to-date list. At the time of writing they are:
- Visual Studio Code
- Visual Studio
- IntelliJ IDEA

The steps required to open this project inside a devcontainer depend on the IDE.
To maximize your chances of success we suggest following the instructions on the site
of your IDE of choice. The `devcontainers.json` file contains a `customizations` section
that can be used to declare IDE plugins or extensions to install in the devcontainer
automatically. These "customizations" are specific for your IDE. At the time of writing
this section only contains extensions to install in Visual Studio Code. If you have
no preference for an IDE, you should probabily go with Visual Studio Code. If you have
experience using the devcontainer in other IDE's, feel free to suggest extensions to
add to the `devcontainers.json`. This file is tracked in git, so anything you add in this
file will be visible to everyone else once your changes to it get merged to the `main` branch.

### Docker
If you are on Windows, we assume you will have 
[WSL](https://learn.microsoft.com/en-us/windows/wsl/install) installed, along with a 
Linux distribution of your choice. You should be able to at least open a terminal 
inside the Linux distribution, or to open a directory inside the Linux distribution 
in your IDE of choice.

#### [Docker Desktop](https://www.docker.com/products/docker-desktop/)
One option is to install Docker Desktop on Windows. Docker Desktop comes with an
install wizard that will install Docker not just on Windows, but also in all Linux
distributions you have installed with WSL. The `docker` command line program is
available in both Windows and your WSL Linux distributions. In addition you will
get the Docker Desktop GUI in Windows, that you can use to configure your Docker
installation and to do most things that you can also do with the `docker` command
line tool. You are also able to switch between running 'Linux containers'
and 'Windows containers'. You need a license to use Docker Desktop. If you are a
Deltares employee you can request a Docker Desktop license by contacting Edward Melger.

#### Installing Docker in Linux (WSL or native)
Docker Desktop requires a license. But the official "Docker Engine", which includes
the `docker` command line tool and the "Docker daemon", is
[open source](https://docs.docker.com/engine/#licensing) software and doesn't require 
a commercial license to use. For this reason, some people may prefer to install Docker 
directly in their Linux distribution of choice (be it on Windows using WSL, or on a native 
Linux installation). Usually docker can be installed using the Linux distribution's standard
package manager. If you do that, you should be aware of the following:
1. You will only be able to run Linux containers (You can't run Windows containers on Linux).
2. In some Linux distributions, you will need to configure the package manager to install
   the official Docker Engine. Read [this page](https://docs.docker.com/engine/install/#installation-procedures-for-supported-platforms) 
   for instructions.
3. [Podman](https://docs.docker.com/engine/install/#installation-procedures-for-supported-platforms)
   is an alternative to Docker Engine. It provides the `podman` command line tool that acts
   as a drop-in replacement for the `docker` command line tool and supports most of the same
   options. The advantage of Podman is that, unlike Docker Engine, it does not require you
   to run a daemon with elevated privileges on your Linux system, and so it is more secure.
   On some Linux distribution, installing "docker" using the default package manager will
   actually install Podman instead.
   Podman supports most of the Docker Engine's features. But it is sometimes behind. We may
   end up relying on Docker Engine features that Podman does not support yet. We have run into
   compatibility issues between Podman and Docker Engine mostly when 'building' container
   images with [BuildKit](https://docs.docker.com/build/buildkit/) features. 

### Opening Delft3D 'in the devcontainer' in your IDE
Once you have Docker installed you can follow the instructions for your IDE to open the
Delft3D repository inside the devcontainer. If you are on Windows, we **strongly recommend**
cloning the Delft3D repository in your WSL Linux distribution's filesystem. Somewhere under
your home directory in Linux, for example (e.g. `/home/${USER}/repo/delft3d`). 
That means that you will most likely have two separate checkouts of the Delft3D repository on
your computer (one in your `C:\` drive and one in your WSL Linux distribution). Once you open 
the Delft3D repository inside the devcontainer, your IDE will 'mount' the directory containing 
the Delft3D repository on the 'host' inside the 'container'. For example, this means that the 
files in WSL Linux under `/home/${USER}/repo/delft3d` will be available in the container under 
`/workspaces/delft3d`. It is possible to mount a directory from the `C:\`
drive inside the container, but we **discourage** this for the following reasons:
1. The performance of file system operations (reading and writing files to disk) is noticably
   worse when a directory in the `C:\` drive is mounted inside the devcontainer. For example, we've noticed that compilation can take over four times longer than usual.
2. You have used Git on Windows to clone the Delft3D repository on the `C:\`
   drive. This means that most of the text files inside the repository have been 
   written to disk using 'Windows line endings' (`\r\n`) instead of 'Unix line endings' 
   (`\n`). Inside the devcontainer, some tools (most notably Git itself) will have trouble
   handling 'Windows line endings' in the text files.

After cloning the Delft3D repository to a directory on Linux, you should first open
that directory in your IDE. On Windows, Visual Studio Code requires a "WSL" extension
to open a directory in a WSL Linux distribution. You can search for this extension in
the 'Extensions' menu. While you are at it, you should also search for the "Dev Containers"
extension. You will need to install both of these extensions.

Deltares hosts it's own our 'container registry' at
[containers.deltares.nl](https://containers.deltares.nl). Our devcontainer is based on the
'third-party-libs' build container stored under the `delft3d-dev` project in 
`containers.deltares.nl`. You will need access to the `delft3d-dev` project before you can
use the build container. Then you need to login with the `docker` command line tool. You
can do so with the following command:
```bash
docker login --username $YOUR_EMAIL_ADDRESS --password $YOUR_CLI_SECRET containers.deltares.nl
```
When you log into the 
[web interface of `containers.deltares.nl`](https://containers.deltares.nl),
you can find the missing values you need to pass to the `--username` and `--password` 
arguments in your 'User Profile'.

If you feel uneasy about your "CLI secret" ending up in your bash history. You can instead save
it in a file somewhere (e.g. ~/.cli-secret) and use the following command:
```bash
cat ~/.cli-secret | docker login --username $YOUR_EMAIL_ADDRESS --password-stdin containers.deltares.nl
```

Now you're finally ready to open the Delft3D repository in the devcontainer. The first time
you do this may take a few minutes. The devcontainer extension must do the following:
1. Pull the 'third-party-libs' base container image from `containers.deltares.nl` (Which is quite large)
2. Run the instructions in the `Dockerfile` to create the final devcontainer image. 

Once the devcontainer image is done building it is cached on your computer. It will show
up in `docker image ls`. This will ensure
that the next time you open your IDE, it will connect to the devcontainer much faster. You
do not need to repeat the `docker login` command and wait for the container to build anymore,
unless you explicitly request to rebuild the container and pull the latest version of the
'third-party-libs' container from `containers.deltares.nl`.

Please don't hesitate to contact us if you need access to the `delft3d-dev` project in
`containers.deltares.nl`, or you run into any errors using or starting to use the
devcontainer. We have probably run into the same problems and are able to help you out.

## Configuring your IDE

### Visual Studio Code
Now that you are able to open Delft3D in the devcontainer it's time to start using it.
The first thing you can do to start exploring the devcontainer is to open a 
terminal in your IDE and look around a bit. Most likely, the terminal will open `bash` in the
`/workspaces/delft3d` directory. This is the directory containing the Delft3D repository.
Your IDE has mounted the directory from the Linux "host" (e.g. `/home/${USER}/repo/delft3d`) 
to the `/workspaces/delft3d` directory inside the container. Any changes that you
make to the files inside the `/workspaces/delft3d` directory are persisted in the
`/home/${USER}/repo/delft3d` directory in the host. If you restart your computer, the changes
you saved in `/workspaces/delft3d` will still be there. in Docker terminology this is called
a [bind mount](https://docs.docker.com/engine/storage/bind-mounts/).

Outside of `/workspaces/delft3d`, the file system in the container looks completely different
from the one in your host (WSL) Linux system. Inside the container, you are logged in as the
user `dev`. `dev`'s home directory is in `/home/dev` inside the container. While in the
container you can change the files inside `/home/dev`, but these changes will not be persisted.
If you restart your computer the files in this directory will be reset to how they were saved
inside the devcontainer image. The only way to persist changes to files inside a container is 
by using [volumes](https://docs.docker.com/engine/storage/volumes/) (or bind mounts).

The tools you need for development should already installed. You can check by running the
following commands:
```bash
git --version
cmake --version
ifx --version
```

If you are familiar with the command line tools you should be good to go. However, the real benefit
of the devcontainer is being able to use the IDE's editing and debugging capabilities with
the tools in the container. This requires language specific extensions, and configuring your
editor. In the case of Visual Studio Code, some common extensions you will likely need are
installed automatically. Some extensions will work right away. Other extensions will need
some configuration to work at all, or work in a way you are satisfied with. Visual Studio
Code configuration files live inside the `.vscode` directory in the root directory of the
repository. This directory is in our `.gitignore` file, because editor configuration is often
platform dependent, environment dependent, and dependent on the "personal preferences" of
the developer. We do provide example configuration files for Visual Studio Code in the 
`.devcontainer/delft3d/examples/.vscode-example/` directory. You can copy the contents of this directory
to the `.vscode/` directory (in the repository root) as a starting point.

We currently have two files inside the `.vscode/` directory:
- [`settings.json`](https://code.visualstudio.com/docs/configure/settings#_settings-json-file): VSCode settings specific for this project. Includes extension settings.
- [`launch.json`](https://code.visualstudio.com/docs/debugtest/debugging-configuration): Configuration for running and debugging code. These show up in the 'Debug' menu.

Thankfully, most extensions don't require any customization in `settings.json` and will work fine with
the default settings. There are a few exceptions, that we will list here:

#### The Modern Fortran extension
This extension provides syntax highlighting for Fortran source code and the Fortran language server: `fortls`
(which is pre-installed in the devcontainer). It is listed in the `devcontainer.json` file as one of the
extensions to install. Unfortunately, the extension fails to install when (re-)building the devcontainer. But
installing the extension 'manually' still works. Navigate to the "extensions" menu in VSCode and search for
"Modern Fortran". The "Install" button may be greyed out, with the following warning:

```⚠️ This extension is not signed by the Extension Marketplace.```

You can work around this by doing the following:
1. Clicking on the extension menu item.
2. Then clicking on the gear (⚙️) icon.
3. Clicking on "Install" in the context menu.

This may open a dialog box, repeating the warning that the extension is not signed by the extension marketplace.
But you can opt to install it anyway.

The developers of the extension have been made aware of the issue, but it doesn't seem fixed yet.
See: https://fortran-lang.discourse.group/t/cant-install-vscode-modern-fortran-not-signed-by-marketplace/8709

You can easily verify that the "Modern Fortan" extension is installed by opening any Fortran source code file.
By default the extension enables syntax highlighting. You may get a dialog in the bottom right of the screen
saying "Couldn't update fortls". You can ignore this warning. We don't want the extension to be able to update
`fortls` anyway.

#### The CMake VSCode extension
This extension enables the "CMake menu". Look for the CMake icon in the column at the far left
of the VSCode window. You can use this menu to configure, build and install our products.
However, it requires a file called `CMakePresets.json` in the CMake source directory to work
properly. There's an example `CMakePresets.json` file in `.devcontainer/delft3d/examples/cmake-example`.
You can copy this file to `src/cmake/CMakePresets.json`. The CMake extension will automatically
read this file, so you can use the extension to configure and build the project. The example currenlty
only has examples to configure and build the `fm-suite` product in `Debug` and `Release`. But this example
can be extended to build only `dflowfm` or other products.

The CMake extension will also discover any unit tests that have been added in the CMake configuration.
These tests show up in the "Test menu". And you should be able to run and debug individual unit tests
after a build.

We currently don't track `src/cmake/CMakePresets.json` in git because we have trouble making a
`CMakePresets.json` file that will work on both Linux and Windows without problems. Making two
separate platform specific preset files and configuring `CMake` to load a specific preset file from
a path other than `src/cmake/CMakePresets.json` is not supported at the moment.

##### Building `fm-suite` with CMake
You can use the CMake menu to select the "FM-suite Debug" or the "FM-suite Release" preset, listed in the example
`CMakePresets.json`. If you select the target `install` in the "Build" menu, then the install phase will run after the build has succeeded (This copies the executables and libraries to `./build_fm-suite_{debug,release}/install`). 
Start a build by hovering over the "Build" menu item and clicking the icon that appears. The CMake 'configure' phase will automatically be performed if necessary. The `./build_fm-suite_{debug,release}` directory should appear in the repository root.

The CMake extension will show all of the CMake command line invocations it is doing in the terminal. If you
prefer working with the `cmake` command line tool, you can see what the extension is doing and repeat the
same commands in the command line yourself.

#### Python virtual environment for running `TestBench.py`
There's a shell script: `.devcontainer/delft3d/scripts/post_create_command.sh` that should install
all of the required Python dependencies for `TestBench.py` in `test/deltares_testbench` 
in a "virtual environment" (or "venv"). This script is run automatically after 
(re-)building the devcontainer. If the directory `test/deltares_testbench/.venv`
already exists this step will be *skipped*.

There is a potential problem if you already had an existing venv inside the
`test/deltares_testbench` directory before you started working in the devcontainer.
Remember that the files under  `/workspaces/delft3d` are bind mounted from the host
to the container. If you already had installed a venv in 
`test/deltares_testbench` before on the host, then this venv is referencing a version
of Python that only exists on the host as well. Inside the container this venv will
not work. To work around this you can delete the existing `.venv` directory and
make a new one from within the devcontainer:

```bash
# From test/deltares_testbench
rm -rf .venv
uv venv --python=3.12 .venv
uv pip sync pip/lnx-dev-requirements.txt
```

The `settings.json` example sets the `python.defaultInterpreterPath` setting to the
python version inside the venv created in `test/deltares_testbench`. The linter, formatter
and type checker are also installed in this venv, and they should automatically
activate when you're browsing the testbench source code. There is an additional
extension (the "Ruff" extension) that runs the formatter on the python source code
whenever you change and save a python source code file.

#### Running `TestBench.py`
Before running `TestBench.py`, you need to first activate the venv. This ensures
that the correct version of `Python` occurs first on your `PATH`,
and that `Python` has access to all of the installed dependencies:

```bash
# Activating the virtual environment in this shell session:
# The following commands should be executed in `test/deltares_testbench`
source ./.venv/bin/activate
python TestBench.py --help
```

If successful, after the "sourcing" the activation script in `.venv/bin/activate`, the
text `(.venv)` will appear in your command prompt. This way, you can be sure that whenever
you invoke `python` you will be using the correct interpreter and the dependencies you
need for running the `TestBench.py` are installed.

##### Setting the path to the Delft3D binaries
For historical reasons, `TestBench.py` tries to look for programs to run in the directory 
`./data/engines/teamcity_artifacts/lnx64/`. To point `TestBench.py` to your own
freshly built Delft3D binaries, you can make this a "symbolic link" to the CMake install
directory. If this symbolic link does not exist yet, or you want to modify the location of 
the binaries, you can do the following:
```bash
# The following commands should be executed in `test/deltares_testbench`
mkdir -p data/engines/teamcity_artifacts/
ln -s -T $(realpath ../../build_fm-suite_release/install/) data/engines/teamcity_artifacts/lnx64
```

##### Installing your MinIO credentials
To run testbench configs `TestBench.py` automatically downloads case and reference files from MinIO 
(https://s3.deltares.nl). Accessing files on MinIO requires your personal MinIO credentials.
You can generate an "access key id" and "secret access key" on the 
[MinIO web interface](https://s3-console.deltares.nl/access-keys). You can save these values in the file
`/home/dev/.aws/credentials` with the following content:
```
[default]
aws_access_key_id=<your-access-key-id>
aws_secret_access_key=<your-secret-access-key>
```

The directory `/home/dev/.aws` is mounted in the devcontainer as a volume. So any files written there will
be persisted. You only have to write your MinIO keys to this file once.

##### Running a test case
You can try running a test case to verify that the path to the binaries and your credentials work. This
command runs only a single test case from the `configs/dimr/dimr_dflowfm_lnx64.xml` testbench config.
```bash
# The following commands should be executed in `test/deltares_testbench`
source ./.venv/bin/activate  # Only necessary if the venv is not already active.

# To run a single test case
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64.xml --filter 'testcase=e02_f002_c100'

# To run all of the test cases in the `dimr_dflowfm_lnx64.xml` config (The --parallel flag is recommended)
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64.xml --parallel
```

`TestBench.py` saves the case input files downloaded from MinIO in `./data/cases`, and the "references" 
in `./data/reference_results`. The logs for each test case are saved in `./logs`.
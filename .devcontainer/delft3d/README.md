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

We want to make it as easier for people to set up a development environment for Delft3D
on Linux. The large amount of third-party libraries required have made this difficult in
the past. Now that we're already using "build containers" in our CI pipelines to do our
builds, we can reuse them to make "development containers" as well. Any change made to the
build container can automatically be applied to the development container (provided
that a clean pull/rebuild of the development container is performed). This ensures that the
CI environment and the development environment are kept in sync. All of the dockerfiles
and devcontainer configuration should be tracked in this Git repository. So we can easily
plan, perform, review and share changes.

## Prerequisites

You need [Docker](https://docker.com) to build and run the devcontainer, and an
IDE that supports opening a project in the devcontainer. For supported IDEs you
can visit [containers.dev](https://containers.dev/supporting#editors) for an
up-to-date list. At the time of writing they are:
- Visual Studio Code
- Visual Studio
- IntelliJ IDEA

The steps required to open this project inside a devcontainer depends on the IDE.
To maximize your chances of success we suggest following the instructions on the site
of your IDE of choice.

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
line tool. In addition you will be able to switch between running 'Linux containers'
and 'Windows containers'. You will need a license to use Docker Desktop. If you are 
a Deltares employee you can request a Docker Desktop license by contacting Edward Melger.

#### Installing Docker in Linux (WSL or native)
Docker Desktop requires a license. But the official "Docker Engine", which includes
the `docker` command line tool and the "Docker daemon", is
[open source](https://docs.docker.com/engine/#licensing) software and doesn't require 
a license. Some people may prefer to install Docker directly in their Linux
distribution of choice (be it on Windows using WSL, or on a native Linux installation).
Usually docker can be installed using the Linux distribution's standard package manager.
You should be aware of the following:
1. You will only be able to run Linux containers (You can't run Windows containers on Linux).
2. In some Linux distributions, you will need to configure the package manager to install
   the official Docker engine. Read [this page](https://docs.docker.com/engine/install/#installation-procedures-for-supported-platforms) 
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



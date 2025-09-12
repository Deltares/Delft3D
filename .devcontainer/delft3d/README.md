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

## Prerequisites (Windows)



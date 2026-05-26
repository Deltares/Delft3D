# User interfaces for the Delft3D open source community
Back to [main page](../README.md).

## Delft3D Flexible Mesh
- To be specified.

## Delft3D 4
- Register and login to the https://download.deltares.nl/en website.
- Go to https://download.deltares.nl/en/delft3d-4-gui-open-source and add the zipped installer for the Delft3D 4 Suite to your cart.
- Click on the cart symbol at the top of the page, and fill out the forms, accept the general license term of Deltares software, select one of the available download sites, and press "send request".
- You will automatically receive an email containing with a download link for the installer (share link) and the license file (download link).
- Unzip and install the software; remember where you installed the software.
By default the software will be installed in `C:\Program Files\Deltares\Delft3D <version>\`.
- Locate where the kernels should be copied: the folder containing the installed software contains five subdirectories: `guis`, `release_notes`, `manuals`, `source` and `kernels`.
The `kernels` folder is initially empty.
You will need to create a folder `x64` here.
- Build the kernels yourself from the source code in this repository.
Build a release version of either the `d3d4-suite` or `all` configuration (see [this page](compiling_Windows.md) for detailed Windows compilation instructions).
- Copy the content (`bin`, `lib` and `share` directories) of the `install_d3d4-suite` (or `install_all`) folder in your development environment to the `kernels\x64` folder created above.

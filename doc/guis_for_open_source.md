# User interfaces for the Delft3D open source community
Back to [main page](../README.md).

## Delft3D Flexible Mesh
- To be specified.

## Delft3D 4
- Register and log in to the https://download.deltares.nl/en website.
- Go to https://download.deltares.nl/en/delft3d-4-gui-open-source and add the zipped installer for the Delft3D 4 Suite to your cart.
- Click the cart symbol at the top of the page. Complete the required forms, accept the Deltares software license terms, select one of the available download sites and press "Send Request".
- You will automatically receive an email containing a download link for the installer (share link) and a download link for the license file.
- Download and unzip the installer, then install the software. Make a note of the installation directory.
  By default, the software is installed in `C:\Program Files\Deltares\Delft3D <version>\`.
- Open the installation directory. It contains the following subdirectories: `guis`, `release_notes`, `manuals`, `source` and `kernels`.
  The `kernels` folder is initially empty.
  Create a new folder named `x64` inside the `kernels` folder.
- Build the kernels from the source code in this repository.
  Build the Release configuration for either `d3d4-suite` or `all` (see [this page](compiling_Windows.md) for detailed Windows compilation instructions).
- Copy the contents (`bin`, `lib` and `share` directories) of the `install_d3d4-suite` (or `install_all`) folder from your development environment into the `kernels\x64` folder created above.

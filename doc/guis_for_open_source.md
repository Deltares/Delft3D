# User interfaces for the Delft3D open source community
Back to [main page](../README.md).

## Delft3D Flexible Mesh 2D3D
- Register and log in to the https://download.deltares.nl/en website.
- Go to https://download.deltares.nl/delft3d-fm-suite-2d3d-graphical-user-interface-gui-open-source and add the zipped installer for the Delft3D FM Suite 2D3D to your cart.
- Click the cart symbol at the top of the page.
  Complete the required forms, accept the Deltares software license terms, select one of the available download sites and press "Send Request".
- You will automatically receive an email containing a download link for the installer (share link) and a download link for the license file.
- Download and unzip the installer, then install the software.
  Make a note of the installation directory.
  By default, the software is installed in `C:\Program Files\Deltares\Delft3D FM Suite <version> OpenHMWQ\`.
- Open the installation directory.
  It contains two subdirectories: `bin` and `plugins`.
  Look for the subdirectory `plugins\DeltaShell.Dimr`.
  Initially, it will only contain a `DeltaShell.Dimr.dll`.
  Create a new folder named `kernels` with subdirectory `x64` next to this dll-file, such that the folder `plugins\DeltaShell.Dimr\kernels\x64` exists.
- Build the kernels from the source code in this repository.
  Build the Release configuration for either `fm-suite` or `all` (see [this page](compiling_Windows.md) for detailed Windows compilation instructions).
- Copy the contents (`bin` and `share` directories) of the `install_fm-suite` (or `install_all`) folder from your development environment into the `plugins\DeltaShell.Dimr\kernels\x64` folder created above.

**Note:** when starting with Delft3D FM, it is best to combine user interface version YYYY.RR (such as 2026.02 combining year and release number) with the kernels built from the [corresponding release tag `DIMRset_YYYY.RR`](https://github.com/Deltares/Delft3D/releases).
The main branch may include changes that are not compatible with previous releases of the user interface.
For a list of known compatibility issues that you might run into when mixing different versions of user interfaces and kernels, see the bottom of this page.

## Delft3D 4
- Register and log in to the https://download.deltares.nl/en website.
- Go to https://download.deltares.nl/delft3d-4-gui-open-source and add the zipped installer for the Delft3D 4 Suite to your cart.
- Click the cart symbol at the top of the page.
  Complete the required forms, accept the Deltares software license terms, select one of the available download sites and press "Send Request".
- You will automatically receive an email containing a download link for the installer (share link) and a download link for the license file.
- Download and unzip the installer, then install the software.
  Make a note of the installation directory.
  By default, the software is installed in `C:\Program Files\Deltares\Delft3D <version>\`.
- Open the installation directory.
  It contains the following subdirectories: `guis`, `release_notes`, `manuals`, `source` and `kernels`.
  The `kernels` folder is initially empty.
  Create a new folder named `x64` inside the `kernels` folder.
- Build the kernels from the source code in this repository.
  Build the Release configuration for either `d3d4-suite` or `all` (see [this page](compiling_Windows.md) for detailed Windows compilation instructions).
- Copy the contents (`bin`, `lib` and `share` directories) of the `install_d3d4-suite` (or `install_all`) folder from your development environment into the `kernels\x64` folder created above.

## Known compatibility issues
- Shortly after the 2026.02, the libraries were moved from the `lib` to the `bin` folder on Windows.
  Subsequent builds don't produce a `lib` folder anymore, whereas the 2026.02 user interface expects it.

For an updated list of known compatibility issues see the bottom of this page on [main](https://github.com/Deltares/Delft3D/blob/main/doc/guis_for_open_source.md).
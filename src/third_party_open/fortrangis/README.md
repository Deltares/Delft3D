[![Build Status](https://simc.arpae.it/moncic-ci/fortrangis/centos7.png)](https://simc.arpae.it/moncic-ci/fortrangis/)
[![Build Status](https://simc.arpae.it/moncic-ci/fortrangis/rocky8.png)](https://simc.arpae.it/moncic-ci/fortrangis/)
[![Build Status](https://simc.arpae.it/moncic-ci/fortrangis/rocky9.png)](https://simc.arpae.it/moncic-ci/fortrangis/)
[![Build Status](https://simc.arpae.it/moncic-ci/fortrangis/fedora36.png)](https://simc.arpae.it/moncic-ci/fortrangis/)
[![Build Status](https://simc.arpae.it/moncic-ci/fortrangis/fedora38.png)](https://simc.arpae.it/moncic-ci/fortrangis/)
[![Build Status](https://copr.fedorainfracloud.org/coprs/simc/stable/package/fortrangis/status_image/last_build.png)](https://copr.fedorainfracloud.org/coprs/simc/stable/package/fortrangis/)

## [FortranGIS project](http://fortrangis.sourceforge.net/index.php)

FortranGIS project includes a collection of Fortran interfaces to some
common Open Source GIS (Geographic Information System) software
libraries, plus some more Fortran-specific tools.

The libraries interfaced at the moment are Shapelib, GDAL, Proj and
ReadOSM.

#### Shapelib

[Shapelib](http://shapelib.maptools.org/) is a library for handling ESRI shapefiles, a very common
vector file format in GIS. This Fortran interface covers the full
Shapelib API, including reading and writing of all kinds of shapes and
dbf attributes.  All the procedures interfaced in Fortran
are documented with Doxygen.  The Fortran API for Shapelib is stable
and unlikely to undergo any change.

#### Gdal

[GDAL](http://www.gdal.org/) is a library for the input/output of a
number of georeferenced raster (gridded) data formats. Since release
2.0, this Fortran interface includes bindings to almost all the GDAL C
functions, allowing read and write of any file format supported by
GDAL. This incidentally allows to read in Fortran also common graphic
formats such as jpeg, png and gif. For some common procedures, a
Fortran-specific interface is available, thus simplifying the work for
Fortran programmers. The Fortran-specific procedures are documented
with Doxygen, while for the procedures directly interfaced to the
native GDAL functions you should refer to the original GDAL C API
documentation.

#### Proj

[Proj](https://proj.org/) is a library for handling a big number of
geographical projections and coordinate conversions. The Fortran
interface covers bith the deprecated proj.4 API (available as legacy
bindings up to proj version 7) and the new proj version 6 API. The
availability of each of the interfaces is detected automatically at
configure time. In both cases the most common procedures of the proj
library are interfaced and they are considered stable. Here as well a
Fortran-specific interface is available for some procedures.

#### ReadOSM

[ReadOSM](https://www.gaia-gis.it/fossil/readosm/index) is a library
for extracting data from Open Street Map .osm and .osm.pbf files which
can be downloaded with various tools from the
[OpenStreetMap](http://openstreetmap.org/) platform. It is still
experimental and is not built by default.

## Interfacing to C

The interfacing to the C APIs within this project is done through the
ISO_C_BINDING Fortran2003 intrinsic module. This is supported nowadays
by most commercial Fortran compilers as well as by the free gfortran
compiler. Since version 2.3, FortranGIS makes use of some extra
Fortran 2003 features besides bindings to C, thus gfortran version 4.6
or higher is recommended.

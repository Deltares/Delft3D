$Id: README.TXT 540 2013-01-02 16:08:38Z pwessel $

--------------------------------------------------------------------
Global Self-consistent Hierarchical High-resolution Geography
			  GSHHG
	
                Version 2.2.2 January 1, 2013
         Distributed under the Lesser GNU Public License

This data set consists of two related components:
	
GSHHS:	Global Self-consistent Hierarchical High-resolution Shorelines:
	These originate as individual polygons at five different
	resolutions.  The ocean-land shorelines derive from WVS (World
	Vector Shoreline project) [Soluri and Woodson, 1990] while the
	polygons for lakes, islands-in-lakes, and ponds-in-islands-
	in-lakes derive from WDBII [Gorny, 1977], which is a much older
	and lower-quality data product.  Our compilation combines these
	data into a self-consistent product; see Wessel and Smith [1996]
	for processing details.  Over the years we have manually added
	new data in areas that were poorly represented in the original
	data set; however, as users zoom in closely they can see that
	the old data may in places be mis-registered relative to recent
	data such as used in Google Earth.
	
WDBII:	CIA World Data Bank II lineaments for borders and rivers.
	Over the years, political boundaries have changed and we have
	updated these to reflect realities based on feedback from our
	users. 

GSHHG is distributed in several representations:

	1. The binary and shapefile distributions provide the complete
	   GSHHS polygons and WDBII lineaments in their five resolutions
	   (i.e., after our full processing), and differ only in the
	   file formats (native binary data files versus standard GIS
	   shapefiles).  These distributions are normally used by
	   users interested to use these data outside the standard
	   GMT-based environment, or GMT users who wish to access the
	   whole GSHHS polygons.
	2. The netCDF distribution provide specially processed netCDF
	   representation of GSHHS and WDBII where the polygons and
	   lines have been subdivided and indexed to deliver rapid map-
	   making for GMT.  Users who wish to access GSHHS outside of
	   GMT are advised to use the binary and shapefile version of
	   the actual polygons as there are no user documentation for
	   how to access the netCDF files.  There are two versions,
	   one using netcdf-4 features (deflation) and another that is
	   suitable for legacy netcdf-3 libraries.
	
Many thanks to Tom Kratzke, Metron Inc., for patiently testing
many draft versions of GSHHS and reporting inconsistencies such as
erratic data points and crossings.

References:

Gorny, A. J. (1977), World Data Bank II General User GuideRep. PB 271869,
	10pp, Central Intelligence Agency, Washington, DC.
Soluri, E. A., and V. A. Woodson (1990), World Vector Shoreline,
	Int. Hydrograph. Rev., LXVII(1), 27–35.
Wessel, P., and W. H. F. Smith (1996), A global, self-consistent, hierarchical,
	high-resolution shoreline database, J. Geophys. Res., 101(B4), 8741–8743.

GSHHG Version-specific comments:
====================================================================
Version 2.2.2 January 2013: We have removed Sandy Island, Coral Sea
(non feature), shifted Society Island polygons ~1 arc minute to the west,
and replaced Mehetia Island with better data.  Furthermore, 50 islands
that were imprecise duplicates of more accurate WVS features were removed.
Apart from the Agalega islands, these duplicates were mostly found in
the Red Sea, the Persian Gulf, and in the Cook-Austral region.  GSHHG is
now released under the lesser GNU License, v3 or any earlier version.
--------------------------------------------------------------------

Version 2.2.1 July 2012: We have renamed the product GSHHG since it
contains more than just shorelines (we distribute political boundaries
and rivers as well).  The GSHHG building and distribution is now
fully decoupled from GMT.  We have also changed the name of the
netCDF files for GMT to use the more standard extension *.nc.
Furthermore, the packages have been renamed for clarity and follow
the form gshhs+wdbii-<version>-gmt|gmt-nc3|bin|shp.tar|zip

There are no significant changes to the actual data features, other
than a glitch in SA-NT border in Australia and removal of 7 zero-length
border segments.  Following the rebranding to GSHHG the names of the
distribution files have changed as well.
--------------------------------------------------------------------

Version 2.2.0 July 2011: The area of small (< 0.1 km^2) polygons
got truncated to 0.  This would cause gshhs to consider them
as lines (borders or rivers) instead of polygons.  Furthermore,
the areas were recomputed using the WGS-84 ellipsoid as the previous
area values were based on a spherical calculation.  Thanks to
José Luis García Pallero for pointing this out.  We now store
the area with a magnitude scale tuned to each polygon.  Also, the
greenwich flag is now a 2-bit flag composed of 1 (crosses Greenwich),
2 (crosses Dateline), 3 (both) or 0 (no such crossing).  See gshhs.[ch] for
details.  Finally, the binary gshhs files now store Antarctica in
-180/+180 range so as to avoid a jump when dumped to ASCII.
Also, the WDBII shapefiles  only had the first 3 levels of rivers;
version 2.2.0 has all 11.  Finally, to be able to detect the river-lake
features in the WDBII binary files we set the river flag to 1 if a closed feature.
--------------------------------------------------------------------

Version 2.1.1 March 2011: Relatively minor fixes to low-resolution
polygons, including editing errors introduced in v 2.1, removing
a few spikes from 4-5 polygons, and fixing Germany-Poland border
near the Baltic Sea.
--------------------------------------------------------------------

Version 2.1 July 2010: Fixes lack of river-lake flag in the binary
and shapefile release.  Shapefile polygons of level = 2 and with a
negative area are river-lakes.  Also include WDBII border and river
data as shapefiles.
--------------------------------------------------------------------

version 2.0 July 15, 2009: Differs from the previous version 1.x in
the following ways.

1.  Free from internal and external crossings and erratic spikes
    at all five resolutions.
2.  The original Eurasiafrica polygon has been split into Eurasia
    (polygon # 0) and Africa (polygon # 1) along the Suez canal.
3.  The original Americas polygon has now been split into North
    America (polygon # 2) and South America (polygon # 3) along
    the Panama canal.
4.  Antarctica is now polygon # 4 and Australia is polygon # 5, in
    all the five resolutions.
5.  Fixed numerous problems, including missing islands and lakes
    in the Amazon and Nile deltas.
6.  Flagged "riverlakes" which are the fat part of major rivers so
    they may easily be identified by users.
7.  Determined container ID for all polygons (== -1 for level 1
    polygons) which is the ID of the polygon that contains a smaller
    polygon.
8.  Determined full-resolution ancestor ID for lower res polygons,
    i.e., the ID of the polygon that was reduced to yield the lower-
    res version.
9.  Ensured consistency across resolutions (i.e., a feature that is
    an island at full resolution should not become a lake in low!).
10. Sorted tables on level, then on the area of each feature.
11. Made sure no feature is missing in one resolution but
    present in the next lower resolution.
12. Store both the actual area of the lower-res polygons and the
    area of the full-resolution ancestor so users may exclude fea-
    tures that represent less that a fraction of the original full
    area.

There was some duplication and wrong levels assigned to maritime
political boundaries in the Persian Gulf that has been fixed.

These changes required us to enhance the GSHHS C-structure used to
read and write the data.  As of version 2.0 the header structure is

struct GSHHS {  /* Global Self-consistent Hierarchical High-resolution Shorelines */
        int id;         /* Unique polygon id number, starting at 0 */
        int n;          /* Number of points in this polygon */
        int flag;       /* = level + version << 8 + greenwich << 16 + source << 24 + river << 25 */
        /* flag contains 5 items, as follows:
         * low byte:    level = flag & 255: Values: 1 land, 2 lake, 3 island_in_lake, 4 pond_in_island_in_lake
         * 2nd byte:    version = (flag >> 8) & 255: Values: Should be 7 for GSHHS release 7 (i.e., version 2.0)
         * 3rd byte:    greenwich = (flag >> 16) & 1: Values: Greenwich is 1 if Greenwich is crossed
         * 4th byte:    source = (flag >> 24) & 1: Values: 0 = CIA WDBII, 1 = WVS
         * 4th byte:    river = (flag >> 25) & 1: Values: 0 = not set, 1 = river-lake and level = 2
         */
        int west, east, south, north;   /* min/max extent in micro-degrees */
        int area;       /* Area of polygon in 1/10 km^2 */
        int area_full;  /* Area of original full-resolution polygon in 1/10 km^2 */
        int container;  /* Id of container polygon that encloses this polygon (-1 if none) */
        int ancestor;   /* Id of ancestor polygon in the full resolution set that was the source of this polygon (-1 if none) */
};

Some useful information:

A) To avoid headaches the binary files were written to be big-endian.
   If you use the GMT supplement gshhs it will check for endian-ness and if needed will
   byte swab the data automatically. If not then you will need to deal with this yourself.

B) In addition to GSHHS we also distribute the files with political boundaries and
   river lines.  These derive from the WDBII data set.

C) As to the best of our knowledge, the GSHHS data are geodetic longitude, latitude
   locations on the WGS-84 ellipsoid.  This is certainly true of the WVS data (the coastlines).
   Lakes, riverlakes (and river lines and political borders) came from the WDBII data set
   which may have been on WGS072.  The difference in ellipsoid is way less then the data
   uncertainties.  Offsets have been noted between GSHHS and modern GPS positions.

D) Originally, the gshhs_dp tool was used on the full resolution data to produce the lower
   resolution versions.  However, the Douglas-Peucker algorithm often produce polygons with
   self-intersections as well as create segments that intersect other polygons.  These problems
   have been corrected in the GSHHS lower resolutions over the years.  If you use gshhs_dp to
   generate your own lower-resolution data set you should expect these problems.

E) The shapefiles release was made by formatting the GSHHS data using the extended GMT/GIS
   metadata understood by OGR, then using ogr2ogr to build the shapefiles.  Each resolution
   is stored in its own subdirectory (e.g., f, h, i, l, c) and each level (1-4) appears in
   its own shapefile.  Thus, GSHHS_h_L3.shp contains islands in lakes for the high res
   data. Because of GIS limitations some polygons that straddle the Dateline (including
   Antarctica) have been split into two parts (east and west).

F) The netcdf-formatted coastlines distributed with GMT derives directly from GSHHS; however
   the polygons have been broken into segments within tiles.  These files are not meant
   to be used by users other than via GMT tools (pscoast, grdlandmask, etc).  The latest
   GMT comes with version 2.0.2 of the netcdf files, still based on GSHHS 2.0.

Paul Wessel     Primary contact: pwessel@hawaii.edu
Walter H. F. Smith

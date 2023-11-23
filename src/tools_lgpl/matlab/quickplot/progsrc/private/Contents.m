% Delft3D-MATLAB interface toolbox.
%   Private functions.
%
% Data data access functions
%   adcircmesh                  - Read an Adcirc fort.14 mesh topology file.
%   ai_ungen                    - Read/write ArcInfo (un)generate files.
%   aukepc                      - Read AUKE/pc files.
%   bagdpt                      - Read output files BAGGER-BOS-RIZA bagger option.
%   bagmap                      - Read output files BAGGER-BOS-RIZA bagger option.
%   bna                         - Read/write for ArcInfo (un)generate files.
%   boxfile                     - Read/write SIMONA box files.
%   cfx                         - File operations for CFX file.
%   cfx1block                   - Merge CFX 4 multiblock data into one block
%   clip2single                 - Reduce precision to single precision accuracy.
%   delwaqt0                    - Parse Delwaq T0 string.
%   dbase                       - Read data from a dBase file.
%   delwaqtimfile               - Reads in a Delwaq .tim input file (Lex Yacc type).
%   fls                         - Read Delft-FLS and SOBEK2D incremental files.
%   geomesh                     - Read a Geo mesh topology file.
%   getlds                      - Get LDS information from SDS file.
%   grib_disp                   - Display meta-info from grib file
%   grib_find                   - Get block index from meta-info
%   gsharp                      - Read/write GSharp files.
%   incremental                 - Read Delft-FLS, SOBEK2D and SIMONA incremental files.
%   jspost                      - Read JSPost files.
%   map2golder                  - Convert bed stratigraphy into Golder input file.
%   mike                        - Read/write DHI Mike files.
%   mikemesh                    - Read a DHI Mike FM mesh topology file.
%   morf                        - Read Delft3D-MOR morf files.
%   nc_interpret                - Interpret the netCDF data based on conventions.
%   nodelemesh                  - Read a node/element mesh topology files.
%   pcraster                    - Read/write PC-Raster files.
%   qnhls                       - Read/write Quickin HLS files.
%   qp_netcdf_get               - Get data from netcdf file and reshape.
%   read_ecom_corners           - Read ECOMSED corners grid file.
%   read_ecom_modelgrid         - Read ECOMSED grid file.
%   readswan                    - Read SWAN 1D and 2D spectral files.
%   shapewrite                  - Write ESRI shape files.
%   shipma                      - Read Shipma project (and embedded) files.
%   shyfemmesh                  - Read a SHYFEM mesh topology file.
%   smsmesh                     - Read a Surface-water Modeling System mesh topology file.
%   sobek                       - Read and plot SOBEK topology.
%   swan                        - Read/write SWAN files.
%   tecplot                     - Read/write for Tecplot files.
%   telemac                     - Read Telemac selafin files.
%   trtarea                     - Read Delft3D trachytope/WAQUA ecotope area files.
%   unibest                     - Read Unibest files.
%   waterml2                    - Read WaterML 2.0 files.
%
% Special graphics routines
%   balanceplot                 - Create a balance plot.
%   contourfcorr                - Filled contour plot (corrected).
%   getnormpos                  - Select an area (using normalized units).
%   hls2rgb                     - Convert hue-lightness-saturation to red-green-blue colors.
%   idx2rgb                     - Converts an indexed image into a true color RGB image.
%   lddplot                     - Plot local drainage direction for PC-Raster LDD data file.
%   progressbar                 - Display progress bar.
%   qncmap                      - QuickIn color map.
%   recolor                     - Replaces one color by another color.
%   rgb2hls                     - Convert red-green-blue colors to hue-lightness-saturation.
%   series_frame                - Saves a figure in one of a series of bitmap images.
%   series_init                 - Initiates the creation of a series of bitmap files.
%
% Generic helper routines
%   abbrevfn                    - Abbreviate filename.
%   absfullfile                 - Build full absolute file name from parts.
%   ap2ep                       - Convert tidal amplitude and phase lag (ap-) parameters into tidal ellipse
%   asciiload                   - A compiler compatible version of LOAD -ASCII.
%   avi                         - MATLAB AVI interface.
%   clockwise                   - Determines polygon orientation.
%   exepath                     - MEX file to determine path of deployed mode.
%   filesequal                  - Determines whether the contents of two files is the same.
%   findseries                  - Find series of nonzero elements.
%   incanalysis                 - Analyse incremental data.
%   inlist                      - Match cell arrays of strings.
%   int32_byteflip              - Convert integers into integers with flipped byte order.
%   int_lnln                    - Intersection of two lines.
%   int_lntri                   - Intersection of line and triangular mesh.
%   isstandalone                - Determines stand alone execution.
%   legendbox                   - Display legend.
%   multiline                   - Converts a string containing LineFeeds to a char matrix.
%   realset                     - Manipulate sets of real values.
%   reducepoints_r13_6p5        - MEX file for REDUCEPOINTS compiled with R13.
%   reducepoints_r2007a_7p4     - MEX file for REDUCEPOINTS compiled with R2007A.
%   relativepath                - Determine file name relative to second file.
%   removequant                 - QP support function for removing quantities from poperty lists.
%   setProperty                 - Generic routine to set values in PropertyName-PropertyValue pairs
%   stdbeep                     - Produce beep sound.
%   trim2rst                    - Extract Delft3D-FLOW restart file from TRIM-file.
%   ui_type                     - Simple selection dialog.
%   ui_typeandname              - Selection dialog with name specification.
%   uigetfolder                 - Standard Windows browse for folder dialog box.
%   writeavi                    - MEX interface to Windows AVI functions.
%
% QuickPlot file dependent routines
%   arcgridfil                  - QP support for ARCGRID files.
%   asciiwindfil                - QP support for Delft3D meteo files.
%   aukepcfil                   - QP support for AUKE/pc files.
%   bagdptfil                   - QP support for Delft3D-MOR dredge type 2 files.
%   bctfil                      - QP support for Delft3D boundary files.
%   bilhdrfil                   - QP support for BIL files.
%   bitmapfil                   - QP support for bitmap files.
%   cfxfil                      - QP support for CFX4 files.
%   d3d_bagrfil                 - QP support for Delft3D-MOR dredge type 1 files.
%   d3d_bothfil                 - QP support for Delft3D-MOR bottom module history files.
%   d3d_botmfil                 - QP support for Delft3D-MOR bottom module map files.
%   d3d_comfil                  - QP support for Delft3D communication files.
%   d3d_hwgxyfil                - QP support for Delft3D-WAVE files.
%   d3d_trahfil                 - QP support for Delft3D-MOR transport history files.
%   d3d_tramfil                 - QP support for Delft3D-MOR transport map files.
%   d3d_tridfil                 - QP support for Delft3D-FLOW drogue output files.
%   d3d_trihfil                 - QP support for Delft3D-FLOW history files.
%   d3d_trimfil                 - QP support for Delft3D-FLOW map files.
%   d3d_waqfil                  - QP support for Delft3D-WAQ and -PART map and history files.
%   difffil                     - QP support for file differences.
%   ecomsedfil                  - QP support for binary ECOM/ECOMSED files.
%   flexmeshfil                 - QP support for various unstructured mesh files.
%   flsfil                      - QP support for FLS and SOBEK incremental files.
%   geodatafil                  - QP support for geodata items.
%   gribfil                     - QP support for GRIB files.
%   gridfil                     - QP support for Delft3D grid and attribute files.
%   jspostfil                   - QP support for JSPost files.
%   matlabfil                   - QP support for Matlab files saved by QuickPlot.
%   mikezerofil                 - QP support for Mike 11, 21 and 3 files.
%   morftreefil                 - QP support for Delft3D-MOR input files.
%   netcdffil                   - QP support for netCDF files.
%   nfs_tritonfil               - QP support for TRITON output files.
%   pcrasterfil                 - QP support for PC Raster files.
%   pharosfil                   - QP support for Pharos files.
%   resourceobject              - Implements old interface for new QUICKPLOT Data Resource Object.
%   samplesfil                  - QP support for XYZ sample files.
%   shipmafil                   - QP support for Shipma project files.
%   skyllafil                   - QP support for Skylla files.
%   sobekfil                    - QP support for SOBEK-RE and Rural/Urban/River network and output files.
%   swanfil                     - QP support for SWAN spectral files.
%   tekalfil                    - QP support for Tekal, ESRI shape and ArcInfo generate files.
%   telemacfil                  - QP support for Telemac files.
%   unibestfil                  - QP support for UNIBEST files.
%   usrdeffil                   - QP support for user defined variables.
%   waquafil                    - QP support for SIMONA (WAQUA/TRIWAQ) files.
%
% QuickPlot specific helper routines
%   adddimension                - Add a dimension to a dimension list.
%   addlocation                 - Add a location to a location list.
%   auto_map_detect             - Autodetect function for Delft3D map files.
%   compthresholds              - Determine automatic threshold levels.
%   computecomponent            - Compute component of vector data set.
%   corner2center               - Interpolate data from cell corners to cell centers.
%   cur2ca                      - Rotate velocity components.
%   default_quantities          - Default implementation for quantities.
%   determine_frompoint         - Determine shortest path distance(s) from a point.
%   dimensions                  - Default implementation for dimensions.
%   dir2uv                      - Convert magnitude and angle to (x,y) components.
%   domains                     - Default implementation for domains.
%   gencontour                  - Generic plot routine for contour plot.
%   genfaces                    - Generic plot routine for patches plot.
%   genmarkers                  - Generic plot routine for marker plot.
%   gensurface                  - Generic plot routine for surface plot.
%   gentext                     - Generic plot routine for a single text.
%   gentextfld                  - Generic plot routine for a text field.
%   geodist                     - Distance between two points on a sphere.
%   valuemap                    - One-to-one mapping of enumerables.
%   get_matching_grid           - Get grid file that matches size of current dataset.
%   get_nondialogs              - Get handles of all non-dialog windows.
%   getdata                     - Default implementation for getdata.
%   getsubfields                - Default implementation for subfields.
%   getvalstr                   - Get string associated with value of object.
%   grid_fopen                  - Routine for opening attribute files on grid.
%   gridcelldata                - Convert gridcelldata string to boolean flags.
%   gridinterp                  - Compute grid locations from corner co-ordinates.
%   insstruct                   - Insert array.
%   interp2cen                  - Interpolate to center.
%   limitresize                 - Constrained resize.
%   limits                      - Determine real x,y,z,c limits.
%   listnames                   - Name graphics objects.
%   locations                   - Default implementation for locations.
%   md_dialog                   - Simple dialog tool.
%   md_print                    - Send a figure to a printer.
%   options                     - Default implementation for options.
%   piecewise                   - Checks and fixes a piecewise grid line.
%   plotstatestruct             - Create old cell plot state to structure plot state.
%   procargs                    - General function for argument processing.
%   protectstring               - Protect special characters in strings.
%   ps2pdf                      - Function to convert a PostScript file to PDF using Ghostscript
%   qp_basedir                  - Get various base directories.
%   qp_cmdstr                   - Process QuickPlot command string.
%   qp_colormap                 - QuickPlot colormap repository.
%   qp_createaxes               - Create an axes for plotting.
%   qp_createfig                - Create a figure for plotting.
%   qp_createscroller           - Create a QuickPlot animation scroller bar.
%   qp_datafield_name2prop      - Convert data field string to structure.
%   qp_defaultaxessettings      - Set axes preferences for plot axes.
%   qp_dimsqueeze               - Average data for non-plotted data dimensions.
%   qp_export                   - Export data set from a QuickPlot support data source.
%   qp_figaspect                - Reshapes figure to match paper size.
%   qp_figurebars               - Update menu and toolbars for QuickPlot figures.
%   qp_file2function            - Retrieve function name associated with file structure.
%   qp_filefilters              - Obtain a list of file filters.
%   qp_fmem                     - Routine for opening data files.
%   qp_fontsettings             - Convert between INI file font settings and font structures.
%   qp_getscreen                - Get position of current screen
%   qp_gettype                  - Determine file type for file structure.
%   qp_icon                     - QuickPlot icon repository.
%   qp_interface                - Initialize QuickPlot user interface.
%   qp_interface_update_options - Update QuickPlot user interface options.
%   qp_option                   - Set/get QuickPlot file options.
%   qp_plot                     - Plot function of QuickPlot.
%   qp_plot_default             - Plot function of QuickPlot for structured data sets.
%   qp_plot_pnt                 - Plot function of QuickPlot for point data sets.
%   qp_plot_polyl               - Plot function of QuickPlot for polyline data sets.
%   qp_plot_seg                 - Plot function of QuickPlot for 1D line segment data sets.
%   qp_plotmanager              - QuickPlot Plot Manager callback functions.
%   qp_preferences_interface    - Show QuickPlot preferences user interface.
%   qp_prefs                    - QuickPlot preferences dialog callback functions.
%   qp_proj_rotatepole          - Support routine to convert rotated pole coordinates to regular longitude and latitude.
%   qp_proxy                    - Proxy for file data in figures.
%   qp_refresh                  - Refresh data for current data resource.
%   qp_scalarfield              - Plot scalar data for curvilinear and triangular meshes.
%   qp_settings                 - Routine to store and retreive settings.
%   qp_showabout                - Show QuickPlot about window.
%   qp_state_startup            - Initialize QuickPlot plot state.
%   qp_state_version            - Check state.
%   qp_strrep                   - Replace string with another (case insensitive).
%   qp_time2str                 - Convert time double to cell of time strings.
%   qp_title                    - Set automatic title and parameters.
%   qp_toolbarpush              - Create a toolbar push button.
%   qp_toolbartoggle            - Create a toolbar toggle button.
%   qp_tooltip                  - Add a tooltip to gui or toolbar button.
%   qp_uifigure                 - Create a new empty dialog figure.
%   qp_uimenu                   - Create a new menu with submenus.
%   qp_unwrapfi                 - Remove QuickPlot wrapper from file structure.
%   qp_update_evalhistmenu      - Update list of recent QuickPlot macro commands.
%   qp_updatefieldprop          - Update subfield, time, M, N, K and grid view in QP dialog.
%   qp_updaterecentfiles        - Update list of recently opened data files in QP main dialog.
%   qp_updatescroller           - Update list of items/dimensions that can be animated.
%   qp_vector                   - Wrapper for QUIVER and QUIVER3.
%   qp_wrapfi                   - Add QuickPlot wrapper to file structure.
%   quantities                  - Wrapper for default implementation for quantities.
%   readsts                     - Default implementation for stations.
%   readtim                     - Default implementation for times.
%   separators                  - Remove double sepators and separators at end of list.
%   setaxesprops                - Set appropriate axes properties.
%   shiftcontrol                - Change the position of a uicontrol object.
%   simsteps                    - Performs an timestep analysis.
%   spatiallystructured         - Convert MNK space to xyz equivalent.
%   spirint                     - Computes spiral intensity from 3D flow field.
%   stack2str                   - Convert exception stack into cell string.
%   stdinputdlg                 - Input dialog box using standard settings.
%   str2file                    - Convert string to filename.
%   str2vec                     - Convert string into a vector.
%   tofront                     - Move graphics objects to front in children list.
%   transferfields              - Copy specified fields to another structure.
%   uigeturl                    - UIGETLINK Open URL dialog box.
%   update_option_positions     - Update vertical position of plot option controls.
%   updateuicontrols            - Force an update of the uicontrol properties.
%   uv2cen                      - Interpolate velocities.
%   vec2str                     - Create a string representation of a vector.
%   writelog                    - Write QuickPlot logfile or MATLAB script.
%   xx_constants                - Define several constants.
%   xx_logo                     - Plot a logo in an existing coordinate system.
%   xyz_or_mnk                  - Determine MNK flag for vector components to retrieve.
%
%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2023 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL$
%   $Id$

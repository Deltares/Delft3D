!> module to hold unstruc_model data, to avoid cyclic dependencies
module m_unstruc_model_data

   use m_globalparameters, only: t_filenames
   use tree_structures, only: tree_data
   use properties, only: max_prop_length
   use precision, only: dp

   implicit none(type, external)

   public

   !> The version number of the MDU File format: d.dd, [config_major].[config_minor], e.g., 1.03
    !!
    !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
    !! Convention for format version changes:
    !! * if a new format is backwards compatible with old MDU files, only
    !!   the minor version number is incremented.
    !! * if a new format is not backwards compatible (i.e., old MDU files
    !!   need to be converted/updated by user), then the major version number
    !!   is incremented.

   ! MDUFormatVersion = 1.09
   integer, parameter :: MDUFormatMajorVersion = 1
   integer, parameter :: MDUFormatMinorVersion = 9

   ! History MDUFormatVersion:
   ! 1.09 (2019-08-21): Renamed [geometry] roughnessFiles to frictFile.
   ! 1.08 (2019-07-27): Default option for density changed from Eckart to UNESCO (idensform=2 instead of 1)
   ! 1.07 (2019-06-13): Renamed [model] block as [General] block, replace keyword MDUFormatVersion by FileVersion
   ! 1.06 (2016-05-16): Removed 1 variable for secondary flow, EffectSpiral as it is given by Espir contained in .mor file
   ! 1.05 (2015-07-22): The structure parameters are added (jahisstr, jahisdam, his_write_settings%pump, his_write_settings%gate)
   ! 1.04 (2015-03-19): Anti-Creep option is added
   ! 1.03 (2015-02-25): Added 2 variable for secondary flow, EffectSpiral and BetaSpiral
   ! 1.02 (2015-01-07): Remove [time] AutoTimestep (always automatic).
   ! 1.01 (2014-11-10): Renamed ThindykeFile/Scheme/Contraction -> FixedWeirFile/Scheme/Contraction.
   ! 1.00 (2014-09-22): first version of new permissive checking procedure. All (older) unversioned input remains accepted.

   ! ExtfileNewMajorVersion = 3.00
   integer, parameter :: ExtfileNewMajorVersion = 3
   integer, parameter :: ExtfileNewMinorVersion = 0

   ! History ExtfileNewVersion:
   ! 2.02 (2024-10-24): add [SourceSink] blocks.
   ! 2.01 (2019-12-04): optional fields targetMaskFile and targetMaskInvert for [Meteo] blocks.
   ! 2.00 (2019-08-06): enabled specifying "nodeId" in a 1D network node.

   !> The version number of the 1D2DFile format: d.dd, [config_major].[config_minor], e.g., 1.03
    !!
    !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
    !! Convention for format version changes:
    !! * if a new format is backwards compatible with old 1D2D files, only
    !!   the minor version number is incremented.
    !! * if a new format is not backwards compatible (i.e., old 1D2D files
    !!   need to be converted/updated by user), then the major version number
    !!   is incremented.

   ! File1D2DLinkMajorVersion = 1.00
   integer, parameter :: File1D2DLinkMajorVersion = 1
   integer, parameter :: File1D2DLinkMinorVersion = 0

   ! History File1D2DLinkVersion:
   ! 1.00 (2019-12-04): Initial version.

   type(tree_data), pointer, public :: md_ptr !< Unstruc Model Data in tree_data

   character(len=64), target :: md_ident = ' ' !< Identifier of the model, used as suggested basename for some files. (runid)

   character(len=64) :: md_ident_sequential = ' ' !< Sequential model identifier, used for parallel outputdir

   character(len=64) :: md_specific = ' ' !< Optional 'model specific ID', read from MDU, to enable certain custom runtime function calls (instead of via MDU name/md_ident).

   character(len=4) :: md_tunit = ' ' !< Unit of tstart_user and tstop_user (only for read and write, while running these are always in seconds).

   integer :: md_paths_relto_parent = 0 !< Option whether or not (1/0) to resolve filenames (e.g. inside the *.ext file) w.r.t. their direct parent, instead of the toplevel MDU working dir. (UNST-1144)
   type(t_filenames) :: md_1dfiles
   character(len=max_prop_length) :: md_netfile = ' ' !< Net definition                    (e.g., *_net.nc)
   character(len=max_prop_length) :: md_flowgeomfile = ' ' !< Storing flow geometry (output)    (e.g., *_flowgeom.nc)
   character(len=max_prop_length) :: md_dryptsfile = ' ' !< Dry points file (list)            (e.g., *.xyz, *.pol)
   character(len=max_prop_length) :: md_encfile = ' ' !< Enclosure file (list)             (e.g., *.xyz, *.pol)
   character(len=max_prop_length) :: md_s1inifile = ' ' !< Initial water levels sample file using floodfill  (e.g., *.xyz)
   character(len=max_prop_length) :: md_ldbfile = ' ' !< Land boundary file    (show)      (e.g., *.ldb)
   character(len=max_prop_length) :: md_plifile = ' ' !< polylinefile file     (show)      (e.g., *.pli)
   character(len=max_prop_length) :: md_thdfile = ' ' !< Thin dam file (polygons)          (e.g., *_thd.pli) (block flow)
   character(len=max_prop_length) :: md_cutcelllist = ' ' !< contains list of cutcell polygons (e.g., *_cut.lst)
   character(len=max_prop_length) :: md_fixedweirfile = ' ' !< Fixed weir pliz's                 (e.g., *_fxw.pli), = pli with x,y, Z  column
   character(len=max_prop_length) :: md_pillarfile = ' ' !< pillar pliz's                     (e.g., *_pillar.pli), = pli with x,y, diameter and Cd columns
   integer :: md_pillar_use_far_field_velocity = 0 !< 0: use local velocity, 1: use far-field velocity for computing pillar drag force
   character(len=max_prop_length) :: md_roofsfile = ' ' !< Roof pliz's                      (e.g., *_roof.pli), = pli with x,y, Z  column
   character(len=max_prop_length) :: md_gulliesfile = ' ' !< gullies pliz's                    (e.g., *_gul.pli), = pli with x,y, Z  column
   character(len=max_prop_length) :: md_vertplizfile = ' ' !< Vertical layering pliz's          (e.g., *_vlay.pliz), = pliz with x,y, Z, first Z =nr of layers, second Z = laytyp
   character(len=max_prop_length) :: md_proflocfile = ' ' !< X,Y,and a profile reference nr    (e.g., *_profloc.xyz)
   character(len=max_prop_length) :: md_profdeffile = ' ' !< Profile definition of these nrs   (e.g., *_profdef.txt)
   character(len=max_prop_length) :: md_profdefxyzfile = ' ' !< XYZ profile definition in pliz of these nrs ic yz-def (e.g., *_xyzprof.pliz)
   character(len=max_prop_length) :: md_1d2dlinkfile = ' ' !< File containing custom parameters for 1D2D links (e.g., *.ini)
   character(len=max_prop_length) :: md_pipefile = ' ' !< File containing pipe-based 'culverts' (e.g., *.pliz)
   character(len=max_prop_length) :: md_shipdeffile = ' ' !< File containing shipdefinition    (e.g., *.shd)
   character(len=max_prop_length) :: md_inifieldfile = ' ' !< File of initial fields            (e.g., *.ini)

   character(len=max_prop_length) :: md_restartfile = ' ' !< File containing map-files to restart a computation          (e.g., *_map.nc), input only, NOT used for storing the names of output restart files.

   character(len=max_prop_length) :: md_extfile = ' ' !< External forcing specification file (e.g., *.ext)
   character(len=max_prop_length) :: md_extfile_new = ' ' !< External forcing specification file new style (bct format), (e.g., *.ext)
   character(len=max_prop_length) :: md_extfile_dir = ' ' !< Directory containing the old-style external forcing specification file (e.g., *.ext) (relative to MDU/current working dir)

   character(len=max_prop_length) :: md_structurefile = ' ' !< Structure file, (e.g., *.ini)
   character(len=max_prop_length) :: md_structurefile_dir = ' ' !< Directory containing the structure file (e.g., *.ini) (relative to MDU/current working dir).

   character(len=max_prop_length) :: md_wavefile = ' ' !< File containing wave input (e.g., *_wave.nc)
   character(len=max_prop_length) :: md_surfbeatfile = ' ' !< File containing surfbeat input (e.g., params.txt)

   character(len=max_prop_length) :: md_sedfile = ' ' !< File containing sediment characteristics (e.g., *.sed)
   character(len=max_prop_length) :: md_morfile = ' ' !< File containing morphology settings (e.g., *.mor)
   character(len=max_prop_length) :: md_dredgefile = ' ' !< File containing dredging settings (e.g., *.dad)
   character(len=max_prop_length) :: md_bedformfile = ' ' !< File containing bedform settings (e.g., *.bfm)
   character(len=max_prop_length) :: md_morphopol = ' ' !< File containing boundaries of morphologic change extent (e.g., *.pol)
   character(len=max_prop_length) :: md_sedtrailsfile = ' ' !< File containing extent of sedtrails output grid

   character(len=max_prop_length) :: md_obsfile = ' ' !< File containing observation points  (e.g., *_obs.xyn, *_obs.ini)
   integer :: md_delete_observation_points_outside_grid !< 0 - do not delete, 1 - delete
   character(len=max_prop_length) :: md_crsfile = ' ' !< File containing cross sections (e.g., *_crs.pli, observation cross section *_crs.ini)
   character(len=max_prop_length) :: md_rugfile = ' ' !< File containing runup gauges (e.g., *_rug.pli)
   character(len=max_prop_length) :: md_foufile = ' ' !< File containing fourier modes to be analyzed

   character(len=max_prop_length) :: md_hisfile = ' ' !< Output history file for monitoring  (e.g., *_his.nc)
   character(len=max_prop_length) :: md_mapfile = ' ' !< Output map     file for full flow fields (e.g., *_map.nc)
   character(len=max_prop_length) :: md_classmapfile = ' ' !< Output classmap file for full flow fields in classes (formerly: incremental file) (e.g., *_clm.nc)
   character(len=max_prop_length) :: md_comfile = ' ' !< Output com     file for communication (e.g., *_com.nc)
   character(len=max_prop_length) :: md_timingsfile = ' ' !< Output timings file (auto-set)
   character(len=max_prop_length) :: md_avgwavquantfile = ' ' !< Output map file for time-averaged wave output (e.g., *_wav.nc)
   character(len=max_prop_length) :: md_avgsedquantfile = ' ' !< Output map file for time-averaged sedmor output (e.g., *_sed.nc)
   character(len=max_prop_length) :: md_avgsedtrailsfile = ' ' !< Output map file for time-averaged sedtrails output (e.g., *_sedtrails.nc)
   character(len=max_prop_length) :: md_waqfilebase = ' ' !< File basename for all Delwaq files. (defaults to md_ident)
   character(len=max_prop_length) :: md_waqoutputdir = ' ' !< Output directory for all WAQ communication files (waqgeom, vol, flo, etc.)
   character(len=max_prop_length) :: md_waqhoraggr = ' ' !< DELWAQ output horizontal aggregation file (*.dwq)
   character(len=max_prop_length) :: md_waqvertaggr = ' ' !< DELWAQ output vertical aggregation file (*.vag)

   character(len=max_prop_length) :: md_partitionfile = ' ' !< File with domain partitioning polygons (e.g. *_part.pol)
   character(len=max_prop_length) :: md_outputdir = ' ' !< Output directory for map-, his-, rst-, dat- and timings-files

!   processes (WAQ)
   character(len=max_prop_length) :: md_subfile = ' ' !< substance file
   character(len=max_prop_length) :: md_ehofile = ' ' !< extra history output file
   character(len=max_prop_length) :: md_pdffile = ' ' !< [-] process library file
   character(len=max_prop_length) :: md_oplfile = ' ' !< [-] open process library dll/so file
   character(len=max_prop_length) :: md_blmfile = ' ' !< [-] BLOOM aglae species definition file
   character(len=max_prop_length) :: md_sttfile = ' ' !< statistics definition file
   real(kind=dp) :: md_thetav_waq = 0.0_dp !< thetav for waq
   real(kind=dp) :: md_dt_waqproc = 0.0_dp !< processes time step
   real(kind=dp) :: md_dt_waqbal = 0.0_dp !< mass balance output time step (old)

   ! TODO: reading for trachytopes is still within rdtrt, below was added for partitioning (when no initialization)
   character(len=4) :: md_trtrfile = ' ' !< Variable that stores information if trachytopes are used ('Y') or not ('N')
   character(len=max_prop_length) :: md_trtdfile = ' ' !< File containing trachytopes definitions
   character(len=max_prop_length) :: md_trtlfile = ' ' !< File containing distribution of trachytope definitions
   integer :: md_mxrtrach = 8 !< Maximum recursion level for combined trachytope definitions
   character(len=max_prop_length) :: md_trtcllfile = ' ' !< Overall calibration factor file for roughness from trachytopes (see also [calibration] block)
   real(kind=dp) :: md_mnhtrach = 0.1_dp !< Minimum water depth for roughness computations
   integer :: md_mthtrach = 1 !< Area averaging method, 1: Nikuradse k based, 2: Chezy C based (parallel and serial)

   character(len=max_prop_length) :: md_mptfile = ' ' !< File (.mpt) containing fixed map output times w.r.t. RefDate (in TUnit)
   character(len=max_prop_length) :: md_ctvfile = ' ' !< File (.ctv) containing fixed com output times w.r.t. RefDate (in TUnit)

! calibration factor
   character(len=max_prop_length) :: md_cldfile = ' ' !< File containing calibration definitions
   character(len=max_prop_length) :: md_cllfile = ' ' !< File containing distribution of calibration definitions area percentage

! incremental output
   character(len=max_prop_length) :: md_classmap_file = ' ' !< File for output of classes output

   character(len=200) :: md_snapshotdir = ' ' !< Directory where hardcopy snapshots should be saved.
                                                 !! Created if non-existent.

   integer :: md_input_specific = 0 !< use (0: no, 1: yes) specific hardcoded input.
   integer :: md_snapshot_seqnr = 0 !< Sequence number of last snapshot file written.
!   partitioning command line options
   integer :: md_japartition = 0 !< partition (1) or not (0)
   integer :: md_pmethod = 1 !< partition method: K-way (=1, default), Recursive Bisection(=2), Mesh-dual(=3)
   integer :: md_ndomains = 0 !< METIS/number of domains (>0) or use polygon (0)
   integer :: md_jacontiguous = 1 !< METIS/contiguous domains (1, default) or not (0)
   integer :: md_icgsolver = 0 !< intended solver
   integer :: md_genpolygon = 0 !< generate partition polygons and use it in parallel runs (1) or writing cell subdomain information to partitioned net files (0)
   integer :: md_partugrid = 0 !< partitioned netfile is ugrid or not
   integer :: md_partseed = 0 !< User-defined seed value, passed to METIS. Useful for reproducible partitionings, but only used when /= 0.
   integer :: md_jaopenGL = 0 !< use openGL (1) or not (0)
   integer :: md_jagridgen = 0 !< Commandline-based simple grid generation.
   integer :: md_jarefine = 0 !< sample based mesh refinement or not
   integer :: md_jamake1d2dlinks = 0 !< Make 1D2D links from commandline (1) or not (0)
   integer :: md_numthreads = 0 !< number of openmp threads to set (0: default)
   integer :: md_jatest = 0 !< only perform a (speed)test (1), or not (0)
   integer :: md_M = 1024 !< size of x in Axpy
   integer :: md_N = 2048 !< size of y in Axpy
   integer :: md_Nruns = 10 ! number of test runs
   integer :: md_soltest = 0 !< solver test (1) or not (0)
   integer :: md_CFL = 0 !< wave-based Courant number (if > 0)
   integer :: md_maxmatvecs = 0 !< maximum number of matrix-vector multiplications in Krylov solver (if > 0 )
   integer :: md_epscg = 0 !< -10log(epscg) (if > 0), tolerance in (inner) Krylov iterations
   integer :: md_epsdiff = 0 !< -10log(epsdiff) (if > 0), tolerance in (outer) Schwarz iterations
   integer :: md_convnetcells = 0 !< Convert _net.nc files with only netnodes/links into _net.nc files with netcell info.
   integer :: md_findcells = 0 !< read netcell info from files and bypass findcells. If not 0, findcells are called.
   integer :: md_pressakey = 0 !< press a key (1) or not (0)
   character(len=128) :: md_cfgfile = ' ' !< cfg-file
   integer :: md_jasavenet = 0 !< save network ito UGRID file after reading input network (1) or not (0)
   integer :: md_exportnet_bedlevel = 0 !< Export interpreted bed levels after initialization (1) or not (0)
   integer :: md_cutcells = 0
   integer :: npolf = 0 !< nr of polygonplotfiles saved with n key in editpol
   logical :: md_usecaching !< Use and/or generate cache file if true

   integer :: md_convertlongculverts = 0 !< convert culverts (and exit program) yes (1) or no (0)
   character(len=128) :: md_culvertprefix = ' ' !< prefix for generating long culvert files
   character(len=128) :: md_dambreak_widening_method !< method for dambreak widening

   integer, parameter :: IFORMAT_NETCDF = 1
   integer, parameter :: IFORMAT_TECPLOT = 2 !< No longer_supported, used for error message
   integer, parameter :: IFORMAT_NETCDF_AND_TECPLOT = 3 !< No longer_supported, used for error message
   integer, parameter :: IFORMAT_UGRID = 4

   integer :: md_mapformat !< map file output format (one of IFORMAT_*)
   integer :: md_unc_conv !< Unstructured NetCDF conventions (either UNC_CONV_CFOLD or UNC_CONV_UGRID)
   integer :: md_ncformat !< NetCDF format (3: classic, 4: NetCDF4+HDF5)
   logical :: md_nccompress !< Whether or not to apply compression to NetCDF output files - NOTE: only works when NcFormat = 4
   integer :: md_fou_step !< determines if fourier analysis is updated at the end of the user time step or comp. time step

contains

!> get output directory, lives here to avoid cyclic dependencies.
   function getoutputdir(dircat)
      use m_flowtimes
      use system_utils, only: FILESEP
      use m_datum2, only: datum2
      implicit none

      character(len=*), optional, intent(in) :: dircat !< (optional) The type of the directory: currently supported only 'waq'.
      character(len=255) :: getoutputdir

      character(len=16) :: dircat_

      if (present(dircat)) then
         dircat_ = dircat
      else
         dircat_ = ''
      end if

      call datum2(rundat2)
      select case (trim(dircat_))
      case ('waq')
         if (len_trim(md_waqoutputdir) == 0) then
            getoutputdir = 'DFM_DELWAQ_'//trim(md_ident_sequential)//trim(rundat2)
         else
            getoutputdir = trim(md_waqoutputdir)//FILESEP
         end if

      case default
         if (len_trim(md_outputdir) == 0) then
            !     default
            if (len_trim(md_ident_sequential) > 0) then
               getoutputdir = 'DFM_OUTPUT_'//trim(md_ident_sequential)//trim(rundat2)
            else
               getoutputdir = 'DFM_OUTPUT_'//trim(rundat2)
            end if
         else
            getoutputdir = trim(md_outputdir)//FILESEP
         end if
      end select

      return
   end function getoutputdir

end module m_unstruc_model_data

module m_unstruc_netcdf_data

   use m_ug_meta, only: t_ug_meta
   use m_ug_mesh, only: t_ug_mesh
   use m_ug_meshgeom, only: t_ug_meshgeom
   use m_ug_network, only: t_ug_network
   use m_ug_contacts, only: t_ug_contact
   use m_ug_crs, only: t_crs

   implicit none(type, external)

!> D-Flow FM-specific flow geometry object, intended for two features:
!! * UGRID-compliant output must write 1D and 2D mesh parts as separate geometries.
!! * An optional masking polygon to restrict output to a particular region needs bookkeeping of
!!   the reduced geometry numbering in relation to the original global numbering.
!! The t_ug_meshgeom members hold the UGRID-compliant mesh data (nodes, edges, faces,
!! connectivity) for 1d and 2d separately, such that they can be passed directly to io_ugrid write routines.
!! The remaining members hold D-FlowFM-specific administration, amongst others mapping from 1D+2D
!! global numbering in D-Flow FM to the 1D/2D separate (possibly mask-reduced) UGRID numbering.
   type t_fm_flowgeom

      type(t_ug_meshgeom) :: mesh2D !< Node/edge/face topology and coordinates for the 2D mesh.
      type(t_ug_meshgeom) :: mesh1D !< Node/edge/face topology and coordinates for the 1D mesh.

      integer, dimension(:), allocatable :: face_map !< 2D: mapping from reduced output set UGRID face index to global flow cell number.
      integer, dimension(:), allocatable :: edge_map !< 2D: mapping from reduced output set UGRID node index to global flow node number.
      integer, dimension(:), allocatable :: node_map !< 2D: mapping from reduced output set UGRID node index to global flow node number.
      integer, dimension(:), allocatable :: node_map_1d !< 1D: mapping from reduced output set UGRID node index to global flow node number.

      integer, dimension(:), allocatable :: edge_type !< Edge type array (size numl2d): encodes the flow-link relation for each 2D mesh edge.
      integer, dimension(:), allocatable :: edgetoln !< 1D: mapping from mesh1D UGRID edge index to flow link number.
      integer, dimension(:), allocatable :: contactstoln !< 1D2D: mapping from contact index to flow link number.
      integer, dimension(:, :), allocatable :: contacts !< 1D2D contact node pairs [2, n1d2dcontacts].
      integer, dimension(:), allocatable :: contacttype !< 1D2D contact type per contact entry.
      integer :: n1d2dcontacts = 0 !< Number of 1D2D contacts.

      !> fm-specific counters, difference between internal and boundary nodes/links, needed for output writing
      integer :: lnx2d_int = 0 !< Number of internal 2D flow links in the output set.
      integer :: lnx2d_bnd = 0 !< Number of boundary 2D flow links in the output set.
      integer :: numl2d_closed = 0 !< Number of closed 2D edges in the output set.
      integer :: ndx_out = 0 !< Total output nodes (3D work array loop bound).
      integer, allocatable :: netlink_perm(:) !< Pre-computed permutation for UNC_LOC_L writing.

   end type t_fm_flowgeom

   integer :: nerr_
   logical :: err_firsttime_
   character(len=255) :: err_firstline_
   integer :: err_level_

!> All NetCDF files should be opened through unc_open or unc_create,
!! such that all opened files are maintained and can be properly closed
!! upon exit of the program by unc_closeall.
   integer, parameter :: maxopenfiles = 50
   character(len=255) :: open_files_(maxopenfiles) !< Names of open NetCDF files.
   integer :: open_datasets_(maxopenfiles) !< Dataset IDs of open NetCDF files.
   integer :: nopen_files_ = 0 !< Nr. of NetCDF files currently open.

   integer, parameter :: UNC_CONV_CFOLD = 1 !< Old CF-only conventions.
   integer, parameter :: UNC_CONV_UGRID = 2 !< New CF+UGRID conventions.

   integer :: unc_cmode = 0 !< Default NetCDF creation mode flag value, used in nf90_create calls (e.g., NF90_NETCDF4).
   logical :: unc_nccompress !< Whether or not to apply compression to NetCDF output files - NOTE: only works when NcFormat = 4
   integer :: unc_nounlimited !< NetCDF output with time dimension set to full length of simulation, avoids "unlimited dimension" overhead. Often requires md_ncformat=4/unc_cmode=NF90_NETCDF4.
   integer :: unc_noforcedflush !< Do not force NetCDF file flushing every output timestep (map-like files).
   integer :: unc_writeopts !< Default write options (currently only: UG_WRITE_LATLON)
   integer :: unc_uuidgen !< Generate UUID and store into each newly created NetCDF file.

! The following location codes generalize for 1D/2D/3D models. See function unc_def_var_map for the details.

   integer, parameter :: MAX_ID_VAR = 4 !< Maximum dimension for id_var arrays

   type(t_ug_meta) :: ug_meta_fm !< Meta information on file.
   character(len=255) :: unc_metadatafile !< Input metadata NetCDF file to be included into other NetCDF output files, (e.g., *_meta.nc)
   character(len=64) :: unc_meta_md_ident !< Identifier of the model, provided via unstruc_model, to be used in pattern substitution of attribute values.
   character(len=64) :: unc_meta_net_file !< Filename of input net/grid file, provided via unstruc_model, to be used in pattern substitution of attribute values.

!> List of attribute names that are forbidden to be set via a custom metadata file by the user.
   character(len=32), dimension(19), parameter :: unc_meta_forbidden_atts = [character(len=32) :: &
                                                                             'references', &
                                                                             'source', &
                                                                             'history', &
                                                                             'Conventions', &
                                                                             'uuid', &
                                                                             'date_created', &
                                                                             'date_modified', &
                                                                             'geospatial_bounds', &
                                                                             'geospatial_bounds_crs', &
                                                                             'geospatial_lat_min', &
                                                                             'geospatial_lat_max', &
                                                                             'geospatial_lat_units', &
                                                                             'geospatial_lon_min', &
                                                                             'geospatial_lon_max', &
                                                                             'geospatial_lon_units', &
                                                                             'time_coverage_start', &
                                                                             'time_coverage_end', &
                                                                             'time_coverage_duration', &
                                                                             'time_coverage_resolution' &
                                                                             ]

!> List of attribute names that can be set via environment variables.
!! Associated environment variable name for a particular attname is DFM_META_<str_toupper(attname)>.
   character(len=32), dimension(3), parameter :: unc_meta_fromenv_atts = [character(len=32) :: &
                                                                          'creator_name', &
                                                                          'creator_email', &
                                                                          'creator_url' &
                                                                          ]

! This type collects the time and space administration relevant for repeat writes to
! netcdf files in FM
! The original t_unc_mapids now incorporates this type for time and space dims
!
   type t_unc_timespace_id

      type(t_ug_mesh) :: meshids1d
      type(t_ug_mesh) :: meshids2d
      type(t_ug_mesh) :: meshids3d
      type(t_ug_network) :: network1d
      type(t_ug_contact) :: meshcontact_1D2D
      type(t_ug_contact) :: meshcontact_2D2D

      !
      ! Dimensions
      !
      integer :: id_timedim = -1 !< Time dimension (the only nf90_unlimited in file).
      integer :: id_laydim = -1 !< Layer (center) dimension. TODO: AvD: to be moved to meshids3d
      integer :: id_wdim = -1 !< Layer interfaces dimension. TODO: AvD: to be moved to meshids3d.
      !id_flowelemdim, &
      integer :: id_maxfracdim = -1 !<
      integer :: id_erolaydim = -1 !< Dimension ID for location of erodable layer thickness.
      integer :: id_sedtotdim = -1 !< Dimension ID for number of all sediment fractions.
      integer :: id_sedsusdim = -1 !< Dimension ID for number of suspended sediment fractions.
      ! arrays to identify 1d mesh and 1d2d contacts
      integer, allocatable :: edgetoln(:)
      integer, allocatable :: contactstoln(:)
      ! geometry fieldss
      integer :: id_flowelemba(MAX_ID_VAR) = -1 !< Variable ID for flow node bottom area (on 1D, 2D, 3D, 1D2D grid parts resp.).
      integer :: id_flowelembl(MAX_ID_VAR) = -1 !< Variable ID for flow node bed level (on 1D, 2D, 3D, 1D2D grid parts resp.).
      integer :: id_bldepth(MAX_ID_VAR) = -1 !< Variable ID for sea floor depth below geoid (for sigma layering in map/any flowgeom output file)
      integer :: id_s1max(MAX_ID_VAR) = -1 !< Variable ID for maximum water level (for sigma layering in Fourier output file)

      integer :: id_flowelemcrsz(MAX_ID_VAR) = -1 !< Variable ID for cross-section point levels passing through flow node (on 1D).
      integer :: id_flowelemcrsn(MAX_ID_VAR) = -1 !< Variable ID for cross-section point widths passing through flow node (on 1D).
      integer :: id_jmax = -1
      integer :: id_nCrs = -1
      integer :: id_morCrsName = -1
      integer :: id_netnodez(MAX_ID_VAR) = -1 !< Variable ID for net node bed level. TODO: AvD: UNST-1318: consider removing here.

      integer :: id_nlyrdim = -1 !< Dimension ID for number of bed layers in bed stratigraphy
      integer :: id_ntheta = -1 !< Dimension ID for number of wave directional bins in surfbeat model

      integer :: id_flowelemdomain(MAX_ID_VAR) = -1 ! domain number of flow elem (face)
      integer :: id_flowelemglobalnr(MAX_ID_VAR) = -1 ! global flow element numbering

      integer :: idx_curtime = 0 !< Index of current time (typically of latest snapshot being written).

      integer :: id_strlendim = -1 !< string length for e.g. sediment fraction names. To do AvD: should this go here?

   end type t_unc_timespace_id

!> This type collects all NetCDF ids that are relevant for repeated file writing.
!! Not only the file pointer, but also all variable ids, dimension ids, etc.
!! Create a separate variable of this type for each map file.
   type t_unc_mapids
      !
      ! Toplevel
      !
      integer :: ncid = 0 !< NetCDF data set id (typically NetCDF file pointer)
      type(t_unc_timespace_id) :: id_tsp
      !type(t_ug_mesh)     :: meshids1d
      !type(t_ug_mesh)     :: meshids2d
      !type(t_ug_mesh)     :: meshids3d
      !type(t_ug_network)  :: network1d
      !type(t_ug_contact) :: meshcontact_1D2D
      !
   !!
   !! Dimensions
   !!
      !integer :: id_timedim = -1 !< Time dimension (the only nf90_unlimited in file).
      !integer :: id_laydim  = -1 !< Layer (center) dimension. TODO: AvD: to be moved to meshids3d
      !integer :: id_wdim    = -1 !< Layer interfaces dimension. TODO: AvD: to be moved to meshids3d.
   !!id_flowelemdim, &
      !integer :: id_maxfracdim = -1 !<
      !integer :: id_erolaydim  = -1 !< Dimension ID for location of erodable layer thickness.
      !integer :: id_sedtotdim  = -1 !< Dimension ID for number of all sediment fractions.
      !integer :: id_sedsusdim  = -1 !< Dimension ID for number of suspended sediment fractions.
   !! arrays to identify 1d mesh and 1d2d contacts
      !integer, allocatable :: edgetoln(:)
      !integer, allocatable :: contactstoln(:)
      !
      !integer :: id_nlyrdim    = -1 !< Dimension ID for number of bed layers in bed stratigraphy
      !integer :: id_ntheta     = -1 !< Dimension ID for number of wave directional bins in surfbeat model
      ! TODO: AvD: replace all data var ids below by 1D/2D/3D generalization.
      !
      ! Data variables
      !
      !integer :: id_flowelemba(MAX_ID_VAR)     = -1 !< Variable ID for flow node bottom area (on 1D, 2D, 3D, 1D2D grid parts resp.).
      !integer :: id_flowelembl(MAX_ID_VAR)     = -1 !< Variable ID for flow node bed level (on 1D, 2D, 3D, 1D2D grid parts resp.).
      !integer :: id_flowelemcrsz(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point levels passing through flow node (on 1D).
      !integer :: id_flowelemcrsn(MAX_ID_VAR)   = -1 !< Variable ID for cross-section point widths passing through flow node (on 1D).
      !integer :: id_jmax
      !integer :: id_netnodez(MAX_ID_VAR)       = -1 !< Variable ID for net node bed level. TODO: AvD: UNST-1318: consider removing here.
      integer :: id_time = -1 !< Variable ID for
      integer :: id_timestep = -1 !< Variable ID for
      integer :: id_numlimdt(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_s1(MAX_ID_VAR) = -1 !< Variable ID for water level (on 1D, 2D, 3D grid parts resp.)
      integer :: id_evap(MAX_ID_VAR) = -1 !< Variable ID for prescribed evaporation
      integer :: id_potevap(MAX_ID_VAR) = -1 !< Variable ID for potential evaporation
      integer :: id_qin(MAX_ID_VAR) = -1 !< Variable ID for sum of all influxes
      integer :: id_actevap(MAX_ID_VAR) = -1 !< Variable ID for actual evaporation
      integer :: id_s0(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_hs(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_vol1(MAX_ID_VAR) = -1 !< Variable ID for volume
      integer :: id_au(MAX_ID_VAR) = -1 !< Variable ID for flow area
      integer :: id_taus(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_tausmax(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_tausx(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_tausy(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_tidep(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_salp(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_IntTidesDiss(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucx(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucy(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucz(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucmag(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucdir(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucxa(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucya(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucmaga(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucxq(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ucyq(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_hu(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_q1(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_q1main(MAX_ID_VAR) = -1 !< Variable ID for main channel discharge (1D quantity)
      integer :: id_fwel(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_u1(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_u0(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_viu(MAX_ID_VAR) = -1 !< Variable ID for horizontal eddy viscosity
      integer :: id_diu(MAX_ID_VAR) = -1 !< Variable ID for horizontal eddy diffusivity
      integer :: id_ww1(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_rho(MAX_ID_VAR) = -1 !< Variable ID for in-situ density of water
      integer :: id_potential_density(MAX_ID_VAR) = -1 !< Variable ID for potential density of water
      integer :: id_sa1(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_tem1(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_nrfld(MAX_ID_VAR) = -1 !< Variable ID for
      integer, dimension(:, :), allocatable :: id_const !< Variable ID for (3, NUM_CONST) constituents (on 1D, 2D, 3D grid parts resp.)
      integer, dimension(:, :), allocatable :: id_wqb !< Variable ID for (3, numwqbots) water quality bottom variables output (on 2D grid only)
      integer, dimension(:, :), allocatable :: id_wqb3d !< Variable ID for (3, numwqbots) water quality bottom variables output (on 3D grid only)
      integer, dimension(:, :), allocatable :: id_waq !< Variable ID for (3, noout) waq output (on 1D, 2D, 3D grid parts resp.)
      integer, dimension(:, :), allocatable :: id_wqst !< Variable ID for (3, noout) waq time stat output (on 1D, 2D, 3D grid parts resp.)
      integer, dimension(:, :), allocatable :: id_wqse !< Variable ID for (3, noout) waq end stat output (on 1D, 2D, 3D grid parts resp.)
      integer :: id_mba(MAX_ID_VAR) = -1 !< Variable ID for mass balance areas
      integer, dimension(:, :), allocatable :: id_sed !< Variable ID for
      integer, dimension(:, :), allocatable :: id_ero !< Variable ID for
      integer :: id_cfcl(MAX_ID_VAR) = -1 !< Variable ID for netlink data of calibration factor for friction
      integer :: id_cftrt(MAX_ID_VAR) = -1 !< Variable ID for netlink data of friction from trachytopes
      integer :: id_czs(MAX_ID_VAR) = -1 !< Variable ID for flow node data of chezy roughness
      integer :: id_czu(MAX_ID_VAR) = -1 !< Variable ID for flow link data of chezy roughness
      integer :: id_cfu(MAX_ID_VAR) = -1 !< Variable ID for flow link data of input roughness
      integer :: id_cfutyp(MAX_ID_VAR) = -1 !< Variable ID for flow link data of input roughness type
      integer :: id_qsun(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qeva(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qcon(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qlong(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qfreva(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qfrcon(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_qtot(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_rain(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_icepths(MAX_ID_VAR) = -1 !< Variable ID for interception layer waterdepth.
      integer :: id_wind(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_air_pressure(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ice_s1(MAX_ID_VAR) = -1 !< Variable ID for water level of open water (in between ice floes)
      integer :: id_ice_zmin(MAX_ID_VAR) = -1 !< Variable ID for lower surface of ice
      integer :: id_ice_zmax(MAX_ID_VAR) = -1 !< Variable ID for surface of ice
      integer :: id_ice_area_fraction(MAX_ID_VAR) = -1 !< Variable ID for sea_ice_area_fraction
      integer :: id_ice_thickness(MAX_ID_VAR) = -1 !< Variable ID for sea_ice_thickness
      integer :: id_ice_pressure(MAX_ID_VAR) = -1 !< Variable ID for the pressure exerted by the sea ice cover
      integer :: id_ice_temperature(MAX_ID_VAR) = -1 !< Variable ID for temperature of the ice cover
      integer :: id_snow_thickness(MAX_ID_VAR) = -1 !< Variable ID for snow_thickness
      integer :: id_snow_temperature(MAX_ID_VAR) = -1 !< Variable ID for temperature of the snow cover
      integer :: id_air_temperature(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_relative_humidity(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_cloudiness(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_E(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_R(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_hwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_twav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_phiwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_mxwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_mywav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_dsurf(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_dwcap(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_distot(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_D(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_DR(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_Df(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_uorb(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_thetamean(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_cwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_cgwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_kwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_nwav(MAX_ID_VAR) = -1
      integer :: id_l1(MAX_ID_VAR) = -1
      integer :: id_ctheta(MAX_ID_VAR) = -1
      integer :: id_sigmwav(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_SwE(MAX_ID_VAR) = -1 !< Variable ID for wind source term on E
      integer :: id_SwT(MAX_ID_VAR) = -1 !< Variable ID for wind source term on T
      integer :: id_ustokes(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_vstokes(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_Fx(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_Fy(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_Fxlink(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_Fylink(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ustokeslink(MAX_ID_VAR) = -1
      integer :: id_vstokeslink(MAX_ID_VAR) = -1
      integer :: id_Sxx(MAX_ID_VAR) = -1
      integer :: id_Syy(MAX_ID_VAR) = -1
      integer :: id_Sxy(MAX_ID_VAR) = -1
      integer :: id_dsdx(MAX_ID_VAR) = -1
      integer :: id_dsdy(MAX_ID_VAR) = -1
      integer :: id_ducxdx(MAX_ID_VAR) = -1
      integer :: id_ducxdy(MAX_ID_VAR) = -1
      integer :: id_ducydx(MAX_ID_VAR) = -1
      integer :: id_ducydy(MAX_ID_VAR) = -1
      integer :: id_windx(MAX_ID_VAR) = -1 !< Variable ID for wind on cell center, x-component
      integer :: id_windy(MAX_ID_VAR) = -1 !< Variable ID for wind on cell center, y-component
      integer :: id_windxu(MAX_ID_VAR) = -1 !< Variable ID for wind on flow links, x-component
      integer :: id_windyu(MAX_ID_VAR) = -1 !< Variable ID for wind on flow links, y-component
      integer :: id_windstressx(MAX_ID_VAR) = -1 !< Variable ID for wind stress, on cell center, x-component
      integer :: id_windstressy(MAX_ID_VAR) = -1 !< Variable ID for wind stress, on cell center, y-component
      integer :: id_air_density(MAX_ID_VAR) = -1 !< Variable ID for air density
      integer :: id_turkin1(MAX_ID_VAR) = -1 !< Variable ID for turbulent kinetic energy
      integer :: id_vicwwu(MAX_ID_VAR) = -1 !< Variable ID for turbulent vertical eddy viscosity at velocity points
      integer :: id_vicwws(MAX_ID_VAR) = -1 !< Variable ID for turbulent vertical eddy viscosity at pressure points
      integer :: id_tureps1(MAX_ID_VAR) = -1 !< Variable ID for turbulent kinetic energy dissipation
      integer :: id_sbcx(MAX_ID_VAR) = -1 !< Variable ID for current related bedload sediment transport at cell centre before upwinding, secondary flow and bed slope effect (x-component)
      integer :: id_sbcy(MAX_ID_VAR) = -1 !< Variable ID for current related bedload sediment transport at cell centre before upwinding, secondary flow and bed slope effect (y-component)
      integer :: id_sbcx_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for reconstructed bedload sediment transport at cell centre after upwinding, secondary flow and bed slope effect (x-component)
      integer :: id_sbcy_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for reconstructed bedload sediment transport at cell centre after upwinding, secondary flow and bed slope effect (y-component)
      integer :: id_sbwx(MAX_ID_VAR) = -1 !< Variable ID for wave related bedload sediment transport at cell centre before upwinding and bed slope effect (x-component)
      integer :: id_sbwy(MAX_ID_VAR) = -1 !< Variable ID for wave related bedload sediment transport at cell centre before upwinding and bed slope effect (y-component)
      integer :: id_sbwx_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for wave related bedload sediment transport at cell centre after upwinding and bed slope effect (x-component)
      integer :: id_sbwy_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for wave related bedload sediment transport at cell centre after upwinding and bed slope effect (y-component)
      integer :: id_sswx(MAX_ID_VAR) = -1 !< Variable ID for wave related suspended sediment transport at cell centre before upwinding and bed slope effect (x-component)
      integer :: id_sswy(MAX_ID_VAR) = -1 !< Variable ID for wave related suspended sediment transport at cell centre before upwinding and bed slope effect (y-component)
      integer :: id_sswx_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for wave related suspended sediment transport at cell centre after upwinding and bed slope effect (x-component)
      integer :: id_sswy_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for wave related suspended sediment transport at cell centre after upwinding and bed slope effect (y-component)
      integer :: id_sscx(MAX_ID_VAR) = -1 !< Variable ID for current related suspended sediment transport at cell centre before upwinding and bed slope effect (x-component)
      integer :: id_sscy(MAX_ID_VAR) = -1 !< Variable ID for current related suspended sediment transport at cell centre before upwinding and bed slope effect (y-component)
      integer :: id_sscx_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for current related suspended sediment transport at cell centre after upwinding and bed slope effect (x-component)
      integer :: id_sscy_reconstructed(MAX_ID_VAR) = -1 !< Variable ID for current related suspended sediment transport at cell centre after upwinding and bed slope effect (y-component)
      integer :: id_sbxcum(MAX_ID_VAR) = -1 !< Variable ID's for time-averaged cell centre transports
      integer :: id_sbycum(MAX_ID_VAR) = -1
      integer :: id_ssxcum(MAX_ID_VAR) = -1
      integer :: id_ssycum(MAX_ID_VAR) = -1
      integer :: id_sbn(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_sbt(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_sst(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ssn(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_sbwn(MAX_ID_VAR) = -1
      integer :: id_sbwt(MAX_ID_VAR) = -1
      integer :: id_sswn(MAX_ID_VAR) = -1
      integer :: id_sswt(MAX_ID_VAR) = -1
      integer :: id_sbcn(MAX_ID_VAR) = -1
      integer :: id_sbct(MAX_ID_VAR) = -1
      integer :: id_sourse(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_sinkse(MAX_ID_VAR) = -1
      integer :: id_scrn(MAX_ID_VAR) = -1
      integer :: id_zk(MAX_ID_VAR) = -1 ! TODO: AvD: HK's timedep zk
      integer :: id_bl(MAX_ID_VAR) = -1 ! TODO: AvD: HK's timedep bl
! nudging
      integer :: id_nudge_time(MAX_ID_VAR) = -1 ! nudging time
      integer :: id_nudge_salinity(MAX_ID_VAR) = -1 ! nudging salinity
      integer :: id_nudge_temperature(MAX_ID_VAR) = -1 ! nudging temperature
      integer :: id_nudge_Dsal(MAX_ID_VAR) = -1 ! difference of nudging salinity with salinity
      integer :: id_nudge_Dtem(MAX_ID_VAR) = -1 ! difference of nudging temperature with temperature
!vegetation
      integer :: id_rnveg(MAX_ID_VAR) = -1 !< Variable ID for vegetation stem density
      integer :: id_diaveg(MAX_ID_VAR) = -1 !< Variable ID for vegetation stem diameter
      integer :: id_veg_stemheight(MAX_ID_VAR) = -1 !< Variable ID for vegetation stem height
! particles
      integer :: id_depth_averaged_particle_concentration(MAX_ID_VAR) = -1 ! depth-averaged particle concentration
! for parallel
      !integer :: id_flowelemdomain(MAX_ID_VAR) = -1 ! domain number of flow elem (face)
      !integer :: id_flowelemglobalnr(MAX_ID_VAR) = -1 ! global flow element numbering

      integer :: id_zb(MAX_ID_VAR) = -1 !< Variable ID for bed elevation
      !
      integer :: id_spircrv(MAX_ID_VAR) = -1 !< Variable ID for flow streamline curvature
      integer :: id_spirint(MAX_ID_VAR) = -1 !< Variable ID for spiral intensity
      !
      integer :: id_ws(MAX_ID_VAR) = -1 ! fall velocity
      integer :: id_rsedeq(MAX_ID_VAR) = -1 !
      integer :: id_aks(MAX_ID_VAR) = -1 !
      integer :: id_rca(MAX_ID_VAR) = -1 !
      integer :: id_e_dzdn(MAX_ID_VAR) = -1 !
      integer :: id_e_dzdt(MAX_ID_VAR) = -1 !
      integer :: id_umod(MAX_ID_VAR) = -1 !
      integer :: id_zumod(MAX_ID_VAR) = -1 !
      integer :: id_uuu(MAX_ID_VAR) = -1 !
      integer :: id_vvv(MAX_ID_VAR) = -1 !
      integer :: id_ustar(MAX_ID_VAR) = -1 !
      integer :: id_sxtot(MAX_ID_VAR) = -1 !
      integer :: id_sytot(MAX_ID_VAR) = -1 !
      integer :: id_mor_bl(MAX_ID_VAR) = -1 !
      integer :: id_bodsed(MAX_ID_VAR) = -1 !
      integer :: id_dpsed(MAX_ID_VAR) = -1 !
      integer :: id_msed(MAX_ID_VAR) = -1 !
      integer :: id_aldiff(MAX_ID_VAR) = -1 !
      integer :: id_lyrfrac(MAX_ID_VAR) = -1 !
      integer :: id_thlyr(MAX_ID_VAR) = -1 !
      integer :: id_preload(MAX_ID_VAR) = -1 !
      integer :: id_sedshort(MAX_ID_VAR) = -1 !
      integer :: id_poros(MAX_ID_VAR) = -1 !
      integer :: id_duneheight(MAX_ID_VAR) = -1 !
      integer :: id_dunelength(MAX_ID_VAR) = -1 !
      integer :: id_ksr(MAX_ID_VAR) = -1 !
      integer :: id_ksmr(MAX_ID_VAR) = -1 !
      integer :: id_ksd(MAX_ID_VAR) = -1 !
      integer :: id_ks(MAX_ID_VAR) = -1 !
      integer :: id_taub(MAX_ID_VAR) = -1 !
      integer :: id_taurat(MAX_ID_VAR) = -1 !
      integer :: id_dm(MAX_ID_VAR) = -1 !
      integer :: id_dg(MAX_ID_VAR) = -1 !
      integer :: id_dgsd(MAX_ID_VAR) = -1 !
      integer, allocatable, dimension(:, :) :: id_dxx
      integer, allocatable, dimension(:, :, :) :: id_sedpar
      integer :: id_frac(MAX_ID_VAR) = -1
      integer :: id_mudfrac(MAX_ID_VAR) = -1
      integer :: id_sandfrac(MAX_ID_VAR) = -1
      integer :: id_fixfac(MAX_ID_VAR) = -1
      integer :: id_hidexp(MAX_ID_VAR) = -1
      integer :: id_mfluff(MAX_ID_VAR) = -1
      integer :: id_sxwav(MAX_ID_VAR) = -1
      integer :: id_sywav(MAX_ID_VAR) = -1
      integer :: id_sbxwav(MAX_ID_VAR) = -1
      integer :: id_sbywav(MAX_ID_VAR) = -1
      integer :: id_z0c(MAX_ID_VAR) = -1
      integer :: id_z0r(MAX_ID_VAR) = -1
      integer :: id_dtcell(MAX_ID_VAR) = -1
      integer :: id_morft = -1
      integer :: id_morfac = -1
      integer :: id_sedavgtim = -1
      integer :: id_frac_name = -1
      integer :: id_susfrac_name = -1
      integer :: id_sedfrac(MAX_ID_VAR) = -1
      integer :: id_kmxsed(MAX_ID_VAR) = -1
      integer :: id_subsupl(MAX_ID_VAR) = -1
      ! for 1d only
      integer :: id_adve(MAX_ID_VAR) = -1
      integer :: id_advi(MAX_ID_VAR) = -1
      integer :: id_q1d_1(MAX_ID_VAR) = -1
      integer :: id_q1d_2(MAX_ID_VAR) = -1
      integer :: id_volu1D(MAX_ID_VAR) = -1
      integer :: id_au1d_1(MAX_ID_VAR) = -1
      integer :: id_au1d_2(MAX_ID_VAR) = -1
      integer :: id_wu1d_1(MAX_ID_VAR) = -1
      integer :: id_wu1d_2(MAX_ID_VAR) = -1
      integer :: id_sar1d_1(MAX_ID_VAR) = -1
      integer :: id_sar1d_2(MAX_ID_VAR) = -1
      integer :: id_alpha_mom_1d(MAX_ID_VAR) = -1
      integer :: id_alpha_ene_1d(MAX_ID_VAR) = -1
      ! for urban, only for 1d now
      integer :: id_timewetground(MAX_ID_VAR) = -1 !< Variable ID for cumulative time when water is above ground level
      integer :: id_freeboard(MAX_ID_VAR) = -1 !< Variable ID for freeboard
      integer :: id_hs_on_ground(MAX_ID_VAR) = -1 !< Variable ID for waterdepth when water is above ground level
      integer :: id_vol_on_ground(MAX_ID_VAR) = -1 !< Variable ID for volume when water is above ground level
      integer :: id_qCur1d2d(MAX_ID_VAR) = -1 !< Variable ID for current total 1d2d inflow (discharge)
      integer :: id_vTot1d2d(MAX_ID_VAR) = -1 !< Variable ID for cumulative total 1d2d inflow (volume)
      integer :: id_qCurLat(MAX_ID_VAR) = -1 !< Variable ID for current total lateral inflow (discharge)
      integer :: id_vTotLat(MAX_ID_VAR) = -1 !< Variable ID for cumulative total lateral inflow (volume)
      integer :: id_s1Gradient(MAX_ID_VAR) = -1 !< Variable ID for water level gradient
      ! for river morphology, only for 1d
      integer :: id_blave(MAX_ID_VAR) = -1 !< Variable ID for main channel averaged bed level
      integer :: id_bamor(MAX_ID_VAR) = -1 !< Variable ID for main channel cell area
      integer :: id_wumor(MAX_ID_VAR) = -1 !< Variable ID for main channel width at flow link
      integer :: id_flowelemzcc(MAX_ID_VAR) = -1 !< Variable ID for time dependent layer centre z-coord
      integer :: id_flowelemzcc_bnd(MAX_ID_VAR) = -1 !< Variable ID for time dependent layer centre z-coord bounds
      integer :: id_flowelemzw(MAX_ID_VAR) = -1 !< Variable ID for time dependent layer interface z-coord
      integer :: id_flowlinkzu(MAX_ID_VAR) = -1 !< Variable ID for time dependent layered flow link z-coord
      integer :: id_flowlinkzu_bnd(MAX_ID_VAR) = -1 !< Variable ID for time dependent layered flow link z-coord bounds
      integer :: id_flowlinkzwu(MAX_ID_VAR) = -1 !< Variable ID for time dependent layered flow link interface z-coord
      integer :: id_negdpt(MAX_ID_VAR) = -1 !< Variable ID for number of times negative depth is calculated in a node
      integer :: id_negdpt_cum(MAX_ID_VAR) = -1 !< Variable ID for cumulative number of times negative depth is calculated in a node
      integer :: id_noiter(MAX_ID_VAR) = -1 !< Variable ID for number of times no iteration is generated in a node
      integer :: id_noiter_cum(MAX_ID_VAR) = -1 !< Variable ID for cumulative number of times no iteration is generated in a node
      integer :: id_limtstep(MAX_ID_VAR) = -1 !< Variable ID for number of times a node was limiting for the computational time step
      integer :: id_limtstep_cum(MAX_ID_VAR) = -1 !< Variable ID for cumulative number of times a node was limiting for the computational time step
      integer :: id_courant(MAX_ID_VAR) = -1 !< Variable ID for the Courant number in a node
      !
      ! for debug purposes JRE
      integer :: id_dbg1d(MAX_ID_VAR) = -1 !< Variable ID for the 1D debug output array
      integer :: id_dbg2d(MAX_ID_VAR) = -1 !< Variable ID for the 2D debug output array
      integer :: id_dbg3d(MAX_ID_VAR) = -1 !<  Variable ID for the 3D debug output array
      !
      ! Other
      !
      !integer :: idx_curtime  = 0  !< Index of current time (typically of latest snapshot being written).
   end type t_unc_mapids

!> type for clustering ids regarding netelements and netlinks
!! only used within this module, but between a few functions
   type t_unc_netelem_ids
      integer :: id_netelemmaxnodedim !< id for netelemmaxnodedim
      integer :: id_netelemdim !< id for netelemdim
      integer :: id_netlinkcontourptsdim !< id for netlinkcontourptsdim
      integer :: id_netlinkdim !< id for netlinkdim
      integer :: id_netelemnode !< id for netelemnode
      integer :: id_netelemlink !< id for netelemlink
      integer :: id_netlinkcontourx !< id for netlinkcontourx
      integer :: id_netlinkcontoury !< id for netlinkcontoury
      integer :: id_netlinkxu !< id for netlinkxu
      integer :: id_netlinkyu !< id for netlinkyu
   end type t_unc_netelem_ids

!> type for the administration of reading a merged map/rst file
   type t_unc_merged
      integer :: jamergedmap !< 0:input was NOT read from a merged map file (i.e. requires no shift), 1:input WAS read from a merged map file
      integer :: jafillghost !< 0:omit (use kdtree for this) or 1:perform filling certain variables at ghostcells from the map
      integer :: jamergedmap_same !< 0:merged, but NOT from the same partitioning, 1:merged and from the same partitioning
      integer :: idmn_ghost !< domain number of links to ghostcells
      integer :: ndxi_own !< number of internal flow nodes in the current domain
      integer :: ndxi_ghost !< number of internal flow nodes in the current domain belonging to a neighbouring domain
      integer :: lnx_own !< number of internal links in the current domain
      integer :: lnx_ghost !< number of internal links in the current domain belonging to a neighbouring domain
      integer :: nbnd_read !< number of boundary flow nodes read
      integer :: ndxi_read !< number of internal flow nodes read
      integer :: lnx_read !< number of nodes/links that are a domain's own (if jampi==0, ndxi_own===ndxi, lnx_own===lnx)
      integer :: id_bnddim !< id for boundary flow elements dimension
      integer, allocatable :: inode_own(:) !< mapping of the local administration of internal flow cells to the global flow cell numbering
      integer, allocatable :: inode_ghost(:) !< mapping of the local administration of ghost cells to the global flow cell numbering
      integer, allocatable :: ilink_own(:) !< mapping of the local administration of internal flow links to the global flow link numbering
      integer, allocatable :: ilink_ghost(:) !< mapping of the local administration of ghost links to the global flow cell numbering
      integer, allocatable :: inodeghost_merge(:) !< like inode_ghost, but from the merged restart file
      integer, allocatable :: ilinkghost_merge(:) !< like ilink_ghost, but from the merged restart file
      integer, allocatable :: ibnd_merge(:) !< mapping of the local administration of boundary flow cells to the global flow cell numbering
      integer, allocatable :: inode_merge(:) !< like inode_own, but from the merged restart file
      integer, allocatable :: ilink_merge(:) !< like ilink_own, but from the merged restart file
   end type t_unc_merged

   type(t_unc_mapids) :: mapids !< Global descriptor for the (open) map-file
   integer :: ihisfile = 0 !< Global netcdf ID of the his-file

   type(t_crs), target :: crs !< crs read from net file, to be written to flowgeom. TODO: AvD: temp, move this global CRS into ug_meshgeom (now a bit difficult with old and new file format)

   character(len=:), allocatable :: face_z_stdname

end module m_unstruc_netcdf_data

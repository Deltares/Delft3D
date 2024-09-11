!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

! TODO: FB: #define NC_CHECK if (ierr .ne. 0 ) call mess(LEVEL_ERROR, nf90_strerror(ierr))

! TODO: AvD:
! * flowgeom should work now for CFOLD and UGRID. Test this before moving to map writer.
! * map writer: start adding data variables
! * waq writer: migrate to new UGRID?
! * com writer: stay at old CF for now???
! * zk in ugrid flowgeom
! * Solve all TODO's.

!> Reads and writes unstructured net/flow data in netCDF format.
module unstruc_netcdf

   use precision
   use netcdf
   use unstruc_messages
   use dflowfm_version_module
   use io_ugrid
   use m_sediment
   use string_module
   use io_netcdf_acdd
   use time_module
   use m_debug
   use m_readyy
   use m_qnerror

   implicit none

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

   private :: nerr_, err_firsttime_, err_firstline_, &
              t_unc_netelem_ids, unc_def_net_elem, unc_write_net_elem, &
              unc_def_idomain, unc_def_iglobal, fill_netlink_geometry, &
              open_files_, open_datasets_, nopen_files_, unc_read_merged_map, t_unc_merged, &
              blcell, read_mesh2d_face_z, face_z_stdname

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
      type(t_ug_contacts) :: meshcontacts

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
      !type(t_ug_contacts) :: meshcontacts
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
      integer :: id_rho(MAX_ID_VAR) = -1 !< Variable ID for density of water
      integer :: id_rhop(MAX_ID_VAR) = -1 !< Variable ID for potential density of water
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
      integer :: id_patm(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_ice_af(MAX_ID_VAR) = -1 !< Variable ID for sea_ice_area_fraction
      integer :: id_ice_h(MAX_ID_VAR) = -1 !< Variable ID for sea_ice_thickness
      integer :: id_ice_p(MAX_ID_VAR) = -1 !< Variable ID for the pressure exerted by the sea ice cover
      integer :: id_ice_t(MAX_ID_VAR) = -1 !< Variable ID for temperature of the ice cover
      integer :: id_snow_h(MAX_ID_VAR) = -1 !< Variable ID for snow_thickness
      integer :: id_snow_t(MAX_ID_VAR) = -1 !< Variable ID for temperature of the snow cover
      integer :: id_tair(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_rhum(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_clou(MAX_ID_VAR) = -1 !< Variable ID for
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
      integer :: id_airdensity(MAX_ID_VAR) = -1 !< Variable ID for air density
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
      integer :: id_sourse(MAX_ID_VAR) = -1 !< Variable ID for
      integer :: id_sinkse(MAX_ID_VAR) = -1
      integer :: id_scrn(MAX_ID_VAR) = -1
      integer :: id_zk(MAX_ID_VAR) = -1 ! TODO: AvD: HK's timedep zk
      integer :: id_bl(MAX_ID_VAR) = -1 ! TODO: AvD: HK's timedep bl
! nudging
      integer :: id_nudge_time(MAX_ID_VAR) = -1 ! nudging time
      integer :: id_nudge_sal(MAX_ID_VAR) = -1 ! nudging salinity
      integer :: id_nudge_tem(MAX_ID_VAR) = -1 ! nudging temperature
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

   interface unc_put_var_map
      module procedure unc_put_var_map_int
      module procedure unc_put_var_map_real
      module procedure unc_put_var_map_dble
      module procedure unc_put_var_map_dble2
      module procedure unc_put_var_map_dble3
   end interface unc_put_var_map

   interface unc_put_var_rst
      module procedure unc_put_var_rst_dble
   end interface unc_put_var_rst

   interface unc_put_att
      module procedure unc_put_att_int
      module procedure unc_put_att_dble
      module procedure unc_put_att_char
   end interface unc_put_att

   real(kind=hp), allocatable :: blcell(:)
   character(len=:), allocatable :: face_z_stdname

contains

!> Initializes some global variables needed for writing NetCDF files during a run.
   subroutine init_unstruc_netcdf()
      use dflowfm_version_module

      integer :: ierr

      ug_meta_fm%institution = trim(company)
      ug_meta_fm%source = trim(product_name)
      ug_meta_fm%references = trim(company_url)
      ug_meta_fm%version = trim(version_full)
      ug_meta_fm%modelname = ''
      unc_metadatafile = ''
      unc_meta_md_ident = ''
      unc_meta_net_file = ''

      ierr = ug_reset_mesh(mapids%id_tsp%meshids1d)
      ierr = ug_reset_mesh(mapids%id_tsp%meshids2d)
      ierr = ug_reset_mesh(mapids%id_tsp%meshids3d)

      unc_cmode = NF90_CLOBBER ! Default: 0, use NetCDF library default.
      unc_writeopts = UG_WRITE_NOOPTS ! Default: 0, no special write options.
      unc_nounlimited = 0 !< NetCDF output with time dimension set to full length of simulation, avoids "unlimited dimension" overhead. Often requires md_ncformat=4/unc_cmode=NF90_NETCDF4.
      unc_noforcedflush = 0 !< Do not force NetCDF file flushing every output timestep (map-like files).
      unc_uuidgen = 1
   end subroutine init_unstruc_netcdf

!> Sets the default NetCDF cmode flag values for all D-Flow FM's created files.
!! Recommended use via calling unc_set_ncformat.
   subroutine unc_set_cmode(cmode)
      integer, intent(in) :: cmode !< NetCDF creation mode flags value, intended for use in nf90_create calls.
      unc_cmode = cmode
   end subroutine unc_set_cmode

!> Sets the default NetCDF format for all D-Flow FM's created files (NetCDF 3 or 4).
   subroutine unc_set_ncformat(iformatnumber)
      use netcdf_utils, only: ncu_format_to_cmode
      integer, intent(in) :: iformatnumber !< The NetCDF format version (3 or 4, colloquially speaking)

      call unc_set_cmode(ncu_format_to_cmode(iformatnumber))
   end subroutine unc_set_ncformat

!> Sets the default NetCDF compression setting (only applied when ncformat = NetCDF 4)
   subroutine unc_set_nccompress(md_nccompress)
      logical, intent(in) :: md_nccompress !< Whether or not to apply compression to NetCDF output files - NOTE: only works when NcFormat = 4
      if (md_nccompress .and. unc_cmode /= nf90_netcdf4) then
         call mess(LEVEL_ERROR, 'NetCDF compression (deflation) is a NetCDF4 feature; make sure NcFormat is set to 4.')
      end if
      unc_nccompress = md_nccompress
   end subroutine unc_set_nccompress

   function unc_add_uuid(ncid) result(ierr)
      use m_universally_unique_id_generator
      use dfm_error
      integer, intent(in) :: ncid !< NetCDF dataset id
      integer :: ierr !< Result status, DFM_NOERR if successful.

      character(len=40) :: uuid

      ierr = DFM_NOERR

      ! Generate the UUID and store it as an attibute
      call generate_uuid(uuid)
      ierr = nf90_put_att(ncid, nf90_global, "uuid", uuid)
   end function unc_add_uuid

!> Wrapper function around ionc_add_time_coverage() to set the time coverage
!! attributes for dflowfm's various output files (map/his, etc.).
!! Input are parameters in seconds since refdat; this subroutine will take
!! care of date calculations, time zones and string conversion.
   function unc_add_time_coverage(ncid, start_since_ref, end_since_ref, resolution) result(ierr)
      use time_module, only: duration_to_string, datetime_to_string, ymd2modified_jul
      use m_flowtimes, only: refdat, tzone
      use dfm_error
      implicit none
      integer, intent(in) :: ncid !< NetCDF dataset id
      double precision, intent(in) :: start_since_ref !< Start of time coverage/output [seconds since refdat]
      double precision, intent(in) :: end_since_ref !< End   of time coverage/output [seconds since refdat]
      double precision, intent(in) :: resolution !< Time interval between outputs [seconds]
      integer :: ierr !< Result status, DFM_NOERR if successful.

      logical :: success_
      real(kind=hp) :: refdate_rjul

      ierr = DFM_NOERR

      success_ = ymd2modified_jul(refdat, refdate_rjul)
      if (.not. success_) then
         ierr = DFM_WRONGINPUT
         return
      end if

      ierr = ionc_add_time_coverage(ncid, datetime_to_string(refdate_rjul + start_since_ref / 86400d0, int(tzone), int(mod(tzone, 1d0) * 60d0), ierr), &
                                    datetime_to_string(refdate_rjul + end_since_ref / 86400d0, int(tzone), int(mod(tzone, 1d0) * 60d0), ierr), &
                                    duration_to_string(end_since_ref - start_since_ref), duration_to_string(resolution))
   end function unc_add_time_coverage

!> Adds additional metadata into an output file, given a separate metadata NetCDF file.
   function unc_meta_add_from_file(ncid, ncmeta_filename) result(ierr)
      use dfm_error
      use netcdf_utils, only: ncu_copy_atts

      integer, intent(in) :: ncid !< NetCDF dataset ID to write into
      character(len=*), intent(in) :: ncmeta_filename !< Filename of NetCDF containing (only) metadata.
      integer :: ierr !< Result status (DFM_NOERR if successful)

      integer :: ncid_meta

      ierr = DFM_NOERR

      if (len_trim(ncmeta_filename) == 0) then
         return
      end if

      ierr = unc_open(ncmeta_filename, nf90_nowrite, ncid_meta)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not open NetCDF metadata file '''//trim(ncmeta_filename)//''' for inclusion in output files.')
         return
      end if

      ierr = ncu_copy_atts(ncid_meta, ncid, nf90_global, nf90_global, unc_meta_forbidden_atts, unc_meta_fill_placeholders)

      ierr = unc_close(ncid_meta)

   end function unc_meta_add_from_file

!> Adds some standard metadata into an output file, if set as environment variables.
   function unc_meta_add_from_environment(ncid) result(ierr)
      use dfm_error
      use netcdf_utils, only: ncu_copy_atts
      use m_alloc

      integer, intent(in) :: ncid !< NetCDF dataset ID to write into
      integer :: ierr !< Result status (DFM_NOERR if successful)

      character(len=9), parameter :: env_prefix = 'DFM_META_'
      integer :: iatt, natt, vallen, istat, ierr_
      character(len=32) :: envvar
      character(len=:), allocatable :: envval

      ierr = DFM_NOERR

      natt = size(unc_meta_fromenv_atts)
      do iatt = 1, natt
         envvar = env_prefix//str_toupper(trim(unc_meta_fromenv_atts(iatt)))
         call get_environment_variable(envvar, length=vallen)
         call realloc(envval, vallen, keepExisting=.false., fill=' ')
         call get_environment_variable(envvar, envval, status=istat)
         if (istat <= 0) then
            if (istat < 0) then
               call mess(LEVEL_WARN, 'While adding metadata from environment variable '//trim(envvar)//': value is too long,  will be trimmed in output file.')
               continue
            end if
            if (len_trim(envval) > 0) then
               ierr_ = unc_meta_fill_placeholders(envval)
               ierr_ = nf90_put_att(ncid, nf90_global, trim(unc_meta_fromenv_atts(iatt)), trim(envval))
               if (ierr_ /= nf90_noerr) then
                  call mess(LEVEL_WARN, 'While adding metadata from environment variable '//trim(envvar)//': error while putting into output file, error code:', ierr_)
                  ierr = ierr_ ! Remember error occurred
               end if
            end if
         end if
      end do

   end function unc_meta_add_from_environment

!> Returns the given valuetext with any placeholder variables substituted by the actual value.
!! Currently supported placeholders:
!!  * ${dfm_md_ident}: Model identifier (MDU name without extension)
!!  * ${dfm_net_file}: Net/grid filename (as specified via MDU NetFile, without the full directory path if present.)
!!  * ${dfm_program_name}: "D-Flow FM"
!!
!! NOTE: this function is an implementation of the netcdf_utils::ncu_apply_to_att interface.
   function unc_meta_fill_placeholders(valuetext) result(ierr)
      use dfm_error
      use dflowfm_version_module, only: product_name

      character(len=:), allocatable, intent(inout) :: valuetext !< attribute value text, placeholders will be replaced in-place.
      integer :: ierr !< Result status (DFM_NOERR if successful)

      ierr = DFM_NOERR

      valuetext = replace_string(valuetext, '${dfm_md_ident}', trim(unc_meta_md_ident))
      valuetext = replace_string(valuetext, '${dfm_net_file}', trim(unc_meta_net_file))
      valuetext = replace_string(valuetext, '${dfm_program_name}', trim(product_name))
   end function unc_meta_fill_placeholders

!> Adds user-defined metadata into an output file.
!! Done in two steps:
!! 1. read from a NetCDF metadata file (if provided).
!! 2. read from some environment variables (if set).
   function unc_meta_add_user_defined(ncid) result(ierr)
      use dfm_error
      integer, intent(in) :: ncid !< NetCDF dataset ID to write into
      integer :: ierr !< Result status (DFM_NOERR if successful)

      integer :: ierr_

      ierr = DFM_NOERR

      ierr_ = unc_meta_add_from_file(ncid, unc_metadatafile)
      if (ierr_ /= DFM_NOERR) then
         ierr = ierr_
      end if

      ierr_ = unc_meta_add_from_environment(ncid)
      if (ierr_ /= DFM_NOERR) then
         ierr = ierr_
      end if

   end function unc_meta_add_user_defined

!> Defines a NetCDF variable that has no spatial dimension, also setting the most used attributes.
!! Typically only used for variables without a space dimension.
!! For variables with either his-station-range or map-grid-range in the dimensions:
!! @see unc_def_var_map @see unc_def_var_his
   function unc_def_var_nonspatial(ncid, id_var, itype, idims, var_name, standard_name, long_name, unit) result(ierr)
      use dfm_error
      implicit none

      integer, intent(in) :: ncid !< NetCDF file unit
      integer, intent(inout) :: id_var !< Returned variable id.
      integer, intent(in) :: itype !< Variable's data type, one of nf90_double, nf90_int, etc.
      integer, intent(in) :: idims(:) !< Array with the dimension ids across which this new variable should range. For example (/ id_flowelem, id_time /).
      character(len=*), intent(in) :: var_name !< Name for this variable in the file.
      character(len=*), intent(in) :: standard_name !< Standard name for this variable. May be empty, otherwise it should be CF-compliant.
      character(len=*), intent(in) :: long_name !< Description text, used in long_name attribute.
      character(len=*), intent(in) :: unit !< Unit for this variable, should be UDUNITS-compliant.
      integer :: ierr !< Result status, DFM_NOERR if successful.

      ierr = DFM_NOERR

      ierr = nf90_def_var(ncid, var_name, itype, idims, id_var)
      if (len_trim(standard_name) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'standard_name', standard_name)
      end if
      if (len_trim(long_name) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'long_name', long_name)
      end if
      if (len_trim(unit) > 0) then
         ierr = nf90_put_att(ncid, id_var, 'units', unit)
      end if

   end function unc_def_var_nonspatial

!> Defines a NetCDF variable inside a map file, taking care of proper attributes and coordinate references.
!! Produces a UGRID-compliant map file.
!! Typical call: unc_def_var(mapids, mapids%id_s1(:), nf90_double, UNC_LOC_S, 's1', 'sea_surface_height', 'water level', 'm')
!! Space-dependent variables will be multiply defined: on mesh1d and mesh2d-based variables (unless specified otherwise via which_meshdim argument).
   function unc_def_var_map(ncid, id_tsp, id_var, itype, iloc, var_name, standard_name, long_name, unit, is_timedep, dimids, cell_method, which_meshdim, jabndnd, ivalid_max) result(ierr)
      use m_save_ugrid_state, only: mesh2dname, mesh1dname, contactname
      use netcdf_utils, only: ncu_append_atts
      use m_flowgeom
      use m_flowparameters, only: jamapvol1, jamapau, jamaphs, jamaphu, jamapanc
      use network_data, only: numl, numl1d
      use dfm_error
      use m_missing
      use fm_location_types

      use string_module, only: strcmpi
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(out) :: id_var(:) !< Resulting variable ids, one for each submesh (1d/2d/3d/1d2d-contact if applicable)
      integer, intent(in) :: itype !< NetCDF data type (e.g. nf90_double).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W, UNC_LOC_WU).
      character(len=*), intent(in) :: var_name !< Variable name for in NetCDF variable, will be prefixed with mesh name.
      character(len=*), intent(in) :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
      character(len=*), intent(in) :: long_name !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
      character(len=*), intent(in) :: unit !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
      integer, optional, intent(in) :: is_timedep !< (Optional) Whether or not (1/0) this variable is time-dependent. (Default: 1)
      integer, optional, intent(in) :: dimids(:) !< (Optional) Array with dimension ids, replaces default dimension ordering. Default: ( layerdim, spatialdim, timedim ).
                                                 !! This array may contain special dummy values: -1 will be replaced by time dim, -2 by spatial dim, -3 by layer dim. Example: (/ -2, id_seddim, -1 /).
      character(len=*), optional, intent(in) :: cell_method !< cell_method for this variable (one of 'mean', 'point', see CF for details). Default: mean
      integer, optional, intent(in) :: which_meshdim !< Selects which (horizontal) mesh dimension(s) need to be defined and written (1: 1D, 2: 2D, 4: 1D2D contacts, 7: all) Default: 7: all.
      integer, optional, intent(in) :: jabndnd
      integer, optional, intent(in) :: ivalid_max !< valid_max attribute for integer variables

      integer :: ierr !< Result status, DFM_NOERR if successful.
! TODO: AvD: inject vectormax dim here AND timedim!!
      character(len=10) :: cell_method_ !< cell_method for this variable (one of 'mean', 'point', see CF for details).
      character(len=50) :: cell_measures !< cell_measures for this variable (e.g. 'area: mesh2d_ba', see CF for details).
      integer :: ndx1d !< Number of 1D nodes.
      integer :: numl2d

      integer, parameter :: maxrank = 5
      integer :: idims(maxrank) !< The (max maxrank) dimensions for this variable, pattern: (id_vectormaxdim, id_spacedim, id_timedim). For time-independent scalar data it is filled as: (<empty>, <empty>, id_spacedim)
      integer :: idx_timedim !< Will point to the position in idims where the time dimension should be injected (typically the slowest index).
      integer :: idx_spacedim !< Will point to the position in idims where the spatial dimension (face/node/edge) should be injected.
      integer :: idx_layerdim !< Will point to the position in idims where the layer dimension (3D) should be injected (only if applicable).
      integer :: idx_fastdim !< Will point to the first used position in idims (i.e. the fastest varying dimension).
      integer :: is_timedep_
      integer :: is_layerdep_
      integer :: ndims, i
      integer :: which_meshdim_
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: varid
      character(len=50) :: checkvars(5) ! small array to check on presence of some variables.

      ierr = DFM_NOERR

      idims = 0

      if (present(is_timedep)) then
         is_timedep_ = is_timedep
      else
         is_timedep_ = 1
      end if

      if (present(which_meshdim)) then
         which_meshdim_ = which_meshdim
      else
         which_meshdim_ = 1 + 2 + 4 ! 1D and 2D and 1d2d contacts (if applicable)
      end if

      if (iloc == UNC_LOC_S3D .or. iloc == UNC_LOC_U3D .or. iloc == UNC_LOC_W .or. iloc == UNC_LOC_WU) then
         is_layerdep_ = 1
      else
         is_layerdep_ = 0
      end if

      ! Set idx_*dim variables.
      idx_timedim = -1
      idx_spacedim = -1
      idx_layerdim = -1
      if (present(dimids)) then
         ! Special case: caller supplied its own idims array:
         ndims = size(dimids, 1)
         if (ndims > maxrank) then
            ierr = UG_NOTIMPLEMENTED
            goto 888
         end if

         ! idims will be filled backward, starting from last element
         idx_fastdim = maxrank - ndims + 1
         idims(idx_fastdim:maxrank) = dimids(1:ndims)

         ! Loop all given dimension ids and detect any macro ids that need to be replaced later with time/space/layer dimension id.
         do i = idx_fastdim, maxrank
            if (idims(i) == -1 .or. idims(i) == id_tsp%id_timedim) then
               idx_timedim = i
            else if (idims(i) == -2) then
               idx_spacedim = i
            else if (idims(i) == -3 .or. idims(i) == id_tsp%id_laydim .or. idims(i) == id_tsp%id_wdim) then
               idx_layerdim = i
            end if
         end do
      else
         ! Use default order of dimensions.
         if (is_layerdep_ > 0) then
            if (is_timedep_ > 0) then
               idx_timedim = maxrank
               idx_spacedim = maxrank - 1
               idx_layerdim = maxrank - 2
            else
               idx_spacedim = maxrank
               idx_layerdim = maxrank - 1
            end if
            idx_fastdim = idx_layerdim
         else
            if (is_timedep_ > 0) then
               idx_timedim = maxrank
               idx_spacedim = maxrank - 1
            else
               idx_spacedim = maxrank
            end if
            idx_fastdim = idx_spacedim
         end if
      end if

      ! TODO: AvD: here vector max handling

      ! Set the time dimension
      if (idx_timedim > 0) then
         idims(idx_timedim) = id_tsp%id_timedim
      end if

      if (present(cell_method)) then
         cell_method_ = cell_method
      else
         cell_method_ = 'mean' !< Default cell average.
      end if
      cell_measures = ''

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
      else
         ndxndxi = ndxi
      end if

      ndx1d = ndxi - ndx2d

      select case (iloc)
      case (UNC_LOC_CN) ! Corner point location
         ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
         if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
            ierr = UG_NOTIMPLEMENTED ! Not implemented corner location for 1D grids yet
            goto 888
         end if
         ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
         if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
            cell_method_ = 'point' ! NOTE: for now don't allow user-defined cell_method for corners, always point.
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_node)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if

      case (UNC_LOC_S) ! Pressure point location
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then
            cell_measures = 'area: '//trim(mesh1dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer
            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then
            cell_measures = 'area: '//trim(mesh2dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if
         if (jamapanc > 0 .and. jamaphs > 0 .and. .not. strcmpi(var_name, 'waterdepth')) then
            ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'waterdepth')
         end if

      case (UNC_LOC_U, UNC_LOC_L) ! Horizontal velocity point location, or horizontal net link. Note: defvar for netlinks and flowlinks is the same, putvar not.
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
            !1d mesh
            if (size(id_tsp%edgetoln, 1) > 0) then
               !cell_measures = 'area: '//trim(mesh1dname)//'_au' ! TODO: AvD: UNST-1100: au is not yet in map file at all.
               idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
               ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                                 trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                                 do_deflate=unc_nccompress)
            end if
         end if
         if (iand(which_meshdim_, 4) > 0 .and. numl1d > 0) then
            !1d2d contacts
            if (size(id_tsp%contactstoln, 1) > 0) then
               idims(idx_spacedim) = id_tsp%meshcontacts%dimids(cdim_ncontacts)
               ierr = ug_def_var(ncid, id_var(4), idims(idx_fastdim:maxrank), itype, UG_LOC_CONTACT, &
                                 trim(contactname), var_name, standard_name, long_name, unit, ' ', cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                                 do_deflate=unc_nccompress)
            end if
         end if
         numl2d = numl - numl1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
            !cell_measures = 'area: '//trim(mesh2dname)//'_au' ! TODO: AvD: UNST-1100: au is not yet in map file at all.
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if

         if (jamapanc > 0 .and. jamaphu > 0 .and. .not. strcmpi(var_name, 'hu')) then
            ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'hu')
         end if

      case (UNC_LOC_S3D) ! Pressure point location in all layers.
         ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
         if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then
            if (jamapvol1 > 0) then
               cell_measures = 'volume: '//trim(mesh1dname)//'_vol1'
            end if

            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
            idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_layer)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if
         ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
         if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then
            if (jamapvol1 > 0) then
               cell_measures = 'volume: '//trim(mesh2dname)//'_vol1'
            end if

            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
            idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_layer)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)

         end if

         if (jamapanc > 0 .and. jamaphs > 0 .and. .not. strcmpi(var_name, 'waterdepth')) then
            ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'waterdepth')
         end if

      case (UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
         ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
         if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
            if (jamapau > 0) then
               cell_measures = 'area: '//trim(mesh1dname)//'_au'
            end if
            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
            idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_layer)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if
         ! TODO: AvD: 1d2d links as mesh contacts in layered 3D are not handled here yet.

         numl2d = numl - numl1d
         ! Internal 3d horizontal flowlinks. Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
            if (jamapau > 0) then
               cell_measures = 'area: '//trim(mesh2dname)//'_au'
            end if
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
            idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_layer)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if

         if (jamapanc > 0 .and. jamaphu > 0 .and. .not. strcmpi(var_name, 'hu')) then
            ierr = unc_put_att_map_char(ncid, id_tsp, id_var, 'ancillary_variables', 'hu')
         end if

      case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
         ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
         if (iand(which_meshdim_, 1) > 0 .and. ndx1d > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
            cell_measures = 'area: '//trim(mesh1dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer ! TODO: AvD: UNST-1100: or do we need to use a1 here??
            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_node)
            idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_interface)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_NODE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, &
                              do_deflate=unc_nccompress)
         end if
         ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
         if (iand(which_meshdim_, 2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
            cell_measures = 'area: '//trim(mesh2dname)//'_flowelem_ba' ! relies on unc_write_flowgeom_ugrid_filepointer ! TODO: AvD: UNST-1100: or do we need to use a1 here??
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_face)
            idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_interface)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_FACE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if

      case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (iand(which_meshdim_, 1) > 0 .and. numl1d > 0) then
            idims(idx_spacedim) = id_tsp%meshids1d%dimids(mdim_edge)
            idims(idx_layerdim) = id_tsp%meshids1d%dimids(mdim_interface)
            ierr = ug_def_var(ncid, id_var(1), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh1dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if
         ! TODO: AvD: 1d2d links as mesh contacts in layered 3D are not handled here yet.

         numl2d = numl - numl1d
         ! Internal 3d vertical viscosity points. Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (iand(which_meshdim_, 2) > 0 .and. numl2d > 0) then
            idims(idx_spacedim) = id_tsp%meshids2d%dimids(mdim_edge)
            idims(idx_layerdim) = id_tsp%meshids2d%dimids(mdim_interface)
            ierr = ug_def_var(ncid, id_var(2), idims(idx_fastdim:maxrank), itype, UG_LOC_EDGE, &
                              trim(mesh2dname), var_name, standard_name, long_name, unit, cell_method_, cell_measures, crs, ifill=-999, dfill=dmiss, writeopts=unc_writeopts, &
                              do_deflate=unc_nccompress)
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      if (present(ivalid_max)) then
         ierr = nf90_put_att(ncid, id_var(1), 'valid_max', ivalid_max)
         ierr = nf90_put_att(ncid, id_var(2), 'valid_max', ivalid_max)
      end if

      select case (iloc)
      case (UNC_LOC_S3D)
         ! Check which vertical coordinate variable is present in the file, and add it to the :coordinate attribute.
         checkvars(1:4) = [character(len=50) :: 'layer_sigma_z', 'layer_z', 'layer_sigma', 'flowelem_zcc']
         do i = 1, 4
            if (nf90_inq_varid(ncid, trim(mesh2dname)//'_'//trim(checkvars(i)), varid) == NF90_NOERR) then
               ierr = ncu_append_atts(ncid, id_var(2), 'coordinates', trim(mesh2dname)//'_'//trim(checkvars(i)))
               exit
            end if
         end do
      case (UNC_LOC_W)
         ! Check which vertical coordinate variable is present in the file, and add it to the :coordinate attribute.
         checkvars(1:4) = [character(len=50) :: 'interface_sigma_z', 'interface_z', 'interface_sigma', 'flowelem_zw']
         do i = 1, 4
            if (nf90_inq_varid(ncid, trim(mesh2dname)//'_'//trim(checkvars(i)), varid) == NF90_NOERR) then
               ierr = ncu_append_atts(ncid, id_var(2), 'coordinates', trim(mesh2dname)//'_'//trim(checkvars(i)))
               exit
            end if
         end do
      end select

! RL: separate cases needed for iloc==UNC_LOC_U3D and UNC_LOC_WU, see Issue UNST-4880

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_def_var_map

   function unc_put_att_dble(ncid, id_var, att_name, att_value) result(ierr)
      use dfm_error
      implicit none

      integer, intent(in) :: ncid !< NetCDF file unit
      integer, intent(in) :: id_var(:) !< Returned variable id.
      character(len=*), intent(in) :: att_name !< Name of the attribute to be set
      double precision, intent(in) :: att_value !< (Character) attribute value to be set.
      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: i, numvar

      ierr = DFM_NOERR

      numvar = size(id_var)

      do i = 1, numvar
         if (id_var(i) /= -1) then
            ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
         end if
      end do

   end function unc_put_att_dble

   function unc_put_att_int(ncid, id_var, att_name, att_value) result(ierr)
      use dfm_error
      implicit none

      integer, intent(in) :: ncid !< NetCDF file unit
      integer, intent(in) :: id_var(:) !< Returned variable id.
      character(len=*), intent(in) :: att_name !< Name of the attribute to be set
      integer, intent(in) :: att_value !< (Character) attribute value to be set.
      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: i, numvar

      ierr = DFM_NOERR

      numvar = size(id_var)

      do i = 1, numvar
         if (id_var(i) /= -1) then
            ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
         end if
      end do

   end function unc_put_att_int

!> Puts a normal NetCDF attribute into multiple variable ids, if they are valid.
!! Typically only used for variables that are defined on multiple meshes (e.g. 1d,2d,3d)
!! This routine assumes that unc_def_var_map has already left some id_var(:) values on -1,
!! if that mesh/location is not applicable.
   function unc_put_att_char(ncid, id_var, att_name, att_value) result(ierr)
      use dfm_error
      implicit none

      integer, intent(in) :: ncid !< NetCDF file unit
      integer, intent(in) :: id_var(:) !< The variable ids for which to put the attribute.
      character(len=*), intent(in) :: att_name !< Name of the attribute to be set.
      character(len=*), intent(in) :: att_value !< (Character) attribute value to be set.
      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: i, numvar

      ierr = DFM_NOERR

      numvar = size(id_var)

      do i = 1, numvar
         if (id_var(i) /= -1) then
            ierr = nf90_put_att(ncid, id_var(i), att_name, att_value)
         end if
      end do

   end function unc_put_att_char

!> Puts a normal NetCDF attribute into multiple variable ids in a map file, if they are valid.
!! Typically only used for variables that are defined on multiple meshes (e.g. 1d,2d,3d)
!! This function is different from unc_put_att_char as follows: it checks whether the att_value
!! is a variable name that was already defined in the file. If so, it will correctly prefix that
!! variable name with the correct mesh names.
!! This routine assumes that unc_def_var_map has already left some id_var(:) values on -1,
!! if that mesh/location is not applicable.
   function unc_put_att_map_char(ncid, id_tsp, id_var, att_name, att_value) result(ierr)
      use dfm_error
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Returned variable id.
      character(len=*), intent(in) :: att_name !< Name of the attribute to be set.
      character(len=*), intent(in) :: att_value !< (Character) attribute value to be set.
      integer :: ierr !< Result status, DFM_NOERR if successful.

      character(len=255) :: att_value_ug
      integer :: valvarid

      ierr = DFM_NOERR

      ! Check whether attribute value is a reference to another variable name, because then it must be prefixed with the mesh1d/2d3d
      ! 1D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
      att_value_ug = att_value
      ierr = ug_inq_varid(ncid, id_tsp%meshids1d, att_value, valvarid)
      if (ierr == ug_noerr) then
         ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
      end if

      if (id_var(1) /= -1) then
         ierr = nf90_put_att(ncid, id_var(1), att_name, att_value_ug)
      end if

      ! 2D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
      att_value_ug = att_value
      ierr = ug_inq_varid(ncid, id_tsp%meshids2d, att_value, valvarid)
      if (ierr == ug_noerr) then
         ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
      end if

      if (id_var(2) /= -1) then
         ierr = nf90_put_att(ncid, id_var(2), att_name, att_value_ug)
      end if

      ! 1D ! TODO: AvD: change meshids1d/2d/3d into array(3), such that we can loop 1,3
      att_value_ug = att_value
      ierr = ug_inq_varid(ncid, id_tsp%meshids3d, att_value, valvarid)
      if (ierr == ug_noerr) then
         ierr = nf90_inquire_variable(ncid, valvarid, att_value_ug)
      end if

      if (id_var(3) /= -1) then
         ierr = nf90_put_att(ncid, id_var(3), att_name, att_value_ug)
      end if

   end function unc_put_att_map_char

! TODO: AvD: support integer/other data types
! TODO: AvD: support in/exclude boundary points/links

!> Writes a map field of a flow variable to a NetCDF map file, taking care of 1D/2D/3D specifics and s/u/w-point specifics.
!! Only writes data for the current time. Assumes that the mapids%id_tsp%idx_curtime contains the new time index where to write to.
!! Produces a UGRID-compliant map file.
!! If there is a 1d and a 2d mesh, then values are written for both meshes in one call to this function.
!! Typical call: unc_put_var(mapids, mapids%id_s1(:), UNC_LOC_S, s1)

   function unc_put_var_map_int(ncid, id_tsp, id_var, iloc, integers, default_value, jabndnd) result(ierr)
      implicit none
      integer :: ierr
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
      integer, dimension(:), intent(in) :: integers
      double precision, optional :: default_value
      integer, optional, intent(in) :: jabndnd

      double precision, dimension(:), allocatable :: values
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      allocate (values(size(integers)))
      values = integers ! casting an array of integers to an array of doubles
      if (present(default_value)) then
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd=jabndnd_)
      else
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, jabndnd=jabndnd_)
      end if
      deallocate (values)
   end function unc_put_var_map_int

   function unc_put_var_map_real(ncid, id_tsp, id_var, iloc, reals, default_value, jabndnd) result(ierr)
      implicit none
      integer :: ierr
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
      real(kind=4), dimension(:), intent(in) :: reals
      double precision, optional :: default_value
      integer, optional, intent(in) :: jabndnd

      double precision, dimension(:), allocatable :: values
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      allocate (values(size(reals)))
      values = reals ! casting an array of reals to an array of doubles
      if (present(default_value)) then
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd=jabndnd_)
      else
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, jabndnd=jabndnd_)
      end if
      deallocate (values)
   end function unc_put_var_map_real

   function unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd) result(ierr)
      use m_flowgeom
      use network_data, only: numk, numl, numl1d
      use m_flow, only: kmx
      use dfm_error
      use m_alloc
      use m_missing
      use m_save_ugrid_state
      use fm_location_types
      use m_get_kbot_ktop

      implicit none

      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      double precision, intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      double precision, optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
      double precision, allocatable, save :: workL(:)
      double precision, allocatable, save :: workS3D(:, :), workU3D(:, :), workW(:, :), workWU(:, :)
! temporary UGRID fix
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last 2/3D node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1D node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)
      case (UNC_LOC_CN) ! Corner point location
         ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. ndxi > ndx2d) then ! If there are 1d flownodes, then there are 1d netnodes.
            ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
            goto 888
         end if
         ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
            ierr = nf90_put_var(ncid, id_var(2), values(1:numk), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:last_1d), start=(/1, id_tsp%idx_curtime/))
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ! 1d mesh
            if (size(id_tsp%edgetoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), values(id_tsp%edgetoln(:)), start=(/1, id_tsp%idx_curtime/))
            end if
         end if

         if (id_var(4) > 0 .and. lnx1d > 0) then
            ! 1d2d contacts
            if (size(id_tsp%contactstoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(4), values(id_tsp%contactstoln(:)), start=(/1, id_tsp%idx_curtime/))
            end if
         end if

         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=(/1, id_tsp%idx_curtime/))
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=(/lnx2d + 1, id_tsp%idx_curtime/))
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/lnx2d + lnx2db + 1, id_tsp%idx_curtime/), count=(/numl2d - lnx2d - lnx2db, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
            end if
         end if

      case (UNC_LOC_L) ! Horizontal net link location
         ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.

         call realloc(workL, numl, keepExisting=.false.)

         ! Permute the input values(:) from netlink ordering to flow link ordering.
         ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
         do Lf = 1, lnx1d
            L = abs(ln2lne(Lf))
            workL(Lf) = values(L)
         end do

         ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
         if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
            ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start=(/1, id_tsp%idx_curtime/))
         end if

         ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
         lnx2d = lnxi - lnx1d
         lnx2db = lnx - lnx1db
         i = lnx2d + lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
         do L = numl1d + 1, numl ! Only 2D net links
            Lf = lne2ln(L) ! If negative, then no flow link

            if (Lf > lnx1db) then ! 2D open boundary flow link
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1db + lnx2d) = values(L)
            else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1d) = values(L)
            else
               ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
               i = i + 1
               workL(i) = values(L)
            end if
         end do
         if (id_var(2) > 0 .and. numl - numl1d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl - numl1d)), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_S3D) ! Pressure point location in all layers.
         ! Fill work array.
         call realloc(workS3D, (/kmx, ndxndxi/), keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxndxi
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workS3D(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layers.
            do k = kb, kt
               workS3D(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d + 1:last_1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, n1d_write, 1/))
         end if
         ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, ndx2d, 1/))
         end if

         ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
      case (UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
         ! Fill work array.
         call realloc(workU3D, (/kmx, lnx/), keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workU3D(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layers.
            do L = Lb, Ltx
               workU3D(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, 1:lnx1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, lnx1d, 1/))
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d + 1:lnx), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, lnx2d, 1/))
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + 1, id_tsp%idx_curtime/), count=(/kmx, numl2d - lnx2d, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workW, (/kmx, ndxndxi/), lindex=(/0, 1/), keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxndxi
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workW(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do k = kb - 1, kt
               workW(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d + 1:last_1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, n1d_write, 1/))
         end if
         ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, ndx2d, 1/))
         end if

      case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workWU, (/kmx, lnx/), lindex=(/0, 1/), keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workWU(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do L = Lb - 1, Ltx
               workWU(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, lnx1d, 1/))
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d + 1:lnx), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, lnx2d, 1/))
         end if
         ! Default value is different from a fill value, use for example for zero values on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + 1, id_tsp%idx_curtime/), count=(/kmx + 1, numl2d - lnx2d, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble

!> copy of unc_put_var_map_dble for writing bytes
!! TODO: use templating
   function unc_put_var_map_byte(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd) result(ierr)
      use m_flowgeom
      use network_data, only: numk, numl, numl1d
      use m_flow, only: kmx
      use dfm_error
      use m_alloc
      use m_missing
      use fm_location_types
      use m_get_kbot_ktop
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      integer(kind=1), intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      integer(kind=1), optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last 2d/3d node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
      double precision, allocatable, save :: workL(:)
      double precision, allocatable, save :: workS3D(:, :), workU3D(:, :), workW(:, :), workWU(:, :)

      ierr = DFM_NOERR

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)
      case (UNC_LOC_CN) ! Corner point location
         ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. ndxi > ndx2d) then ! If there are 1d flownodes, then there are 1d netnodes.
            ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
            goto 888
         end if
         ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
            ierr = nf90_put_var(ncid, id_var(2), values(1:numk), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:last_1d), start=(/1, id_tsp%idx_curtime/))
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ! 1d mesh
            if (size(id_tsp%edgetoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), values(id_tsp%edgetoln(:)), start=(/1, id_tsp%idx_curtime/))
            end if
         end if

         if (id_var(4) > 0 .and. lnx1d > 0) then
            ! 1d2d contacts
            if (size(id_tsp%contactstoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(4), values(id_tsp%contactstoln(:)), start=(/1, id_tsp%idx_curtime/))
            end if
         end if

         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=(/1, id_tsp%idx_curtime/))
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=(/lnx2d + 1, id_tsp%idx_curtime/))
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/lnx2d + lnx2db + 1, id_tsp%idx_curtime/), count=(/numl2d - lnx2d - lnx2db, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
            end if
         end if

      case (UNC_LOC_L) ! Horizontal net link location
         ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.

         call realloc(workL, numl, keepExisting=.false.)

         ! Permute the input values(:) from netlink ordering to flow link ordering.
         ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
         do Lf = 1, lnx1d
            L = abs(ln2lne(Lf))
            workL(Lf) = values(L)
         end do

         ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
         if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
            ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start=(/1, id_tsp%idx_curtime/))
         end if

         ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
         lnx2d = lnxi - lnx1d
         lnx2db = lnx - lnx1db
         i = lnx2d + lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
         do L = numl1d + 1, numl ! Only 2D net links
            Lf = lne2ln(L) ! If negative, then no flow link

            if (Lf > lnx1db) then ! 2D open boundary flow link
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1db + lnx2d) = values(L)
            else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1d) = values(L)
            else
               ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
               i = i + 1
               workL(i) = values(L)
            end if
         end do
         if (id_var(2) > 0 .and. numl - numl1d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl - numl1d)), start=(/1, id_tsp%idx_curtime/))
         end if

      case (UNC_LOC_S3D) ! Pressure point location in all layers.
         ! Fill work array.
         call realloc(workS3D, (/kmx, ndxi/), keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxi
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workS3D(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layers.
            do k = kb, kt
               workS3D(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d + 1:last_1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, n1d_write, 1/))
         end if
         ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, ndx2d, 1/))
         end if

         ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
      case (UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
         ! Fill work array.
         call realloc(workU3D, (/kmx, lnx/), keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workU3D(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layers.
            do L = Lb, Ltx
               workU3D(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, 1:lnx1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, lnx1d, 1/))
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d + 1:lnx), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx, lnx2d, 1/))
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + 1, id_tsp%idx_curtime/), count=(/kmx, numl2d - lnx2d, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workW, (/kmx, ndxi/), lindex=(/0, 1/), keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxi
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workW(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do k = kb - 1, kt
               workW(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d + 1:last_1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, n1d_write, 1/))
         end if
         ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, ndx2d, 1/))
         end if

      case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workWU, (/kmx, lnx/), lindex=(/0, 1/), keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workWU(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do L = Lb - 1, Ltx
               workWU(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, lnx1d, 1/))
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d + 1:lnx), start=(/1, 1, id_tsp%idx_curtime/), count=(/kmx + 1, lnx2d, 1/))
         end if
         ! Default value is different from a fill value, use for example for zero values on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + 1, id_tsp%idx_curtime/), count=(/kmx + 1, numl2d - lnx2d, 1/), map=(/0/)) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_byte

!> copy of unc_put_var_map_byte with buffered time
!! TODO: only implemented for UNC_LOC_S
   function unc_put_var_map_byte_timebuffer(ncid, id_tsp, id_var, iloc, values, t1, tl, jabndnd) result(ierr)
      use m_flowgeom
      use dfm_error
      use m_alloc
      use m_missing
      use fm_location_types
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      integer(kind=1), intent(in) :: values(:, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      integer, intent(in) :: t1 !< first time in buffer to be written
      integer, intent(in) :: tl !< last time in buffer to be written
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: tstart !< time index of t1
      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)

      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         tstart = id_tsp%idx_curtime - tl + t1
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:last_1d, t1:tl), start=(/1, tstart/))
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d, t1:tl), start=(/1, tstart/))
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_byte_timebuffer

   function unc_put_var_map_dble2(ncid, id_tsp, id_var, iloc, values, default_value, locdim, jabndnd) result(ierr)
      use m_flowgeom
      use network_data, only: numl, numl1d
      use dfm_error
      use m_alloc
      use m_missing
      use fm_location_types
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      double precision, intent(in) :: values(:, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      double precision, optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: locdim !< Optional index of the location dimension (default = 1)
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d
      integer :: ilocdim
      integer :: lndim
      integer, dimension(3) :: dimids_var
      double precision, allocatable :: work(:, :)
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)
      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(n1d_write, size(values, 2)))
               work = values(ndx2d + 1:last_1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), n1d_write))
               work = values(:, ndx2d + 1:last_1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(ndx2d, size(values, 2)))
               work = values(1:ndx2d, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), ndx2d))
               work = values(:, 1:ndx2d)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx1d, size(values, 2)))
               work = values(1:lnx1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx1d))
               work = values(:, 1:lnx1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnxi - lnx1d, size(values, 2)))
               work = values(lnx1d + 1:lnxi, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnxi - lnx1d))
               work = values(:, lnx1d + 1:lnxi)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx - lnx1db, size(values, 2)))
               work = values(lnx1db + 1:lnx, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/lnx2d + 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx - lnx1db))
               work = values(:, lnx1db + 1:lnx)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, lnx2d + 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_inquire_variable(ncid, id_var(2), dimids=dimids_var)
               ! Use map = 0 to write a single value on multiple edges in file.
               select case (ilocdim)
               case (1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim)
                  ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/lnx2d + lnx2db + 1, 1, id_tsp%idx_curtime/), count=(/numl2d - lnx2d - lnx2db, lndim, 1/), map=(/0/))
               case (2)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim)
                  ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + lnx2db + 1, id_tsp%idx_curtime/), count=(/lndim, numl2d - lnx2d - lnx2db, 1/), map=(/0/))
               end select
            end if
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble2

   function unc_put_var_map_dble3(ncid, id_tsp, id_var, iloc, values, default_value, locdim, jabndnd) result(ierr)
      use m_flowgeom
      use network_data, only: numl, numl1d
      use dfm_error
      use m_alloc
      use m_missing
      use fm_location_types
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      double precision, intent(in) :: values(:, :, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      double precision, optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: locdim !< Optional index of the location dimension (default = 1)
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d
      integer :: ilocdim
      integer :: lndim1, lndim2
      integer, dimension(4) :: dimids_var
      double precision, allocatable :: work(:, :, :)
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)
      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(n1d_write, size(values, 2), size(values, 3)))
               work = values(ndx2d + 1:last_1d, :, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), n1d_write, size(values, 3)))
               work = values(:, ndx2d + 1:last_1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), n1d_write))
               work = values(:, :, ndx2d + 1:last_1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(ndx2d, size(values, 2), size(values, 3)))
               work = values(1:ndx2d, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), ndx2d, size(values, 3)))
               work = values(:, 1:ndx2d, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), ndx2d))
               work = values(:, :, 1:ndx2d)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx1d, size(values, 2), size(values, 3)))
               work = values(1:lnx1d, :, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx1d, size(values, 3)))
               work = values(:, 1:lnx1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnx1d))
               work = values(:, :, 1:lnx1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnxi - lnx1d, size(values, 2), size(values, 3)))
               work = values(lnx1d + 1:lnxi, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnxi - lnx1d, size(values, 3)))
               work = values(:, lnx1d + 1:lnxi, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnxi - lnx1d))
               work = values(:, :, lnx1d + 1:lnxi)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx - lnx1db, size(values, 2), size(values, 3)))
               work = values(lnx1db + 1:lnx, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/lnx2d + 1, 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx - lnx1db, size(values, 3)))
               work = values(:, lnx1db + 1:lnx, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, lnx2d + 1, 1, id_tsp%idx_curtime/))
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnx - lnx1db))
               work = values(:, :, lnx1db + 1:lnx)
               ierr = nf90_put_var(ncid, id_var(2), work, start=(/1, 1, lnx2d + 1, id_tsp%idx_curtime/))
               deallocate (work)
            end select
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_inquire_variable(ncid, id_var(2), dimids=dimids_var)
               ! Use map = 0 to write a single value on multiple edges in file.
               select case (ilocdim)
               case (1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(3), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/lnx2d + lnx2db + 1, 1, 1, id_tsp%idx_curtime/), count=(/numl2d - lnx2d - lnx2db, lndim1, lndim2, 1/), map=(/0/))
               case (2)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(3), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, lnx2d + lnx2db + 1, 1, id_tsp%idx_curtime/), count=(/lndim1, numl2d - lnx2d - lnx2db, lndim2, 1/), map=(/0/))
               case (3)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), (/default_value/), start=(/1, 1, lnx2d + lnx2db + 1, id_tsp%idx_curtime/), count=(/lndim1, lndim2, numl2d - lnx2d - lnx2db, 1/), map=(/0/))
               end select
            end if
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble3

!> Puts global attributes in NetCDF data set.
!! This includes: institution, Conventions, etc.
   subroutine unc_addglobalatts(ncid)
      !use unstruc_model, only : md_ident
      integer, intent(in) :: ncid

      character(len=8) :: cdate
      character(len=10) :: ctime
      character(len=5) :: czone
      integer :: ierr, jaInDefine
      ierr = nf90_noerr
      jaInDefine = 0

      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         write (msgbuf, '(a,i0,a,i0,a,a)') 'Could not put global attributes in NetCDF #', ncid, '. Error code ', ierr, ': ', nf90_strerror(ierr)
         call err_flush()
         return
      end if

      ierr = nf90_put_att(ncid, nf90_global, 'institution', trim(company))
      ierr = nf90_put_att(ncid, nf90_global, 'references', trim(company_url))
      ierr = nf90_put_att(ncid, nf90_global, 'source', &
                          version_full// &
                          ', model ') !''//trim(md_ident)//'''')

      call date_and_time(cdate, ctime, czone)
      ierr = nf90_put_att(ncid, nf90_global, 'history', &
                          'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
                          ', '//trim(product_name))
      ierr = nf90_put_att(ncid, nf90_global, 'date_created', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))
      ierr = nf90_put_att(ncid, nf90_global, 'date_modified', cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5))

      ierr = nf90_put_att(ncid, nf90_global, 'Conventions', 'CF-1.5 Deltares-0.1')

      if (unc_uuidgen /= 0) then
         ierr = unc_add_uuid(ncid)
      end if

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 0) then
         ierr = nf90_enddef(ncid)
      end if
   end subroutine unc_addglobalatts

! TODO: AvD: add these  (incrementally) to map/his files:
!>       :time_coverage_start = "2010-04-23T00:00:00+01:00" ;
!>       :time_coverage_end = "2010-05-20T00:00:00+01:00" ;
! (can be done outside of definition mode, if att was already created before.)

!> Opens a NetCDF file for reading.
!! The file is maintained in the open-file-list.
   function unc_open(filename, cmode, ncid)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: cmode
      integer, intent(out) :: ncid
      integer :: unc_open
      unc_open = nf90_open(trim(filename), cmode, ncid)
      if (unc_open == nf90_noerr) then
         nopen_files_ = nopen_files_ + 1
         open_files_(nopen_files_) = filename
         open_datasets_(nopen_files_) = ncid
         write (msgbuf, '(a,a,a,i10,a)') 'Opened ''', trim(filename), ''' as #', ncid, '.'
         call dbg_flush()
      else
         call mess(LEVEL_WARN, 'could not open '//trim(filename))
         call dbg_flush()
         call qnerror('Failed to open: '//trim(filename), ' ', ' ')
      end if
   end function unc_open

!> Creates or opens a NetCDF file for writing.
!! The file is maintained in the open-file-list.
   function unc_create(filename, cmode, ncid)
      character(len=*), intent(in) :: filename !< Filename to be created
      integer, intent(in) :: cmode !< Creation mode, must be a valid NetCDF flags integer.
      integer, intent(out) :: ncid !< Resulting NetCDF data set id, undefined in case an error occurred.
      integer :: unc_create !< Integer result status (nf90_noerr if successful).

      integer :: cmode_

      cmode_ = ior(cmode, unc_cmode)

      unc_create = nf90_create(filename, cmode_, ncid)
      if (unc_create == nf90_noerr) then
         nopen_files_ = nopen_files_ + 1
         open_files_(nopen_files_) = filename
         open_datasets_(nopen_files_) = ncid
         write (msgbuf, '(a,a,a,i0,a)') 'Opened NetCDF file ''', trim(filename), ''' as #', ncid, '.'
         call dbg_flush()

         call unc_addglobalatts(ncid)
      else
         write (msgbuf, '(a,a,a,i0,a,i0,a,a)') 'Cannot open NetCDF file ''', trim(filename), ''' as #', ncid, '. Error code: ', unc_create, ': ', nf90_strerror(unc_create)
         call dbg_flush()
      end if

   end function unc_create

!> Closes a NetCDF file.
!! The file is removed from the open-file-list
   integer function unc_close(ncid)
      integer, intent(inout) :: ncid
      integer :: i, j
      logical :: jafound

      unc_close = 0

      jafound = .false.
      ! Search dataset ID
      do i = nopen_files_, 1, -1
         if (open_datasets_(i) == ncid) then
            jafound = .true.
            exit
         end if
      end do
      ! If found, shift all entries behind it one to the left.
      if (jafound) then
         unc_close = nf90_close(ncid)
         write (msgbuf, '(a,a,a)') 'Closed NetCDF file ''', trim(open_files_(nopen_files_)), '.'
         call dbg_flush()
         do j = nopen_files_ - 1, -1, i
            open_files_(j) = open_files_(j + 1)
            open_datasets_(j) = open_datasets_(j + 1)
         end do
         open_files_(nopen_files_) = ' '
         open_datasets_(nopen_files_) = 0
         nopen_files_ = nopen_files_ - 1
         ncid = 0
      else
         write (msgbuf, '(a,I14,a)') 'Tried to close NetCDF id ', ncid, ', not found.'
         call dbg_flush()
      end if
   end function unc_close

!> Closes all NetCDF files that are still open.
   subroutine unc_closeall()
      integer :: i, istat
      do i = nopen_files_, 1, -1
         istat = unc_close(open_datasets_(i))
      end do
   end subroutine unc_closeall

!> Adds coordinate attributes according to CF conventions, based on jsferic.
!! Non-standard attributes (such as long_name) should be set elsewhere.
   function unc_addcoordatts(ncid, id_varx, id_vary, jsferic)
      integer, intent(in) :: ncid !< NetCDF dataset id
      integer, intent(in) :: id_varx !< NetCDF horizontal variable id
      integer, intent(in) :: id_vary !< NetCDF vertical variable id
      integer, intent(in) :: jsferic !< Sferical coords or not (1/0)
      integer :: unc_addcoordatts !< Result status of NetCDF primitives

      integer :: ierr

      if (jsferic == 0) then
         ierr = nf90_put_att(ncid, id_varx, 'units', 'm')
         ierr = nf90_put_att(ncid, id_vary, 'units', 'm')
         ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'projection_x_coordinate')
         ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'projection_y_coordinate')
         ierr = nf90_put_att(ncid, id_varx, 'long_name', 'x-coordinate')
         ierr = nf90_put_att(ncid, id_vary, 'long_name', 'y-coordinate')
      else
         ierr = nf90_put_att(ncid, id_varx, 'units', 'degrees_east')
         ierr = nf90_put_att(ncid, id_vary, 'units', 'degrees_north')
         ierr = nf90_put_att(ncid, id_varx, 'standard_name', 'longitude')
         ierr = nf90_put_att(ncid, id_vary, 'standard_name', 'latitude')
         ierr = nf90_put_att(ncid, id_varx, 'long_name', 'longitude')
         ierr = nf90_put_att(ncid, id_vary, 'long_name', 'latitude')
      end if
      unc_addcoordatts = ierr
   end function unc_addcoordatts

!> Add longitude and latitude coordinates to a NetCDF dataset.
!!
!! Lon/lat coordinates are required by CF-standards, even if the coordinates
!! used are projected Cartesian. Two new coordinate variables are added
!! to the NetCDF id (e.g. a .nc file), but only if jsferic==0.
!! The names for the new variables are based on varbasename and a postfix.
   function unc_add_lonlat_vars(ncid, varnameprefix, varnamepostfix, id_dims, id_varlon, id_varlat, jsferic) result(ierr)
      integer, intent(in) :: ncid !< NetCDF dataset id
      character(len=*), intent(in) :: varnameprefix !< Base text string for new lon lat variable names.
      character(len=*), intent(in) :: varnamepostfix !< Text string to be appended for new lon lat variable names.
      integer, dimension(:), intent(in) :: id_dims !< Array with NetCDF dimension ids for the coordinate variables.
      integer, intent(out) :: id_varlon !< NetCDF horizontal variable id
      integer, intent(out) :: id_varlat !< NetCDF vertical variable id
      integer, intent(in) :: jsferic !< Spherical coords or not (1/0). If 1, nothing happens.
      integer :: ierr !< Result status of NetCDF primitives

      ierr = 0

      ! If current system is already spherical, lon/lat should already be present.
      if (jsferic == 1) then
         return
      end if

      ! Define lon and lat variables
      ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lon'//trim(varnamepostfix), nf90_double, id_dims, id_varlon)
      call check_error(ierr, 'Add longitude variable for '//trim(varnameprefix))
      ierr = nf90_def_var(ncid, trim(varnameprefix)//'_lat'//trim(varnamepostfix), nf90_double, id_dims, id_varlat)
      call check_error(ierr, 'Add latitude variable for '//trim(varnameprefix))

      ! Add standard spherical coordinate attributes
      ierr = unc_addcoordatts(ncid, id_varlon, id_varlat, 1)

      !ierr = unc_addcoordmapping(ncid, 1)
      !call check_error(ierr, 'Add grid_mapping variable for '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

      !ierr = unc_add_gridmapping_att(ncid, (/ id_varlon, id_varlat /), 1)
      !call check_error(ierr, 'Add grid_mapping attributes to '//trim(varnameprefix)//'_lon'//trim(varnamepostfix)//'/_lat'//trim(varnamepostfix))

   end function unc_add_lonlat_vars

!> Adds coordinate mapping attributes according to CF conventions, based on jsferic.
!! Attributes are put in a scalar integer variable.
   function unc_addcoordmapping(ncid, jsferic)
      use m_sferic, only: ra
      integer, intent(in) :: ncid !< NetCDF dataset id
      integer, intent(in) :: jsferic !< Spherical coords or not (1/0)
      integer :: unc_addcoordmapping !< Result status of NetCDF primitives

      integer :: ierr, id_crs
      integer :: epsg
      character(len=11) :: epsgstring
      character(len=30) :: varname !< Name of the created grid mapping variable.
      epsgstring = ' '

      ! AvD: TODO: Name and params are now hardcoded globally based on a single jsferic=0/1 flag.
      ! generalize this!

      if (jsferic == 1) then
         crs%epsg_code = 4326
      end if

      ierr = ug_add_coordmapping(ncid, crs) ! TODO: AvD: temp, this now uses the global crs instead of in meshgeom/jsferic
      if (ierr /= ug_noerr) then
         ierr = ug_get_message(msgbuf)
         if (len_trim(msgbuf) > 0) then
            call warn_flush()
         end if
      end if

      unc_addcoordmapping = ierr
      return
    !! RETURN !!

      varname = ' '
      if (jsferic == 0) then
         varname = 'projected_coordinate_system'
      else
         varname = 'wgs84'
      end if

      ierr = nf90_inq_varid(ncid, trim(varname), id_crs)
      if (ierr == nf90_noerr) then
         ! A variable with that name already exists. Return.
         unc_addcoordmapping = ierr
         return
      end if

      ierr = nf90_def_var(ncid, trim(varname), nf90_int, id_crs)

      if (jsferic == 0) then
         epsg = 28992
         epsgstring = 'EPSG:28992'
         ierr = nf90_put_att(ncid, id_crs, 'name', 'Amersfoort / RD New') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'epsg', epsg) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name', 'stereographic') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis', ra) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis', 6356752.314245d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening', 298.257223563d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'proj4_params', '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs') ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'EPSG_code', trim(epsgstring)) ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'projection_name', 'Amersfoort / RD New') ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'crs_wkt', ' ') ! WKT
         ierr = nf90_put_att(ncid, id_crs, 'comment', ' ')
         ierr = nf90_put_att(ncid, id_crs, 'value', 'value is equal to EPSG code')
      else
         epsg = 4326
         epsgstring = 'EPSG:4326'
         ierr = nf90_put_att(ncid, id_crs, 'name', 'WGS84') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'epsg', epsg) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'grid_mapping_name', 'latitude_longitude') ! CF
         ierr = nf90_put_att(ncid, id_crs, 'longitude_of_prime_meridian', 0d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_major_axis', ra) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'semi_minor_axis', 6356752.314245d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'inverse_flattening', 298.257223563d0) ! CF
         ierr = nf90_put_att(ncid, id_crs, 'proj4_params', ' ') ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'EPSG_code', trim(epsgstring)) ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'projection_name', ' ') ! ADAGUC
         ierr = nf90_put_att(ncid, id_crs, 'crs_wkt', ' ') ! WKT
         ierr = nf90_put_att(ncid, id_crs, 'comment', ' ')
         ierr = nf90_put_att(ncid, id_crs, 'value', 'value is equal to EPSG code')
      end if
      unc_addcoordmapping = ierr
   end function unc_addcoordmapping

!> Add the grid mapping attribute to one or more NetCDF variables.
!!
!! The specified gridmappingname should be an existing variable in the NetCDF dataset.
   function unc_add_gridmapping_att(ncid, id_vars, jsferic) result(ierr)
      integer, intent(in) :: ncid !< NetCDF dataset id
      integer, dimension(:), intent(in) :: id_vars !< Array of NetCDF variable ids
      integer, intent(in) :: jsferic !< Spherical coords or not (1/0)
      integer :: ierr !< Result status of NetCDF primitives

      integer :: i, n, ierr_
      character(len=30) :: gridmappingvar !< Name of grid mapping variable

      gridmappingvar = ' '
      if (jsferic == 0) then
         gridmappingvar = 'projected_coordinate_system' ! TODO: AvD: this works, but we have parts in ug_add_coord_mapping, and parts here. Unify!
      else
         gridmappingvar = 'wgs84'
      end if

      ierr = nf90_noerr
      n = size(id_vars)

      do i = 1, n
         if (id_vars(i) == nf90_global .or. id_vars(i) == -1) then
            cycle ! Sometimes id_vars has value 0 (== unintended nf90_global)
         end if

         ierr_ = nf90_put_att(ncid, id_vars(i), 'grid_mapping', trim(gridmappingvar))
         if (ierr_ /= nf90_noerr) then
            ierr = ierr_
         end if
      end do

   end function unc_add_gridmapping_att

!> Defines 3d net data structure for an already opened netCDF dataset.
   subroutine unc_append_3dflowgeom_def(imapfile)
      use m_flow !only kmx, zws, layertype
      use m_flowparameters !only jafullgridoutput

      integer, intent(in) :: imapfile

      integer, save :: ierr, &
                       id_laydim, id_wdim, &
                       id_timedim, &
                       id_flowelemdim, &
                       id_flowelemzcc, &
                       id_flowelemzw, &
                       id_laycoordcc, &
                       id_laycoordw

      !define file structure
      ierr = nf90_def_dim(imapfile, 'laydim', kmx, id_laydim)
      ierr = nf90_def_dim(imapfile, 'wdim', kmx + 1, id_wdim)
      !
      if (jafullgridoutput == 0) then
         if (layertype < 3) then !time-independent sigma layer and z layer
            ierr = nf90_def_var(imapfile, 'LayCoord_cc', nf90_double, (/id_laydim/), id_laycoordcc)
            ierr = nf90_def_var(imapfile, 'LayCoord_w', nf90_double, (/id_wdim/), id_laycoordw)
            !
            !define and write compact form output of sigma or z-layer
            if (layertype == 1) then !all sigma layers
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'standard_name', 'ocean_sigma_coordinate')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'long_name', 'sigma layer coordinate at flow element center')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'units', '')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'positive', 'up')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'formula_terms', 'sigma: LayCoord_cc eta: s1 bedlevel: FlowElem_bl')
               !
               ierr = nf90_put_att(imapfile, id_laycoordw, 'standard_name', 'ocean_sigma_coordinate')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'long_name', 'sigma layer coordinate at vertical interface')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'units', '')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'positive', 'up')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'formula_terms', 'sigma: LayCoord_w eta: s1 bedlevel: FlowElem_bl')
               !
            elseif (layertype == 2) then !all z layers
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'standard_name', '')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'long_name', 'z layer coordinate at flow element center')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'positive', 'up')
               ierr = nf90_put_att(imapfile, id_laycoordcc, 'units', 'm')
               !
               ierr = nf90_put_att(imapfile, id_laycoordw, 'standard_name', '')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'long_name', 'z layer coordinate at vertical interface')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'positive', 'up')
               ierr = nf90_put_att(imapfile, id_laycoordw, 'units', 'm')
               !
            end if
         else
            call mess(LEVEL_WARN, 'No grid outputdata given - Set "FullGridOutput = 1" in .mdu file to output grid data')
         end if
      else
         ! structured 3d time-dependent output data
         ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
         ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
         if (ierr /= nf90_noerr) then
            ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim)
         end if
         !
         ierr = nf90_def_var(imapfile, 'FlowElem_zcc', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_flowelemzcc)
         ierr = nf90_def_var(imapfile, 'FlowElem_zw', nf90_double, (/id_wdim, id_flowelemdim, id_timedim/), id_flowelemzw)
         !
         ierr = nf90_put_att(imapfile, id_flowelemzcc, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(imapfile, id_flowelemzcc, 'standard_name', '')
         ierr = nf90_put_att(imapfile, id_flowelemzcc, 'long_name', 'flow element center z')
         ierr = nf90_put_att(imapfile, id_flowelemzcc, 'units', 'm')
         !
         ierr = nf90_put_att(imapfile, id_flowelemzw, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(imapfile, id_flowelemzw, 'standard_name', '')
         ierr = nf90_put_att(imapfile, id_flowelemzw, 'long_name', 'flow element z at vertical interface')
         ierr = nf90_put_att(imapfile, id_flowelemzw, 'units', 'm')
      end if
      !
   end subroutine unc_append_3dflowgeom_def

   subroutine unc_append_3dflowgeom_put(imapfile, jaseparate, itim_in)
      use m_flow !only kmx, zws, layertype
      use m_flowgeom !only Ndxi
      use m_missing
      use m_flowparameters !only jafullgridoutput
      use m_get_kbot_ktop

      integer, intent(in) :: imapfile
      integer, intent(in) :: jaseparate
      integer, optional, intent(in) :: itim_in

      integer :: iid, kk, kb, kt, itim
      integer :: nrlay, nlayb, k

      integer, save :: ierr
      integer, dimension(2), save :: &
         id_laydim, id_wdim, &
         id_timedim, &
         id_flowelemdim, &
         id_flowelemzcc, &
         id_flowelemzw, &
         id_laycoordcc, &
         id_laycoordw

      logical, dimension(2), save :: firststep = .true.

      if (present(itim_in)) then
         itim = itim_in
      else
         itim = 1
      end if
      if (jaseparate == 2) then
         ! comfile, store/use ids number 2
         iid = 2
      else
         ! mapfile, store/use ids number 1
         iid = 1
      end if
      !
      ! inquire ids of netcdf file if it is the first step of writing to the file
      !                         or if it is a seperate write file
      !
      if (firststep(iid) .or. jaseparate > 0) then
         firststep(iid) = .false.
         ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
         ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
         !
         if (jafullgridoutput == 0) then
            if (layertype < 3) then
               !time-independent sigma layer and z layer
               ierr = nf90_inq_varid(imapfile, 'LayCoord_cc', id_laycoordcc(iid))
               ierr = nf90_inq_varid(imapfile, 'LayCoord_w', id_laycoordw(iid))
               !
               ! write 3d time-independent output data to netcdf file
               !
               if (layertype == 1) then
                  ! structured 3d time-independent output data (sigma-layer)
                  ierr = nf90_put_var(imapfile, id_laycoordcc(iid), 0.5d0 * (zslay(1:kmx, 1) + zslay(0:kmx - 1, 1)), start=(/1/), count=(/kmx/))
                  ierr = nf90_put_var(imapfile, id_laycoordw(iid), zslay(0:kmx, 1), start=(/1/), count=(/kmx + 1/))
               elseif (layertype == 2) then
                  ! structured 3d time-independent output data (z-layer)
                  !  ierr = nf90_put_var(imapfile, id_laycoordcc(iid), 0.5d0*(zslay(1:kmx,1)+zslay(0:kmx-1,1)), start=(/ 1 /), count=(/ kmx /))
                  !  ierr = nf90_put_var(imapfile, id_laycoordw(iid) , zslay(0:kmx,1), start=(/ 1 /), count=(/ kmx+1 /))
               end if
            end if
         else
            ! get id's for structured 3d time-dependant output data
            ierr = nf90_inq_dimid(imapfile, 'time', id_timedim(iid))
            ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
            if (ierr /= nf90_noerr) then
               ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
            end if

            !
            ierr = nf90_inq_varid(imapfile, 'FlowElem_zcc', id_flowelemzcc(iid))
            ierr = nf90_inq_varid(imapfile, 'FlowElem_zw', id_flowelemzw(iid))
            !
         end if
         ! write structured 3d time-dependant output data
         work0 = dmiss ! For zws, can start at index 0 (kmx+1 vertical values)
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb - 1, kt
               work0(k - kb + nlayb, kk) = zws(k)
            end do
         end do
         ierr = nf90_put_var(imapfile, id_flowelemzw(iid), work0(0:kmx, 1:ndxi), (/1, 1, itim/), (/kmx + 1, ndxi, 1/))

         work1 = dmiss ! For zcc, can start at index 1 (kmx   vertical values)
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = 0.5 * (zws(k) + zws(k - 1))
            end do
         end do
         ierr = nf90_put_var(imapfile, id_flowelemzcc(iid), work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))
      end if
      !
   end subroutine unc_append_3dflowgeom_put

!> Writes the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated rst file should be filled by calling
!! unc_write_rst_filepointer directly.
   subroutine unc_write_rst(filename)
      character(len=*), intent(in) :: filename

      integer :: irstfile, ierr

      ierr = unc_create(filename, 0, irstfile)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create rst file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      call unc_write_rst_filepointer(irstfile, 0d0)

      ierr = unc_close(irstfile)
   end subroutine unc_write_rst

!> Writes rst/flow data to a newly opened netCDF dataset.
!! The netnode and -links have been written already.
   subroutine unc_write_rst_filepointer(irstfile, tim)
      use m_flow
      use m_flowtimes
      use m_flowgeom
      use m_sferic
      use network_data
      use m_sediment
      use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, ITRAN0, constituents, itrac2const, const_names, const_units, ifrac2const
      use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbotunits, wqbot
      use m_xbeach_data, only: E, thetamean, sigmwav
      use fm_external_forcings_data, only: numtracers
      use m_partitioninfo
      use m_missing
      use m_turbulence
      use m_alloc
      use m_CrossSections
      use unstruc_channel_flow, only: network
      use m_save_ugrid_state, only: mesh1dname
      use m_structures
      use m_1d_structures
      use m_GlobalParameters
      use m_longculverts
      use m_structures_saved_parameters
      use m_gettaus
      use m_gettauswave
      use m_get_kbot_ktop

      integer, intent(in) :: irstfile
      real(kind=hp), intent(in) :: tim

      integer, save :: ierr
      integer, save :: &
         !id_netcelldim, id_netcellmaxnodedim, id_netcellcontourptsdim, &
         id_laydim, id_wdim, &
         id_flowelemdim, &
         id_maxfracdim, &
         id_erolaydim, &
         id_flowlinkdim, &
         id_timedim, &
         id_bndsaldim, &
         id_bndtemdim, &
         id_bndseddim, &
         id_bnddim, &
         id_culvertdim, &
         id_longculvertdim, &
         id_genstrudim, &
         id_genstru_furudim, &
         id_genstru_linkdim, &
         id_weirgendim, &
         id_weirgen_furudim, &
         id_weirgen_linkdim, &
         id_orifgendim, &
         id_orifgen_furudim, &
         id_orifgen_linkdim, &
         id_pumpdim, &
         id_pump_stagedim, &
         id_1dflowlink_dim, &
         id_time, id_timestep, &
         id_s1, id_taus, id_ucx, id_ucy, id_ucz, id_unorm, id_q1, id_ww1, id_sa1, id_tem1, id_sed, id_ero, id_s0, id_u0, &
         id_czs, id_E, id_thetamean, &
         id_sigmwav, &
         id_tsalbnd, id_zsalbnd, id_ttembnd, id_ztembnd, id_tsedbnd, id_zsedbnd, &
         id_morbl, id_bodsed, id_msed, id_thlyr, id_lyrfrac, id_preload, id_poros, id_sedshort, id_dpsed, id_mfluff, id_sedtotdim, id_sedsusdim, id_nlyrdim, &
         id_netelemmaxnodedim, id_netnodedim, id_flowlinkptsdim, id_netelemdim, id_netlinkdim, id_netlinkptsdim, &
         id_flowelemdomain, id_flowelemglobalnr, id_flowlink, id_netelemnode, id_netlink, &
         id_flowelemxzw, id_flowelemyzw, id_flowlinkxu, id_flowlinkyu, &
         id_flowelemxbnd, id_flowelemybnd, id_bl, id_s0bnd, id_s1bnd, id_blbnd, &
         id_unorma, id_vicwwu, id_tureps1, id_turkin1, id_qw, id_qa, id_squ, id_sqi, &
         id_squbnd, id_sqibnd, &
         id_weirdte, &
         id_jmax, id_flowelemcrsz, id_ncrs, id_morft, id_morCrsName, id_strlendim, &
         id_culvert_openh, id_longculvert_valveopen, &
         id_genstru_crestl, id_genstru_edgel, id_genstru_openw, id_genstru_fu, id_genstru_ru, id_genstru_au, id_genstru_crestw, &
         id_genstru_area, id_genstru_linkw, id_genstru_state, id_genstru_sOnCrest, &
         id_weirgen_crestl, id_weirgen_crestw, id_weirgen_area, id_weirgen_linkw, id_weirgen_fu, id_weirgen_ru, id_weirgen_au, id_weirgen_state, id_weirgen_sOnCrest, &
         id_orifgen_crestl, id_orifgen_edgel, id_orifgen_openw, id_orifgen_fu, id_orifgen_ru, id_orifgen_au, id_orifgen_crestw, &
         id_orifgen_area, id_orifgen_linkw, id_orifgen_state, id_orifgen_sOnCrest, &
         id_pump_cap, id_pump_ssTrigger, id_pump_dsTrigger, &
         id_hysteresis, id_flowelemxcc, id_flowelemycc, &
         id_spirint, id_hu, &
         id_ucxq, id_ucyq, &
         id_ucxqbnd, id_ucyqbnd, &
         id_fvcoro, &
         id_rho, id_rho_bnd, &
         id_rhowat, id_rhowat_bnd

      integer, allocatable, save :: id_tr1(:), id_rwqb(:), id_bndtradim(:), id_ttrabnd(:), id_ztrabnd(:)
      integer, allocatable, save :: id_sf1(:), id_bndsedfracdim(:), id_tsedfracbnd(:), id_zsedfracbnd(:)
      integer, allocatable, save :: id_sf1_bnd(:)

      integer :: i, itim, k, kb, kt, kk, LL, Lb, iconst, L, j, nv, nv1, nm, ndxbnd, nlayb, nrlay, LTX, nlaybL, nrlaylx, maxNumLinks, numLinks, L0, nlen, istru, maxNumStages, numStages, nfuru
      double precision :: svthick
      double precision, dimension(:), pointer :: dens
      double precision, allocatable :: max_threttim(:)
      double precision, dimension(:), allocatable :: dum
      double precision, dimension(:, :, :), allocatable :: frac
      double precision, dimension(:, :), allocatable :: poros
      integer, allocatable, dimension(:, :) :: netcellnod
      integer, allocatable, dimension(:) :: kn1write, kn2write
      double precision, allocatable, dimension(:) :: tmp_x, tmp_y, tmp_s0, tmp_s1, tmp_bl, tmp_sa1, tmp_tem1
      double precision, allocatable, dimension(:) :: tmp_squ, tmp_sqi
      double precision, allocatable, dimension(:) :: tmp_ucxq, tmp_ucyq
      logical :: is_wq_bot_3d

      character(len=8) :: numformat
      character(len=2) :: numtrastr, numsedfracstr
      character(len=255) :: tmpstr

      type(t_CSType), pointer :: pCS
      type(t_CSType), pointer, dimension(:) :: pCSs
      integer :: jmax, ndx1d, nCrs
      double precision, dimension(:, :), allocatable :: work1d_z
      double precision, dimension(:, :), allocatable :: work2d
      integer, dimension(:, :), allocatable :: work2di
      integer, dimension(:), allocatable :: work1di
      double precision, dimension(:, :, :), allocatable :: work3d
      integer, dimension(:, :, :), allocatable :: work3di
      type(t_structure), pointer :: pstru

      integer, allocatable, dimension(:) :: id1, id1_bnd

      ! Grid and flow geometry
      ! hk: not now  call unc_write_net_filepointer(irstfile)      ! Write standard net data as well
      ! please fix   call unc_write_flowgeom_filepointer(irstfile) ! Write time-independent flow geometry data
      ! ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_flowelemdim)
      ! ierr = nf90_inq_dimid(irstfile, 'nFlowLink', id_flowlinkdim)

      numformat = '(I2.2)'
      if (lnx > 0) then
         ierr = nf90_def_dim(irstfile, 'nFlowLink', lnx, id_flowlinkdim)
         ierr = nf90_def_dim(irstfile, 'nFlowLinkPts', 2, id_flowlinkptsdim)
      end if

      if (ndx > 0) then
         ndx1d = ndxi - ndx2d
         ierr = nf90_def_dim(irstfile, 'nFlowElem', ndxi, id_flowelemdim)
         if (jarstbnd > 0) then ! If write boundary points
            if (jampi == 0) then
               ndxbnd = ndx - ndxi
            else
               ndxbnd = ndxbnd_own ! In parallel run, do not write ghost boundary points
            end if
            if (ndxbnd > 0) then ! if boundary points exist
               call realloc(tmp_x, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_y, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_s0, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_s1, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_bl, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_squ, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_sqi, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_ucxq, ndxbnd, stat=ierr, keepExisting=.false.)
               call realloc(tmp_ucyq, ndxbnd, stat=ierr, keepExisting=.false.)
               if (kmx == 0) then
                  call realloc(tmp_sa1, ndxbnd, stat=ierr, keepExisting=.false.)
                  call realloc(tmp_tem1, ndxbnd, stat=ierr, keepExisting=.false.)
               end if
               ierr = nf90_def_dim(irstfile, 'nFlowElemBnd', ndxbnd, id_bnddim)
            end if
         end if
      end if

      ierr = nf90_def_dim(irstfile, 'nNetElem', nump1d2d, id_netelemdim)
      ierr = nf90_def_dim(irstfile, 'nNetNode', numk, id_netnodedim)
      nv = 0
      do k = 1, nump1d2d
         nv = max(nv, netcell(k)%n)
      end do
      ierr = nf90_def_dim(irstfile, 'nNetElemMaxNode', nv, id_netelemmaxnodedim)
      ierr = nf90_def_dim(irstfile, 'nNetLink', numl, id_netlinkdim)
      ierr = nf90_def_dim(irstfile, 'nNetLinkPts', 2, id_netlinkptsdim)
      ! Definition and attributes of time
      ierr = nf90_def_dim(irstfile, 'time', nf90_unlimited, id_timedim)
      call check_error(ierr, 'def time dim')
      ierr = nf90_def_var(irstfile, 'time', nf90_double, id_timedim, id_time)
      ierr = nf90_put_att(irstfile, id_time, 'units', trim(Tudunitstr))
      ierr = nf90_put_att(irstfile, id_time, 'standard_name', 'time')

      ! Definition and attributes of 3D geometry
      if (kmx > 0) then
         ! call unc_append_3dflowgeom_def(irstfile)              ! Append definition of time-independent 3d flow geometry data
         ! ierr = nf90_inq_dimid(irstfile, 'laydim', id_laydim)
         ! ierr = nf90_inq_dimid(irstfile, 'wdim', id_wdim)

         ierr = nf90_def_dim(irstfile, 'laydim', kmx, id_laydim)
         ierr = nf90_def_dim(irstfile, 'wdim', kmx + 1, id_wdim)
      end if

      ! Thatcher-Harleman boundary data dimensions
      ! TODO: AvD, GvO: NOTE! I renamed al TH-stuff below consistent with the original meaning of numtracers.
      !       BUT, we should double-check: should we not maintain TH lags for *all* open boundaries, for *all* constituents?
      if (allocated(threttim)) then
         allocate (max_threttim(NUMCONST))
         max_threttim = maxval(threttim, dim=2)
         if (jasal > 0) then
            if (max_threttim(ISALT) > 0d0) then
               ierr = nf90_def_dim(irstfile, 'salbndpt', nbnds, id_bndsaldim)
            end if
         end if
         if (jatem > 0) then
            if (max_threttim(ITEMP) > 0d0) then
               ierr = nf90_def_dim(irstfile, 'tembndpt', nbndtm, id_bndtemdim)
            end if
         end if
         if (jased > 0 .and. .not. stm_included) then
            if (max_threttim(ISED1) > 0d0) then
               ierr = nf90_def_dim(irstfile, 'sedbndpt', nbndsd, id_bndseddim)
            end if
         end if
         if (numfracs > 0) then
            if (.not. allocated(id_bndsedfracdim)) then
               allocate (id_bndsedfracdim(numfracs))
            end if
            do i = 1, numfracs
               iconst = ifrac2const(i)
               if (iconst == 0) cycle
               if (max_threttim(iconst) > 0d0) then ! allocated on NUMCONST
                  write (numsedfracstr, numformat) i
                  ierr = nf90_def_dim(irstfile, 'sfbndpt'//trim(numsedfracstr), nbndsf(i), id_bndsedfracdim(i))
               end if
            end do
         end if
         if (numtracers > 0) then
            if (.not. allocated(id_bndtradim)) then
               allocate (id_bndtradim(numtracers))
            end if
            do i = 1, numtracers
               iconst = itrac2const(i)
               if (max_threttim(iconst) > 0d0) then
                  write (numtrastr, numformat) i
                  ierr = nf90_def_dim(irstfile, 'trbndpt'//trim(numtrastr), nbndtr(i), id_bndtradim(i))
               end if
            end do
         end if
      end if

      ! 2D or 3D vector
      ! We should tr to reduce the 2D-3D branching in this routine. We can do this by using this vector.
      if (kmx > 0) then
         id1 = (/id_laydim, id_flowelemdim, id_timedim/)
         id1_bnd = (/id_laydim, id_bnddim, id_timedim/)
      else
         id1 = (/id_flowelemdim, id_timedim/)
         id1_bnd = (/id_bnddim, id_timedim/)
      end if

      call process_structures_saved_parameters(DEFINE_NCDF_DATA_ID, irstfile)

      ! Definition and attributes of size of latest timestep
      ierr = nf90_def_var(irstfile, 'timestep', nf90_double, id_timedim, id_timestep)
      ierr = nf90_put_att(irstfile, id_timestep, 'units', 'seconds')
      ierr = nf90_put_att(irstfile, id_timestep, 'standard_name', 'timestep')

      ! Definition and attributes of flow data on centres: water level at latest timestep
      ierr = nf90_def_var(irstfile, 's1', nf90_double, (/id_flowelemdim, id_timedim/), id_s1)
      ierr = nf90_put_att(irstfile, id_s1, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_s1, 'standard_name', 'sea_surface_height') ! sorry for inland water people
      ierr = nf90_put_att(irstfile, id_s1, 'long_name', 'water level')
      ierr = nf90_put_att(irstfile, id_s1, 'units', 'm')

      ! Definition and attributes of flow data on centres: water level timestep before the latest timestep
      ierr = nf90_def_var(irstfile, 's0', nf90_double, (/id_flowelemdim, id_timedim/), id_s0)
      ierr = nf90_put_att(irstfile, id_s0, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_s0, 'standard_name', 'sea_surface_height') ! sorry for inland water people
      ierr = nf90_put_att(irstfile, id_s0, 'long_name', 'water level at previous timestep')
      ierr = nf90_put_att(irstfile, id_s0, 'units', 'm')

      ! Definition and attributes of flow data on centres: shear stress
      ierr = nf90_def_var(irstfile, 'taus', nf90_double, (/id_flowelemdim, id_timedim/), id_taus)
      ierr = nf90_put_att(irstfile, id_taus, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_taus, 'standard_name', 'taucurrent')
      ierr = nf90_put_att(irstfile, id_taus, 'long_name', 'taucurrent in flow element center')
      ierr = nf90_put_att(irstfile, id_taus, 'units', 'N m-2')

      ! Definition and attributes of flow data on centres: chezy roughness
      ierr = nf90_def_var(irstfile, 'czs', nf90_double, (/id_flowelemdim, id_timedim/), id_czs)
      ierr = nf90_put_att(irstfile, id_czs, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_czs, 'long_name', 'Chezy roughness in flow element center')
      ierr = nf90_put_att(irstfile, id_czs, 'units', 'm0.5s-1')

      ! Definition and attributes of flow data on centres: bed level
      ierr = nf90_def_var(irstfile, 'FlowElem_bl', nf90_double, (/id_flowelemdim, id_timedim/), id_bl)
      ierr = nf90_put_att(irstfile, id_bl, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
      ierr = nf90_put_att(irstfile, id_bl, 'long_name', 'bed level at flow element circumcenter')
      ierr = nf90_put_att(irstfile, id_bl, 'units', 'm')

      ! Definition of 3D data
      if (kmx > 0) then

         ! Definition and attributes of flow data on edges: velocity magnitude at latest timestep
         ierr = nf90_def_var(irstfile, 'unorm', nf90_double, (/id_laydim, id_flowlinkdim, id_timedim/), id_unorm)
         ierr = nf90_put_att(irstfile, id_unorm, 'long_name', 'normal component of sea_water_speed')
         ierr = nf90_put_att(irstfile, id_unorm, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_unorm, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
         ierr = nf90_def_var(irstfile, 'u0', nf90_double, (/id_laydim, id_flowlinkdim, id_timedim/), id_u0)
         ierr = nf90_put_att(irstfile, id_u0, 'long_name', 'normal component of sea_water_speed at previous time t0')
         ierr = nf90_put_att(irstfile, id_u0, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_u0, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on edges: cell center velocity, q based  global x-dir (m/s)
         ierr = nf90_def_var(irstfile, 'ucxq', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_ucxq)
         ierr = nf90_put_att(irstfile, id_ucxq, 'long_name', 'q-based cell center velocity, x-dir')
         ierr = nf90_put_att(irstfile, id_ucxq, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucxq, 'coordinates', 'FlowElem_xcc FlowElem_ycc')

         ! Definition and attributes of flow data on edges: cell center velocity, q based  global y-dir (m/s)
         ierr = nf90_def_var(irstfile, 'ucyq', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_ucyq)
         ierr = nf90_put_att(irstfile, id_ucyq, 'long_name', 'q-based cell center velocity, y-dir')
         ierr = nf90_put_att(irstfile, id_ucyq, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucyq, 'coordinates', 'FlowElem_xcc FlowElem_ycc')

         ! Definition and attributes of flow data on edges: discharge
         ierr = nf90_def_var(irstfile, 'q1', nf90_double, (/id_laydim, id_flowlinkdim, id_timedim/), id_q1)
         ierr = nf90_put_att(irstfile, id_q1, 'standard_name', 'discharge')
         ierr = nf90_put_att(irstfile, id_q1, 'long_name', 'discharge through flow link at current time')
         ierr = nf90_put_att(irstfile, id_q1, 'units', 'm3 s-1')
         ierr = nf90_put_att(irstfile, id_q1, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on centres: x-component of the velocity
         ierr = nf90_def_var(irstfile, 'ucx', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_ucx)
         ierr = nf90_put_att(irstfile, id_ucx, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         if (jsferic == 0) then
            ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'sea_water_x_velocity')
         else
            ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'eastward_sea_water_velocity')
         end if
         ierr = nf90_put_att(irstfile, id_ucx, 'long_name', 'flow element center velocity vector, x-component')
         ierr = nf90_put_att(irstfile, id_ucx, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucx, '_FillValue', dmiss)

         ! Definition and attributes of flow data on centres: y-component of the velocity
         ierr = nf90_def_var(irstfile, 'ucy', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_ucy)
         ierr = nf90_put_att(irstfile, id_ucy, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         if (jsferic == 0) then
            ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'sea_water_y_velocity')
         else
            ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'northward_sea_water_velocity')
         end if
         ierr = nf90_put_att(irstfile, id_ucy, 'long_name', 'flow element center velocity vector, y-component')
         ierr = nf90_put_att(irstfile, id_ucy, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucy, '_FillValue', dmiss)

         ! Definition and attributes of flow data on centres: z-component of the velocity
         ierr = nf90_def_var(irstfile, 'ucz', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_ucz)
         ierr = nf90_put_att(irstfile, id_ucz, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_ucz, 'standard_name', 'upward_sea_water_velocity')
         ierr = nf90_put_att(irstfile, id_ucz, 'long_name', 'upward velocity on flow element center')
         ierr = nf90_put_att(irstfile, id_ucz, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucz, '_FillValue', dmiss)

         ! Definition and attributes of flow data on centres: z-component of the velocity on vertical interface
         ierr = nf90_def_var(irstfile, 'ww1', nf90_double, (/id_wdim, id_flowelemdim, id_timedim/), id_ww1)
         ierr = nf90_put_att(irstfile, id_ww1, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_ww1, 'standard_name', 'upward_sea_water_velocity') ! same standard name allowed?
         ierr = nf90_put_att(irstfile, id_ww1, 'long_name', 'upward velocity on vertical interface') ! (upward normal or upward)?
         ierr = nf90_put_att(irstfile, id_ww1, 'units', 'm s-1')

         ! Definition and attributes of depth averaged velocity u1(1:lnx)
         ierr = nf90_def_var(irstfile, 'unorm_averaged', nf90_double, (/id_flowlinkdim, id_timedim/), id_unorma)
         ierr = nf90_put_att(irstfile, id_unorma, 'long_name', 'depth averaged normal component of sea_water_speed')
         ierr = nf90_put_att(irstfile, id_unorma, 'units', 'm3 s-1')
         ierr = nf90_put_att(irstfile, id_unorma, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of vertical flux through interface qw
         ierr = nf90_def_var(irstfile, 'qw', nf90_double, (/id_wdim, id_flowelemdim, id_timedim/), id_qw)
         ierr = nf90_put_att(irstfile, id_qw, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_qw, 'long_name', 'vertical flux through interface')
         ierr = nf90_put_att(irstfile, id_qw, 'units', 'm3 s-1')

         ! Definition and attributes of discharge used in advection qa
         ierr = nf90_def_var(irstfile, 'qa', nf90_double, (/id_laydim, id_flowlinkdim, id_timedim/), id_qa)
         ierr = nf90_put_att(irstfile, id_qa, 'coordinates', 'FlowLink_xu FlowLink_yu')
         ierr = nf90_put_att(irstfile, id_qa, 'long_name', 'discharge used in advection')
         ierr = nf90_put_att(irstfile, id_qa, 'units', 'm3 s-1')

         ! Definition and attributes of cell center incoming flux
         ierr = nf90_def_var(irstfile, 'sqi', nf90_double, (/id_wdim, id_flowelemdim, id_timedim/), id_sqi)
         ierr = nf90_put_att(irstfile, id_sqi, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_sqi, 'long_name', 'cell center incoming flux')
         ierr = nf90_put_att(irstfile, id_sqi, 'units', 'm3 s-1')

         ! Definition and attributes of cell center outcoming flux
         ierr = nf90_def_var(irstfile, 'squ', nf90_double, (/id_wdim, id_flowelemdim, id_timedim/), id_squ)
         ierr = nf90_put_att(irstfile, id_squ, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_squ, 'long_name', 'cell center outcoming flux')
         ierr = nf90_put_att(irstfile, id_squ, 'units', 'm3 s-1')

         ! Flow depth at links
         ierr = nf90_def_var(irstfile, 'hu', nf90_double, (/id_wdim, id_flowlinkdim, id_timedim/), id_hu)
         ierr = nf90_put_att(irstfile, id_hu, 'coordinates', 'FlowLink_xu FlowLink_yu')
         ierr = nf90_put_att(irstfile, id_hu, 'long_name', 'flow depth at link')
         ierr = nf90_put_att(irstfile, id_hu, 'units', 'm')

         ! Coriolis
         if (Corioadamsbashfordfac > 0d0) then
            ierr = nf90_def_var(irstfile, 'fvcoro', nf90_double, (/id_wdim, id_flowlinkdim, id_timedim/), id_fvcoro)
            ierr = nf90_put_att(irstfile, id_fvcoro, 'coordinates', 'FlowLink_xu FlowLink_yu')
            ierr = nf90_put_att(irstfile, id_fvcoro, 'long_name', 'Coriolis term Adams-Bashford')
            ierr = nf90_put_att(irstfile, id_fvcoro, 'units', 'm s-2')
         end if

         if (iturbulencemodel >= 3) then
            ! Definition and attributes of vertical eddy viscosity vicwwu
            ierr = nf90_def_var(irstfile, 'vicwwu', nf90_double, (/id_wdim, id_flowlinkdim, id_timedim/), id_vicwwu)
            ierr = nf90_put_att(irstfile, id_vicwwu, 'coordinates', 'FlowLink_xu FlowLink_yu')
            ierr = nf90_put_att(irstfile, id_vicwwu, 'long_name', 'turbulent vertical eddy viscosity')
            ierr = nf90_put_att(irstfile, id_vicwwu, 'units', 'm2 s-1')
            ierr = nf90_put_att(irstfile, id_vicwwu, '_FillValue', dmiss)

            ! Definition and attributes of kinetic energy
            ierr = nf90_def_var(irstfile, 'turkin1', nf90_double, (/id_wdim, id_flowlinkdim, id_timedim/), id_turkin1)
            ierr = nf90_put_att(irstfile, id_turkin1, 'coordinates', 'FlowLink_xu FlowLink_yu')
            ierr = nf90_put_att(irstfile, id_turkin1, 'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
            ierr = nf90_put_att(irstfile, id_turkin1, 'long_name', 'turbulent kinetic energy')
            ierr = nf90_put_att(irstfile, id_turkin1, 'units', 'm2 s-2')
            ierr = nf90_put_att(irstfile, id_turkin1, '_FillValue', dmiss)

            ! Definition and attributes of energy_dissipation or turbulence_time_scale
            ierr = nf90_def_var(irstfile, 'tureps1', nf90_double, (/id_wdim, id_flowlinkdim, id_timedim/), id_tureps1)
            ierr = nf90_put_att(irstfile, id_tureps1, 'coordinates', 'FlowLink_xu FlowLink_yu')
            ierr = nf90_put_att(irstfile, id_tureps1, '_FillValue', dmiss)
            if (iturbulencemodel == 3) then
               ierr = nf90_put_att(irstfile, id_tureps1, 'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
               ierr = nf90_put_att(irstfile, id_tureps1, 'long_name', 'turbulent energy dissipation')
               ierr = nf90_put_att(irstfile, id_tureps1, 'units', 'm2 s-3')
            else if (iturbulencemodel == 4) then
               !ierr = nf90_put_att(irstfile, id_tureps1,  'standard_name', '')
               ierr = nf90_put_att(irstfile, id_tureps1, 'long_name', 'turbulent time scale')
               ierr = nf90_put_att(irstfile, id_tureps1, 'units', 's-1')
            end if
         end if
      else
         ! Definition and attributes of flow data on centres: x-component of the velocity
         ierr = nf90_def_var(irstfile, 'ucx', nf90_double, (/id_flowelemdim, id_timedim/), id_ucx)
         ierr = nf90_put_att(irstfile, id_ucx, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         if (jsferic == 0) then
            ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'sea_water_x_velocity')
         else
            ierr = nf90_put_att(irstfile, id_ucx, 'standard_name', 'eastward_sea_water_velocity')
         end if
         ierr = nf90_put_att(irstfile, id_ucx, 'long_name', 'velocity on flow element center, x-component')
         ierr = nf90_put_att(irstfile, id_ucx, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucx, '_FillValue', dmiss)

         ! Definition and attributes of flow data on centres: y-component of the velocity
         ierr = nf90_def_var(irstfile, 'ucy', nf90_double, (/id_flowelemdim, id_timedim/), id_ucy)
         ierr = nf90_put_att(irstfile, id_ucy, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         if (jsferic == 0) then
            ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'sea_water_y_velocity')
         else
            ierr = nf90_put_att(irstfile, id_ucy, 'standard_name', 'northward_sea_water_velocity')
         end if
         ierr = nf90_put_att(irstfile, id_ucy, 'long_name', 'velocity on flow element center, y-component')
         ierr = nf90_put_att(irstfile, id_ucy, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucy, '_FillValue', dmiss)

         ! Definition and attributes of flow data on edges: velocity magnitude at latest timestep
         ierr = nf90_def_var(irstfile, 'unorm', nf90_double, (/id_flowlinkdim, id_timedim/), id_unorm)
         ierr = nf90_put_att(irstfile, id_unorm, 'long_name', 'normal component of sea_water_speed')
         ierr = nf90_put_att(irstfile, id_unorm, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_unorm, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
         ierr = nf90_def_var(irstfile, 'u0', nf90_double, (/id_flowlinkdim, id_timedim/), id_u0)
         ierr = nf90_put_att(irstfile, id_u0, 'long_name', 'normal component of velocity through flow link at previous time t0')
         ierr = nf90_put_att(irstfile, id_u0, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_u0, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on edges: cell center velocity, q based  global x-dir (m/s)
         ierr = nf90_def_var(irstfile, 'ucxq', nf90_double, (/id_flowelemdim, id_timedim/), id_ucxq)
         ierr = nf90_put_att(irstfile, id_ucxq, 'long_name', 'q-based cell center velocity, x-dir')
         ierr = nf90_put_att(irstfile, id_ucxq, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucxq, 'coordinates', 'FlowElem_xcc FlowElem_ycc')

         ! Definition and attributes of flow data on edges: cell center velocity, q based  global y-dir (m/s)
         ierr = nf90_def_var(irstfile, 'ucyq', nf90_double, (/id_flowelemdim, id_timedim/), id_ucyq)
         ierr = nf90_put_att(irstfile, id_ucyq, 'long_name', 'q-based cell center velocity, y-dir')
         ierr = nf90_put_att(irstfile, id_ucyq, 'units', 'm s-1')
         ierr = nf90_put_att(irstfile, id_ucyq, 'coordinates', 'FlowElem_xcc FlowElem_ycc')

         ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
         ierr = nf90_def_var(irstfile, 'q1', nf90_double, (/id_flowlinkdim, id_timedim/), id_q1)
         ierr = nf90_put_att(irstfile, id_q1, 'standard_name', 'discharge')
         ierr = nf90_put_att(irstfile, id_q1, 'long_name', 'discharge through flow link at current time')
         ierr = nf90_put_att(irstfile, id_q1, 'units', 'm3 s-1')
         ierr = nf90_put_att(irstfile, id_q1, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of flow data on edges: velocity magnitude at previous timestep
         ierr = nf90_def_var(irstfile, 'qa', nf90_double, (/id_flowlinkdim, id_timedim/), id_qa)
         ierr = nf90_put_att(irstfile, id_qa, 'long_name', 'discharge used in advection')
         ierr = nf90_put_att(irstfile, id_qa, 'units', 'm3 s-1')
         ierr = nf90_put_att(irstfile, id_qa, 'coordinates', 'FlowLink_xu FlowLink_yu')

         ! Definition and attributes of cell center outcoming flux
         ierr = nf90_def_var(irstfile, 'squ', nf90_double, (/id_flowelemdim, id_timedim/), id_squ)
         ierr = nf90_put_att(irstfile, id_squ, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_squ, 'long_name', 'cell center outcoming flux')
         ierr = nf90_put_att(irstfile, id_squ, 'units', 'm3 s-1')

         ! Definition and attributes of cell center outcoming flux
         ierr = nf90_def_var(irstfile, 'sqi', nf90_double, (/id_flowelemdim, id_timedim/), id_sqi)
         ierr = nf90_put_att(irstfile, id_sqi, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_sqi, 'long_name', 'cell center incoming flux')
         ierr = nf90_put_att(irstfile, id_sqi, 'units', 'm3 s-1')

         ! Flow depth at links
         ierr = nf90_def_var(irstfile, 'hu', nf90_double, (/id_flowlinkdim, id_timedim/), id_hu)
         ierr = nf90_put_att(irstfile, id_hu, 'coordinates', 'FlowLink_xu FlowLink_yu')
         ierr = nf90_put_att(irstfile, id_hu, 'long_name', 'flow depth at link')
         ierr = nf90_put_att(irstfile, id_hu, 'units', 'm')

         if (Corioadamsbashfordfac > 0d0) then
            ierr = nf90_def_var(irstfile, 'fvcoro', nf90_double, (/id_flowlinkdim, id_timedim/), id_fvcoro)
            ierr = nf90_put_att(irstfile, id_fvcoro, 'coordinates', 'FlowLink_xu FlowLink_yu')
            ierr = nf90_put_att(irstfile, id_fvcoro, 'long_name', 'Coriolis term Adams-Bashford')
            ierr = nf90_put_att(irstfile, id_fvcoro, 'units', 'm s-2')
         end if

      end if

      !fixed weirs data
      if (ncdamsg > 0 .or. ifixedweirscheme > 0) then
         ierr = nf90_def_var(irstfile, 'weirdte', nf90_double, (/id_flowlinkdim, id_timedim/), id_weirdte)
         ierr = nf90_put_att(irstfile, id_weirdte, 'coordinates', 'FlowLink_xu FlowLink_yu')
         ierr = nf90_put_att(irstfile, id_weirdte, 'long_name', 'energy-head loss')
         ierr = nf90_put_att(irstfile, id_weirdte, 'units', 'm')
      end if

      ! Definition and attributes of flow data on centres: salinity
      if (jasal > 0) then
         if (kmx > 0) then
            ierr = nf90_def_var(irstfile, 'sa1', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_sa1)
         else
            ierr = nf90_def_var(irstfile, 'sa1', nf90_double, (/id_flowelemdim, id_timedim/), id_sa1)
         end if
         ierr = nf90_put_att(irstfile, id_sa1, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_sa1, 'standard_name', 'sea_water_salinity')
         ierr = nf90_put_att(irstfile, id_sa1, 'long_name', 'salinity')
         ierr = nf90_put_att(irstfile, id_sa1, 'units', '1e-3')
      end if

      ! Definition and attributes of flow data on centres: temperature
      if (jatem > 0) then
         if (kmx > 0) then
            ierr = nf90_def_var(irstfile, 'tem1', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_tem1)
         else
            ierr = nf90_def_var(irstfile, 'tem1', nf90_double, (/id_flowelemdim, id_timedim/), id_tem1)
         end if
         ierr = nf90_put_att(irstfile, id_tem1, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_tem1, 'standard_name', 'sea_water_temperature')
         ierr = nf90_put_att(irstfile, id_tem1, 'long_name', 'temperature in flow element')
         ierr = nf90_put_att(irstfile, id_tem1, 'units', 'degC')
      end if

      ! Tracer fields
      if (ITRA1 > 0) then
         if ((.not. allocated(id_tr1)) .or. (ITRAN0 /= ITRAN)) then ! If id_tri is not allocated, or if last  tracer changes
            call realloc(id_tr1, ITRAN - ITRA1 + 1)
         end if
         do i = ITRA1, ITRAN
            j = i - ITRA1 + 1
            tmpstr = const_names(i)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr, 32, 95)
            call replace_char(tmpstr, 47, 95)
            if (kmx > 0) then
               ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_tr1(j))
            else
               ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/id_flowelemdim, id_timedim/), id_tr1(j))
            end if
            ierr = nf90_put_att(irstfile, id_tr1(j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_tr1(j), 'standard_name', trim(tmpstr))
            ierr = nf90_put_att(irstfile, id_tr1(j), 'long_name', trim(tmpstr))
            if (const_units(i) /= ' ') then
               tmpstr = const_units(i)
            else
               tmpstr = '-'
            end if
            ierr = nf90_put_att(irstfile, id_tr1(j), 'units', tmpstr)
         end do
         ITRAN0 = ITRAN
      end if

      is_wq_bot_3d = jahiswqbot3d == 1 .or. jamapwqbot3d == 1
      ! water quality bottom variables
      if (numwqbots > 0) then
         call realloc(id_rwqb, numwqbots, keepExisting=.false., fill=0)
         do j = 1, numwqbots
            tmpstr = wqbotnames(j)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr, 32, 95)
            call replace_char(tmpstr, 47, 95)
            if (is_wq_bot_3d) then
               ierr = nf90_def_var(irstfile, trim(tmpstr)//'_3D', nf90_double, (/id_laydim, id_flowelemdim, id_timedim/), id_rwqb(j))
            else
               ierr = nf90_def_var(irstfile, trim(tmpstr), nf90_double, (/id_flowelemdim, id_timedim/), id_rwqb(j))
            end if
            ierr = nf90_put_att(irstfile, id_rwqb(j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_rwqb(j), 'standard_name', trim(tmpstr))
            ierr = nf90_put_att(irstfile, id_rwqb(j), 'long_name', trim(tmpstr))
            tmpstr = wqbotunits(j)
            ierr = nf90_put_att(irstfile, id_rwqb(j), 'units', trim(tmpstr))
         end do
         ITRAN0 = ITRAN
      end if

      if (jawave == 4) then
         ierr = nf90_def_var(irstfile, 'E', nf90_double, (/id_flowelemdim, id_timedim/), id_E)
         ierr = nf90_put_att(irstfile, id_E, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_E, 'standard_name', 'sea_surface_bulk_wave_energy') ! not CF
         ierr = nf90_put_att(irstfile, id_E, 'long_name', 'wave energy per square meter')
         ierr = nf90_put_att(irstfile, id_E, 'units', 'J m-2')

         ierr = nf90_def_var(irstfile, 'thetamean', nf90_double, (/id_flowelemdim, id_timedim/), id_thetamean)
         ierr = nf90_put_att(irstfile, id_thetamean, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_thetamean, 'standard_name', 'sea_surface_wave_from_direction') ! not CF
         ierr = nf90_put_att(irstfile, id_thetamean, 'long_name', 'mean wave angle')
         ierr = nf90_put_att(irstfile, id_thetamean, 'units', 'rad')

         ierr = nf90_def_var(irstfile, 'sigmwav', nf90_double, (/id_flowelemdim, id_timedim/), id_sigmwav)
         ierr = nf90_put_att(irstfile, id_sigmwav, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_sigmwav, 'standard_name', 'sea_surface_wave_mean_frequency') ! not CF
         ierr = nf90_put_att(irstfile, id_sigmwav, 'long_name', 'mean wave frequency')
         ierr = nf90_put_att(irstfile, id_sigmwav, 'units', 'rad s-1')
      end if

      ndx1d = ndxi - ndx2d

      if (jased > 0 .and. stm_included) then
         ierr = nf90_def_dim(irstfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim)
         ierr = nf90_def_dim(irstfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim)
         ierr = nf90_def_dim(irstfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim)
         !
         if (stmpar%lsedsus > 0) then
            ! Fill internal cells
            call add_att_sediment(id_sf1, stmpar%lsedsus, id_laydim, id_flowelemdim, id_timedim, irstfile, '', 'FlowElem_xcc FlowElem_ycc')
            ! Fill boundary cells
            if (jarstbnd > 0 .and. ndxbnd > 0) then
               call add_att_sediment(id_sf1_bnd, stmpar%lsedsus, id_laydim, id_bnddim, id_timedim, irstfile, '_bnd', 'FlowElem_xbnd FlowElem_ybnd')
            end if
            ! density (only necessary if morphodynamics and fractions in suspension and consider concentrations in density)
            if (stmpar%morpar%densin) then
               ! rho
               ierr = nf90_def_var(irstfile, 'rho', nf90_double, id1, id_rho)
               ierr = nf90_put_att(irstfile, id_rho, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(irstfile, id_rho, 'long_name', 'Water density')
               ierr = nf90_put_att(irstfile, id_rho, 'units', 'kg m-3')
               if (jarstbnd > 0 .and. ndxbnd > 0) then
                  ierr = nf90_def_var(irstfile, 'rho_bnd', nf90_double, id1_bnd, id_rho_bnd)
                  ierr = nf90_put_att(irstfile, id_rho_bnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
                  ierr = nf90_put_att(irstfile, id_rho_bnd, 'long_name', 'Water density at boundaries')
                  ierr = nf90_put_att(irstfile, id_rho_bnd, 'units', 'kg m-3')
               end if !(jarstbnd > 0 .and. ndxbnd > 0) then
               !rhowat
               ierr = nf90_def_var(irstfile, 'rhowat', nf90_double, id1, id_rhowat)
               ierr = nf90_put_att(irstfile, id_rhowat, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(irstfile, id_rhowat, 'long_name', 'Water density (without sediment)')
               ierr = nf90_put_att(irstfile, id_rhowat, 'units', 'kg m-3')
               if (jarstbnd > 0 .and. ndxbnd > 0) then
                  ierr = nf90_def_var(irstfile, 'rhowat_bnd', nf90_double, id1_bnd, id_rhowat_bnd)
                  ierr = nf90_put_att(irstfile, id_rhowat_bnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
                  ierr = nf90_put_att(irstfile, id_rhowat_bnd, 'long_name', 'Water density (without sediment) at boundaries')
                  ierr = nf90_put_att(irstfile, id_rhowat_bnd, 'units', 'kg m-3')
               end if !(jarstbnd > 0 .and. ndxbnd > 0) then
            end if
         end if
         !
         ierr = nf90_def_var(irstfile, 'mor_bl', nf90_double, (/id_flowelemdim, id_timedim/), id_morbl)
         ierr = nf90_put_att(irstfile, id_morbl, 'coordinates', 'FlowElem_xcc FlowElem_ycc') ! not CF
         ierr = nf90_put_att(irstfile, id_morbl, 'long_name', 'Time-varying bottom level in flow cell center')
         ierr = nf90_put_att(irstfile, id_morbl, 'units', 'm')
         !
         ierr = nf90_def_var(irstfile, 'morft', nf90_double, id_timedim, id_morft)
         ierr = nf90_put_att(irstfile, id_morft, 'units', 'morphological days since '//refdat(1:4)//'-'//refdat(5:6)//'-'//refdat(7:8)//' 00:00:00')
         ierr = nf90_put_att(irstfile, id_morft, 'standard_name', 'morphological time')
         ierr = nf90_put_att(irstfile, id_morbl, 'units', 'days')
         !
         if (ndx1d > 0 .and. stm_included) then
            if (stmpar%morpar%bedupd) then
               nCrs = 0
               do i = 1, size(network%crs%cross)
                  if (network%crs%cross(i)%crossindx == 0) exit
                  nCrs = nCrs + 1
               end do
               pCSs => network%CSDefinitions%CS
               jmax = 0
               do i = 1, size(pCSs)
                  if (pCSs(i)%levelscount == 0) exit
                  jmax = max(jmax, pCSs(i)%levelscount)
               end do
               ierr = nf90_def_dim(irstfile, trim(mesh1dname)//'_crs_maxdim', jmax, id_jmax)
               ierr = nf90_def_dim(irstfile, trim(mesh1dname)//'_ncrs', nCrs, id_ncrs)
               ierr = nf90_def_dim(irstfile, 'nStringlen', 100, id_strlendim)
               ierr = nf90_def_var(irstfile, trim(mesh1dname)//'_mor_crs_z', nf90_double, (/id_jmax, id_ncrs, id_timedim/), id_flowelemcrsz)
               ierr = nf90_put_att(irstfile, id_flowelemcrsz, 'long_name', 'cross-section points level')
               ierr = nf90_put_att(irstfile, id_flowelemcrsz, 'unit', 'm')
               ierr = nf90_def_var(irstfile, trim(mesh1dname)//'_mor_crs_name', nf90_char, (/id_strlendim, id_nCrs/), id_morCrsName)
               ierr = nf90_put_att(irstfile, id_morCrsName, 'long_name', 'name of cross-section')
            end if
         end if
         !
         select case (stmpar%morlyr%settings%iunderlyr)
         case (1)
            ierr = nf90_def_var(irstfile, 'bodsed', nf90_double, (/id_sedtotdim, id_flowelemdim, id_timedim/), id_bodsed)
            ierr = nf90_put_att(irstfile, id_bodsed, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_bodsed, 'long_name', 'Available sediment mass in the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_bodsed, 'units', 'kg m-2')

            ierr = nf90_def_var(irstfile, 'dpsed', nf90_double, (/id_flowelemdim, id_timedim/), id_dpsed)
            ierr = nf90_put_att(irstfile, id_dpsed, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_dpsed, 'long_name', 'Sediment thickness in the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_dpsed, 'units', 'm')
         case (2)
            ierr = nf90_def_var(irstfile, 'msed', nf90_double, (/id_sedtotdim, id_nlyrdim, id_flowelemdim, id_timedim/), id_msed)
            ierr = nf90_put_att(irstfile, id_msed, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_msed, 'long_name', 'Available sediment mass in a layer of the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_msed, 'units', 'kg m-2')

            ierr = nf90_def_var(irstfile, 'lyrfrac', nf90_double, (/id_sedtotdim, id_nlyrdim, id_flowelemdim, id_timedim/), id_lyrfrac)
            ierr = nf90_put_att(irstfile, id_lyrfrac, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_lyrfrac, 'long_name', 'Volume fraction in a layer of the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_lyrfrac, 'units', '-')

            ierr = nf90_def_var(irstfile, 'thlyr', nf90_double, (/id_nlyrdim, id_flowelemdim, id_timedim/), id_thlyr)
            ierr = nf90_put_att(irstfile, id_thlyr, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_thlyr, 'long_name', 'Thickness of a layer of the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_thlyr, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'preload', nf90_double, (/id_nlyrdim, id_flowelemdim, id_timedim/), id_preload)
            ierr = nf90_put_att(irstfile, id_preload, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_preload, 'long_name', 'Historical largest load on layer of the bed in flow cell center')
            ierr = nf90_put_att(irstfile, id_preload, 'units', 'kg')

            if (stmpar%morlyr%settings%iporosity > 0) then
               ierr = nf90_def_var(irstfile, 'porosity', nf90_double, (/id_nlyrdim, id_flowelemdim, id_timedim/), id_poros)
               ierr = nf90_put_att(irstfile, id_poros, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(irstfile, id_poros, 'long_name', 'Porosity of layer of the bed in flow cell center')
               ierr = nf90_put_att(irstfile, id_poros, 'units', '-')
            end if
         end select

         if (stmpar%morlyr%settings%morlyrnum%track_mass_shortage) then
            ierr = nf90_def_var(irstfile, 'sedshort', nf90_double, (/id_sedtotdim, id_flowelemdim, id_timedim/), id_sedshort)
            ierr = nf90_put_att(irstfile, id_sedshort, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_sedshort, 'long_name', 'Sediment shortage of transport layer in flow cell center')
            ierr = nf90_put_att(irstfile, id_sedshort, 'units', 'kg m-2')
         end if
         ! Fluff layers
         if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
            ierr = nf90_def_var(irstfile, 'mfluff', nf90_double, (/id_sedsusdim, id_flowelemdim, id_timedim/), id_mfluff)
            ierr = nf90_put_att(irstfile, id_mfluff, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_mfluff, 'long_name', 'Sediment mass in fluff layer')
            ierr = nf90_put_att(irstfile, id_mfluff, 'units', 'kg m-2 ')
         end if

      end if

      ! Old morphology
      ! Definition and attributes of flow data on centres: sediment concentation and erodable layer thickness
      if (jased > 0 .and. .not. stm_included) then
         ! Sediment concentration
         ierr = nf90_def_dim(irstfile, 'nFrac', mxgr, id_maxfracdim)
         if (jaceneqtr == 1) then
            ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
         else
            ierr = nf90_inq_dimid(irstfile, 'nNetNode', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
         end if
         ierr = nf90_def_var(irstfile, 'sed', nf90_double, (/id_maxfracdim, id_flowelemdim, id_timedim/), id_sed)
         ierr = nf90_put_att(irstfile, id_sed, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_sed, 'standard_name', 'sediment_concentration')
         ierr = nf90_put_att(irstfile, id_sed, 'long_name', 'sediment concentration at flow element centres')
         ierr = nf90_put_att(irstfile, id_sed, 'units', 'kg m-3')

         ! Erodable thickness
         ierr = nf90_def_var(irstfile, 'ero', nf90_double, (/id_maxfracdim, id_erolaydim, id_timedim/), id_ero)
         if (jaceneqtr == 1) then
            ierr = nf90_put_att(irstfile, id_ero, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(irstfile, id_ero, 'long_name', 'erodable layer thickness per size fraction in flow element centers')
         else
            ierr = nf90_put_att(irstfile, id_ero, 'coordinates', 'NetNode_x NetNode_y')
            ierr = nf90_put_att(irstfile, id_ero, 'long_name', 'erodable layer thickness per size fraction at flow element corners')
         end if
         !ierr = nf90_put_att(irstfile, id_ero,  'standard_name'    , 'Erodable layer thickness') ! Not CF
         ierr = nf90_put_att(irstfile, id_ero, 'units', 'm')
      end if

      ! Thatcher-Harleman boundary data
      if (allocated(threttim)) then
         if (jasal > 0) then
            if (max_threttim(ISALT) > 0d0) then
               ierr = nf90_def_var(irstfile, 'tsalbnd', nf90_double, (/id_bndsaldim, id_timedim/), id_tsalbnd)
               ierr = nf90_put_att(irstfile, id_tsalbnd, 'long_name', 'Thatcher-Harlem time interval for salinity')
               ierr = nf90_put_att(irstfile, id_tsalbnd, 'units', 's')
               ierr = nf90_def_var(irstfile, 'zsalbnd', nf90_double, (/id_bndsaldim, id_timedim/), id_zsalbnd)
               ierr = nf90_put_att(irstfile, id_zsalbnd, 'long_name', 'Thatcher-Harleman salinity')
               ierr = nf90_put_att(irstfile, id_zsalbnd, 'units', '1e-3')
            end if
         end if
         if (jatem > 0) then
            if (max_threttim(ITEMP) > 0d0) then
               ierr = nf90_def_var(irstfile, 'ttembnd', nf90_double, (/id_bndtemdim, id_timedim/), id_ttembnd)
               ierr = nf90_put_att(irstfile, id_ttembnd, 'long_name', 'Thatcher-Harleman time interval for temperature')
               ierr = nf90_put_att(irstfile, id_ttembnd, 'units', 's')
               ierr = nf90_def_var(irstfile, 'ztembnd', nf90_double, (/id_bndtemdim, id_timedim/), id_ztembnd)
               ierr = nf90_put_att(irstfile, id_ztembnd, 'long_name', 'Thatcher-Harleman temperature')
               ierr = nf90_put_att(irstfile, id_ztembnd, 'units', 'degrees celsius')
            end if
         end if
         if (jased > 0 .and. .not. stm_included) then
            if (max_threttim(ISED1) > 0d0) then
               ierr = nf90_def_var(irstfile, 'tsedbnd', nf90_double, (/id_bndseddim, id_timedim/), id_tsedbnd)
               ierr = nf90_put_att(irstfile, id_tsedbnd, 'long_name', 'Thatcher-Harleman time interval for sediment')
               ierr = nf90_put_att(irstfile, id_tsedbnd, 'units', 's')
               ierr = nf90_def_var(irstfile, 'zsedbnd', nf90_double, (/id_bndseddim, id_timedim/), id_zsedbnd)
               ierr = nf90_put_att(irstfile, id_zsedbnd, 'long_name', 'Thatcher-Harleman sediment concentration')
               ierr = nf90_put_att(irstfile, id_zsedbnd, 'units', 'kg m-3')
            end if
         end if
         if (numfracs > 0) then ! TO DO
            if (.not. allocated(id_tsedfracbnd)) then
               allocate (id_tsedfracbnd(numfracs))
            end if
            if (.not. allocated(id_zsedfracbnd)) then
               allocate (id_zsedfracbnd(numfracs))
            end if
            do i = 1, numfracs
               iconst = ifrac2const(i)
               if (iconst == 0) cycle
               if (max_threttim(iconst) > 0d0) then
                  write (numsedfracstr, numformat) i
                  ierr = nf90_def_var(irstfile, 'tsedfracbnd'//numsedfracstr, nf90_double, (/id_bndsedfracdim(i), id_timedim/), id_tsedfracbnd(i))
                  ierr = nf90_put_att(irstfile, id_tsedfracbnd(i), 'long_name', 'TH time interval '//numsedfracstr)
                  ierr = nf90_put_att(irstfile, id_tsedfracbnd(i), 'units', 's')
                  ierr = nf90_def_var(irstfile, 'zsedfracbnd'//numtrastr, nf90_double, (/id_bndsedfracdim(i), id_timedim/), id_zsedfracbnd(i))
                  ierr = nf90_put_att(irstfile, id_zsedfracbnd(i), 'long_name', 'TH sediment fraction '//numsedfracstr)
                  ierr = nf90_put_att(irstfile, id_zsedfracbnd(i), 'units', 'kg m-3')
               end if
            end do
         end if
         if (numtracers > 0) then
            if (.not. allocated(id_ttrabnd)) then
               allocate (id_ttrabnd(numtracers))
            end if
            if (.not. allocated(id_ztrabnd)) then
               allocate (id_ztrabnd(numtracers))
            end if
            do i = 1, numtracers
               iconst = itrac2const(i)
               if (max_threttim(iconst) > 0d0) then
                  write (numtrastr, numformat) i
                  ierr = nf90_def_var(irstfile, 'ttrabnd'//numtrastr, nf90_double, (/id_bndtradim(i), id_timedim/), id_ttrabnd(i))
                  ierr = nf90_put_att(irstfile, id_ttrabnd(i), 'long_name', 'Thatcher-Harleman time interval for tracer '//numtrastr)
                  ierr = nf90_put_att(irstfile, id_ttrabnd(i), 'units', 's')
                  ierr = nf90_def_var(irstfile, 'ztrabnd'//numtrastr, nf90_double, (/id_bndtradim(i), id_timedim/), id_ztrabnd(i))
                  ierr = nf90_put_att(irstfile, id_ztrabnd(i), 'long_name', 'Thatcher-Harleman concentration of tracer '//numtrastr)
                  if (const_units(iconst) /= ' ') then
                     tmpstr = const_units(iconst)
                  else
                     tmpstr = '-'
                  end if
                  ierr = nf90_put_att(irstfile, id_ztrabnd(i), 'units', trim(tmpstr))
               end if
            end do
         end if
      end if

      ! Gridmapping
      ierr = unc_add_gridmapping_att(irstfile, (/id_s1, id_taus, id_ucx, id_ucy, id_unorm, id_sa1, id_sed/), jsferic) ! add id_ucz?

      ! Flow cells
      ierr = nf90_def_var(irstfile, 'FlowElem_xzw', nf90_double, id_flowelemdim, id_flowelemxzw) ! For later cell-matching based on center of mass.
      ierr = nf90_def_var(irstfile, 'FlowElem_yzw', nf90_double, id_flowelemdim, id_flowelemyzw)
      ierr = unc_addcoordatts(irstfile, id_flowelemxzw, id_flowelemyzw, jsferic)
      ierr = nf90_put_att(irstfile, id_flowelemxzw, 'long_name', 'x-coordinate of flow element center of mass')
      ierr = nf90_put_att(irstfile, id_flowelemyzw, 'long_name', 'y-coordinate of flow element center of mass')

      ! Flow cells cell center
      ierr = nf90_def_var(irstfile, 'FlowElem_xcc', nf90_double, id_flowelemdim, id_flowelemxcc)
      ierr = nf90_def_var(irstfile, 'FlowElem_ycc', nf90_double, id_flowelemdim, id_flowelemycc)
      ierr = unc_addcoordatts(irstfile, id_flowelemxcc, id_flowelemycc, jsferic)
      ierr = nf90_put_att(irstfile, id_flowelemxcc, 'long_name', 'x-coordinate of flow element circumcenter')
      ierr = nf90_put_att(irstfile, id_flowelemycc, 'long_name', 'y-coordinate of flow element circumcenter')

      if (lnx > 0) then
         ierr = nf90_def_var(irstfile, 'FlowLink_xu', nf90_double, (/id_flowlinkdim/), id_flowlinkxu)
         ierr = nf90_def_var(irstfile, 'FlowLink_yu', nf90_double, (/id_flowlinkdim/), id_flowlinkyu)
         ierr = unc_addcoordatts(irstfile, id_flowlinkxu, id_flowlinkyu, jsferic)
         ierr = nf90_put_att(irstfile, id_flowlinkxu, 'long_name', 'x-coordinate of flow link center (velocity point)')
         ierr = nf90_put_att(irstfile, id_flowlinkyu, 'long_name', 'y-coordinate of flow link center (velocity point)')
      end if

      ! Secondary flow
      if (jasecflow > 0) then
         ierr = nf90_def_var(irstfile, 'spirint', nf90_double, (/id_flowelemdim, id_timedim/), id_spirint)
         ierr = nf90_put_att(irstfile, id_spirint, 'coordinates', 'FlowElem_xcc FlowElem_ycc')
         ierr = nf90_put_att(irstfile, id_spirint, 'long_name', 'Spiral flow intensity')
         ierr = nf90_put_att(irstfile, id_spirint, 'units', 'm/s')
      end if

      ! The following variables will be used to merge the rst files, therefore, they are written only in parallel run
      if (jampi == 1) then
         !   domain numbers and global node
         ierr = nf90_def_var(irstfile, 'FlowElemDomain', nf90_int, id_flowelemdim, id_flowelemdomain)
         ierr = nf90_put_att(irstfile, id_flowelemdomain, 'long_name', 'domain number of flow element')
         ierr = nf90_def_var(irstfile, 'FlowElemGlobalNr', nf90_int, id_flowelemdim, id_flowelemglobalnr)
         ierr = nf90_put_att(irstfile, id_flowelemglobalnr, 'long_name', 'global flow element numbering')

         if (lnx > 0) then
            ierr = nf90_def_var(irstfile, 'FlowLink', nf90_int, (/id_flowlinkptsdim, id_flowlinkdim/), id_flowlink)
            ierr = nf90_put_att(irstfile, id_flowlink, 'long_name', 'link/interface between two flow elements')
         end if
         if (nump1d2d > 0) then
            ierr = nf90_def_var(irstfile, 'NetElemNode', nf90_int, (/id_netelemmaxnodedim, id_netelemdim/), id_netelemnode)
            ierr = nf90_put_att(irstfile, id_netelemnode, 'long_name', 'mapping from net cell to its net nodes (counterclockwise)')
            ierr = nf90_put_att(irstfile, id_netelemnode, 'cf_role', 'face_node_connectivity')
            ierr = nf90_put_att(irstfile, id_netelemnode, 'start_index', 1)
            ierr = nf90_put_att(irstfile, id_netelemnode, '_FillValue', intmiss)
         end if
         if (numl > 0) then
            ! Netlinks
            ierr = nf90_def_var(irstfile, 'NetLink', nf90_int, (/id_netlinkptsdim, id_netlinkdim/), id_netlink)
            ierr = nf90_put_att(irstfile, id_netlink, 'standard_name', 'netlink')
            ierr = nf90_put_att(irstfile, id_netlink, 'long_name', 'link between two netnodes')
            ierr = nf90_put_att(irstfile, id_netlink, 'cf_role', 'edge_node_connectivity')
            ierr = nf90_put_att(irstfile, id_netlink, 'start_index', 1)
         end if
      end if
      if (jarstbnd > 0 .and. ndxbnd > 0) then
         ierr = nf90_def_var(irstfile, 'FlowElem_xbnd', nf90_double, (/id_bnddim/), id_flowelemxbnd)
         ierr = nf90_def_var(irstfile, 'FlowElem_ybnd', nf90_double, (/id_bnddim/), id_flowelemybnd)
         if (jsferic == 0) then
            ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'units', 'm')
            ierr = nf90_put_att(irstfile, id_flowelemybnd, 'units', 'm')
            ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'long_name', 'x-coordinate of boundary points')
            ierr = nf90_put_att(irstfile, id_flowelemybnd, 'long_name', 'y-coordinate of boundary points')
         else
            ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'units', 'degrees_east')
            ierr = nf90_put_att(irstfile, id_flowelemybnd, 'units', 'degrees_north')
            ierr = nf90_put_att(irstfile, id_flowelemxbnd, 'long_name', 'longitude for boundary points')
            ierr = nf90_put_att(irstfile, id_flowelemybnd, 'long_name', 'latitude for boundary points')
         end if

         ierr = nf90_def_var(irstfile, 's0_bnd', nf90_double, (/id_bnddim, id_timedim/), id_s0bnd)
         ierr = nf90_put_att(irstfile, id_s0bnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_s0bnd, 'long_name', 'water level at boundaries of previous timestep')
         ierr = nf90_put_att(irstfile, id_s0bnd, 'units', 'm')

         ierr = nf90_def_var(irstfile, 's1_bnd', nf90_double, (/id_bnddim, id_timedim/), id_s1bnd)
         ierr = nf90_put_att(irstfile, id_s1bnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_s1bnd, 'long_name', 'water level at boundaries')
         ierr = nf90_put_att(irstfile, id_s1bnd, 'units', 'm')

         ierr = nf90_def_var(irstfile, 'bl_bnd', nf90_double, (/id_bnddim, id_timedim/), id_blbnd)
         ierr = nf90_put_att(irstfile, id_blbnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_blbnd, 'long_name', 'bed level at boundaries')
         ierr = nf90_put_att(irstfile, id_blbnd, 'units', 'm')

         ierr = nf90_def_var(irstfile, 'sqi_bnd', nf90_double, (/id_bnddim, id_timedim/), id_sqibnd)
         ierr = nf90_put_att(irstfile, id_sqibnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_sqibnd, 'long_name', 'cell center incoming flux at boundaries')
         ierr = nf90_put_att(irstfile, id_sqibnd, 'units', 'm3 s-1')

         ierr = nf90_def_var(irstfile, 'squ_bnd', nf90_double, (/id_bnddim, id_timedim/), id_squbnd)
         ierr = nf90_put_att(irstfile, id_squbnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_squbnd, 'long_name', 'cell center outcoming flux at boundaries')
         ierr = nf90_put_att(irstfile, id_squbnd, 'units', 'm3 s-1')

         ierr = nf90_def_var(irstfile, 'ucxq_bnd', nf90_double, (/id_bnddim, id_timedim/), id_ucxqbnd)
         ierr = nf90_put_att(irstfile, id_ucxqbnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_ucxqbnd, 'long_name', 'q-based cell center velocity, x-dir at boundaries')
         ierr = nf90_put_att(irstfile, id_ucxqbnd, 'units', 'm s-1')

         ierr = nf90_def_var(irstfile, 'ucyq_bnd', nf90_double, (/id_bnddim, id_timedim/), id_ucyqbnd)
         ierr = nf90_put_att(irstfile, id_ucyqbnd, 'coordinates', 'FlowElem_xbnd FlowElem_ybnd')
         ierr = nf90_put_att(irstfile, id_ucyqbnd, 'long_name', 'q-based cell center velocity, y-dir at boundaries')
         ierr = nf90_put_att(irstfile, id_ucyqbnd, 'units', 'm s-1')
      end if

      ! Write structure info.
      if (network%loaded) then
         nfuru = 3
         if (network%sts%numCulverts > 0) then ! write culvert info.
            ierr = nf90_def_dim(irstfile, 'nCulvert', network%sts%numculverts, id_culvertdim)
            ierr = nf90_def_var(irstfile, 'culvert_valve_opening_height', nf90_double, (/id_culvertdim, id_timedim/), id_culvert_openh)
            ierr = nf90_put_att(irstfile, id_culvert_openh, 'long_name', 'Valve opening height of culvert')
            ierr = nf90_put_att(irstfile, id_culvert_openh, 'units', 'm')
         end if

         if (nlongculverts > 0) then ! write longculvert info.
            ierr = nf90_def_dim(irstfile, 'nLongCulvert', nlongculverts, id_longculvertdim)
            ierr = nf90_def_var(irstfile, 'longculvert_valve_relative_opening', nf90_double, (/id_longculvertdim, id_timedim/), id_longculvert_valveopen)
            ierr = nf90_put_att(irstfile, id_longculvert_valveopen, 'long_name', 'Relative valve opening of long culvert')
            ierr = nf90_put_att(irstfile, id_longculvert_valveopen, 'units', '1')
         end if

         nlen = network%sts%numGeneralStructures
         if (nlen > 0) then ! write info. of general structure
            ! define dimensions
            ierr = nf90_def_dim(irstfile, 'nGeneral_structures', nlen, id_genstrudim)
            ierr = nf90_def_dim(irstfile, 'nGeneral_structure_furu', nfuru, id_genstru_furudim)
            maxNumLinks = get_max_numLinks(ST_GENERAL_ST, nlen)
            ierr = nf90_def_dim(irstfile, 'nGeneral_structure_max_numlinks', maxNumLinks, id_genstru_linkdim)

            ! define variables
            ! The following variables are crucial for computation
            ierr = nf90_def_var(irstfile, 'general_structure_crest_level', nf90_double, (/id_genstrudim, id_timedim/), id_genstru_crestl)
            ierr = nf90_put_att(irstfile, id_genstru_crestl, 'long_name', 'Crest level of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_crestl, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'general_structure_crest_width', nf90_double, (/id_genstrudim, id_timedim/), id_genstru_crestw)
            ierr = nf90_put_att(irstfile, id_genstru_crestw, 'long_name', 'Crest width of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_crestw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'general_structure_gate_lower_edge_level', nf90_double, (/id_genstrudim, id_timedim/), id_genstru_edgel)
            ierr = nf90_put_att(irstfile, id_genstru_edgel, 'long_name', 'Gate lower edge level of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_edgel, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'general_structure_gate_opening_width', nf90_double, (/id_genstrudim, id_timedim/), id_genstru_openw)
            ierr = nf90_put_att(irstfile, id_genstru_openw, 'long_name', 'Gate opening width of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_openw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'general_structure_flow_area', nf90_double, (/id_genstru_linkdim, id_genstrudim, id_timedim/), id_genstru_area)
            ierr = nf90_put_att(irstfile, id_genstru_area, 'long_name', 'Flow area of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_area, 'units', 'm2')

            ierr = nf90_def_var(irstfile, 'general_structure_link_width_closed_by_gate', nf90_double, (/id_genstru_linkdim, id_genstrudim, id_timedim/), id_genstru_linkw)
            ierr = nf90_put_att(irstfile, id_genstru_linkw, 'long_name', 'Part of the link width that is closed by the gate')
            ierr = nf90_put_att(irstfile, id_genstru_linkw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'general_structure_fu', nf90_double, dimids=(/id_genstru_furudim, id_genstru_linkdim, id_genstrudim, id_timedim/), varid=id_genstru_fu)
            ierr = nf90_put_att(irstfile, id_genstru_fu, 'long_name', 'partial computational value for fu (under/over/between gate, respectively) of general structure')

            ierr = nf90_def_var(irstfile, 'general_structure_ru', nf90_double, dimids=(/id_genstru_furudim, id_genstru_linkdim, id_genstrudim, id_timedim/), varid=id_genstru_ru)
            ierr = nf90_put_att(irstfile, id_genstru_ru, 'long_name', 'partial computational value for ru (under/over/between gate, respectively) of general structure')

            ierr = nf90_def_var(irstfile, 'general_structure_au', nf90_double, dimids=(/id_genstru_furudim, id_genstru_linkdim, id_genstrudim, id_timedim/), varid=id_genstru_au)
            ierr = nf90_put_att(irstfile, id_genstru_au, 'long_name', 'partial computational value for au (under/over/between gate, respectively) of general structure')

            ! The following variables are only for the output at the initial time in history file
            ierr = nf90_def_var(irstfile, 'general_structure_state', nf90_int, dimids=(/id_genstru_furudim, id_genstru_linkdim, id_genstrudim, id_timedim/), varid=id_genstru_state)
            ierr = nf90_put_att(irstfile, id_genstru_state, 'long_name', 'Flow state at general structure')

            ierr = nf90_def_var(irstfile, 'general_structure_water_level_on_crest', nf90_double, dimids=(/id_genstru_linkdim, id_genstrudim, id_timedim/), varid=id_genstru_sOncrest)
            ierr = nf90_put_att(irstfile, id_genstru_sOnCrest, 'long_name', 'water level on crest of general structure')
            ierr = nf90_put_att(irstfile, id_genstru_sOnCrest, 'units', 'm')
         end if

         nlen = network%sts%numWeirs
         if (nlen > 0) then ! write info. of weirs
            ! define dimensions
            ierr = nf90_def_dim(irstfile, 'nWeirs', nlen, id_weirgendim)
            ierr = nf90_def_dim(irstfile, 'nWeir_furu', nfuru, id_weirgen_furudim)
            maxNumLinks = get_max_numLinks(ST_WEIR, nlen)
            ierr = nf90_def_dim(irstfile, 'nWeir_max_numlinks', maxNumLinks, id_weirgen_linkdim)

            ! define variables
            ! The following variables are crucial for computation
            ierr = nf90_def_var(irstfile, 'weirgen_crest_level', nf90_double, (/id_weirgendim, id_timedim/), id_weirgen_crestl)
            ierr = nf90_put_att(irstfile, id_weirgen_crestl, 'long_name', 'Crest level of weir')
            ierr = nf90_put_att(irstfile, id_weirgen_crestl, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'weirgen_crest_width', nf90_double, (/id_weirgendim, id_timedim/), id_weirgen_crestw)
            ierr = nf90_put_att(irstfile, id_weirgen_crestw, 'long_name', 'Crest width of weir')
            ierr = nf90_put_att(irstfile, id_weirgen_crestw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'weirgen_flow_area', nf90_double, (/id_weirgen_linkdim, id_weirgendim, id_timedim/), id_weirgen_area)
            ierr = nf90_put_att(irstfile, id_weirgen_area, 'long_name', 'Flow area of weir')
            ierr = nf90_put_att(irstfile, id_weirgen_area, 'units', 'm2')

            ierr = nf90_def_var(irstfile, 'weirgen_link_width_closed_by_gate', nf90_double, (/id_weirgen_linkdim, id_weirgendim, id_timedim/), id_weirgen_linkw)
            ierr = nf90_put_att(irstfile, id_weirgen_linkw, 'long_name', 'Part of the link width that is closed by the gate')
            ierr = nf90_put_att(irstfile, id_weirgen_linkw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'weirgen_fu', nf90_double, dimids=(/id_weirgen_furudim, id_weirgen_linkdim, id_weirgendim, id_timedim/), varid=id_weirgen_fu)
            ierr = nf90_put_att(irstfile, id_weirgen_fu, 'long_name', 'partial computational value for fu (under/over/between gate, respectively) of weir')

            ierr = nf90_def_var(irstfile, 'weirgen_ru', nf90_double, dimids=(/id_weirgen_furudim, id_weirgen_linkdim, id_weirgendim, id_timedim/), varid=id_weirgen_ru)
            ierr = nf90_put_att(irstfile, id_weirgen_ru, 'long_name', 'partial computational value for ru (under/over/between gate, respectively) of weir')

            ierr = nf90_def_var(irstfile, 'weirgen_au', nf90_double, dimids=(/id_weirgen_furudim, id_weirgen_linkdim, id_weirgendim, id_timedim/), varid=id_weirgen_au)
            ierr = nf90_put_att(irstfile, id_weirgen_au, 'long_name', 'partial computational value for au (under/over/between gate, respectively) of weir')

            ! The following variables are only for the output at the initial time in history file
            ierr = nf90_def_var(irstfile, 'weirgen_state', nf90_int, dimids=(/id_weirgen_furudim, id_weirgen_linkdim, id_weirgendim, id_timedim/), varid=id_weirgen_state)
            ierr = nf90_put_att(irstfile, id_weirgen_state, 'long_name', 'Flow state at weir')

            ierr = nf90_def_var(irstfile, 'weirgen_water_level_on_crest', nf90_double, dimids=(/id_weirgen_linkdim, id_weirgendim, id_timedim/), varid=id_weirgen_sOncrest)
            ierr = nf90_put_att(irstfile, id_weirgen_sOncrest, 'long_name', 'water level on crest of weir')
            ierr = nf90_put_att(irstfile, id_weirgen_sOncrest, 'units', 'm')
         end if

         nlen = network%sts%numOrifices
         if (nlen > 0) then ! write info. of orifice
            ! define dimensions
            ierr = nf90_def_dim(irstfile, 'nOrifices', nlen, id_orifgendim)
            ierr = nf90_def_dim(irstfile, 'nOrifice_furu', nfuru, id_orifgen_furudim)
            maxNumLinks = get_max_numLinks(ST_ORIFICE, nlen)
            ierr = nf90_def_dim(irstfile, 'nOrifice_max_numlinks', maxNumLinks, id_orifgen_linkdim)

            ! define variables
            ! The following variables are crucial for computation
            ierr = nf90_def_var(irstfile, 'orifice_crest_level', nf90_double, (/id_orifgendim, id_timedim/), id_orifgen_crestl)
            ierr = nf90_put_att(irstfile, id_orifgen_crestl, 'long_name', 'Crest level of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_crestl, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'orifice_crest_width', nf90_double, (/id_orifgendim, id_timedim/), id_orifgen_crestw)
            ierr = nf90_put_att(irstfile, id_orifgen_crestw, 'long_name', 'Crest width of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_crestw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'orifice_gate_lower_edge_level', nf90_double, (/id_orifgendim, id_timedim/), id_orifgen_edgel)
            ierr = nf90_put_att(irstfile, id_orifgen_edgel, 'long_name', 'Gate lower edge level of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_edgel, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'orifice_gate_opening_width', nf90_double, (/id_orifgendim, id_timedim/), id_orifgen_openw)
            ierr = nf90_put_att(irstfile, id_orifgen_openw, 'long_name', 'Gate opening width of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_openw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'orifice_flow_area', nf90_double, (/id_orifgen_linkdim, id_orifgendim, id_timedim/), id_orifgen_area)
            ierr = nf90_put_att(irstfile, id_orifgen_area, 'long_name', 'Flow area of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_area, 'units', 'm2')

            ierr = nf90_def_var(irstfile, 'orifice_link_width_closed_by_gate', nf90_double, (/id_orifgen_linkdim, id_orifgendim, id_timedim/), id_orifgen_linkw)
            ierr = nf90_put_att(irstfile, id_orifgen_linkw, 'long_name', 'Part of the link width that is closed by the gate')
            ierr = nf90_put_att(irstfile, id_orifgen_linkw, 'units', 'm')

            ierr = nf90_def_var(irstfile, 'orifice_fu', nf90_double, dimids=(/id_orifgen_furudim, id_orifgen_linkdim, id_orifgendim, id_timedim/), varid=id_orifgen_fu)
            ierr = nf90_put_att(irstfile, id_orifgen_fu, 'long_name', 'partial computational value for fu (under/over/between gate, respectively) of orifice')

            ierr = nf90_def_var(irstfile, 'orifice_ru', nf90_double, dimids=(/id_orifgen_furudim, id_orifgen_linkdim, id_orifgendim, id_timedim/), varid=id_orifgen_ru)
            ierr = nf90_put_att(irstfile, id_orifgen_ru, 'long_name', 'partial computational value for ru (under/over/between gate, respectively) of orifice')

            ierr = nf90_def_var(irstfile, 'orifice_au', nf90_double, dimids=(/id_orifgen_furudim, id_orifgen_linkdim, id_orifgendim, id_timedim/), varid=id_orifgen_au)
            ierr = nf90_put_att(irstfile, id_orifgen_au, 'long_name', 'partial computational value for au (under/over/between gate, respectively) of orifice')

            ! The following variables are only for the output at the initial time in history file
            ierr = nf90_def_var(irstfile, 'orifice_state', nf90_int, dimids=(/id_orifgen_furudim, id_orifgen_linkdim, id_orifgendim, id_timedim/), varid=id_orifgen_state)
            ierr = nf90_put_att(irstfile, id_orifgen_state, 'long_name', 'Flow state at orifice')

            ierr = nf90_def_var(irstfile, 'orifice_water_level_on_crest', nf90_double, dimids=(/id_orifgen_linkdim, id_orifgendim, id_timedim/), varid=id_orifgen_sOncrest)
            ierr = nf90_put_att(irstfile, id_orifgen_sOnCrest, 'long_name', 'water level on crest of orifice')
            ierr = nf90_put_att(irstfile, id_orifgen_sOnCrest, 'units', 'm')
         end if

         nlen = network%sts%numPumps
         if (nlen > 0) then
            ! define dimensions
            ierr = nf90_def_dim(irstfile, 'nPumps', nlen, id_pumpdim)
            maxNumStages = 0
            do i = 1, nlen
               istru = network%sts%pumpIndices(i)
               maxNumStages = max(maxNumStages, network%sts%struct(istru)%pump%nrstages)
            end do
            ierr = nf90_def_dim(irstfile, 'nPump_max_numstages', maxNumStages, id_pump_stagedim)

            ! define variables
            ierr = nf90_def_var(irstfile, 'pump_capacity', nf90_double, (/id_pumpdim, id_timedim/), id_pump_cap)
            ierr = nf90_put_att(irstfile, id_pump_cap, 'long_name', 'Capacity of pump')
            ierr = nf90_put_att(irstfile, id_pump_cap, 'units', 'm3 s-1')

            ierr = nf90_def_var(irstfile, 'pump_suction_side_trigger', nf90_int, dimids=(/id_pump_stagedim, id_pumpdim, id_timedim/), varid=id_pump_ssTrigger)
            ierr = nf90_put_att(irstfile, id_pump_ssTrigger, 'long_name', 'Sunction trigger of pump. 1 means true and 0 means false.')

            ierr = nf90_def_var(irstfile, 'pump_delivery_side_trigger', nf90_int, dimids=(/id_pump_stagedim, id_pumpdim, id_timedim/), varid=id_pump_dsTrigger)
            ierr = nf90_put_att(irstfile, id_pump_dsTrigger, 'long_name', 'Delivery trigger of pump. 1 means true and 0 means false.')
         end if
      end if

      ! Write network%adm%hysteresis_for_summerdike for all 1d links
      if (network%loaded) then
         if (network%numl > 0) then
            ierr = nf90_def_dim(irstfile, 'n1DFlowLink', network%numl, id_1dflowlink_dim)
            ierr = nf90_def_var(irstfile, 'hysteresis_for_summerdike', nf90_short, (/id_1dflowlink_dim/), id_hysteresis)
            ierr = nf90_put_att(irstfile, id_hysteresis, 'long_name', 'Hysteresis information for summer dike. 3 is true-true, 2 is false-true, 1 is true-false, 0 is false-false.')
            ierr = nf90_put_att(irstfile, id_hysteresis, 'units', '')
         end if
      end if

      ierr = nf90_enddef(irstfile)

      ierr = nf90_put_var(irstfile, id_flowelemxcc, xz(1:ndxi))
      ierr = nf90_put_var(irstfile, id_flowelemycc, yz(1:ndxi))

      ! 1D profile names
      if (ndx1d > 0 .and. stm_included) then
         if (stmpar%morpar%bedupd) then
            do i = 1, nCrs
               ierr = nf90_put_var(irstfile, id_morCrsName, trim(network%crs%cross(i)%CSID), (/1, i/), (/len(trim(network%crs%cross(i)%CSID)), 1/)) ! only write once
            end do
         end if
      end if

      ! Inquire var-id's
      ! NOTE: alle inq_varids below are not needed, since they have just been def_var'd above in this subroutine. Cleanup later together with rst/map cleanup. [AvD]
      if (kmx > 0) then
         ierr = nf90_inq_dimid(irstfile, 'laydim', id_laydim)
         ierr = nf90_inq_dimid(irstfile, 'wdim', id_wdim)
      end if
      ierr = nf90_inq_varid(irstfile, 'timestep', id_timestep)
      ierr = nf90_inq_varid(irstfile, 's1', id_s1)
      ierr = nf90_inq_varid(irstfile, 's0', id_s0)
      ierr = nf90_inq_varid(irstfile, 'unorm', id_unorm)
      ierr = nf90_inq_varid(irstfile, 'u0', id_u0)
      ierr = nf90_inq_varid(irstfile, 'q1', id_q1)
      ierr = nf90_inq_varid(irstfile, 'ucx', id_ucx)
      ierr = nf90_inq_varid(irstfile, 'ucy', id_ucy)
      ierr = nf90_inq_varid(irstfile, 'taus', id_taus)
      ierr = nf90_inq_varid(irstfile, 'czs', id_czs)
      ierr = nf90_inq_varid(irstfile, 'qa', id_qa)
      ierr = nf90_inq_varid(irstfile, 'squ', id_squ)
      ierr = nf90_inq_varid(irstfile, 'sqi', id_sqi)

      if (ncdamsg > 0 .or. ifixedweirscheme > 0) then
         ierr = nf90_inq_varid(irstfile, 'weirdte', id_weirdte)
      end if

      if (kmx > 0) then
         ierr = nf90_inq_varid(irstfile, 'ucz', id_ucz)
         ierr = nf90_inq_varid(irstfile, 'ww1', id_ww1)
      end if
      if (jasal > 0) then
         ierr = nf90_inq_varid(irstfile, 'sa1', id_sa1)
      end if

      ! JRE
      if (jawave == 4) then
         ierr = nf90_inq_varid(irstfile, 'E', id_E)
         ierr = nf90_inq_varid(irstfile, 'thetamean', id_thetamean)
         ierr = nf90_inq_varid(irstfile, 'sigmwav', id_sigmwav)
      end if

      !
      if (jatem > 0) then
         ierr = nf90_inq_varid(irstfile, 'tem1', id_tem1)
      end if
      !
      if (jased > 0 .and. .not. stm_included) then
         ierr = nf90_inq_dimid(irstfile, 'nFrac', id_maxfracdim)
         if (jaceneqtr == 1) then
            ierr = nf90_inq_dimid(irstfile, 'nFlowElem', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
         else
            ierr = nf90_inq_dimid(irstfile, 'nNetNode', id_erolaydim) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
         end if
         ierr = nf90_inq_varid(irstfile, 'sed', id_sed)
         ierr = nf90_inq_varid(irstfile, 'ero', id_ero)
      end if
      !
      if (network%loaded) then
         if (network%sts%numCulverts > 0) then
            ierr = nf90_inq_varid(irstfile, 'culvert_valve_opening_height', id_culvert_openh)
         end if
         if (network%sts%numGeneralStructures > 0) then
            ierr = nf90_inq_varid(irstfile, 'general_structure_crest_level', id_genstru_crestl)
            ierr = nf90_inq_varid(irstfile, 'general_structure_crest_width', id_genstru_crestw)
            ierr = nf90_inq_varid(irstfile, 'general_structure_gate_lower_edge_level', id_genstru_edgel)
            ierr = nf90_inq_varid(irstfile, 'general_structure_gate_opening_width', id_genstru_openw)
            ierr = nf90_inq_varid(irstfile, 'general_structure_flow_area', id_genstru_area)
            ierr = nf90_inq_varid(irstfile, 'general_structure_link_width_closed_by_gate', id_genstru_linkw)
            ierr = nf90_inq_varid(irstfile, 'general_structure_fu', id_genstru_fu)
            ierr = nf90_inq_varid(irstfile, 'general_structure_ru', id_genstru_ru)
            ierr = nf90_inq_varid(irstfile, 'general_structure_au', id_genstru_au)
            ierr = nf90_inq_varid(irstfile, 'general_structure_state', id_genstru_state)
            ierr = nf90_inq_varid(irstfile, 'general_structure_water_level_on_crest', id_genstru_sOnCrest)
         end if
         if (network%sts%numWeirs > 0) then
            ierr = nf90_inq_varid(irstfile, 'weirgen_crest_level', id_weirgen_crestl)
            ierr = nf90_inq_varid(irstfile, 'weirgen_crest_width', id_weirgen_crestw)
            ierr = nf90_inq_varid(irstfile, 'weirgen_flow_area', id_weirgen_area)
            ierr = nf90_inq_varid(irstfile, 'weirgen_link_width_closed_by_gate', id_weirgen_linkw)
            ierr = nf90_inq_varid(irstfile, 'weirgen_fu', id_weirgen_fu)
            ierr = nf90_inq_varid(irstfile, 'weirgen_ru', id_weirgen_ru)
            ierr = nf90_inq_varid(irstfile, 'weirgen_au', id_weirgen_au)
            ierr = nf90_inq_varid(irstfile, 'weirgen_state', id_weirgen_state)
            ierr = nf90_inq_varid(irstfile, 'weirgen_water_level_on_crest', id_weirgen_sOnCrest)
         end if
         if (network%sts%numOrifices > 0) then
            ierr = nf90_inq_varid(irstfile, 'orifice_crest_level', id_orifgen_crestl)
            ierr = nf90_inq_varid(irstfile, 'orifice_crest_width', id_orifgen_crestw)
            ierr = nf90_inq_varid(irstfile, 'orifice_gate_lower_edge_level', id_orifgen_edgel)
            ierr = nf90_inq_varid(irstfile, 'orifice_gate_opening_width', id_orifgen_openw)
            ierr = nf90_inq_varid(irstfile, 'orifice_flow_area', id_orifgen_area)
            ierr = nf90_inq_varid(irstfile, 'orifice_link_width_closed_by_gate', id_orifgen_linkw)
            ierr = nf90_inq_varid(irstfile, 'orifice_fu', id_orifgen_fu)
            ierr = nf90_inq_varid(irstfile, 'orifice_ru', id_orifgen_ru)
            ierr = nf90_inq_varid(irstfile, 'orifice_au', id_orifgen_au)
            ierr = nf90_inq_varid(irstfile, 'orifice_state', id_orifgen_state)
            ierr = nf90_inq_varid(irstfile, 'orifice_water_level_on_crest', id_orifgen_sOnCrest)
         end if
         if (network%sts%numPumps > 0) then
            ierr = nf90_inq_varid(irstfile, 'pump_capacity', id_pump_cap)
            ierr = nf90_inq_varid(irstfile, 'pump_suction_side_trigger', id_pump_ssTrigger)
            ierr = nf90_inq_varid(irstfile, 'pump_delivery_side_trigger', id_pump_dsTrigger)
         end if
      end if

      ! -- Start data writing (flow data) ------------------------
      itim = 1

      ! Write the data: time
      ierr = nf90_put_var(irstfile, id_time, tim, (/itim/))
      ierr = nf90_put_var(irstfile, id_timestep, dts, (/itim/))

      ! Write the data: water level (new and old)
      ierr = nf90_put_var(irstfile, id_s1, s1, (/1, itim/), (/ndxi, 1/))
      ierr = nf90_put_var(irstfile, id_s0, s0, (/1, itim/), (/ndxi, 1/))

      ! Write the data: bed level
      ierr = nf90_put_var(irstfile, id_bl, bl, (/1, itim/), (/ndxi, 1/))

      ! Write the data: tau current
      if (jawave == 0) then ! Else, get taus from subroutine tauwave (taus = taucur + tauwave). Bas; Mind for jawind!
         call gettaus(1, 1)
      else if (jamap_chezy_links > 0) then
         call gettaus(2, 1)
      end if
      !
      if (jawave > 0 .and. .not. flowWithoutWaves) then
         call gettauswave(jawaveswartdelwaq)
      end if
      !
      if (jamaptaucurrent > 0) then
         ierr = nf90_put_var(irstfile, id_taus, taus, (/1, itim/), (/ndxi, 1/))
      end if
      if (jamap_chezy_elements > 0) then
         ierr = nf90_put_var(irstfile, id_czs, czs, (/1, itim/), (/ndxi, 1/))
      end if

      if (jasecflow > 0) then
         ierr = nf90_put_var(irstfile, id_spirint, spirint, (/1, itim/), (/ndxi, 1/))
      end if

      ! Write the data: velocities (components and magnitudes)
      if (kmx > 0) then
!      3D
         call reconstructucz(0)
         call unc_append_3dflowgeom_put(irstfile, 1, itim)
         !do kk=1,Ndxi
         !   call getkbotktop(kk,kb,kt)
         !   ierr = nf90_put_var(irstfile, id_ucx  , ucx(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
         !   ierr = nf90_put_var(irstfile, id_ucy  , ucy(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
         !   ierr = nf90_put_var(irstfile, id_ucz  , ucz(kb:kt),  start=(/ 1, kk, itim /), count=(/ kt-kb+1, 1, 1 /))
         !   ierr = nf90_put_var(irstfile, id_ww1  , ww1(kb-1:kt),start=(/ 1, kk, itim /), count=(/ kt-kb+2, 1, 1 /))
         !enddo
         !do LL=1,lnx
         !   call getLbotLtopmax(LL,Lb,Lt)
         !   ierr = nf90_put_var(irstfile, id_unorm, u1(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
         !   ierr = nf90_put_var(irstfile, id_u0   , u0(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
         !   ierr = nf90_put_var(irstfile, id_q1   , q1(Lb:Lt),  start=(/ 1, LL, itim /), count=(/ Lt-Lb+1, 1, 1 /))
         !enddo

         work1 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = ucx(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ucx, work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))

         work1 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = ucy(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ucy, work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))

         work1 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = ucz(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ucz, work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))

         work1 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = ucxq(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ucxq, work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))

         work1 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1(k - kb + nlayb, kk) = ucyq(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ucyq, work1(1:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx, ndxi, 1/))

         work0 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb - 1, kt
               work0(k - kb + nlayb, kk) = ww1(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_ww1, work0(0:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx + 1, ndxi, 1/))

         work1 = dmiss
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb, Ltx
               work1(L - Lb + nlaybL, LL) = u1(L)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_unorm, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))

         work1 = dmiss
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb, Ltx
               work1(L - Lb + nlaybL, LL) = u0(L)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_u0, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))

         work1 = dmiss
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb, Ltx
               work1(L - Lb + nlaybL, LL) = q1(L)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_q1, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))

         work1 = dmiss
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb, Ltx
               work1(L - Lb + nlaybL, LL) = hu(L)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_hu, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))

         if (Corioadamsbashfordfac > 0d0) then
            work1 = dmiss
            do LL = 1, lnx
               call getLbotLtopmax(LL, Lb, Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb, Ltx
                  work1(L - Lb + nlaybL, LL) = fvcoro(L)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_fvcoro, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
         end if

         ! write averaged u1
         ierr = nf90_put_var(irstfile, id_unorma, u1(1:lnx), start=(/1, itim/), count=(/lnx, 1/))

         if (iturbulencemodel >= 3) then
            ! write vertical eddy viscosity vicwwu
            work0 = dmiss
            do LL = 1, lnx
               call getLbotLtopmax(LL, Lb, Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb - 1, Ltx
                  work0(L - Lb + nlaybL, LL) = vicwwu(L)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_vicwwu, work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))

            ! write tureps1
            work0 = dmiss
            do LL = 1, lnx
               call getLbotLtopmax(LL, Lb, Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb - 1, Ltx
                  work0(L - Lb + nlaybL, LL) = tureps1(L)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_tureps1, work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))

            ! write turkin1
            work0 = dmiss
            do LL = 1, lnx
               call getLbotLtopmax(LL, Lb, Ltx)
               call getlayerindicesLmax(LL, nlaybL, nrlayLx)
               do L = Lb - 1, Ltx
                  work0(L - Lb + nlaybL, LL) = turkin1(L)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_turkin1, work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))
         end if

         ! qw
         work0 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb - 1, kt
               work0(k - kb + nlayb, kk) = qw(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_qw, work0(0:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx + 1, ndxi, 1/))

         ! qa
         work1 = dmiss
         do LL = 1, lnx
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            do L = Lb, Ltx
               work1(L - Lb + nlaybL, LL) = qa(L)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_qa, work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))

         ! sqi
         work0 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb - 1, kt
               work0(k - kb + nlayb, kk) = sqi(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_sqi, work0(0:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx + 1, ndxi, 1/))

         ! squ
         work0 = dmiss
         do kk = 1, ndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb - 1, kt
               work0(k - kb + nlayb, kk) = squ(k)
            end do
         end do
         ierr = nf90_put_var(irstfile, id_squ, work0(0:kmx, 1:ndxi), start=(/1, 1, itim/), count=(/kmx + 1, ndxi, 1/))

      else
         ierr = nf90_put_var(irstfile, id_ucx, ucx, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_ucy, ucy, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_ucxq, ucxq, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_ucyq, ucyq, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_unorm, u1, (/1, itim/), (/lnx, 1/))
         ierr = nf90_put_var(irstfile, id_u0, u0, (/1, itim/), (/lnx, 1/))
         ierr = nf90_put_var(irstfile, id_q1, q1, (/1, itim/), (/lnx, 1/))
         ierr = nf90_put_var(irstfile, id_qa, qa, (/1, itim/), (/lnx, 1/))
         ierr = nf90_put_var(irstfile, id_squ, squ, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_sqi, sqi, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_hu, hu, (/1, itim/), (/lnx, 1/))
         if (Corioadamsbashfordfac > 0d0) then
            ierr = nf90_put_var(irstfile, id_fvcoro, fvcoro, (/1, itim/), (/lnx, 1/))
         end if
      end if

      if (ncdamsg > 0 .or. ifixedweirscheme > 0) then
         ierr = nf90_put_var(irstfile, id_weirdte, map_fixed_weir_energy_loss, (/1, itim/), (/lnx, 1/))
      end if

      if (jasal > 0) then ! Write the data: salinity
         if (kmx > 0) then
            !do kk=1,Ndxi
            !   call getkbotktop(kk,kb,kt)
            !   ierr = nf90_put_var(irstfile, id_sa1, sa1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
            !enddo
            work1 = dmiss
            do kk = 1, ndxi
               call getkbotktop(kk, kb, kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb, kt
                  work1(k - kb + nlayb, kk) = constituents(isalt, k)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_sa1, work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
         else
            do k = 1, ndxi
               sa1(k) = constituents(isalt, k)
            end do
            ierr = nf90_put_var(irstfile, id_sa1, sa1, (/1, itim/), (/ndxi, 1/))
         end if
      end if

      if (jatem > 0) then ! Write the data: temperature
         if (kmx > 0) then
            !do kk=1,Ndxi
            !   call getkbotktop(kk,kb,kt)
            !   ierr = nf90_put_var(irstfile, id_tem1, tem1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
            !enddo
            work1 = dmiss
            do kk = 1, ndxi
               call getkbotktop(kk, kb, kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb, kt
                  work1(k - kb + nlayb, kk) = constituents(itemp, k)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_tem1, work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
         else
            do k = 1, ndxi
               tem1(k) = constituents(itemp, k)
            end do
            ierr = nf90_put_var(irstfile, id_tem1, tem1, (/1, itim/), (/ndxi, 1/))
         end if
      end if

      if (ITRA1 > 0) then
         allocate (dum(ndxi))
         do j = ITRA1, ITRAN
            if (kmx > 0) then
!           3D
               work1 = dmiss
               do kk = 1, ndxi
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = constituents(j, k)
                  end do
               end do
               ierr = nf90_put_var(irstfile, id_tr1(j - ITRA1 + 1), work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
               !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
            else
               do kk = 1, ndxi
                  dum(kk) = constituents(j, kk)
               end do
               ierr = nf90_put_var(irstfile, id_tr1(j - ITRA1 + 1), dum, (/1, itim/), (/ndxi, 1/))
            end if
         end do
         if (allocated(dum)) then
            deallocate (dum)
         end if
      end if

      ! water quality bottom variables
      if (numwqbots > 0) then
         allocate (dum(ndxi))
         do j = 1, numwqbots
            if (is_wq_bot_3d) then
!           3D
               work1 = dmiss
               do kk = 1, ndxi
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = wqbot(j, k)
                  end do
               end do
               ierr = nf90_put_var(irstfile, id_rwqb(j), work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
            else
               do kk = 1, ndxi
                  call getkbotktop(kk, kb, kt)
                  dum(kk) = wqbot(j, kb)
               end do
               ierr = nf90_put_var(irstfile, id_rwqb(j), dum(1:ndxi), (/1, itim/), (/ndxi, 1/))
            end if
         end do
         if (allocated(dum)) then
            deallocate (dum)
         end if
      end if

      ! JRE: review what is really necessary
      if (jawave == 4) then
         ierr = nf90_put_var(irstfile, id_E, E, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_thetamean, thetamean, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_sigmwav, sigmwav, (/1, itim/), (/ndxi, 1/))
      end if

      if (jased > 0 .and. stm_included) then
         ! concentrations
         if (stmpar%lsedsus > 0) then
            !Internal cells
            allocate (dum(ndxi))
            do j = ISED1, ISEDN
               if (kmx > 0) then !3D
                  do kk = 1, ndxi
                     call getkbotktop(kk, kb, kt)
                     call getlayerindices(kk, nlayb, nrlay)
                     do k = kb, kt
                        work1(k - kb + nlayb, kk) = sed(j - ISED1 + 1, k)
                     end do !k
                  end do !kk
                  ierr = nf90_put_var(irstfile, id_sf1(j - ISED1 + 1), work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
               else !2D
                  do kk = 1, ndxi
                     dum(kk) = sed(j - ISED1 + 1, kk)
                  end do !kk
                  ierr = nf90_put_var(irstfile, id_sf1(j - ISED1 + 1), dum, (/1, itim/), (/ndxi, 1/))
               end if !kmx
            end do !j
            if (allocated(dum)) then
               deallocate (dum)
            end if
            !Boundary cells
            if (jarstbnd > 0 .and. ndxbnd > 0) then
               allocate (dum(ndxbnd))
               do j = ISED1, ISEDN
                  if (kmx > 0) then !3D
                     i = 0
                     do kk = ndxi + 1, ndx
                        i = i + 1
                        call getkbotktop(kk, kb, kt)
                        call getlayerindices(kk, nlayb, nrlay)
                        do k = kb, kt
                           work1(k - kb + nlayb, i) = sed(j - ISED1 + 1, k)
                        end do !k
                     end do !kk
                     ierr = nf90_put_var(irstfile, id_sf1_bnd(j - ISED1 + 1), work1(1:kmx, 1:ndxbnd), (/1, 1, itim/), (/kmx, ndxbnd, 1/))
                  else !2D
                     dum = sed(j - ISED1 + 1, ndxi + 1:ndx)
                     ierr = nf90_put_var(irstfile, id_sf1_bnd(j - ISED1 + 1), dum, (/1, itim/), (/ndxbnd, 1/))
                  end if !kmx
               end do !j
               if (allocated(dum)) then
                  deallocate (dum)
               end if
            end if !(jarstbnd > 0 .and. ndxbnd > 0)
            !
            ! density (only necessary if morphodynamics and fractions in suspension and consider concentrations in density)
            if (stmpar%morpar%densin) then
               ierr = unc_put_var_rst(irstfile, id_rho, id_rho_bnd, rho, itim)
               ierr = unc_put_var_rst(irstfile, id_rhowat, id_rhowat_bnd, rhowat, itim)
            end if !(stmpar%morpar%densin)
         end if !(stmpar%lsedsus .gt. 0)
         ! morbl
         ierr = nf90_put_var(irstfile, id_morbl, bl, (/1, itim/), (/ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_morft, stmpar%morpar%morft, (/itim/))

         select case (stmpar%morlyr%settings%iunderlyr)
         case (1)
            ! bodsed
            ierr = nf90_put_var(irstfile, id_bodsed, stmpar%morlyr%state%bodsed(:, 1:ndxi), (/1, 1, itim/), (/stmpar%lsedtot, ndxi, 1/))
            ! dpsed
            ierr = nf90_put_var(irstfile, id_dpsed, stmpar%morlyr%state%dpsed(1:ndxi), (/1, itim/), (/ndxi, 1/))
         case (2)
            ! msed
            ierr = nf90_put_var(irstfile, id_msed, stmpar%morlyr%state%msed(:, :, 1:ndxi), (/1, 1, 1, itim/), (/stmpar%lsedtot, stmpar%morlyr%settings%nlyr, ndxi, 1/))
            ! lyrfrac
            if (.not. allocated(frac)) allocate (frac(stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndx))
            frac = -999d0
            if (stmpar%morlyr%settings%iporosity == 0) then
               dens => stmpar%sedpar%cdryb
            else
               dens => stmpar%sedpar%rhosol
            end if
            do k = 1, stmpar%morlyr%settings%nlyr
               do nm = 1, ndxi
                  if (stmpar%morlyr%state%thlyr(k, nm) > 0.0_fp) then
                     svthick = stmpar%morlyr%state%svfrac(k, nm) * stmpar%morlyr%state%thlyr(k, nm)
                     do l = 1, stmpar%lsedtot
                        frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm) / (dens(l) * svthick)
                     end do
                  else
                     frac(:, k, nm) = 0d0
                  end if
               end do
            end do
            ! thlyr
            ierr = nf90_put_var(irstfile, id_thlyr, stmpar%morlyr%state%thlyr(:, 1:ndxi), (/1, 1, itim/), (/stmpar%morlyr%settings%nlyr, ndxi, 1/))
            ! lyrfrac
            ierr = nf90_put_var(irstfile, id_lyrfrac, frac(:, :, 1:ndxi), (/1, 1, 1, itim/), (/stmpar%lsedtot, stmpar%morlyr%settings%nlyr, ndxi, 1/))
            ! preload
            ierr = nf90_put_var(irstfile, id_preload, stmpar%morlyr%state%preload(:, 1:ndxi), (/1, 1, itim/), (/stmpar%morlyr%settings%nlyr, ndxi, 1/))
            ! porosity
            if (stmpar%morlyr%settings%iporosity > 0) then
               if (.not. allocated(poros)) allocate (poros(1:stmpar%morlyr%settings%nlyr, 1:ndx))
               poros = 1d0 - stmpar%morlyr%state%svfrac
               ierr = nf90_put_var(irstfile, id_poros, poros(:, 1:ndxi), (/1, 1, itim/), (/stmpar%morlyr%settings%nlyr, ndxi, 1/))
            end if
         end select
         ! sedshort
         if (stmpar%morlyr%settings%morlyrnum%track_mass_shortage) then
            ierr = nf90_put_var(irstfile, id_sedshort, stmpar%morlyr%state%sedshort(:, 1:ndxi), (/1, 1, itim/), (/stmpar%lsedtot, ndxi, 1/))
         end if
         ! mfluff
         if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
            do l = 1, stmpar%lsedsus
               ierr = nf90_put_var(irstfile, id_mfluff, stmpar%morpar%flufflyr%mfluff(l, 1:ndxi), (/l, 1, itim/), (/1, ndxi, 1/))
            end do
         end if
      end if

      ! Write the data: sediment Herman
      if (jased > 0 .and. jased < 4) then ! Write the data: sediment
         ierr = nf90_put_var(irstfile, id_sed, sed, (/1, 1, itim/), (/mxgr, ndxi, 1/))
         ierr = nf90_put_var(irstfile, id_ero, grainlay, (/1, 1, itim/), (/mxgr, size(grainlay, 2), 1/))
         ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
         ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
         ! (both length numk), whereas ero itself is shorter than numk.
      end if

      ! Write the data: TH boundaries
      if (allocated(threttim)) then
         if (jasal > 0) then
            if (max_threttim(ISALT) > 0d0) then
               ierr = nf90_put_var(irstfile, id_tsalbnd, thtbnds, (/1, itim/), (/nbnds, 1/))
               ierr = nf90_put_var(irstfile, id_zsalbnd, thzbnds, (/1, itim/), (/nbnds * kmxd, 1/))
            end if
         end if
         if (jatem > 0) then
            if (max_threttim(ITEMP) > 0d0) then
               ierr = nf90_put_var(irstfile, id_ttembnd, thtbndtm, (/1, itim/), (/nbndtm, 1/))
               ierr = nf90_put_var(irstfile, id_ztembnd, thzbndtm, (/1, itim/), (/nbndtm * kmxd, 1/))
            end if
         end if
         if (jased > 0 .and. .not. stm_included) then
            if (max_threttim(ISED1) > 0d0) then
               ierr = nf90_put_var(irstfile, id_tsedbnd, thtbndsd, (/1, itim/), (/nbndsd, 1/))
               ierr = nf90_put_var(irstfile, id_zsedbnd, thzbndsd, (/1, itim/), (/nbndsd * kmxd, 1/))
            end if
         end if
         if (numfracs > 0) then !JRE sedfrac
            do i = 1, numfracs
               iconst = ifrac2const(i)
               if (iconst == 0) cycle
               if (max_threttim(iconst) > 0d0) then
                  ierr = nf90_put_var(irstfile, id_tsedfracbnd(i), bndsf(i)%tht, (/1, itim/), (/nbndsf(i), 1/))
                  ierr = nf90_put_var(irstfile, id_zsedfracbnd(i), bndsf(i)%thz, (/1, itim/), (/nbndsf(i) * kmxd, 1/))
               end if
            end do
         end if
         if (numtracers > 0) then
            do i = 1, numtracers
               iconst = itrac2const(i)
               if (max_threttim(iconst) > 0d0) then
                  ierr = nf90_put_var(irstfile, id_ttrabnd(i), bndtr(i)%tht, (/1, itim/), (/nbndtr(i), 1/))
                  ierr = nf90_put_var(irstfile, id_ztrabnd(i), bndtr(i)%thz, (/1, itim/), (/nbndtr(i) * kmxd, 1/))
               end if
            end do
         end if
      end if

      call process_structures_saved_parameters(WRITE_DATA_TO_FILE, irstfile)

      ! Write 1D cross sections
      if (ndx1d > 0 .and. stm_included) then
         if (stmpar%morpar%bedupd) then
            if (.not. allocated(work1d_z)) then
               allocate (work1d_z(jmax, nCrs))
            end if
            work1d_z = dmiss
            do i = 1, nCrs
               pCS => network%crs%cross(i)%tabdef
               do j = 1, pCS%levelscount
                  work1d_z(j, i) = pCS%height(j)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_flowelemcrsz, work1d_z(1:jmax, 1:nCrs), start=(/1, 1, itim/), count=(/jmax, nCrs, 1/))
            deallocate (work1d_z)
         end if
      end if

      ! Flow cell cc coordinates (only 1D + internal 2D)
      ierr = nf90_put_var(irstfile, id_flowelemxzw, xzw(1:ndxi))
      ierr = nf90_put_var(irstfile, id_flowelemyzw, yzw(1:ndxi))
      if (lnx > 0) then
         ! Flow links velocity points
         ierr = nf90_put_var(irstfile, id_flowlinkxu, xu(1:lnx))
         ierr = nf90_put_var(irstfile, id_flowlinkyu, yu(1:lnx))
      end if

      ! Write structure info.
      if (network%loaded) then
         ! wrtie info. of culvert
         if (network%sts%numculverts > 0) then
            ierr = nf90_put_var(irstfile, id_culvert_openh, valculvert(11, 1:network%sts%numculverts), (/1, itim/), (/network%sts%numculverts, 1/))
         end if

         if (nlongculverts > 0) then
            ierr = nf90_put_var(irstfile, id_longculvert_valveopen, longculverts(1:nlongculverts)%valve_relative_opening, (/1, itim/), (/nlongculverts, 1/))
         end if

         ! write info. of general structure
         nlen = network%sts%numGeneralStructures
         maxNumLinks = get_max_numLinks(ST_GENERAL_ST, nlen)

         if (nlen > 0) then
            ierr = nf90_put_var(irstfile, id_genstru_crestl, valgenstru(9, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_genstru_crestw, valgenstru(10, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_genstru_edgel, valgenstru(14, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_genstru_openw, valgenstru(13, 1:nlen), (/1, itim/), (/nlen, 1/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%au(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_area, work2d, (/1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%gateclosedfractiononlink(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_linkw, work2d, (/1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%fu(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_fu, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%ru(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_ru, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%au(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_au, work3d, (/1, 1, 1, itim/))

            call realloc(work3di, (/nfuru, maxNumLinks, nlen/), keepExisting=.false.)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3di(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%state(1:nfuru, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_state, work3di, (/1, 1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%generalStructureIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%sOnCrest(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_genstru_sOnCrest, work2d, (/1, 1, itim/))
         end if

         ! write info. of weir
         nlen = network%sts%numWeirs
         maxNumLinks = get_max_numLinks(ST_WEIR, nlen)
         if (nlen > 0) then
            ierr = nf90_put_var(irstfile, id_weirgen_crestl, valweirgen(9, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_weirgen_crestw, valweirgen(10, 1:nlen), (/1, itim/), (/nlen, 1/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%au(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_area, work2d, (/1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%gateclosedfractiononlink(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_linkw, work2d, (/1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%fu(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_fu, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%ru(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_ru, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%au(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_au, work3d, (/1, 1, 1, itim/))

            call realloc(work3di, (/nfuru, maxNumLinks, nlen/), keepExisting=.false.)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3di(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%state(1:nfuru, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_state, work3di, (/1, 1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%weirIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%sOnCrest(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_weirgen_sOnCrest, work2d, (/1, 1, itim/))
         end if

         ! write info. of orifice
         nlen = network%sts%numOrifices
         maxNumLinks = get_max_numLinks(ST_ORIFICE, nlen)
         if (nlen > 0) then
            ierr = nf90_put_var(irstfile, id_orifgen_crestl, valorifgen(9, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_orifgen_crestw, valorifgen(10, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_orifgen_edgel, valorifgen(14, 1:nlen), (/1, itim/), (/nlen, 1/))
            ierr = nf90_put_var(irstfile, id_orifgen_openw, valorifgen(13, 1:nlen), (/1, itim/), (/nlen, 1/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%au(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_area, work2d, (/1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%gateclosedfractiononlink(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_linkw, work2d, (/1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%fu(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_fu, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%ru(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_ru, work3d, (/1, 1, 1, itim/))

            call realloc(work3d, (/nfuru, maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3d(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%au(:, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_au, work3d, (/1, 1, 1, itim/))

            call realloc(work3di, (/nfuru, maxNumLinks, nlen/), keepExisting=.false.)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work3di(1:nfuru, L0, i) = network%sts%struct(istru)%generalst%state(1:nfuru, L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_state, work3di, (/1, 1, 1, itim/))

            call realloc(work2d, (/maxNumLinks, nlen/), keepExisting=.false., fill=dmiss)
            do i = 1, nlen
               istru = network%sts%orificeIndices(i)
               numLinks = network%sts%struct(istru)%numlinks
               do L0 = 1, numLinks
                  work2d(L0, i) = network%sts%struct(istru)%generalst%sOnCrest(L0)
               end do
            end do
            ierr = nf90_put_var(irstfile, id_orifgen_sOnCrest, work2d, (/1, 1, itim/))
         end if

         ! write info. of pump
         nlen = network%sts%numPumps
         if (nlen > 0) then
            ierr = nf90_put_var(irstfile, id_pump_cap, valpump(6, 1:nlen), (/1, itim/))

            call realloc(work2di, (/maxNumStages, nlen/), keepExisting=.false.)
            do i = 1, nlen
               istru = network%sts%pumpIndices(i)
               pstru => network%sts%struct(istru)
               numStages = pstru%pump%nrstages
               do L0 = 1, numStages
                  if (pstru%pump%ss_trigger(L0)) then
                     work2di(L0, i) = 1
                  else
                     work2di(L0, i) = 0
                  end if
               end do
            end do
            ierr = nf90_put_var(irstfile, id_pump_ssTrigger, work2di, (/1, 1, itim/))

            call realloc(work2di, (/maxNumStages, nlen/), keepExisting=.false.)
            do i = 1, nlen
               istru = network%sts%pumpIndices(i)
               pstru => network%sts%struct(istru)
               numStages = pstru%pump%nrstages
               do L0 = 1, numStages
                  if (pstru%pump%ds_trigger(L0)) then
                     work2di(L0, i) = 1
                  else
                     work2di(L0, i) = 0
                  end if
               end do
            end do
            ierr = nf90_put_var(irstfile, id_pump_dsTrigger, work2di, (/1, 1, itim/))
         end if
      end if

      if (jampi == 1) then
         ! flow cell domain numbers
         ierr = nf90_put_var(irstfile, id_flowelemdomain, idomain(1:ndxi))
         ierr = nf90_put_var(irstfile, id_flowelemglobalnr, iglobal_s(1:ndxi))

         ! flow link
         ierr = nf90_put_var(irstfile, id_flowlink, ln(:, 1:lnx))

         ! netelemnode
         if (nump1d2d > 0) then
            ierr = nf90_inquire_dimension(irstfile, id_netelemmaxnodedim, len=nv)
            allocate (netcellnod(nv, nump1d2d))
            netcellnod = intmiss
            do k = 1, nump1d2d
               nv1 = netcell(k)%n
               netcellnod(1:nv1, k) = netcell(k)%nod(1:nv1)
            end do
            ierr = nf90_put_var(irstfile, id_netelemnode, netcellnod)
            call check_error(ierr, 'Write netcell nodes')
            deallocate (netcellnod)
         end if
         if (numl > 0) then
            allocate (kn1write(numL))
            allocate (kn2write(numL))
            do L = 1, numL
               kn1write(L) = kn(1, L)
               kn2write(L) = kn(2, L)
            end do
            ierr = nf90_put_var(irstfile, id_netlink, kn1write, count=(/1, numl/), start=(/1, 1/))
            ierr = nf90_put_var(irstfile, id_netlink, kn2write, count=(/1, numl/), start=(/2, 1/))
            deallocate (kn1write)
            deallocate (kn2write)
         end if
      end if
      if (jarstbnd > 0 .and. ndxbnd > 0) then
         ! boundary points coordinates, and correspondng waterlevel and bedlevel
         if (jampi == 0) then
            ierr = nf90_put_var(irstfile, id_flowelemxbnd, xz(ndxi + 1:ndx))
            ierr = nf90_put_var(irstfile, id_flowelemybnd, yz(ndxi + 1:ndx))

            do i = 1, ndxbnd
               j = ln(1, lnxi + i)
               tmp_s0(i) = s0(j)
               tmp_s1(i) = s1(j)
               tmp_bl(i) = bl(j)
               tmp_squ(i) = squ(j)
               tmp_sqi(i) = sqi(j)
               tmp_ucxq(i) = ucxq(j)
               tmp_ucyq(i) = ucyq(j)
            end do
         else
            do i = 1, ndxbnd
               k = ibnd_own(i)
               j = ln(1, lnxi + k)
               tmp_x(i) = xz(j)
               tmp_y(i) = yz(j)
               tmp_s0(i) = s0(j)
               tmp_s1(i) = s1(j)
               tmp_bl(i) = bl(j)
               tmp_squ(i) = squ(j)
               tmp_sqi(i) = sqi(j)
               tmp_ucxq(i) = ucxq(j)
               tmp_ucyq(i) = ucyq(j)
            end do
            ierr = nf90_put_var(irstfile, id_flowelemxbnd, tmp_x)
            ierr = nf90_put_var(irstfile, id_flowelemybnd, tmp_y)
         end if
         ierr = nf90_put_var(irstfile, id_s0bnd, tmp_s0, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_s1bnd, tmp_s1, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_blbnd, tmp_bl, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_squbnd, tmp_squ, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_sqibnd, tmp_sqi, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_ucxqbnd, tmp_ucxq, (/1, itim/), (/ndxbnd, 1/))
         ierr = nf90_put_var(irstfile, id_ucyqbnd, tmp_ucyq, (/1, itim/), (/ndxbnd, 1/))
      end if

      if (network%loaded) then
         if (network%numl > 0) then
            call realloc(work1di, network%numl, keepExisting=.false.)
            call convert_hysteresis_summerdike(.true., work1di)

            ierr = nf90_put_var(irstfile, id_hysteresis, work1di)
         end if
      end if

   end subroutine unc_write_rst_filepointer

!> Writes a single snapshot of the unstructured flow net + flow data to a netCDF file.
!! If file exists, it will be overwritten. Therefore, only use this routine
!! for separate snapshots, the automated map file should be filled by calling
!! unc_write_map_filepointer directly instead!
   subroutine unc_write_map(filename, iconventions)
      use m_flowparameters, only: jamapbnd
      implicit none

      character(len=*), intent(in) :: filename
      integer, optional, intent(in) :: iconventions !< Unstructured NetCDF conventions (either UNC_CONV_CFOLD or UNC_CONV_UGRID)

      type(t_unc_mapids) :: mapids
      integer :: ierr, iconv
      integer :: jabndnd

      if (.not. present(iconventions)) then
         iconv = UNC_CONV_CFOLD
      else
         iconv = iconventions
      end if

      ierr = unc_create(filename, 0, mapids%ncid)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create map file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      if (iconv == UNC_CONV_UGRID) then
         jabndnd = 0
         if (jamapbnd > 0) jabndnd = 1
         call unc_write_map_filepointer_ugrid(mapids, 0d0, jabndnd)
      else
         call unc_write_map_filepointer(mapids%ncid, 0d0, 1)
      end if

      ierr = unc_close(mapids%ncid)
   end subroutine unc_write_map

!> Writes map/flow data to an already opened netCDF dataset. NEW version according to UGRID conventions + much cleanup.
!! The netnode and -links have been written already.
   subroutine unc_write_map_filepointer_ugrid(mapids, tim, jabndnd) ! wrimap
      use m_flow
      use m_flowtimes
      use m_flowgeom
      use m_heatfluxes
      use m_sferic
      use network_data
      use m_sediment
      use m_bedform
      use m_wind
      use m_flowparameters, only: jatrt, ibedlevtyp
      use m_mass_balance_areas
      use m_fm_wq_processes
      use m_xbeach_data
      use m_transportdata
      use m_alloc
      use m_waves, hminlw_waves => hminlw
      use m_missing
      use m_CrossSections
      use unstruc_channel_flow, only: network
      use string_module, only: replace_multiple_spaces_by_single_spaces
      use m_save_ugrid_state, only: mesh1dname, mesh2dname
      use m_hydrology_data, only: jadhyd, ActEvap, PotEvap, interceptionmodel, DFM_HYD_NOINTERCEPT, InterceptHs
      use m_subsidence, only: jasubsupl, subsout, subsupl, subsupl_t0
      use Timers
      use fm_location_types
      use m_map_his_precision
      use m_fm_icecover, only: ice_mapout, ice_af, ice_h, ice_p, ice_t, snow_h, snow_t, ja_icecover, ICECOVER_SEMTNER
      use m_gettaus
      use m_gettauswave
      use m_get_kbot_ktop

      implicit none

      type(t_unc_mapids), intent(inout) :: mapids !< Set of file and variable ids for this map-type file.
      real(kind=hp), intent(in) :: tim
      integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.

      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer, save :: ierr, ndim

      double precision, allocatable :: ust_x(:), ust_y(:), wavout(:), wavout2(:), scaled_rain(:)
      character(len=255) :: tmpstr
      integer :: nm
      integer :: Lf
      character(16) :: dxname
      character(64) :: dxdescr
      character(15) :: transpunit
      double precision :: rhol, mortime, wavfac
      double precision :: moravg, dmorft, dmorfs, rhodt
      double precision :: um, ux, uy
      double precision, dimension(:, :), allocatable :: poros, toutputx, toutputy, sxtotori, sytotori
      double precision, dimension(:, :, :), allocatable :: frac
      integer, dimension(:), allocatable :: flag_val
      character(len=10000) :: flag_mean

      double precision, dimension(:), allocatable :: numlimdtdbl
      double precision, dimension(:), allocatable :: work1d, work1d2
      double precision :: vicc, dicc

      double precision, dimension(:), pointer :: dens

!    Secondary Flow
!        id_rsi, id_rsiexact, id_dudx, id_dudy, id_dvdx, id_dvdy, id_dsdx, id_dsdy

      integer :: i, j, jj, itim, n, LL, L, Lb, Lt, k, k1, k2
      integer :: id_twodim
      integer :: kk, kb, kt, kkk, found, iloc
      integer :: nlayb, nrlay
      integer :: Ltx, nlaybL, nrlayLx
      integer :: iLocS ! Either UNC_LOC_S or UNC_LOC_S3D, depending on whether layers are present.
      integer :: iLocU ! Either UNC_LOC_U or UNC_LOC_U3D, depending on whether layers are present.
      integer :: isrc, kbot_, ktop_, nk, nkbot, nktop
      double precision, dimension(:), allocatable :: windx, windy, toutput, rks, wa
      double precision :: zwu0
      character(len=4) :: str

      type(t_CSType), pointer :: pCS
      type(t_CSType), pointer, dimension(:) :: pCSs
      integer :: ndx1d
      integer, save :: jmax, nCrs
      double precision, dimension(:, :), allocatable :: work1d_z, work1d_n
      double precision, dimension(:, :, :), allocatable :: work3d, work3d2
      character(3) :: sednr !< string representation of sediment fraction number
      character(256) :: varname !< name of netCDF variable
      character(1024) :: longname !< long, descriptive name of netCDF variable content

      integer :: nc_precision
      integer, parameter :: FIRST_ARRAY = 1
      integer, parameter :: SECOND_ARRAY = 2

      nc_precision = netcdf_data_type(md_nc_map_precision)

      if (ndxi <= 0) then
         call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
         return
      end if
      if (timon) call timstrt("unc_write_map_filepointer_ugrid", handle_extra(70))

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      ! Include boundary cells in output (ndx) or not (ndxi)
      if (jabndnd_ == 1) then
         ndxndxi = ndx
      else
         ndxndxi = ndxi
      end if

      ndx1d = ndxi - ndx2d

      ! Prepare the U/S location for either 2D or 3D for subsequent def_var and put_var sequences.
      if (kmx > 0) then ! If layers present.
         iLocS = UNC_LOC_S3D
         iLocU = UNC_LOC_U3D
      else
         iLocS = UNC_LOC_S
         iLocU = UNC_LOC_U
      end if

      call realloc(mapids%id_const, (/MAX_ID_VAR, NUMCONST/), keepExisting=.false.)

      ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
      ! before in previous calls.
      ndim = 0
      ierr = nf90_inquire(mapids%ncid, nDimensions=ndim)

      ! Only write net and flow geometry data the first time, or for a separate map file.
      if (ndim == 0) then
         if (timon) call timstrt("unc_write_flowgeom_filepointer_ugrid INIT", handle_extra(71))

         ierr = ug_addglobalatts(mapids%ncid, ug_meta_fm)

         ierr = unc_meta_add_user_defined(mapids%ncid)

         call unc_write_flowgeom_filepointer_ugrid(mapids%ncid, mapids%id_tsp, jabndnd_)

         ierr = unc_add_time_coverage(mapids%ncid, ti_maps, ti_mape, ti_map)

         ! Current time t1
         if (unc_nounlimited > 0) then
            ierr = nf90_def_dim(mapids%ncid, 'time', ceiling((ti_mape - ti_maps) / ti_map) + 1, mapids%id_tsp%id_timedim)
         else
            ierr = nf90_def_dim(mapids%ncid, 'time', nf90_unlimited, mapids%id_tsp%id_timedim)
         end if

         call check_error(ierr, 'def time dim')
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_time, nf90_double, (/mapids%id_tsp%id_timedim/), 'time', 'time', '', trim(Tudunitstr))
         mapids%id_tsp%idx_curtime = 0

         ! Size of latest timestep
         ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_timestep, nc_precision, (/mapids%id_tsp%id_timedim/), 'timestep', '', 'Latest computational timestep size in each output interval', 's')

         if (jamapnumlimdt > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, nc_precision, UNC_LOC_S, 'Numlimdt', '', 'Number of times flow element was Courant limiting', '1', cell_method='point', jabndnd=jabndnd_)
         end if

         ! Time dependent grid layers
         if (kmx > 0 .and. jafullgridoutput > 0) then
            ! Face-centred z-coordinates:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc, nc_precision, UNC_LOC_S3D, 'flowelem_zcc', 'altitude', 'Vertical coordinate of layer centres at pressure points', 'm', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzw, nc_precision, UNC_LOC_W, 'flowelem_zw', 'altitude', 'Vertical coordinate of layer interfaces at pressure points', 'm', jabndnd=jabndnd_)

            ! Edge-centred z-coordinates:
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu, nc_precision, UNC_LOC_U3D, 'flowlink_zu', 'altitude', 'Vertical coordinate of layer centres at velocity points', 'm', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzwu, nc_precision, UNC_LOC_WU, 'flowlink_zwu', 'altitude', 'Vertical coordinate of layer interfaces at velocity points', 'm', jabndnd=jabndnd_)

            if (jafullgridoutput == 2) then
               if (ndx2d > 0) then ! Borrow the "2-dimension" from the already defined mesh (either 2d or 1d, does not matter)
                  id_twodim = mapids%id_tsp%meshids2d%dimids(mdim_two)
               else
                  id_twodim = mapids%id_tsp%meshids1d%dimids(mdim_two)
               end if

               ! Bounds variable for face-centred z-coordinates:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc_bnd, nc_precision, UNC_LOC_S3D, 'flowelem_zcc_bnd', 'altitude', 'Bounds of vertical coordinate of layers at pressure points', 'm', &
                                      dimids=(/id_twodim, -3, -2, -1/), jabndnd=jabndnd_)
               ierr = nf90_put_att(mapids%ncid, mapids%id_flowelemzcc(2), 'bounds', trim(mesh2dname)//'_flowelem_zcc_bnd')
               ierr = nf90_put_att(mapids%ncid, mapids%id_flowelemzcc(1), 'bounds', trim(mesh1dname)//'_flowelem_zcc_bnd')

               ! Bounds variable for edge-centred z-coordinates:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu_bnd, nc_precision, UNC_LOC_U3D, 'flowlink_zu_bnd', 'altitude', 'Bounds of vertical coordinate of layers at velocity points', 'm', &
                                      dimids=(/id_twodim, -3, -2, -1/), jabndnd=jabndnd_)
               ierr = nf90_put_att(mapids%ncid, mapids%id_flowlinkzu(2), 'bounds', trim(mesh2dname)//'_flowlink_zu_bnd')
               ierr = nf90_put_att(mapids%ncid, mapids%id_flowlinkzu(1), 'bounds', trim(mesh1dname)//'_flowlink_zu_bnd')
            end if
         end if

         ! Water levels
         if (jamaps1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1, nc_precision, UNC_LOC_S, 's1', 'sea_surface_height', 'Water level', 'm', jabndnd=jabndnd_)
         end if
         if (jamaps0 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, nc_precision, UNC_LOC_S, 's0', 'sea_surface_height', 'Water level on previous timestep', 'm', jabndnd=jabndnd_)
         end if

         ! Influx
         if (jamapqin > 0 .and. jaqin > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qin, nc_precision, UNC_LOC_S, 'qin', '', 'Sum of all water influx', 'm3 s-1', jabndnd=jabndnd_)
         end if

         if (jamapFlowAnalysis > 0) then
            ! Flow analysis
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt, nc_precision, UNC_LOC_S, 'negdpt', '', 'Number of times negative depth was calculated', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt_cum, nc_precision, UNC_LOC_S, 'negdpt_cum', '', 'Cumulative number of times negative depth was calculated', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter, nc_precision, UNC_LOC_S, 'noiter', '', 'Number of times no nonlinear convergence was caused', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter_cum, nc_precision, UNC_LOC_S, 'noiter_cum', '', 'Cumulative number of times no nonlinear convergence was caused', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep, nc_precision, UNC_LOC_S, 'limtstep', '', 'Number of times a node was limiting for the computational time step', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep_cum, nc_precision, UNC_LOC_S, 'limtstep_cum', '', 'Cumulative number of times a node was limiting for the computational time step', '1', cell_method='point', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_courant, nc_precision, UNC_LOC_S, 'courant', '', 'Courant number', '1', cell_method='point', jabndnd=jabndnd_)
         end if

         ! Evaporation
         if (jamapevap > 0) then
            if (jadhyd == 1) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_potevap, nc_precision, UNC_LOC_S, 'potevap', 'water_potential_evaporation_flux', 'Potential evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_actevap, nc_precision, UNC_LOC_S, 'actevap', 'lwe_water_evaporation_rate', 'Actual evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_) ! Intentionally did not use standard_name='water_potential_evaporation_flux', because that one requires other units: kg m-2 s-1.
            end if
            if (jaevap == 1) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_evap, nc_precision, UNC_LOC_S, 'prescrevap', '', 'Prescribed evaporation rate at pressure points', 'm s-1', jabndnd=jabndnd_)
            end if
         end if

         ! Volumes
         if (jamapvol1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, nc_precision, iLocS, 'vol1', '', 'volume of water in grid cell', 'm3', jabndnd=jabndnd_)
         end if

         ! Calculated time step per cell based on CFL number
         if (jamapdtcell > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, nc_precision, iLocS, 'dtcell', '', 'Time step per cell based on CFL', 's', jabndnd=jabndnd_)
         end if

         ! Water depths
         if (jamaphs > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, nc_precision, UNC_LOC_S, 'waterdepth', 'sea_floor_depth_below_sea_surface', 'Water depth at pressure points', 'm', jabndnd=jabndnd_)
         end if

         if (jamaphu > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hu, nc_precision, UNC_LOC_U, 'hu', 'sea_floor_depth_below_sea_surface', 'water depth at velocity points', 'm', jabndnd=jabndnd_)
         end if

         ! Velocities
         if (jamapau > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, nc_precision, iLocU, 'au', '', 'normal flow area between two neighbouring grid cells', 'm2', jabndnd=jabndnd_)
         end if

         if (jamapu1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, nc_precision, iLocU, 'u1', '', 'Velocity at velocity point, n-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_u1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
         end if
         if (jamapu0 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, nc_precision, iLocU, 'u0', '', 'Velocity at velocity point at previous time step, n-component', 'm s-1', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_u0, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
         end if
         if (jamapucvec > 0) then
            if (jaeulervel == 1 .and. jawave > 0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'sea_water_x_eulerian_velocity', 'Flow element center eulerian velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'sea_water_y_eulerian_velocity', 'Flow element center eulerian velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
            else
               if (jsferic == 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'sea_water_x_velocity', 'Flow element center velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'sea_water_y_velocity', 'Flow element center velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, nc_precision, iLocS, 'ucx', 'eastward_sea_water_velocity', 'Flow element center velocity vector, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, nc_precision, iLocS, 'ucy', 'northward_sea_water_velocity', 'Flow element center velocity vector, y-component', 'm s-1', jabndnd=jabndnd_)
               end if
            end if
            if (kmx > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, nc_precision, UNC_LOC_S3D, 'ucz', 'upward_sea_water_velocity', 'Flow element center velocity vector, z-component', 'm s-1', jabndnd=jabndnd_)
               ! Depth-averaged cell-center velocities in 3D:
               if (jsferic == 0) then
                  if (jaeulervel == 1 .and. jawave > 0) then
                     ! GLM indication needed to report that depth-averaged values are always GLM, even when eulervelocities==1
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'sea_water_glm_x_velocity', 'Flow element center GLM depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_) ! depth-averaged magnitude has no stokes drift
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'sea_water_glm_y_velocity', 'Flow element center GLM depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
                  else
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'sea_water_x_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_)
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'sea_water_y_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
                  end if
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, nc_precision, UNC_LOC_S, 'ucxa', 'eastward_sea_water_velocity', 'Flow element center depth-averaged velocity, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, nc_precision, UNC_LOC_S, 'ucya', 'northward_sea_water_velocity', 'Flow element center depth-averaged velocity, y-component', 'm s-1', jabndnd=jabndnd_)
               end if
            end if
         end if
         if (jamapucmag > 0) then
            if (jaeulervel == 1 .and. jawave > 0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_eulerian_speed', 'Flow element center eulerian velocity magnitude', 'm s-1', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, nc_precision, iLocS, 'ucmag', 'sea_water_speed', 'Flow element center velocity magnitude', 'm s-1', jabndnd=jabndnd_)
            end if
            if (kmx > 0) then
               if (jaeulervel == 1 .and. jawave > 0) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged GLM velocity magnitude', 'm s-1', jabndnd=jabndnd_) ! depth-averaged magnitude has no stokes drift
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, nc_precision, UNC_LOC_S, 'ucmaga', 'sea_water_speed', 'Flow element center depth-averaged velocity magnitude', 'm s-1', jabndnd=jabndnd_)
               end if
            end if
         end if
         if (jamapucqvec > 0) then
            if (jaeulervel == 1 .and. jawave > 0 .and. .not. flowWithoutWaves) then ! TODO: AvD:refactor such that yes<->no Eulerian velocities are in parameters below:
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_eulerian_velocity', 'Flow element center eulerian velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, nc_precision, iLocS, 'ucxq', 'ucxq_velocity', 'Flow element center velocity vector based on discharge, x-component', 'm s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, nc_precision, iLocS, 'ucyq', 'ucyq_velocity', 'Flow element center velocity vector based on discharge, y-component', 'm s-1', jabndnd=jabndnd_)
            end if
         end if
         if (kmx > 0) then
            if (jamapww1 > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, nc_precision, UNC_LOC_W, 'ww1', 'upward_sea_water_velocity', 'Upward velocity on vertical interface, n-component', 'm s-1', jabndnd=jabndnd_)
            end if
            if (jamaprho > 0) then
               if (density_is_pressure_dependent()) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho, nc_precision, UNC_LOC_S3D, 'density', 'sea_water_density', 'Flow element center mass density', 'kg m-3', jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhop, nc_precision, UNC_LOC_S3D, 'rho', 'sea_water_potential_density', 'Flow element center potential density', 'kg m-3', jabndnd=jabndnd_)
               end if
            end if
         end if

         if (jamapq1 > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, nc_precision, iLocU, 'q1', 'discharge', 'Discharge through flow link at current time', 'm3 s-1', cell_method='sum', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_q1, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
         end if

         if (jamapq1main > 0 .and. allocated(q1_main)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, nc_precision, iLocU, 'q1_main', '', 'Main channel discharge through flow link at current time', 'm3 s-1', cell_method='sum', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_q1main, 'comment', 'Positive direction is from first to second neighbouring face (flow element).')
         end if

         if (jamapfw > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fwel, nc_precision, UNC_LOC_U, 'fixed weir energy loss', '', 'Fixed weir energy loss', 'm', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_fwel, '', '')
         end if

         if (jamapviu > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, nc_precision, iLocU, 'viu', '', 'Horizontal eddy viscosity', 'm2 s-1', jabndnd=jabndnd_)
         end if
         if (jamapdiu > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, nc_precision, iLocU, 'diu', '', 'Horizontal eddy diffusivity', 'm2 s-1', jabndnd=jabndnd_)
         end if

         ! Bed shear stress
         if (jamaptaucurrent > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausx, nc_precision, UNC_LOC_S, 'tausx', '', 'Total bed shear stress vector, x-component', 'N m-2', jabndnd=jabndnd_) ! vect shear stress
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausy, nc_precision, UNC_LOC_S, 'tausy', '', 'Total bed shear stress vector, y-component', 'N m-2', jabndnd=jabndnd_) ! vect shear stress
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taus, nc_precision, UNC_LOC_S, 'taus', '', 'Total bed shear stress magnitude', 'N m-2', jabndnd=jabndnd_)
            if (stm_included) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausmax, nc_precision, UNC_LOC_S, 'tausmax', '', 'Bed shear stress magnitude for morphology', 'N m-2', jabndnd=jabndnd_) ! max shear stress
            end if
         end if

         if (jamaptidep > 0 .and. jatidep > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tidep, nc_precision, UNC_LOC_S, &
                                   'TidalPotential', 'TidalPotential', 'Tidal Potential generated by celestial forces in flow element center', 'm2 s-2', &
                                   jabndnd=jabndnd_)
         end if
         if (jamapselfal > 0) then
            if (jaselfal > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_salp, nc_precision, UNC_LOC_S, &
                                      'SALPotential', 'SALPotential', 'Self-attraction and loading Potential in flow element center', 'm2 s-2', jabndnd=jabndnd_)
            end if
         end if

         if (jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_IntTidesDiss, nc_precision, UNC_LOC_S, &
                                   'internal_tides_dissipation', 'internal_tides_dissipation', 'internal tides dissipation in flow element center', &
                                   'J s-1 m-2', jabndnd=jabndnd_)
         end if

         ! Chezy data on flow nodes and flow links
         ! Input roughness value and type on flow links for input check (note: overwritten when jatrt==1)
         if (jamap_chezy_elements > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czs, nc_precision, UNC_LOC_S, 'czs', '', 'Chezy roughness in flow element center', 'm0.5s-1', jabndnd=jabndnd_)
            ! WO: m0.5s-1 does not follow standard ? (which accepts only integral powers?)
         end if
         if (jamap_chezy_links > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czu, nc_precision, UNC_LOC_U, 'czu', '', 'Chezy roughness on flow links', 'm0.5s-1', jabndnd=jabndnd_)
         end if
         if (jamap_chezy_input > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfu, nc_precision, UNC_LOC_U, 'cfu', '', 'Input roughness on flow links', '-', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfutyp, nf90_int, UNC_LOC_U, 'cfutyp', '', 'Input roughness type on flow links', '-', jabndnd=jabndnd_)
         end if

         ! Constituents
         if (jamapsal > 0 .and. jasal > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, nc_precision, iLocS, 'sa1', 'sea_water_salinity', 'Salinity in flow element', '1e-3', jabndnd=jabndnd_)
         end if

         if (jamaptem > 0 .and. jatem > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, nc_precision, iLocS, 'tem1', 'sea_water_temperature', 'Temperature in flow element', 'degC', jabndnd=jabndnd_)
         end if

         if (jamapspir > 0 .and. jasecflow > 0) then
            if (kmx < 1) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, nc_precision, UNC_LOC_S, 'spircrv', 'streamline_curvature', 'Flow streamline curvature', '1/m', jabndnd=jabndnd_)
            end if
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, nc_precision, UNC_LOC_S, 'spirint', 'spiral_intensity', 'Spiral flow intensity', 'm/s', jabndnd=jabndnd_)
         end if

         ! Tracers
         if (jamapconst > 0 .and. ITRA1 > 0) then
            call realloc(mapids%id_const, (/MAX_ID_VAR, NUMCONST/), keepExisting=.false., fill=0)
            do j = ITRA1, ITRAN
               tmpstr = const_names(j)
               ! Forbidden chars in NetCDF names: space, /, and more.
               call replace_char(tmpstr, 32, 95)
               call replace_char(tmpstr, 47, 95)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:, j), nc_precision, iLocS, trim(tmpstr), &
                                      '', trim(const_names(j))//' in flow element', const_units(j), jabndnd=jabndnd_)
            end do
         end if
         ! Discharges
         ! TODO: AVD...
         ! TIDAL TURBINES: Insert equivalent of addturbine_cnst and addturbine_time here

         ! water quality bottom variables
         if (numwqbots > 0) then
            call realloc(mapids%id_wqb, (/3, numwqbots/), keepExisting=.false., fill=0)
            do j = 1, numwqbots
               tmpstr = wqbotnames(j)
               ! Forbidden chars in NetCDF names: space, /, and more.
               call replace_char(tmpstr, 32, 95)
               call replace_char(tmpstr, 47, 95)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:, j), nc_precision, UNC_LOC_S, trim(tmpstr), &
                                      '', trim(wqbotnames(j))//' in flow element', wqbotunits(j), jabndnd=jabndnd_)
            end do
            if (jamapwqbot3d == 1) then
               call realloc(mapids%id_wqb3d, (/3, numwqbots/), keepExisting=.false., fill=0)
               do j = 1, numwqbots
                  tmpstr = wqbotnames(j)
                  ! Forbidden chars in NetCDF names: space, /, and more.
                  call replace_char(tmpstr, 32, 95)
                  call replace_char(tmpstr, 47, 95)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb3d(:, j), nc_precision, UNC_LOC_S3D, trim(tmpstr)//'_3D', &
                                         '', trim(wqbotnames(j))//' in flow element (3D)', wqbotunits(j), jabndnd=jabndnd_)
               end do
            end if
         end if

         ! WAQ extra outputs
         if (jawaqproc > 0) then
            if (noout_map > 0) then
               call realloc(mapids%id_waq, (/3, noout_map/), keepExisting=.false., fill=0)
               do j = 1, noout_map
                  tmpstr = ' '
                  write (tmpstr, "('water_quality_output_',I0)") j
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:, j), nc_precision, iLocS, tmpstr, &
                                         '', outputs%names(j), outputs%units(j), jabndnd=jabndnd_)
                  tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
                  call replace_multiple_spaces_by_single_spaces(tmpstr)
                  ierr = nf90_put_att(mapids%ncid, mapids%id_waq(2, j), 'description', tmpstr)
               end do
            end if
            if (noout_statt > 0) then
               call realloc(mapids%id_wqst, (/3, noout_statt/), keepExisting=.false., fill=0)
               do j = 1, noout_statt
                  jj = noout_user + j
                  tmpstr = ' '
                  write (tmpstr, "('water_quality_stat_',I0)") j
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:, j), nc_precision, iLocS, tmpstr, &
                                         '', outputs%names(jj), outputs%units(jj), jabndnd=jabndnd_)
                  tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                  call replace_multiple_spaces_by_single_spaces(tmpstr)
                  ierr = nf90_put_att(mapids%ncid, mapids%id_wqst(2, j), 'description', tmpstr)
               end do
            end if
            if (noout_state > 0) then
               call realloc(mapids%id_wqse, (/3, noout_state/), keepExisting=.false., fill=0)
               do j = 1, noout_state
                  jj = noout_user + noout_statt + j
                  tmpstr = ' '
                  write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:, j), nc_precision, iLocS, tmpstr, &
                                         '', outputs%names(jj), outputs%units(jj), 0, jabndnd=jabndnd_)
                  tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                  call replace_multiple_spaces_by_single_spaces(tmpstr)
                  ierr = nf90_put_att(mapids%ncid, mapids%id_wqse(2, j), 'description', tmpstr)
               end do
            end if
         end if

         ! mass balance areas
         if (nomba > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mba(:), nf90_int, UNC_LOC_S, 'water_quality_mba', '', 'Water quality mass balance areas', '', is_timedep=0, jabndnd=jabndnd_)
            call realloc(flag_val, nomba, keepExisting=.false., fill=0)
            flag_mean = ' '
            do j = nomba, 1, -1
               flag_val(j) = j
               flag_mean = trim(mbaname(j))//' '//flag_mean
            end do
            ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_values', flag_val)
            ierr = nf90_put_att(mapids%ncid, mapids%id_mba(2), 'flag_meanings', flag_mean)
         end if

         ! Meteo forcings
         if (jamaprain > 0 .and. jarain /= 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain, nc_precision, UNC_LOC_S, 'rainfall_rate', 'rainfall_rate', 'Rainfall rate', 'm s-1', jabndnd=jabndnd_)
         end if

         ! interception
         if (jamapicept > 0 .and. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_icepths, nc_precision, UNC_LOC_S, 'interception_waterdepth', '', 'Waterdepth in interception layer', 'm', jabndnd=jabndnd_)
         end if

         if (jamapwind > 0 .and. japatm /= 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm, nc_precision, UNC_LOC_S, 'Patm', 'surface_air_pressure', 'Atmospheric pressure near surface', 'N m-2', jabndnd=jabndnd_)
         end if

         if (ice_mapout) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_af, nf90_double, UNC_LOC_S, 'ice_af', 'sea_ice_area_fraction', 'Fraction of surface area covered by floating ice', '1', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_h, nf90_double, UNC_LOC_S, 'ice_h', 'sea_ice_area_fraction', 'Thickness of the floating ice cover', 'm', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_p, nf90_double, UNC_LOC_S, 'ice_p', '', 'Pressure exerted by the floating ice cover', 'N m-2', jabndnd=jabndnd_)
            if (ja_icecover == ICECOVER_SEMTNER) then
               ! need to convert this to K if we want to comply with the CF standard name "sea_ice_temperature"
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_t, nf90_double, UNC_LOC_S, 'ice_t', '', 'Temperature of the floating ice cover', 'degC', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_h, nf90_double, UNC_LOC_S, 'snow_h', '', 'Thickness of the snow layer', 'm', jabndnd=jabndnd_)
               ! need to convert this to K if we want to comply with the CF standard name "temperature_in_surface_snow"
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_t, nf90_double, UNC_LOC_S, 'snow_t', '', 'Temperature of the snow layer', 'degC', jabndnd=jabndnd_)
            end if
         end if

         if (jawind > 0) then
            if (jamapwind > 0) then
               if (jsferic == 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx, nc_precision, UNC_LOC_S, 'windx', 'x_wind', 'velocity of air on flow element center, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy, nc_precision, UNC_LOC_S, 'windy', 'y_wind', 'velocity of air on flow element center, y-component', 'm s-1', jabndnd=jabndnd_)
                  ! Also wind on flow links
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nc_precision, UNC_LOC_U, 'windxu', 'x_wind', 'velocity of air on flow links, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nc_precision, UNC_LOC_U, 'windyu', 'y_wind', 'velocity of air on flow links, y-component', 'm s-1', jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx, nc_precision, UNC_LOC_S, 'windx', 'eastward_wind', 'velocity of air on flow element center, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy, nc_precision, UNC_LOC_S, 'windy', 'northward_wind', 'velocity of air on flow element center, y-component', 'm s-1', jabndnd=jabndnd_)
                  ! Also wind on flow links
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, nc_precision, UNC_LOC_U, 'windxu', 'eastward_wind', 'velocity of air on flow links, x-component', 'm s-1', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, nc_precision, UNC_LOC_U, 'windyu', 'northward_wind', 'velocity of air on flow links, y-component', 'm s-1', jabndnd=jabndnd_)
               end if
            end if
            if (jamapwindstress > 0) then
               if (jsferic == 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nc_precision, UNC_LOC_S, 'windstressx', &
                                         'surface_downward_x_stress', 'wind stress on flow element center, x-component', 'N m-2', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nc_precision, UNC_LOC_S, 'windstressy', &
                                         'surface_downward_y_stress', 'wind stress on flow element center, y-component', 'N m-2', jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, nc_precision, UNC_LOC_S, 'windstressx', &
                                         'surface_downward_eastward_stress', 'wind stress on flow element center, x-component', 'N m-2', jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, nc_precision, UNC_LOC_S, 'windstressy', &
                                         'surface_downward_northward_stress', 'wind stress on flow element center, y-component', 'N m-2', jabndnd=jabndnd_)
               end if
            end if
         end if

         if (ja_airdensity + ja_computed_airdensity > 0 .and. jamap_airdensity > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_airdensity, nc_precision, UNC_LOC_S, 'rhoair', 'air_density', 'Air density', 'kg m-3', jabndnd=jabndnd_)
         end if

         ! Heat fluxes
         if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tair, nc_precision, UNC_LOC_S, 'Tair', 'surface_temperature', 'Air temperature near surface', 'degC', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhum, nc_precision, UNC_LOC_S, 'Rhum', 'surface_specific_humidity', 'Relative humidity near surface', '', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_clou, nc_precision, UNC_LOC_S, 'Clou', 'cloud_area_fraction', 'Cloudiness', '1', jabndnd=jabndnd_)

            if (jatem == 5) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun, nc_precision, UNC_LOC_S, 'Qsun', 'surface_net_downward_shortwave_flux', 'Solar influx', 'W m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qeva, nc_precision, UNC_LOC_S, 'Qeva', 'surface_downward_latent_heat_flux', 'Evaporative heat flux', 'W m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qcon, nc_precision, UNC_LOC_S, 'Qcon', 'surface_downward_sensible_heat_flux', 'Sensible heat flux', 'W m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qlong, nc_precision, UNC_LOC_S, 'Qlong', 'surface_net_downward_longwave_flux', 'Long wave back radiation', 'W m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfreva, nc_precision, UNC_LOC_S, 'Qfreva', 'downward_latent_heat_flux_in_sea_water_due_to_convection', 'Free convection evaporative heat flux', 'W m-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qfrcon, nc_precision, UNC_LOC_S, 'Qfrcon', 'surface_downward_sensible_heat_flux_due_to_convection', 'Free convection sensible heat flux', 'W m-2', jabndnd=jabndnd_)
            end if

            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Qtot, nc_precision, UNC_LOC_S, 'Qtot', 'surface_downward_heat_flux_in_sea_water', 'Total heat flux', 'W m-2', jabndnd=jabndnd_)

         end if

         ! Turbulence.
         if (jamaptur > 0 .and. kmx > 0) then
            if (iturbulencemodel >= 3) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, nc_precision, UNC_LOC_WU, 'turkin1', 'specific_turbulent_kinetic_energy_of_sea_water', 'turbulent kinetic energy', 'm2 s-2', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu, nc_precision, UNC_LOC_WU, 'vicwwu', 'eddy_viscosity', 'turbulent vertical eddy viscosity at velocity points', 'm2 s-1', jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwws, nc_precision, UNC_LOC_W, 'vicwws', 'eddy_viscosity', 'turbulent vertical eddy viscosity at pressure points', 'm2 s-1', jabndnd=jabndnd_)
               if (iturbulencemodel == 3) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water', 'turbulent energy dissipation', 'm2 s-3', jabndnd=jabndnd_)
               else if (iturbulencemodel == 4) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, nc_precision, UNC_LOC_WU, 'tureps1', '', 'turbulent time scale', 's-1', jabndnd=jabndnd_)
               end if
            end if
         end if

         ! Sediment transport (via morphology module)
         if ((jamapsed > 0 .and. jased > 0 .and. stm_included) .or. (jasubsupl > 0)) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mor_bl, nc_precision, UNC_LOC_S, 'mor_bl', '', 'Time-varying bottom level in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
         end if
         !
         if (jasubsupl > 0) then
            select case (ibedlevtyp)
            case (1)
               iloc = UNC_LOC_S
            case (2)
               iloc = UNC_LOC_U
            case (3, 4, 5, 6)
               iloc = UNC_LOC_CN
            end select
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_subsupl, nc_precision, iloc, 'subsupl', '', 'Cumulative subsidence/uplift', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
         end if
         !
         if (jamapz0 > 0) then
            ! roughness heights for current and current and wave related roughness
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0c, nc_precision, UNC_LOC_U, 'z0ucur', '', 'Current related roughness height', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0r, nc_precision, UNC_LOC_U, 'z0urou', '', 'Current-wave related roughness height', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
         end if
         !
         if (jamapsed > 0 .and. jased > 0 .and. stm_included) then
            ierr = nf90_def_dim(mapids%ncid, 'nSedTot', stmpar%lsedtot, mapids%id_tsp%id_sedtotdim)
            ierr = nf90_def_dim(mapids%ncid, 'nSedSus', stmpar%lsedsus, mapids%id_tsp%id_sedsusdim)
            ierr = nf90_def_dim(mapids%ncid, 'nBedLayers', stmpar%morlyr%settings%nlyr, mapids%id_tsp%id_nlyrdim)
            ierr = nf90_def_dim(mapids%ncid, 'nStringlen', 100, mapids%id_tsp%id_strlendim)
            !
            if (.not. stmpar%morpar%moroutput%cumavg) then ! only one average transport value at end of model run
               ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_sedavgtim, nc_precision, (/1/), 'sedAvgTim', '', 'Time interval over which cumulative transports are calculated', 's')
            end if
            !
            call realloc(mapids%id_dxx, (/stmpar%morpar%nxx, 3/), keepExisting=.false.)
            !
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morfac, nc_precision, (/mapids%id_tsp%id_timedim/), 'morfac', '', 'Average morphological factor over elapsed morphological time', '-')
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_morft, nc_precision, (/mapids%id_tsp%id_timedim/), 'morft', '', 'Current morphological time', 's')
            !
            ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_frac_name, nf90_char, (/mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedtotdim/), 'sedfrac_name', '', 'Sediment fraction name', '-')
            if (stmpar%lsedsus > 0) then
               ierr = unc_def_var_nonspatial(mapids%ncid, mapids%id_susfrac_name, nf90_char, (/mapids%id_tsp%id_strlendim, mapids%id_tsp%id_sedsusdim/), 'sussedfrac_name', '', 'Suspended sediment fraction name', '-')
            end if
            !
            select case (stmpar%morpar%moroutput%transptype)
            case (0)
               transpunit = 'kg s-1 m-1'
            case (1)
               transpunit = 'm3 s-1 m-1'
            case (2)
               transpunit = 'm3 s-1 m-1'
            end select
            !
            ! Suspended transport related quantities
            !
            if (stmpar%lsedsus > 0) then
               !
               if (kmx > 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, nf90_int, UNC_LOC_S, 'kmxsed', '', 'Bottom layer for sed calculations', '-', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_W, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids=(/-3, -2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, nc_precision, UNC_LOC_S, 'ws', '', 'Sediment settling velocity', 'm s-1', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               !
               if (kmx == 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, nc_precision, UNC_LOC_S, 'rsedeq', '', 'Equilibrium sediment concentration', 'kg m-3', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               !
               if (stmpar%morpar%moroutput%aks) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, nc_precision, UNC_LOC_S, 'aks', '', 'Near-bed reference concentration height', 'm', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, nc_precision, UNC_LOC_S, 'rca', '', 'Near-bed reference concentration', 'kg m-3', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               !
               if (stmpar%morpar%moroutput%sourcesink) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sourse, nc_precision, UNC_LOC_S, 'sourse', '', 'Source term suspended sediment fractions', 'kg m-3 s-1', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sinkse, nc_precision, UNC_LOC_S, 'sinkse', '', 'Sink term suspended sediment fractions', 's-1', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               !
               if (kmx > 0) then
                  if (stmpar%morpar%moroutput%suvcor) then
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_scrn, nc_precision, UNC_LOC_U, 'e_scrn', '', 'Near-bed transport correction in face-normal direction', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  end if
               end if
               !
               if (kmx > 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S3D, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3', dimids=(/-3, -2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               else
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, nc_precision, UNC_LOC_S, 'sedfrac_concentration', '', 'Sediment concentration in flow cell', 'kg m-3', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               !
            end if

            ! intermediate output for sediment formulas
            if (stmpar%morpar%moroutput%sedpar) then
               call realloc(mapids%id_sedpar, (/3, stmpar%trapar%npar, stmpar%lsedtot/), keepExisting=.false.)
               do l = 1, stmpar%lsedtot
                  write (sednr, '(I3.3)') l
                  do k = 1, stmpar%trapar%noutpar(l)
                     varname = trim(stmpar%trapar%outpar_name(k, l))//trim(sednr)
                     longname = trim(stmpar%trapar%outpar_longname(k, l))//' for '//trim(stmpar%sedpar%namsed(l))
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedpar(:, k, l), nf90_double, UNC_LOC_S, trim(varname), '', trim(longname), '', dimids=(/-2, -1/), jabndnd=jabndnd_)
                  end do
               end do
            end if

            ! default sediment transport output (suspended and bedload) on flow links
            if (stmpar%lsedsus > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssn, nc_precision, UNC_LOC_U, 'ssn', '', 'Suspended load transport, n-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sst, nc_precision, UNC_LOC_U, 'sst', '', 'Suspended load transport, t-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)

            end if

            if (stmpar%lsedtot > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbn, nc_precision, UNC_LOC_U, 'sbn', '', 'Bed load transport, n-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbt, nc_precision, UNC_LOC_U, 'sbt', '', 'Bed load transport, t-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_e_dzdn, nc_precision, UNC_LOC_U, 'e_dzdn', '', 'Bed slope, n-component', '-', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_e_dzdt, nc_precision, UNC_LOC_U, 'e_dzdt', '', 'Bed slope, t-component', '-', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%uuuvvv) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uuu, nc_precision, UNC_LOC_S, 'uuu', '', 'Characteristic velocity in cell centre, x-component', 'm s-1', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vvv, nc_precision, UNC_LOC_S, 'vvv', '', 'Characteristic velocity in cell centre, y-component', 'm s-1', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%umod) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_umod, nc_precision, UNC_LOC_S, 'umod', '', 'Characteristic velocity magnitude in cell centre', 'm s-1', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%zumod) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zumod, nc_precision, UNC_LOC_S, 'zumod', '', 'Height above bed for characteristic velocity in cell centre', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%ustar) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustar, nc_precision, UNC_LOC_S, 'ustar', '', 'Bed shear velocity in cell centre', 'm s-1', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%sbcuv) then
               if (stmpar%morpar%moroutput%rawtransports) then ! if either of these is true, the reconstruction is done outside this subroutine, invalidating Willem's approach to have 'unspoiled' transports
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcx, nc_precision, UNC_LOC_S, 'sbcx', '', 'Bed load transport due to currents, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcy, nc_precision, UNC_LOC_S, 'sbcy', '', 'Bed load transport due to currents, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               end if
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcx_reconstructed, nc_precision, UNC_LOC_S, 'sbcx_reconstructed', '', 'Bed load transport due to currents (reconstructed), x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcy_reconstructed, nc_precision, UNC_LOC_S, 'sbcy_reconstructed', '', 'Bed load transport due to currents (reconstructed), y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%sbwuv) then
               if (stmpar%morpar%moroutput%rawtransports) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwx, nc_precision, UNC_LOC_S, 'sbwx', '', 'Bed load transport due to waves, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwy, nc_precision, UNC_LOC_S, 'sbwy', '', 'Bed load transport due to waves, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               end if
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwx_reconstructed, nc_precision, UNC_LOC_S, 'sbwx_reconstructed', '', 'Bed load transport due to waves (reconstructed), x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwy_reconstructed, nc_precision, UNC_LOC_S, 'sbwy_reconstructed', '', 'Bed load transport due to waves (reconstructed), y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%sscuv) then ! This differs from Delft3D 4
               if (stmpar%morpar%moroutput%rawtransports) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscx, nc_precision, UNC_LOC_S, 'sscx', '', 'Suspended load transport due to currents, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscy, nc_precision, UNC_LOC_S, 'sscy', '', 'Suspended load transport due to currents, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscx_reconstructed, nc_precision, UNC_LOC_S, 'sscx_reconstructed', '', 'Suspended load transport due to currents (reconstructed), x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscy_reconstructed, nc_precision, UNC_LOC_S, 'sscy_reconstructed', '', 'Suspended load transport due to currents (reconstructed), y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%sswuv) then
               if (stmpar%morpar%moroutput%rawtransports) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswx, nc_precision, UNC_LOC_S, 'sswx', '', 'Suspended load transport due to waves, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswy, nc_precision, UNC_LOC_S, 'sswy', '', 'Suspended load transport due to waves, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               end if
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswx_reconstructed, nc_precision, UNC_LOC_S, 'sswx_reconstructed', '', 'Suspended load transport due to waves (reconstructed), x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswy_reconstructed, nc_precision, UNC_LOC_S, 'sswy_reconstructed', '', 'Suspended load transport due to waves (reconstructed), y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
            end if

            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxtot, nc_precision, UNC_LOC_S, 'sxtot', '', 'Total sediment transport in flow cell center (reconstructed), x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sytot, nc_precision, UNC_LOC_S, 'sytot', '', 'Total sediment transport in flow cell center (reconstructed), y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)

            ! Time averaged sediment transport values
            if (stmpar%morpar%moroutput%cumavg) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxcum, nc_precision, UNC_LOC_S, 'sbxcum', '', 'Time-averaged bed load transport, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbycum, nc_precision, UNC_LOC_S, 'sbycum', '', 'Time-averaged bed load transport, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssxcum, nc_precision, UNC_LOC_S, 'ssxcum', '', 'Time-averaged suspended load transport, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssycum, nc_precision, UNC_LOC_S, 'ssycum', '', 'Time-averaged suspended load transport, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxcum, nc_precision, UNC_LOC_S, 'sbxcum', '', 'Time-averaged bed load transport, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbycum, nc_precision, UNC_LOC_S, 'sbycum', '', 'Time-averaged bed load transport, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssxcum, nc_precision, UNC_LOC_S, 'ssxcum', '', 'Time-averaged suspended load transport, x-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssycum, nc_precision, UNC_LOC_S, 'ssycum', '', 'Time-averaged suspended load transport, y-component', transpunit, dimids=(/-2, mapids%id_tsp%id_sedtotdim/), jabndnd=jabndnd_)
            end if

            select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bodsed, nc_precision, UNC_LOC_S, 'bodsed', '', 'Available sediment mass in the bed in flow cell center', 'kg m-2', dimids=(/mapids%id_tsp%id_sedtotdim, -2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dpsed, nc_precision, UNC_LOC_S, 'dpsed', '', 'Sediment thickness in the bed in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            case (2)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_msed, nc_precision, UNC_LOC_S, 'msed', '', 'Available sediment mass in a layer of the bed in flow cell center', 'kg m-2', dimids=(/mapids%id_tsp%id_sedtotdim, mapids%id_tsp%id_nlyrdim, -2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thlyr, nc_precision, UNC_LOC_S, 'thlyr', '', 'Thickness of a layer of the bed in flow cell center', 'm', dimids=(/mapids%id_tsp%id_nlyrdim, -2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_lyrfrac, nc_precision, UNC_LOC_S, 'lyrfrac', '', 'Volume fraction in a layer of the bed in flow cell center', '-', dimids=(/mapids%id_tsp%id_sedtotdim, mapids%id_tsp%id_nlyrdim, -2, -1/), jabndnd=jabndnd_)
               !
               if (stmpar%morlyr%settings%iporosity > 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_poros, nc_precision, UNC_LOC_S, 'poros', '', 'Porosity of a layer of the bed in flow cell center', '-', dimids=(/mapids%id_tsp%id_nlyrdim, -2, -1/), jabndnd=jabndnd_)
               end if
               !
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_preload, nc_precision, UNC_LOC_S, 'preload', '', 'Historical largest load on layer of the bed in flow cell center', 'kg', dimids=(/mapids%id_tsp%id_nlyrdim, -2, -1/), jabndnd=jabndnd_)
            end select
            if (stmpar%morlyr%settings%morlyrnum%track_mass_shortage) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedshort, nc_precision, UNC_LOC_S, 'sedshort', '', 'Sediment shortage of transport layer in flow cell center', 'kg m-2', dimids=(/mapids%id_tsp%id_sedtotdim, -2, -1/), jabndnd=jabndnd_)
            end if
            !
            if (stmpar%morpar%moroutput%taub) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taub, nc_precision, UNC_LOC_S, 'taub', '', 'Bed shear stress for morphology', 'N m-2', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%taurat) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taurat, nc_precision, UNC_LOC_S, 'taurat', '', 'Excess bed shear ratio', '-', dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%dm) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dm, nc_precision, UNC_LOC_S, 'dm', '', 'Arithmetic mean sediment diameter', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%dg) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dg, nc_precision, UNC_LOC_S, 'dg', '', 'Geometric mean sediment diameter', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%dgsd) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dgsd, nc_precision, UNC_LOC_S, 'dgsd', '', 'Geometric standard deviation of particle size mix', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%percentiles) then
               do l = 1, stmpar%morpar%nxx
                  write (dxname, '(A,I2.2)') 'DXX', l
                  write (dxdescr, '(A,F4.1,A)') 'Sediment diameter percentile ', stmpar%morpar%xx(l) * 100d0, ' %'
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dxx(l, :), nc_precision, UNC_LOC_S, dxname, '', dxdescr, 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
               end do
            end if
            if (stmpar%morpar%moroutput%frac) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_frac, nc_precision, UNC_LOC_S, 'frac', '', 'Availability fraction in top layer', '-', dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%mudfrac) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mudfrac, nc_precision, UNC_LOC_S, 'mudfrac', '', 'Mud fraction in top layer', '-', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%sandfrac) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sandfrac, nc_precision, UNC_LOC_S, 'sandfrac', '', 'Sand fraction in top layer', '-', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%fixfac) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fixfac, nc_precision, UNC_LOC_S, 'fixfac', '', 'Reduction factor due to limited sediment thickness', '-', dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%hidexp) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hidexp, nc_precision, UNC_LOC_S, 'hidexp', '', 'Hiding and exposure factor', '-', dimids=(/-2, mapids%id_tsp%id_sedtotdim, -1/), jabndnd=jabndnd_)
            end if
            !
            if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mfluff, nc_precision, UNC_LOC_S, 'mfluff', '', 'Sediment mass in fluff layer', 'kg m-2', dimids=(/-2, mapids%id_tsp%id_sedsusdim, -1/), jabndnd=jabndnd_)
            end if
            !
            ! 1D cross sections
            if (ndx1d > 0 .and. stm_included) then
               if (stmpar%morpar%bedupd) then
                  nCrs = 0
                  do i = 1, size(network%crs%cross)
                     if (network%crs%cross(i)%crossindx == 0) exit
                     nCrs = nCrs + 1
                  end do
                  pCSs => network%CSDefinitions%CS
                  jmax = 0
                  do i = 1, size(pCSs)
                     if (pCSs(i)%levelscount == 0) exit
                     jmax = max(jmax, pCSs(i)%levelscount)
                  end do
                  ierr = nf90_def_dim(mapids%ncid, trim(mesh1dname)//'_crs_maxdim', jmax, mapids%id_tsp%id_jmax)
                  ierr = nf90_def_dim(mapids%ncid, trim(mesh1dname)//'_ncrs', nCrs, mapids%id_tsp%id_nCrs)
                  ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_z', nc_precision, (/mapids%id_tsp%id_jmax, mapids%id_tsp%id_nCrs, mapids%id_tsp%id_timedim/), mapids%id_tsp%id_flowelemcrsz(1))
                  ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'long_name', 'time-varying cross-section points level')
                  ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), 'unit', 'm')
                  ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_n', nc_precision, (/mapids%id_tsp%id_jmax, mapids%id_tsp%id_nCrs, mapids%id_tsp%id_timedim/), mapids%id_tsp%id_flowelemcrsn(1))
                  ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'long_name', 'time-varying cross-section points width')
                  ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), 'unit', 'm')
                  ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_mor_crs_name', nf90_char, (/mapids%id_tsp%id_strlendim, mapids%id_tsp%id_nCrs/), mapids%id_tsp%id_morCrsName)
                  ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_morCrsName, 'long_name', 'name of cross-section')
               end if
               if (stmpar%morpar%moroutput%blave) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_blave, nc_precision, UNC_LOC_S, 'bl_ave', '', 'Main channel averaged bed level', 'm', dimids=(/-2, -1/), which_meshdim=1, jabndnd=jabndnd_)
                  !ierr = nf90_def_var(mapids%ncid, trim(mesh1dname)//'_bl_ave', nf90_double, (/ mapids%id_tsp%id_ndx1d, mapids%id_tsp%id_timedim /), mapids%id_tsp%id_blave)
                  !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'long_name','Main channel averaged bed level')
                  !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%id_blave, 'unit', 'm')
               end if
               if (stmpar%morpar%moroutput%bamor) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bamor, nc_precision, UNC_LOC_S, 'mor_area', '', 'Main channel cell area', 'm2', is_timedep=0, which_meshdim=1, jabndnd=jabndnd_)
               end if
               if (stmpar%morpar%moroutput%wumor) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wumor, nc_precision, UNC_LOC_U, 'mor_width_u', '', 'Main channel cell width at flow link', 'm', is_timedep=0, which_meshdim=1, jabndnd=jabndnd_)
               end if
            end if
         end if
         !
         ! BEDFORMS
         !
         if (bfmpar%lfbedfrmout) then
            if (bfmpar%lfbedfrm) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_duneheight, nc_precision, UNC_LOC_S, 'duneheight', '', 'Time-varying dune height in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dunelength, nc_precision, UNC_LOC_S, 'dunelength', '', 'Time-varying dune length in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
            !
            if (bfmpar%lfbedfrmrou) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksr, nc_precision, UNC_LOC_S, 'ksr', '', 'Ripple roughness height in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksmr, nc_precision, UNC_LOC_S, 'ksmr', '', 'Megaripple roughness height in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksd, nc_precision, UNC_LOC_S, 'ksd', '', 'Dune roughness height in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ks, nc_precision, UNC_LOC_S, 'ks', '', 'Bedform roughness height in flow cell center', 'm', dimids=(/-2, -1/), jabndnd=jabndnd_)
            end if
         end if

         ! Sediment transport (via own built-in sed)
         if (jamapsed > 0 .and. jased > 0 .and. .not. stm_included) then
            ierr = nf90_def_dim(mapids%ncid, 'nFrac', mxgr, mapids%id_tsp%id_maxfracdim)
            if (.not. allocated(mapids%id_sed)) then
               allocate (mapids%id_sed(MAX_ID_VAR, mxgr), mapids%id_ero(MAX_ID_VAR, mxgr))
               mapids%id_sed = -1
               mapids%id_ero = -1
            end if
            do j = 1, mxgr
               write (str, "(I4)") j
               str = adjustl(str)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:, j), nc_precision, UNC_LOC_S, 'sed'//trim(str), 'sediment_concentration', 'Sediment concentration', 'kg m-3', jabndnd=jabndnd_) !, dimids = (/ mapids%id_maxfracdim, -2, -1 /))
            end do
            if (jaceneqtr == 1) then ! Bed level in cell center
               do j = 1, mxgr
                  write (str, "(I4)") j
                  str = adjustl(str)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:, j), nc_precision, UNC_LOC_S, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element centers', 'm', jabndnd=jabndnd_)
               end do
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl, nc_precision, UNC_LOC_S, 'flowelem_bedlevel_bl', '', 'Flow element center bedlevel (bl)', 'm', jabndnd=jabndnd_)
            else ! Bed level at cell corner
               do j = 1, mxgr
                  write (str, "(I4)") j
                  str = adjustl(str)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:, j), nc_precision, UNC_LOC_CN, 'ero'//trim(str), 'layer_thickness_per_fraction', 'Erodable layer thickness per size fraction in flow element corners', 'm', jabndnd=jabndnd_)
               end do
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zk, nc_precision, UNC_LOC_CN, 'netnode_bedlevel_zk', '', 'Flow element corner bedlevel (zk)', 'm', jabndnd=jabndnd_)
            end if
         end if

         if (jamapwav > 0) then
            ! TO DO JRE: fix dit voor offline wave koppeling
            if (flowWithoutWaves) then ! Check the external forcing wave quantities and their associated arrays
               if (jamapwav_hwav > 0 .and. allocated(hwav)) then
                  if (jamapsigwav == 0) then
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, nc_precision, UNC_LOC_S, 'hwav', 'sea_surface_wave_rms_height', 'RMS wave height', 'm', jabndnd=jabndnd_) ! not CF
                  else
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, nc_precision, UNC_LOC_S, 'hwav', 'sea_surface_wave_significant_wave_height', 'Significant wave height', 'm', jabndnd=jabndnd_)
                  end if
               end if
               if (jamapwav_twav > 0 .and. allocated(twav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav, nc_precision, UNC_LOC_S, 'tp', '', 'Peak wave period', 's')
               end if
               if (jamapwav_phiwav > 0 .and. allocated(phiwav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_phiwav, nc_precision, UNC_LOC_S, 'dir', '', 'Mean direction of wave propagation relative to ksi-dir. ccw', 'deg', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_sxwav > 0 .and. allocated(sxwav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav, nc_precision, UNC_LOC_S, 'sxwav', 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_sywav > 0 .and. allocated(sywav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav, nc_precision, UNC_LOC_S, 'sywav', 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_sbxwav > 0 .and. allocated(sbxwav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxwav, nc_precision, UNC_LOC_S, 'sbxwav', 'sea_surface_x_wave_force_body', 'Water body wave forcing term, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_sbywav > 0 .and. allocated(sbywav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbywav, nc_precision, UNC_LOC_S, 'sbywav', 'sea_surface_y_wave_force_body', 'Water body wave forcing term, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_mxwav > 0 .and. allocated(mxwav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mxwav, nc_precision, UNC_LOC_S, 'mx', '', 'Wave-induced volume flux in x-direction', 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_mywav > 0 .and. allocated(mywav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mywav, nc_precision, UNC_LOC_S, 'my', '', 'Wave-induced volume flux in y-direction', 'm3 s-1 m-1', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_dsurf > 0 .and. allocated(dsurf)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsurf, nc_precision, UNC_LOC_S, 'dissurf', '', 'Wave energy dissipation rate at the free surface', 'w m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_dwcap > 0 .and. allocated(dwcap)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dwcap, nc_precision, UNC_LOC_S, 'diswcap', '', 'Wave energy dissipation rate due to white capping', 'w m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_distot > 0 .and. allocated(distot)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_distot, nc_precision, UNC_LOC_S, 'distot', '', 'Total wave energy dissipation', 'w m-2', jabndnd=jabndnd_) ! not CF
               end if
               if (jamapwav_uorb > 0 .and. allocated(uorbwav)) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb, nc_precision, UNC_LOC_S, 'uorb', 'sea_surface_wave_orbital_velocity', 'Wave orbital velocity', 'm s-1', jabndnd=jabndnd_) ! not CF
               end if
            else ! flow With Waves
               ! JRE waves
               if (jawave == 4) then
                  ierr = nf90_def_dim(mapids%ncid, 'ntheta', ntheta, mapids%id_tsp%id_ntheta)
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E, nc_precision, UNC_LOC_S, 'E', 'sea_surface_bulk_wave_energy', 'Wave energy per square meter', 'J m-2', jabndnd=jabndnd_) ! not CF
                  if (roller > 0) then
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R, nc_precision, UNC_LOC_S, 'R', 'sea_surface_bulk_roller_energy', 'Roller energy per square meter', 'J m-2', jabndnd=jabndnd_) ! not CF
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR, nc_precision, UNC_LOC_S, 'DR', 'sea_surface_bulk_roller_dissipation', 'Roller energy dissipation per square meter', 'W m-2', jabndnd=jabndnd_) ! not CF
                  end if
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D, nc_precision, UNC_LOC_S, 'D', 'sea_surface_wave_breaking_dissipation', 'Wave breaking energy dissipation per square meter', 'W m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df, nc_precision, UNC_LOC_S, 'Df', 'sea_surface_wave_bottom_dissipation', 'Wave bottom energy dissipation per square meter', 'W m-2', jabndnd=jabndnd_) ! not CF

                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx, nc_precision, UNC_LOC_S, 'Sxx', '', 'Radiation stress, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy, nc_precision, UNC_LOC_S, 'Syy', '', 'Radiation stress, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy, nc_precision, UNC_LOC_S, 'Sxy', 'sea_surface_wave_radiation_stress_NE', 'Radiation stress, xy-component', 'N m-2', jabndnd=jabndnd_) ! not CF

                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav, nc_precision, UNC_LOC_S, 'cwav', 'sea_surface_wave_phase_celerity', 'Sea_surface_wave_phase_celerity', 'm s-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav, nc_precision, UNC_LOC_S, 'cgwav', 'sea_surface_wave_group_celerity', 'Sea_surface_wave_group_celerity', 'm s-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav, nc_precision, UNC_LOC_S, 'sigmwav', 'sea_surface_wave_mean_frequency', 'Sea_surface_wave_mean_frequency', 'rad s-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav, nc_precision, UNC_LOC_S, 'kwav', 'sea_surface_wave_wavenumber', 'Sea_surface_wave_wavenumber', 'rad m-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav, nc_precision, UNC_LOC_S, 'nwav', 'sea_surface_wave_cg_over_c', 'Sea_surface_wave_ratio_group_phase_speed', '-', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ctheta, nc_precision, UNC_LOC_S, 'ctheta', 'sea_surface_wave_refraction_celerity', 'Sea_surface_wave_refraction_celerity', 'rad s-1', dimids=(/mapids%id_tsp%id_ntheta, -2, -1/), jabndnd=jabndnd_) ! not CF
                  !
                  !if (windmodel.eq.0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1, nc_precision, UNC_LOC_S, 'L1', 'sea_surface_wave_wavelength', 'Sea_surface_wave_wavelength', 'm', jabndnd=jabndnd_) ! not CF
                  !elseif ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
                  !   ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE  , nf90_double, UNC_LOC_S, 'SwE'  , 'source_term_wind_on_E'      , 'wind source term on wave energy'                  , 'J m-2 s-1', jabndnd=jabndnd_) ! not CF
                  !   ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT  , nf90_double, UNC_LOC_S, 'SwT'  , 'source_term_wind_on_T'      , 'wind source term on wave period'                  , 's s-1', jabndnd=jabndnd_) ! not CF
                  !endif
               end if

               if ((jawave == 3 .or. jawave == 4) .and. kmx > 0) then
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav, nc_precision, UNC_LOC_S, 'sxwav', 'sea_surface_x_wave_force_surface', 'Surface layer wave forcing term, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav, nc_precision, UNC_LOC_S, 'sywav', 'sea_surface_y_wave_force_surface', 'Surface layer wave forcing term, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxwav, nc_precision, UNC_LOC_S, 'sxbwav', 'sea_surface_x_wave_force_body', 'Water body wave forcing term, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbywav, nc_precision, UNC_LOC_S, 'sybwav', 'sea_surface_y_wave_force_body', 'Water body wave forcing term, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF
               end if

               if (jawave > 0) then
                  if (jamapsigwav == 0) then
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, nc_precision, UNC_LOC_S, 'hwav', 'sea_surface_wave_rms_height', 'RMS wave height', 'm', jabndnd=jabndnd_) ! not CF
                  else
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, nc_precision, UNC_LOC_S, 'hwav', 'sea_surface_wave_significant_wave_height', 'Significant wave height', 'm', jabndnd=jabndnd_)
                  end if
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb, nc_precision, UNC_LOC_S, 'uorb', 'sea_surface_wave_orbital_velocity', 'Wave orbital velocity', 'm s-1', jabndnd=jabndnd_) ! not CF
                  !
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes, nc_precision, iLocS, 'ust_cc', 'sea_surface_x_stokes_drift', 'Stokes drift, x-component', 'm s-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes, nc_precision, iLocS, 'vst_cc', 'sea_surface_y_stokes_drift', 'Stokes drift, y-component', 'm s-1', jabndnd=jabndnd_) ! not CF

                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink, nc_precision, iLocU, 'ustokes', '', 'Stokes drift, n-component', 'm s-1', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink, nc_precision, iLocU, 'vstokes', '', 'Stokes drift, t-component', 'm s-1', jabndnd=jabndnd_) ! not CF

                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, nc_precision, UNC_LOC_S, 'thetamean', 'sea_surface_wave_from_direction', 'Wave from direction', 'deg from N', jabndnd=jabndnd_) ! not CF
                  ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav, nc_precision, UNC_LOC_S, 'twav', 'sea_surface_wave_period', 'Wave period', 's') ! not CF
                  if (jawave == 3 .or. jawave == 4) then
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx, nc_precision, iLocS, 'Fx', 'sea_surface_x_wave_force', 'Wave force, x-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy, nc_precision, iLocS, 'Fy', 'sea_surface_y_wave_force', 'Wave force, y-component', 'N m-2', jabndnd=jabndnd_) ! not CF

                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, nc_precision, iLocU, 'wavfu', '', 'Wave force at velocity point, n-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                     ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, nc_precision, iLocU, 'wavfv', '', 'Wave force at velocity point, t-component', 'N m-2', jabndnd=jabndnd_) ! not CF
                  end if
               end if
            end if
         end if
         !
         ! Trachytope roughnesses on NET links
         if (jamaptrachy > 0 .and. jatrt == 1) then

            if (ifrctypuni == 0) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt', '', 'Chezy roughness from trachytopes', '', jabndnd=jabndnd_)
               ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm0.5s-1')
            else if (ifrctypuni == 1) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt', '', 'Manning roughness from trachytopes', '', jabndnd=jabndnd_)
               ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'sm-0.333')
            else if ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt', '', 'White-Colebrook roughness from trachytopes', '', jabndnd=jabndnd_)
               ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', 'm')
            else
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, nc_precision, UNC_LOC_L, 'cftrt', '', 'Roughness from trachytopes', '', jabndnd=jabndnd_)
               ierr = unc_put_att(mapids%ncid, mapids%id_cftrt, 'non_si_units', ' ')
            end if
         end if

         if (javeg > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rnveg, nc_precision, UNC_LOC_S, 'rnveg', 'stem density of vegetation', 'stem density per square meter', 'm-2')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diaveg, nc_precision, UNC_LOC_S, 'diaveg', 'stem diameter of vegetation', 'stem diameter of vegetation', 'm')
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_veg_stemheight, nc_precision, UNC_LOC_S, 'stemheight', 'stem height of vegetation', 'stem height of vegetation', 'm')
         end if

         if (jamapcali > 0 .and. jacali == 1) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, nc_precision, UNC_LOC_L, 'cfcl', '', 'Calibration factor for roughness', '', jabndnd=jabndnd_)
            ierr = unc_put_att(mapids%ncid, mapids%id_cfcl, 'non_si_units', 'm0.5s-1')
         end if

         ! Secondary Flow ! TODO: AvD: add secondary flow
         !if (jasecflow == 1) then
         !    ierr = nf90_def_var(imapfile, 'rsi' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsi)
         !    ierr = nf90_put_att(imapfile, id_rsi,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_rsi,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_rsi,  'long_name'    , 'inverse streamline curvature in flow element center')
         !    ierr = nf90_put_att(imapfile, id_rsi,  'units'        , 'm-1')
         !    ierr = nf90_def_var(imapfile, 'rsiexact' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_rsiexact)
         !    ierr = nf90_put_att(imapfile, id_rsiexact,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_rsiexact,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_rsiexact,  'long_name'    , 'inverse streamline curvature in flow element center')
         !    ierr = nf90_put_att(imapfile, id_rsiexact,  'units'        , 'm-1')
         !    ierr = nf90_def_var(imapfile, 'dsdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdx)
         !    ierr = nf90_put_att(imapfile, id_dsdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dsdx,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dsdx,  'long_name'    , 'water level gradient in x direction')
         !    ierr = nf90_put_att(imapfile, id_dsdx,  'units'        , 's-1')
         !    ierr = nf90_def_var(imapfile, 'dsdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dsdy)
         !    ierr = nf90_put_att(imapfile, id_dsdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dsdy,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dsdy,  'long_name'    , 'water level gradient in y direction')
         !    ierr = nf90_put_att(imapfile, id_dsdy,  'units'        , 's-1')
         !    ierr = nf90_def_var(imapfile, 'dudx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudx)
         !    ierr = nf90_put_att(imapfile, id_dudx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dudx,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dudx,  'long_name'    , 'x-velocity gradient in x direction')
         !    ierr = nf90_put_att(imapfile, id_dudx,  'units'        , 's-1')
         !    ierr = nf90_def_var(imapfile, 'dudy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dudy)
         !    ierr = nf90_put_att(imapfile, id_dudy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dudy,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dudy,  'long_name'    , 'x-velocity gradient in y direction')
         !    ierr = nf90_put_att(imapfile, id_dudy,  'units'        , 's-1')
         !    ierr = nf90_def_var(imapfile, 'dvdx' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdx)
         !    ierr = nf90_put_att(imapfile, id_dvdx,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dvdx,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dvdx,  'long_name'    , 'y-velocity gradient in x direction')
         !    ierr = nf90_put_att(imapfile, id_dvdx,  'units'        , 's-1')
         !    ierr = nf90_def_var(imapfile, 'dvdy' ,  nf90_double, (/ id_flowelemdim, id_timedim /) , id_dvdy)
         !    ierr = nf90_put_att(imapfile, id_dvdy,  'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
         !    ierr = nf90_put_att(imapfile, id_dvdy,  'standard_name', '')
         !    ierr = nf90_put_att(imapfile, id_dvdy,  'long_name'    , 'y-velocity gradient in y direction')
         !    ierr = nf90_put_att(imapfile, id_dvdy,  'units'        , 's-1')
         !endif

         if (janudge > 0 .and. jamapNudge > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, nc_precision, UNC_LOC_S, 'Tnudge', 'nudging_time', 'Nudging relaxing time', 's', is_timedep=0, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, nc_precision, UNC_LOC_S3D, 'nudge_tem', 'nudging_tem', 'Nudging temperature', 'degC', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, nc_precision, UNC_LOC_S3D, 'nudge_sal', 'nudging_sal', 'Nudging salinity', '1e-3, jabndnd=jabndnd_', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, nc_precision, UNC_LOC_S3D, 'nudge_Dtem', 'nudging_Dtem', 'Difference of nudging temperature with temperature', 'degC', jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, nc_precision, UNC_LOC_S3D, 'nudge_Dsal', 'nudging_Dsal', 'Difference of nudging salinity with salinity', '1e-3', jabndnd=jabndnd_)

         end if

         ! for 1D only
         if (ndxi - ndx2d > 0 .and. jamapPure1D_debug /= 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_adve, nc_precision, UNC_LOC_U, 'adve', '', 'Explicit advection term', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_advi, nc_precision, UNC_LOC_U, 'advi', '', 'Implicit advection term', 's', which_meshdim=1, jabndnd=jabndnd_)
         end if

         if (ndxi - ndx2d > 0 .and. jaPure1D >= 3 .and. jamapPure1D_debug /= 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_1, nc_precision, UNC_LOC_U, 'q1d_1', '', 'Discharge at begin of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_2, nc_precision, UNC_LOC_U, 'q1d_2', '', 'Discharge at end of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_volu1d, nc_precision, UNC_LOC_U, 'volu1d', '', 'Volume of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_1, nc_precision, UNC_LOC_U, 'au1d_1', '', 'Flow area at begin of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_2, nc_precision, UNC_LOC_U, 'au1d_2', '', 'Flow area at end of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_1, nc_precision, UNC_LOC_U, 'wu1d_1', '', 'Total width at begin of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_2, nc_precision, UNC_LOC_U, 'wu1d_2', '', 'Total width at end of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_1, nc_precision, UNC_LOC_U, 'sar1d_1', '', 'Surface area at begin of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_2, nc_precision, UNC_LOC_U, 'sar1d_2', '', 'Surface area at end of flow link', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_mom_1d, nc_precision, UNC_LOC_S, 'alpha_mom_1d', '', 'Alpha factor momentum conservation', 's', which_meshdim=1, jabndnd=jabndnd_)
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_ene_1d, nc_precision, UNC_LOC_S, 'alpha_ene_1d', '', 'Alpha factor kinetic energy conservation', 's', which_meshdim=1, jabndnd=jabndnd_)
         end if

         ! for 1D only, urban
         if (ndxi - ndx2d > 0 .and. network%loaded) then
            if (jamapTimeWetOnGround > 0) then ! cumulative time when water is above ground level
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, nc_precision, UNC_LOC_S, 'time_water_on_ground', '', 'Cumulative time water above ground level', 's', which_meshdim=1, jabndnd=jabndnd_)
            end if
            if (jamapFreeboard > 0) then ! freeboard
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_freeboard, nc_precision, UNC_LOC_S, 'freeboard', '', 'Freeboard', 'm', which_meshdim=1, jabndnd=jabndnd_)
            end if
            if (jamapDepthOnGround > 0) then ! waterdpth that is above ground level
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs_on_ground, nc_precision, UNC_LOC_S, 'waterdepth_on_ground', '', 'Waterdepth above ground level', 'm', which_meshdim=1, jabndnd=jabndnd_)
            end if
            if (jamapVolOnGround > 0) then ! volume that is above ground level
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol_on_ground, nc_precision, UNC_LOC_S, 'volume_on_ground', '', 'Volume above ground level', 'm3', which_meshdim=1, jabndnd=jabndnd_)
            end if
            if (jamapTotalInflow1d2d > 0) then ! total 1d2d net inflow
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, nc_precision, UNC_LOC_S, 'current_total_net_inflow_1d2d', '', 'Current total net inflow via all connected 1d2d links at each 1D node', 'm3 s-1', which_meshdim=1, jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_1d2d', '', 'Cumulative total net inflow via all connected 1d2d links at each 1D node', 'm3', which_meshdim=1, jabndnd=jabndnd_)
            end if
            if (jamapTotalInflowLat > 0) then ! total lateral net inflow
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, nc_precision, UNC_LOC_S, 'current_total_net_inflow_lateral', '', 'Current total net inflow via all laterals at each 1D node', 'm3 s-1', which_meshdim=1, jabndnd=jabndnd_)
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, nc_precision, UNC_LOC_S, 'cumulative_total_net_inflow_lateral', '', 'Cumulative total net inflow via all laterals at each 1D node', 'm3', which_meshdim=1, jabndnd=jabndnd_)
            end if
         end if
         if (lnx1d > 0) then
            if (jamapS1Gradient > 0) then ! water level gradient
               ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, nc_precision, UNC_LOC_U, 'water_level_gradient', '', 'Water level gradient at each 1D flow link', '1', which_meshdim=1, jabndnd=jabndnd_)
            end if
         end if
         if (jamapNearField > 0) then
            ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nrfld, nc_precision, UNC_LOC_S3D, 'nrfld', 'nearfield_discharges', 'Nearfield related discharges', 'm3 s-1', jabndnd=jabndnd_)
         end if
         !
         ! END OF DEFINITION PART
         !
         ierr = nf90_enddef(mapids%ncid)
         if (ierr == NF90_EVARSIZE .and. unc_cmode /= NF90_NETCDF4) then
            call mess(LEVEL_ERROR, 'Error while writing map file. Probably model grid is too large for classic NetCDF format. Try setting [output] NcFormat = 4 in your MDU.')
         else if (ierr /= NF90_NOERR) then
            write (msgbuf, '(a,i0,a)') 'Error while writing map file. Error code: ', ierr, '.'
            call err_flush()
         end if

         if (janudge > 0 .and. jamapnudge > 0) then
!        output static nudging time
            workx = 0d0
            do k = 1, Ndx
               if (nudge_rate(k) > 0d0) then
                  workx(k) = 1d0 / nudge_rate(k)
               end if
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_time, UNC_LOC_S, workx, jabndnd=jabndnd_)
         end if

         if (nomba > 0) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mba(:), UNC_LOC_S, mbadef, jabndnd=jabndnd_)
         end if

         if (jased == 4 .and. stm_included) then
            do j = 1, stmpar%lsedtot
               ierr = nf90_put_var(mapids%ncid, mapids%id_frac_name, trim(stmpar%sedpar%namsed(j)), (/1, j/), (/len(trim(stmpar%sedpar%namsed(j))), 1/)) ! only write once
            end do
            if (stmpar%lsedsus > 0) then
               do j = 1, stmpar%lsedsus
                  ierr = nf90_put_var(mapids%ncid, mapids%id_susfrac_name, trim(stmpar%sedpar%namsed(j)), (/1, j/), (/len(trim(stmpar%sedpar%namsed(j))), 1/)) ! only write once
               end do
            end if
         end if
         !
         ! 1D cross sections
         if (ndx1d > 0 .and. stm_included) then
            if (stmpar%morpar%bedupd) then
               do i = 1, nCrs
                  ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_morCrsName, trim(network%crs%cross(i)%CSID), (/1, i/), (/len(trim(network%crs%cross(i)%CSID)), 1/)) ! only write once
               end do
            end if
         end if

         ! Enable the following when needed:
         ! if (jawritedebug) then
         !    ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_dbg1d  , nf90_double, UNC_LOC_U, 'debug1d', 'debug1d', 'debug1d', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_)
         !    !
         !    if (allocated(debugarr2d)) then
         !       ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg2d, nf90_double, UNC_LOC_S, 'debug2d', 'debug2d', 'debug2d', '-', dimids = (/ -2, mapids%id_tsp%id_sedtotdim,-1 /), jabndnd=jabndnd_) ! not CF
         !    endif
         !    !
         !    if (allocated(debugarr3d)) then
         !       !ierr = unc_def_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg3d, nf90_double, UNC_LOC_S, 'debug3d', 'debug3d', 'debug3d', '-', dimids = (/ -2, -1 /), jabndnd=jabndnd_) ! not CF
         !    endif
         ! endif

         if (timon) call timstop(handle_extra(71))

      end if
      ! End of writing time-independent flow geometry data.

      ! -- Start data writing (flow data) ------------------------
      if (timon) call timstrt("unc_write_map_filepointer_ugrid TIME write", handle_extra(72))

      mapids%id_tsp%idx_curtime = mapids%id_tsp%idx_curtime + 1 ! Increment time dimension index
      itim = mapids%id_tsp%idx_curtime

      ! Time
      ierr = nf90_put_var(mapids%ncid, mapids%id_time, tim, (/itim/))
      ierr = nf90_put_var(mapids%ncid, mapids%id_timestep, dts, (/itim/))

      if (timon) call timstop(handle_extra(72))
      if (timon) call timstrt("unc_write_map_filepointer_ugrid vars", handle_extra(73))
      if (jamapnumlimdt > 0) then
         ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdt) ! TODO: AvD: integer version of this routine
         call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
         numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_numlimdt, UNC_LOC_S, numlimdtdbl, jabndnd=jabndnd_)
         deallocate (numlimdtdbl)
      end if

      ! Time dependent grid layers
      if (kmx > 0 .and. jafullgridoutput > 0) then
         call realloc(work1d, ndkx, keepExisting=.false.)
         call realloc(work3d2, (/2, kmx, max(lnx, ndxndxi)/), keepExisting=.false., fill=dmiss)
         do kk = 1, ndxndxi
            call getkbotktop(kk, kb, kt)
            call getlayerindices(kk, nlayb, nrlay)
            do k = kb, kt
               work1d(k) = (zws(k) + zws(k - 1)) * 0.5d0 ! middle z-coord of this cell in this layer
               work3d2(1:2, k - kb + nlayb, kk) = zws(k - 1:k) ! vertical z-bounds of this cell in this layer
            end do
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc, UNC_LOC_S3D, work1d, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzw, UNC_LOC_W, zws, jabndnd=jabndnd_)
         if (jafullgridoutput == 2) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowelemzcc_bnd, UNC_LOC_S, work3d2, locdim=3, jabndnd=jabndnd_)
            !ierr = nf90_put_var(mapids%ncid, mapids%id_flowelemzcc_bnd(2), work3d2(1:2, 1:kmx, 1:ndxndxi), start=(/ 1, 1, 1, itim /), count=(/ 2, kmx, ndxndxi, 1 /))
            ! TODO: support this in 1D or 1D2D as well, via unc_put_var_map interfaces.
         end if

         call realloc(work1d, lnkx, keepExisting=.false., fill=dmiss)
         call realloc(work1d2, lnkx, keepExisting=.false., fill=dmiss)
         ! work3d2 already sufficiently allocated above.
         do LL = 1, lnx
            zwu0 = blup(LL) ! cached from latest sethu()
            call getLbotLtopmax(LL, Lb, Ltx)
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)

            do L = Lb, Ltx
               work1d(L) = zwu0 + .5d0 * (hu(L) + hu(L - 1))
               work3d2(1:2, L - Lb + nlaybL, LL) = (/zwu0 + hu(L - 1), zwu0 + hu(L)/) ! vertical z-bounds of this cell in this layer
            end do
            do L = Lb - 1, Ltx
               work1d2(L) = zwu0 + hu(L)
            end do
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu, UNC_LOC_U3D, work1d, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzwu, UNC_LOC_WU, work1d2, jabndnd=jabndnd_)
         if (jafullgridoutput == 2) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_flowlinkzu_bnd, UNC_LOC_U, work3d2, locdim=3, jabndnd=jabndnd_)
         end if
      end if

      ! Water level
      if (jamaps1 == 1) then
         ierr = write_array_with_dmiss_for_dry_cells_into_netcdf_file(mapids%ncid, mapids%id_tsp, mapids%id_s1, UNC_LOC_S, s1, jabndnd=jabndnd_)
      end if

      if (jamaps0 == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s0, UNC_LOC_S, s0, jabndnd=jabndnd_)
      end if

      if (jamapqin > 0 .and. jaqin > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qin, UNC_LOC_S, qin, jabndnd=jabndnd_)
      end if

      ! Water depth
      if (jamaphs == 1) then
         !ierr = nf90_inq_varid(mapids%ncid, 'mesh2d'//'_waterdepth', mapids%id_hs(2))
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hs, UNC_LOC_S, hs, jabndnd=jabndnd_)
      end if

      ! Evaporation
      if (jamapevap == 1) then
         if (jadhyd == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_potevap, UNC_LOC_S, PotEvap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_actevap, UNC_LOC_S, ActEvap, jabndnd=jabndnd_)
         end if
         if (jaevap == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_evap, UNC_LOC_S, evap, jabndnd=jabndnd_)
         end if
      end if

      ! Volumes
      if (jamapvol1 == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vol1, ilocS, vol1, jabndnd=jabndnd_)
      end if

      ! Flow areas
      if (jamapau == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au, iLocU, au, jabndnd=jabndnd_)
      end if

      if (jamapflowanalysis == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt, iLocS, negativeDepths, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_negdpt_cum, iLocS, negativeDepths_cum, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter, iLocS, noIterations, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_noiter_cum, iLocS, noIterations_cum, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep, iLocS, limitingTimestepEstimation, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_limtstep_cum, iLocS, limitingTimestepEstimation_cum, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_courant, iLocS, flowCourantNumber, jabndnd=jabndnd_)
      end if

      ! Velocities
      if (jamapu1 > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u1, iLocU, u1, 0d0, jabndnd=jabndnd_)
      end if
      if (jamaphu > 0) then
         ierr = write_array_with_dmiss_for_dry_faces_into_netcdf_file(mapids%ncid, mapids%id_tsp, mapids%id_hu, UNC_LOC_U, hu, jabndnd=jabndnd_)
      end if
      if (jamapu0 == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_u0, iLocU, u0, 0d0, jabndnd=jabndnd_)
      end if
      if (jamapdtcell == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dtcell, iLocS, dtcell, jabndnd=jabndnd_)
      end if

      if (jamapucvec == 1 .or. jamapucmag == 1 .or. jamapucqvec == 1) then
         workx = DMISS
         worky = DMISS
         call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulervel, jamapucmag)
         !
         if (jamapucvec == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucx, iLocS, workx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucy, iLocS, worky, jabndnd=jabndnd_)
         end if
         !
         if (jamapucmag == 1) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmag, iLocS, ucmag, jabndnd=jabndnd_)
         end if
         !
         if (kmx > 0) then
            call reconstructucz(0)
            if (jamapucvec == 1) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucz, UNC_LOC_S3D, ucz, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxa, UNC_LOC_S, ucx(1:ndxndxi), jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucya, UNC_LOC_S, ucy(1:ndxndxi), jabndnd=jabndnd_)
            end if

            if (jamapucmag == 1) then
               call realloc(work1d, ndkx, keepExisting=.false., fill=0d0)
               do k = 1, ndxndxi ! NOTE: this does not include Stokes drift, no Eulerian velocities here!
                  work1d(k) = sqrt(ucx(k)**2 + ucy(k)**2) ! TODO: this does not include vertical/w-component now.
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucmaga, UNC_LOC_S, work1d, jabndnd=jabndnd_)
            end if
         end if

         if (jamapucqvec == 1) then
            ! TODO: AvD/MN: consider removing entire loop and simply unc_put_var_map( ..., ucqx,..)
            if (kmx > 0) then
               do kk = 1, ndx
                  call getkbotktop(kk, kb, kt)
                  do k = kb, kt
                     workx(k) = ucxq(k)
                     worky(k) = ucyq(k)
                  end do
               end do
            else
               do kk = 1, ndx
                  workx(kk) = ucxq(kk)
                  worky(kk) = ucyq(kk)
               end do
            end if
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucxq, iLocS, workx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ucyq, iLocS, worky, jabndnd=jabndnd_)
         end if

      end if
      if (kmx > 0) then
         if (jamapww1 > 0) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ww1, UNC_LOC_W, ww1, jabndnd=jabndnd_)
         end if
         if (jamaprho > 0) then
            if (density_is_pressure_dependent()) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rho, UNC_LOC_S3D, rho, jabndnd=jabndnd_)
            else
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhop, UNC_LOC_S3D, rho, jabndnd=jabndnd_)
            end if
         end if
      end if

      if (jamapq1 == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1, iLocU, q1, 0d0, jabndnd=jabndnd_)
      end if

      if (jamapq1main == 1 .and. allocated(q1_main)) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1main, iLocU, q1_main, 0d0, jabndnd=jabndnd_)
      end if

      if (jamapfw == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fwel, UNC_LOC_U, map_fixed_weir_energy_loss, 0d0, jabndnd=jabndnd_)
      end if

      ! TIDAL TURBINES: Insert equivalent of wrturbine_cnst and wrturbine_time here

      if (kmx > 0) then
         if (jamapviu > 0) then
            ! For all flowlinks and layers add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
            ! Values for inactive layers are set to missing in function unc_put_var_map.
            call realloc(work1d, lnkx, keepExisting=.false.)
            do LL = 1, lnx
               if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
                  vicc = viusp(LL)
               else
                  vicc = vicouv
               end if
               call getLbotLtopmax(LL, Lb, Lt)
               do L = Lb, Lt
                  work1d(L) = viu(L) + vicc
               end do
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d, jabndnd=jabndnd_)
         end if

         if (jamapdiu > 0) then
            ! For all flowlinks and layers add user defined part (diusp(LL) or dicouv) to modeled part (viu(LL)/0.7).
            ! Values for inactive layers are set to missing in function unc_put_var_map.
            call realloc(work1d, lnkx, keepExisting=.false.)
            do LL = 1, lnx
               if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
                  dicc = diusp(LL)
               else
                  dicc = dicouv
               end if
               call getLbotLtopmax(LL, Lb, Lt)
               do L = Lb, Lt
                  work1d(L) = viu(L) / 0.7 + dicc
               end do
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d, jabndnd=jabndnd_)
         end if
      end if

      if (kmx == 0) then
         if (jamapviu > 0) then
            ! For all flowlinks add user defined part (viusp(LL) or vicouv) to modeled part (viu(LL)).
            call realloc(work1d, lnx, keepExisting=.false.)
            do LL = 1, lnx
               if (javiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
                  vicc = viusp(LL)
               else
                  vicc = vicouv
               end if
               work1d(LL) = viu(LL) + vicc
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_viu, iLocU, work1d, jabndnd=jabndnd_)
         end if

         if (jamapdiu > 0) then
            ! For all flowlinks add user defined part (diusp(LL) or dicouv) to modeled part (viu(LL)/0.7).
            call realloc(work1d, lnx, keepExisting=.false.)
            do LL = 1, lnx
               if (jadiusp == 1) then ! If horizontal eddy viscosity is spatially varying.
                  dicc = diusp(LL)
               else
                  dicc = dicouv
               end if
               work1d(LL) = viu(LL) / 0.7 + dicc
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diu, iLocU, work1d, jabndnd=jabndnd_)
         end if
      end if

      if (allocated(work1d)) then
         deallocate (work1d)
      end if

      ! Salinity
      if (jasal > 0 .and. jamapsal > 0) then
         do k = 1, ndkx
            sa1(k) = constituents(isalt, k)
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sa1, iLocS, sa1, jabndnd=jabndnd_)
      end if

      ! Temperature
      if (jatem > 0 .and. jamaptem > 0) then
         do k = 1, ndkx
            tem1(k) = constituents(itemp, k)
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tem1, iLocS, tem1, jabndnd=jabndnd_)
      end if

      if (jasecflow > 0 .and. jamapspir > 0) then
         if (kmx == 0) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spircrv, UNC_LOC_S, spircrv, jabndnd=jabndnd_)
         end if
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_spirint, UNC_LOC_S, spirint, jabndnd=jabndnd_)
      end if

      ! Constituents

!   The following is not stack-safe:
!   if (jamapconst > 0 .and. ITRA1 > 0) then
!      do j=ITRA1,ITRAN
!         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:,j), iLocS, constituents(j,:))
!      enddo
!   endif

!   The following is (almost) copied from unc_wite_map_filepointer
      if (jamapconst > 0 .and. ITRA1 > 0) then

         do j = ITRA1, ITRAN
            workx = DMISS ! For proper fill values in z-model runs.
            if (kmx > 0) then
!            3D
               do kk = 1, ndxndxi
                  call getkbotktop(kk, kb, kt)
                  do k = kb, kt
                     workx(k) = constituents(j, k)
                  end do
               end do
!             ierr = nf90_put_var(imapfile, mapids%id_const(:,j), work1(1:kmx,1:ndxndxi), (/ 1, 1, itim /), (/ kmx, ndxndxi, 1 /))
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:, j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
               !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
            else
               do kk = 1, NdxNdxi
                  workx(kk) = constituents(j, kk)
               end do
!             ierr = nf90_put_var(imapfile, id_const(iid,j), dum, (/ 1, itim /), (/ NdxNdxi, 1 /) )
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_const(:, j), UNC_LOC_S, workx, jabndnd=jabndnd_)
            end if
         end do
      end if

      ! Turbulence.
      if (jamaptur > 0 .and. kmx > 0) then
         if (iturbulencemodel >= 3) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_turkin1, UNC_LOC_WU, turkin1, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwwu, UNC_LOC_WU, vicwwu, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vicwws, UNC_LOC_W, vicwws, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tureps1, UNC_LOC_WU, tureps1, jabndnd=jabndnd_)
         end if
      end if

      !
      ! Sediment transport (via morphology module)
      !
      if ((jamapsed > 0 .and. jased > 0 .and. stm_included) .or. (jasubsupl > 0)) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mor_bl, UNC_LOC_S, bl, jabndnd=jabndnd_)
      end if

      if (jasubsupl > 0) then
         select case (ibedlevtyp)
         case (1)
            iloc = UNC_LOC_S
         case (2)
            iloc = UNC_LOC_U
         case (3, 4, 5, 6)
            iloc = UNC_LOC_CN
         end select
         do k = 1, size(subsupl)
            subsout(k) = subsupl(k) - subsupl_t0(k)
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_subsupl, iloc, subsout, jabndnd=jabndnd_)
      end if

      if (jamapz0 > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0c, UNC_LOC_U, z0ucur, jabndnd=jabndnd_) ! from setcfuhi
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_z0r, UNC_LOC_U, z0urou, jabndnd=jabndnd_) ! from tauwave, update_vp, or above
      end if

      if (jamapsed > 0 .and. jased > 0 .and. stm_included) then

         ! intermediate output for sediment formulas
         if (stmpar%morpar%moroutput%sedpar) then
            call realloc(toutput, ndx)
            do l = 1, stmpar%lsedtot
               do k = 1, stmpar%trapar%noutpar(l)
                  i = stmpar%trapar%ioutpar(k, l)
                  toutput = stmpar%trapar%outpar(i, :)
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedpar(:, k, l), UNC_LOC_S, toutput, jabndnd=jabndnd_)
               end do
            end do
         end if

         if (stmpar%lsedsus > 0) then
            if (kmx > 0) then
               call realloc(toutputx, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               !
               ! convert kmxsed to kmx administration
               if (itim == 1) then
                  toutputx = 1 ! set to bottom layer on first time step
               else
                  do l = 1, stmpar%lsedsus
                     do k = 1, ndx
                        kk = sedtra%kmxsed(k, l)
                        call getkbotktop(k, kb, kt)
                        found = 0
                        do kkk = kb, kt
                           found = found + 1
                           if (kkk == kk) exit ! meh...
                        end do
                        toutputx(k, l) = found
                     end do
                  end do
               end if
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kmxsed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
            end if
            !
            call realloc(toutputx, (/lnx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/lnx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedsus
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
               case (2)
                  rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
               end select
               toutputx(:, l) = sedtra%e_ssn(:, l) / rhol
               toutputy(:, l) = sedtra%e_sst(:, l) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ssn, UNC_LOC_U, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sst, UNC_LOC_U, toutputy, jabndnd=jabndnd_)
         end if
         if (stmpar%lsedtot > 0) then
            call realloc(toutputx, (/lnx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/lnx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedtot
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputx(:, l) = sedtra%e_sbn(:, l) / rhol
               toutputy(:, l) = sedtra%e_sbt(:, l) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbn, UNC_LOC_U, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbt, UNC_LOC_U, toutputy, jabndnd=jabndnd_)
         end if
         !
         if (stmpar%morpar%moroutput%uuuvvv) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uuu, UNC_LOC_S, sedtra%uuu, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vvv, UNC_LOC_S, sedtra%vvv, jabndnd=jabndnd_)
         end if
         !
         if (stmpar%lsedsus > 0) then

            call realloc(work3d, (/kmx, ndxndxi, stmpar%lsedsus/), keepExisting=.false., fill=dmiss)
            if (kmx > 0) then
               ! Concentrations
               do kk = 1, ndxndxi
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work3d(k - kb + nlayb, kk, :) = constituents(ISED1:ISEDN, k)
                  end do
               end do
               ierr = nf90_put_var(mapids%ncid, mapids%id_sedfrac(2), work3d(1:kmx, 1:ndxndxi, 1:stmpar%lsedsus), start=(/1, 1, 1, itim/), count=(/kmx, ndxndxi, stmpar%lsedsus, 1/))
               work3d = dmiss
               ! Settling velocity
               do kk = 1, ndxndxi
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work3d(k - kb + nlayb, kk, :) = mtd%ws(k, 1:stmpar%lsedsus)
                  end do
               end do
               ierr = nf90_put_var(mapids%ncid, mapids%id_ws(2), work3d(1:kmx, 1:ndxndxi, 1:stmpar%lsedsus), start=(/1, 1, 1, itim/), count=(/kmx, ndxndxi, stmpar%lsedsus, 1/))
            else
               call realloc(work1d_z, (/ndxndxi, stmpar%lsedsus/), keepExisting=.false., fill=dmiss)
               work1d_z = transpose(constituents(ISED1:ISEDN, :)) ! avoid array slice on stack
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedfrac, UNC_LOC_S, work1d_z, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ws, UNC_LOC_S, mtd%ws, jabndnd=jabndnd_)
            end if
!
            if (kmx == 0) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rsedeq, UNC_LOC_S, sedtra%rsedeq, jabndnd=jabndnd_)
            end if
!
            if (stmpar%morpar%moroutput%aks) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_aks, UNC_LOC_S, sedtra%aks, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rca, UNC_LOC_S, sedtra%rca, jabndnd=jabndnd_)
            end if
!
            if (stmpar%morpar%moroutput%sourcesink) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sourse, UNC_LOC_S, sedtra%sourse, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sinkse, UNC_LOC_S, sedtra%sinkse, jabndnd=jabndnd_)
            end if

            if (stmpar%morpar%moroutput%suvcor) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_scrn, UNC_LOC_U, sedtra%e_scrn, jabndnd=jabndnd_)
            end if
         end if
         !
         if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_e_dzdn, UNC_LOC_U, sedtra%e_dzdn, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_e_dzdt, UNC_LOC_U, sedtra%e_dzdt, jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%umod) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_umod, UNC_LOC_S, sedtra%umod, jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%zumod) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_zumod, UNC_LOC_S, sedtra%zumod, jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%ustar) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustar, UNC_LOC_S, sqrt(sedtra%ust2), jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%rawtransports) then
            if (stmpar%morpar%moroutput%sbcuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  toutputx(:, l) = sbcx_raw(:, l) / rhol
                  toutputy(:, l) = sbcy_raw(:, l) / rhol
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcx, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcy, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
            end if
            !
            if (stmpar%morpar%moroutput%sbwuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  toutputx(:, l) = sbwx_raw(:, l) / rhol
                  toutputy(:, l) = sbwy_raw(:, l) / rhol
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwx, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwy, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
            end if
            !
            if (stmpar%morpar%moroutput%sswuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  toutputx(:, l) = sswx_raw(:, l) / rhol
                  toutputy(:, l) = sswy_raw(:, l) / rhol
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswx, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswy, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
            end if
         end if
         !
         ! Get cell centre transport values
         call reconstructsedtransports()

         if (stmpar%morpar%moroutput%rawtransports) then
            if (stmpar%morpar%moroutput%sscuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedsus
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
                  case (2)
                     rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
                  end select
                  toutputx(:, l) = sedtra%sscx(:, sedtot2sedsus(l)) / rhol
                  toutputy(:, l) = sedtra%sscy(:, sedtot2sedsus(l)) / rhol
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscx, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscy, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
            end if
         end if

         if (stmpar%morpar%moroutput%sbcuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedtot
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputx(:, l) = sedtra%sbcx(:, l) / rhol
               toutputy(:, l) = sedtra%sbcy(:, l) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcx_reconstructed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbcy_reconstructed, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%sbwuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedtot
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputx(:, l) = sedtra%sbwx(:, l) / rhol
               toutputy(:, l) = sedtra%sbwy(:, l) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwx_reconstructed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbwy_reconstructed, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
         end if
!
         if (stmpar%morpar%moroutput%sswuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedtot
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputx(:, l) = sedtra%sswx(:, l) / rhol
               toutputy(:, l) = sedtra%sswy(:, l) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswx_reconstructed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sswy_reconstructed, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
         end if
         !
         if (stmpar%morpar%moroutput%sscuv) then
            call realloc(toutputx, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
            do l = 1, stmpar%lsedsus
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(sedtot2sedsus(l))
               case (2)
                  rhol = stmpar%sedpar%rhosol(sedtot2sedsus(l))
               end select
               toutputx(:, l) = sedtra%sscx(:, sedtot2sedsus(l)) / rhol
               toutputy(:, l) = sedtra%sscy(:, sedtot2sedsus(l)) / rhol
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscx_reconstructed, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sscy_reconstructed, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
         end if
         !
         if (stmpar%morpar%duneavalan) then
            ! Add avalanching fluxes to total transport for output
            call realloc(sxtotori, (/ndx, stmpar%lsedtot/), stat=ierr, keepExisting=.false., fill=0d0)
            call realloc(sytotori, (/ndx, stmpar%lsedtot/), stat=ierr, keepExisting=.false., fill=0d0)
            sxtotori = sedtra%sxtot
            sytotori = sedtra%sytot
            !
            do l = 1, stmpar%lsedtot
               do Lf = 1, lnx
                  sedtra%e_sbcn(Lf, l) = sedtra%e_sbcn(Lf, l) + avalflux(Lf, l) * wu_mor(Lf)
               end do
            end do
            call reconstructsedtransports()
         end if
         !
         call realloc(toutputx, (/ndxndxi, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
         call realloc(toutputy, (/ndxndxi, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
         do l = 1, stmpar%lsedtot
            select case (stmpar%morpar%moroutput%transptype)
            case (0)
               rhol = 1d0
            case (1)
               rhol = stmpar%sedpar%cdryb(l)
            case (2)
               rhol = stmpar%sedpar%rhosol(l)
            end select
            toutputx(1:ndxndxi, l) = (sedtra%sxtot(1:ndxndxi, l)) / rhol
            toutputy(1:ndxndxi, l) = (sedtra%sytot(1:ndxndxi, l)) / rhol
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxtot, UNC_LOC_S, toutputx, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sytot, UNC_LOC_S, toutputy, jabndnd=jabndnd_)
         !
         if (stmpar%morpar%duneavalan) then
            ! restore for timestep calculation
            sedtra%sxtot = sxtotori
            sedtra%sytot = sytotori
         end if
         !
         ! Time averaged transports, could probably be more concise...
         !morstarthyd = tstart_user + stmpar%morpar%tmor*tfac        ! seconds
         dmorft = stmpar%morpar%morft - stmpar%morpar%morft0 ! days since morstart
         dmorfs = dmorft * 86400.0d0 ! seconds
         mortime = stmpar%morpar%morft * 86400d0 ! seconds*morfac since tstart_user
         if (stmpar%morpar%hydrt > stmpar%morpar%hydrt0) then
            moravg = dmorft / (stmpar%morpar%hydrt - stmpar%morpar%hydrt0)
         else
            moravg = 0d0
         end if
         !
         ierr = nf90_put_var(mapids%ncid, mapids%id_morfac, moravg, (/itim/))
         ierr = nf90_put_var(mapids%ncid, mapids%id_morft, mortime, (/itim/))
         !
         if (stmpar%morpar%moroutput%cumavg) then
            ! Bedload components
            call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
            if (dmorft > 0d0) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhodt = dmorfs
                  case (1)
                     rhodt = stmpar%sedpar%cdryb(l) * dmorfs
                  case (2)
                     rhodt = stmpar%sedpar%rhosol(l) * dmorfs
                  end select
                  toutputx(:, l) = sedtra%sbxcum(:, l) / rhodt
                  toutputy(:, l) = sedtra%sbycum(:, l) / rhodt
               end do
            else
               toutputx = 0d0
               toutputy = 0d0
            end if
            ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxndxi, :), start=(/1, 1, itim/), count=(/ndxndxi, stmpar%lsedtot, 1/))
            ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxndxi, :), start=(/1, 1, itim/), count=(/ndxndxi, stmpar%lsedtot, 1/))
            !
            ! Suspended load
            if (dmorft > 0d0) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhodt = dmorfs
                  case (1)
                     rhodt = stmpar%sedpar%cdryb(l) * dmorfs
                  case (2)
                     rhodt = stmpar%sedpar%rhosol(l) * dmorfs
                  end select
                  toutputx(:, l) = sedtra%ssxcum(:, l) / rhodt
                  toutputy(:, l) = sedtra%ssycum(:, l) / rhodt
               end do
            else
               toutputx = 0d0
               toutputy = 0d0
            end if
            ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxndxi, :), start=(/1, 1, itim/), count=(/ndxndxi, stmpar%lsedtot, 1/))
            ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxndxi, :), start=(/1, 1, itim/), count=(/ndxndxi, stmpar%lsedtot, 1/))
         else
            if (time_map >= ti_mape) then ! to check, last timestep?
               ierr = nf90_put_var(mapids%ncid, mapids%id_sedavgtim, mortime, (/1/))
               ! Bedload components
               call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               if (dmorft > 0d0) then
                  do l = 1, stmpar%lsedtot
                     select case (stmpar%morpar%moroutput%transptype)
                     case (0)
                        rhodt = dmorfs
                     case (1)
                        rhodt = stmpar%sedpar%cdryb(l) * dmorfs
                     case (2)
                        rhodt = stmpar%sedpar%rhosol(l) * dmorfs
                     end select
                     toutputx(:, l) = sedtra%sbxcum(:, l) / rhodt
                     toutputy(:, l) = sedtra%sbycum(:, l) / rhodt
                  end do
               else
                  toutputx = 0d0
                  toutputy = 0d0
               end if
               ierr = nf90_put_var(mapids%ncid, mapids%id_sbxcum(2), toutputx(1:ndxndxi, :), start=(/1, 1/), count=(/ndxndxi, stmpar%lsedtot/))
               ierr = nf90_put_var(mapids%ncid, mapids%id_sbycum(2), toutputy(1:ndxndxi, :), start=(/1, 1/), count=(/ndxndxi, stmpar%lsedtot/))
               !
               ! Suspended load
               if (dmorft > 0d0) then
                  do l = 1, stmpar%lsedtot
                     select case (stmpar%morpar%moroutput%transptype)
                     case (0)
                        rhodt = dmorfs
                     case (1)
                        rhodt = stmpar%sedpar%cdryb(l) * dmorfs
                     case (2)
                        rhodt = stmpar%sedpar%rhosol(l) * dmorfs
                     end select
                     toutputx(:, l) = sedtra%ssxcum(:, l) / rhodt
                     toutputy(:, l) = sedtra%ssycum(:, l) / rhodt
                  end do
               else
                  toutputx = 0d0
                  toutputy = 0d0
               end if
               ierr = nf90_put_var(mapids%ncid, mapids%id_ssxcum(2), toutputx(1:ndxndxi, :), start=(/1, 1/), count=(/ndxndxi, stmpar%lsedtot/))
               ierr = nf90_put_var(mapids%ncid, mapids%id_ssycum(2), toutputy(1:ndxndxi, :), start=(/1, 1/), count=(/ndxndxi, stmpar%lsedtot/))
            end if
         end if
!
         select case (stmpar%morlyr%settings%iunderlyr)
         case (1)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bodsed, UNC_LOC_S, stmpar%morlyr%state%bodsed, locdim=2, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dpsed, UNC_LOC_S, stmpar%morlyr%state%dpsed, jabndnd=jabndnd_)
         case (2)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_msed, UNC_LOC_S, stmpar%morlyr%state%msed, locdim=3, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thlyr, UNC_LOC_S, stmpar%morlyr%state%thlyr, locdim=2, jabndnd=jabndnd_)
            !
            if (.not. allocated(frac)) allocate (frac(stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndx))
            frac = -999d0
            if (stmpar%morlyr%settings%iporosity == 0) then
               dens => stmpar%sedpar%cdryb
            else
               dens => stmpar%sedpar%rhosol
            end if
            do k = 1, stmpar%morlyr%settings%nlyr
               do nm = 1, ndxndxi
                  if (stmpar%morlyr%state%thlyr(k, nm) > 0.0_fp) then
                     do l = 1, stmpar%lsedtot
                        frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm) / (dens(l) * stmpar%morlyr%state%svfrac(k, nm) * &
                                                                               stmpar%morlyr%state%thlyr(k, nm))
                     end do
                  else
                     frac(:, k, nm) = 0d0
                  end if
               end do
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_lyrfrac, UNC_LOC_S, frac, locdim=3, jabndnd=jabndnd_)
            !
            if (stmpar%morlyr%settings%iporosity > 0) then
               if (.not. allocated(poros)) allocate (poros(1:stmpar%morlyr%settings%nlyr, 1:ndx))
               poros = 1d0 - stmpar%morlyr%state%svfrac
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_poros, UNC_LOC_S, poros, locdim=2, jabndnd=jabndnd_)
            end if
            !
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_preload, UNC_LOC_S, stmpar%morlyr%state%preload, locdim=2, jabndnd=jabndnd_)
         case default
            ! do nothing
         end select
         if (stmpar%morlyr%settings%morlyrnum%track_mass_shortage) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sedshort, UNC_LOC_S, stmpar%morlyr%state%sedshort, locdim=2, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%taub) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taub, UNC_LOC_S, sedtra%taub, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%taurat) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taurat, UNC_LOC_S, sedtra%taurat, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%dm) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dm, UNC_LOC_S, sedtra%dm, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%dg) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dg, UNC_LOC_S, sedtra%dg, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%dgsd) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dgsd, UNC_LOC_S, sedtra%dgsd, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%percentiles) then
            do l = 1, stmpar%morpar%nxx
               call realloc(toutput, ndx, keepExisting=.false., fill=-999d0)
               toutput = sedtra%dxx(1:ndx, l)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dxx(l, :), UNC_LOC_S, toutput, jabndnd=jabndnd_)
            end do
         end if
         if (stmpar%morpar%moroutput%frac) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_frac, UNC_LOC_S, sedtra%frac, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%mudfrac) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mudfrac, UNC_LOC_S, sedtra%mudfrac, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%sandfrac) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sandfrac, UNC_LOC_S, sedtra%sandfrac, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%fixfac) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_fixfac, UNC_LOC_S, sedtra%fixfac, jabndnd=jabndnd_)
         end if
         if (stmpar%morpar%moroutput%hidexp) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hidexp, UNC_LOC_S, sedtra%hidexp, jabndnd=jabndnd_)
         end if
         !
         if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
            do l = 1, stmpar%lsedsus
               call realloc(toutput, ndx, keepExisting=.false., fill=-999d0)
               toutput = stmpar%morpar%flufflyr%mfluff(l, 1:ndx)
               ! ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp  , mapids%id_mfluff , UNC_LOC_S, stmpar%morpar%flufflyr%mfluff)
               ierr = nf90_put_var(mapids%ncid, mapids%id_mfluff(2), toutput(1:ndxndxi), start=(/1, l, itim/), count=(/ndxndxi, 1, 1/))
            end do
         end if
         !
         if (ndx1d > 0 .and. stm_included) then
            if (stmpar%morpar%bedupd) then
               if (allocated(work1d_z)) then
                  deallocate (work1d_z)
               end if
               if (allocated(work1d_n)) then
                  deallocate (work1d_n)
               end if
               allocate (work1d_z(jmax, nCrs), work1d_n(jmax, nCrs))
               work1d_z = dmiss
               work1d_n = dmiss
               do i = 1, nCrs
                  pCS => network%crs%cross(i)%tabdef
                  do j = 1, pCS%levelscount
                     work1d_z(j, i) = pCS%height(j)
                     work1d_n(j, i) = pCS%flowwidth(j)
                  end do
               end do
               ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsz(1), work1d_z(1:jmax, 1:nCrs), start=(/1, 1, mapids%id_tsp%idx_curtime/), count=(/jmax, nCrs, 1/))
               ierr = nf90_put_var(mapids%ncid, mapids%id_tsp%id_flowelemcrsn(1), work1d_n(1:jmax, 1:nCrs), start=(/1, 1, mapids%id_tsp%idx_curtime/), count=(/jmax, nCrs, 1/))
            end if
            if (stmpar%morpar%moroutput%blave) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_blave, UNC_LOC_S, bl_ave(ndx2d + 1:ndxndxi), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%bamor) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bamor, UNC_LOC_S, ba_mor(ndx2d + 1:ndxndxi), jabndnd=jabndnd_)
            end if
            if (stmpar%morpar%moroutput%wumor) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wumor, UNC_LOC_U, wu_mor, jabndnd=jabndnd_)
            end if
         end if
      end if
      !
      ! BEDFORMS
      !
      if (bfmpar%lfbedfrmout) then
         if (bfmpar%lfbedfrm) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_duneheight, UNC_LOC_S, bfmpar%duneheight(1:ndxndxi), jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dunelength, UNC_LOC_S, bfmpar%dunelength(1:ndxndxi), jabndnd=jabndnd_)
         end if
         !
         if (bfmpar%lfbedfrmrou) then
            if (.not. allocated(rks)) then
               allocate (rks(1:ndx))
               rks = 0d0
            end if
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksr, UNC_LOC_S, bfmpar%rksr(1:ndxndxi), jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksmr, UNC_LOC_S, bfmpar%rksmr(1:ndxndxi), jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ksd, UNC_LOC_S, bfmpar%rksd(1:ndxndxi), jabndnd=jabndnd_)

            do nm = 1, ndxndxi
               rks(nm) = sqrt(bfmpar%rksr(nm)**2 + bfmpar%rksmr(nm)**2 + bfmpar%rksd(nm)**2)
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ks, UNC_LOC_S, rks(1:ndxndxi), jabndnd=jabndnd_)
         end if
      end if

      ! Sediment transport (via own built-in sed)
      if (jamapsed > 0 .and. jased > 0 .and. .not. stm_included) then
         do j = 1, mxgr
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sed(:, j), UNC_LOC_S, sed(j, :), jabndnd=jabndnd_) ! ,  (/ 1, 1, itim /), (/ mxgr, ndxndxi, 1 /))
         end do
         if (jaceneqtr == 1) then
            do j = 1, mxgr
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ero(:, j), UNC_LOC_S, grainlay(j, :), jabndnd=jabndnd_)
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_bl, UNC_LOC_S, bl, jabndnd=jabndnd_)
         else
            do j = 1, mxgr
               ierr = unc_put_var_map_nodes(mapids%ncid, mapids%id_tsp, mapids%id_ero(:, j), grainlay(j, :), jabndnd_)
            end do
            ierr = unc_put_var_map_nodes(mapids%ncid, mapids%id_tsp, mapids%id_zk, zk, jabndnd_)
         end if

         ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
         ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
         ! (both length numk), whereas ero itself is shorter than numk.
      end if

      ! Meteo forcings
      if (jawind > 0) then
         allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
         if (ierr /= 0) call aerr('windx/windy', ierr, ndxndxi)

         if (jamapwind > 0) then
            call linktonode2(wx, wy, windx, windy, ndxndxi)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windx, UNC_LOC_S, windx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windy, UNC_LOC_S, windy, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windxu, UNC_LOC_U, wx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windyu, UNC_LOC_U, wy, jabndnd=jabndnd_)
         end if

         if (jamapwindstress > 0) then
            call linktonode2(wdsu_x, wdsu_y, windx, windy, ndxndxi)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressx, UNC_LOC_S, windx, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_windstressy, UNC_LOC_S, windy, jabndnd=jabndnd_)
         end if

         deallocate (windx, windy, stat=ierr)

      end if

      if (ja_airdensity + ja_computed_airdensity > 0 .and. jamap_airdensity > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_airdensity, UNC_LOC_S, airdensity, jabndnd=jabndnd_)
      end if

      ! Rain
      if (jamaprain > 0 .and. jarain /= 0) then
         call realloc(scaled_rain, ndx, keepExisting=.false., fill=dmiss)
         do n = 1, ndxndxi
            scaled_rain(n) = rain(n) * bare(n) / ba(n) * 1d-3 / (24d0 * 3600d0) ! mm/day->(m3/s / m2) Average actual rainfall rate on grid cell area (maybe zero bare).
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rain, UNC_LOC_S, scaled_rain, jabndnd=jabndnd_)
         deallocate (scaled_rain)
      end if

      ! Interception
      if (jamapicept > 0 .and. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_icepths, UNC_LOC_S, InterceptHs, jabndnd=jabndnd_)
      end if

      if (jamapwind > 0 .and. japatm > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_patm, UNC_LOC_S, patm, jabndnd=jabndnd_)
      end if

      if (ice_mapout) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_af, UNC_LOC_S, ice_af, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_h, UNC_LOC_S, ice_h, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_p, UNC_LOC_S, ice_p, jabndnd=jabndnd_)
         if (ja_icecover == ICECOVER_SEMTNER) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ice_t, UNC_LOC_S, ice_t, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_h, UNC_LOC_S, snow_h, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_snow_t, UNC_LOC_S, snow_t, jabndnd=jabndnd_)
         end if
      end if

      ! Heat flux models
      if (jamapheatflux > 0 .and. jatem > 1) then ! here less verbose

         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tair, UNC_LOC_S, Tair, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rhum, UNC_LOC_S, Rhum, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_clou, UNC_LOC_S, Clou, jabndnd=jabndnd_)

         if (jatem == 5) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qsun, UNC_LOC_S, Qsunmap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qeva, UNC_LOC_S, Qevamap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qcon, UNC_LOC_S, Qconmap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qlong, UNC_LOC_S, Qlongmap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfreva, UNC_LOC_S, Qfrevamap, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qfrcon, UNC_LOC_S, Qfrconmap, jabndnd=jabndnd_)
         end if
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qtot, UNC_LOC_S, Qtotmap, jabndnd=jabndnd_)
      end if

      if (jamapwav > 0) then
         ! TO DO JRE: fix dit voor offline wave koppeling
         if (flowWithoutWaves) then ! Check the external forcing wave quantities and their associated arrays
            if (jamapwav_hwav > 0 .and. allocated(hwav)) then
               if (jamapsigwav == 0) then
                  wavfac = 1d0
               else
                  wavfac = sqrt(2d0)
               end if
               if (allocated(wa)) then
                  deallocate (wa, stat=ierr)
               end if
               allocate (wa(1:ndx), stat=ierr)
               wa = wavfac * hwav
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, UNC_LOC_S, wa, jabndnd=jabndnd_)
            end if
            if (jamapwav_twav > 0 .and. allocated(twav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav, UNC_LOC_S, twav, jabndnd=jabndnd_)
            end if
            if (jamapwav_phiwav > 0 .and. allocated(phiwav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_phiwav, UNC_LOC_S, phiwav, jabndnd=jabndnd_)
            end if
            if (jamapwav_sxwav > 0 .and. allocated(sxwav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav, UNC_LOC_S, sxwav, jabndnd=jabndnd_)
            end if
            if (jamapwav_sywav > 0 .and. allocated(sywav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav, UNC_LOC_S, sywav, jabndnd=jabndnd_)
            end if
            if (jamapwav_sbxwav > 0 .and. allocated(sbxwav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxwav, UNC_LOC_S, sbxwav, jabndnd=jabndnd_)
            end if
            if (jamapwav_sbywav > 0 .and. allocated(sbywav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbywav, UNC_LOC_S, sbywav, jabndnd=jabndnd_)
            end if
            if (jamapwav_mxwav > 0 .and. allocated(mxwav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mxwav, UNC_LOC_S, mxwav, jabndnd=jabndnd_)
            end if
            if (jamapwav_mywav > 0 .and. allocated(mywav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_mywav, UNC_LOC_S, mywav, jabndnd=jabndnd_)
            end if
            if (jamapwav_dsurf > 0 .and. allocated(dsurf)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dsurf, UNC_LOC_S, dsurf, jabndnd=jabndnd_)
            end if
            if (jamapwav_dwcap > 0 .and. allocated(dwcap)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dwcap, UNC_LOC_S, dwcap, jabndnd=jabndnd_)
            end if
            if (jamapwav_distot > 0 .and. allocated(distot)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_distot, UNC_LOC_S, distot, jabndnd=jabndnd_)
            end if
            if (jamapwav_uorb > 0 .and. allocated(uorbwav)) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb, UNC_LOC_S, uorbwav, jabndnd=jabndnd_)
            end if
         else ! flowWithoutWaves
            ! JRE - XBeach
            if (jawave == 4) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_E, UNC_LOC_S, E, jabndnd=jabndnd_)
               if (roller > 0) then
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_R, UNC_LOC_S, R, jabndnd=jabndnd_)
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_DR, UNC_LOC_S, DR, jabndnd=jabndnd_)
               end if
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_D, UNC_LOC_S, D, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Df, UNC_LOC_S, Df, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxx, UNC_LOC_S, Sxx, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Syy, UNC_LOC_S, Syy, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Sxy, UNC_LOC_S, Sxy, jabndnd=jabndnd_)

               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sigmwav, UNC_LOC_S, sigmwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cwav, UNC_LOC_S, cwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cgwav, UNC_LOC_S, cgwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_kwav, UNC_LOC_S, kwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nwav, UNC_LOC_S, nwav, jabndnd=jabndnd_)

               !if (windmodel.eq.0) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_l1, UNC_LOC_S, L1, jabndnd=jabndnd_)
               !elseif ( (windmodel.eq.1) .and. (jawsource.eq.1 ) ) then
               !   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwE      , UNC_LOC_S, SwE, jabndnd=jabndnd_)
               !   ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_SwT      , UNC_LOC_S, SwT, jabndnd=jabndnd_)
               !endif

               ierr = nf90_put_var(mapids%ncid, mapids%id_ctheta(2), ctheta(:, 1:ndxndxi), start=(/1, 1, itim/), count=(/ntheta, ndxndxi, 1/))
            end if

            ! JRE to do Offline wave
            if ((jawave == 3 .or. jawave == 4) .and. kmx > 0) then
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sxwav, UNC_LOC_S, sxwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sywav, UNC_LOC_S, sywav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbxwav, UNC_LOC_S, sbxwav, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sbywav, UNC_LOC_S, sbywav, jabndnd=jabndnd_)
            end if

            if (jawave > 0) then
               if (jamapsigwav == 0) then
                  wavfac = 1d0
               else
                  wavfac = sqrt(2d0)
               end if
               if (allocated(wa)) then
                  deallocate (wa, stat=ierr)
               end if
               allocate (wa(1:ndx), stat=ierr)
               wa = wavfac * hwav
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_hwav, UNC_LOC_S, wa, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_uorb, UNC_LOC_S, uorb, jabndnd=jabndnd_)

               wa = modulo(270d0 - phiwav, 360d0)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_thetamean, UNC_LOC_S, wa, jabndnd=jabndnd_)
               deallocate (wa)

               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_twav, UNC_LOC_S, twav)

               call realloc(ust_x, ndkx, keepExisting=.false.)
               call realloc(ust_y, ndkx, keepExisting=.false.)
               call reconstruct_cc_stokesdrift(ndkx, ust_x, ust_y)

               ! then write:
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokes, iLocS, ust_x, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokes, iLocS, ust_y, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_ustokeslink, iLocU, ustokes, jabndnd=jabndnd_)
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vstokeslink, iLocU, vstokes, jabndnd=jabndnd_)

               ! Wave forces
               if (jawave == 3 .or. jawave == 4) then
                  call realloc(windx, ndkx, keepExisting=.false., fill=0d0) ! reuse scratch wind arrays, ust_x, y still needed for tausx,y
                  call realloc(windy, ndkx, keepExisting=.false., fill=0d0)
                  call realloc(wavout, lnkx, keepExisting=.false., fill=0d0)
                  call realloc(wavout2, lnkx, keepExisting=.false., fill=0d0)
                  wavout = 0d0; wavout2 = 0d0
                  if (kmx == 0) then
                     do L = 1, lnx
                        k1 = ln(1, L); k2 = ln(2, L)
                        windx(k1) = windx(k1) + wcx1(L) * wavfu(L) * hu(L) * rhomean
                        windx(k2) = windx(k2) + wcx2(L) * wavfu(L) * hu(L) * rhomean
                        windy(k1) = windy(k1) + wcy1(L) * wavfu(L) * hu(L) * rhomean
                        windy(k2) = windy(k2) + wcy2(L) * wavfu(L) * hu(L) * rhomean
                        wavout(L) = wavfu(L) * hu(L) * rhomean ! stack
                        wavout2(L) = wavfv(L) * hu(L) * rhomean
                     end do
                  else
                     do L = 1, lnx
                        call getLbotLtop(L, Lb, Lt)
                        if (Lt < Lb) cycle
                        do LL = Lb, Lt
                           k1 = ln(1, LL); k2 = ln(2, LL)
                           windx(k1) = windx(k1) + wcx1(L) * wavfu(LL) * hu(L) * rhomean ! consider rhoL here
                           windx(k2) = windx(k2) + wcx2(L) * wavfu(LL) * hu(L) * rhomean
                           windy(k1) = windy(k1) + wcy1(L) * wavfu(LL) * hu(L) * rhomean
                           windy(k2) = windy(k2) + wcy2(L) * wavfu(LL) * hu(L) * rhomean
                           wavout(LL) = wavfu(LL) * hu(L) * rhomean ! stack
                           wavout2(LL) = wavfv(LL) * hu(L) * rhomean
                        end do
                     end do
                  end if
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fx, iLocS, windx, jabndnd=jabndnd_)
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fy, iLocS, windy, jabndnd=jabndnd_)
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fxlink, iLocU, wavout, jabndnd=jabndnd_)
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_Fylink, iLocU, wavout2, jabndnd=jabndnd_)
                  deallocate (wavout, wavout2)
               end if
            end if
         end if ! flowWithoutWaves
      end if

      ! Bed shear stress and roughness
      !
      ! Tau current and Chezy
      ! The "logic":
      ! - If no waves:
      !     + Bed shear stress is derived from current-only u* in gettaus (jawaveswartdelwaq=0);
      !     + Bed shear stress for morphology from settaubxu_nowave, or Soulsby-Clarke, filled in array sedtra%taub in fm_erosed.
      ! - If waves present:
      !  * for fetch models (jawave 1,2):
      !     + taus depends on jawaveswartdelwaq;
      !     + taus derived from taubxu (jawaveswartdelwaq==2) calculated in getustbcfuhi (3D), in 2D in tauwave().
      !  * for swan etc (jawave>2):
      !     + taus for output are calculated in gettauswave(), based on jawaveswartdelwaq:
      !        * 0: taus based on soulsby wave-current formulas taubu's
      !        * 1: taus linear sum like gettau2
      !        * 2: taus = sedtra%taub if sediment included, otherwise based on taubxu from wave shear stress subroutines

      !
      if (jamaptaucurrent > 0 .or. jamap_chezy_elements > 0 .or. jamap_chezy_links > 0) then
         if (jawave == 0) then ! Else, get taus from subroutine tauwave (taus = f(taucur,tauwave))
            call gettaus(1, 1)
            workx = DMISS; worky = DMISS
            if (kmx == 0) then
               do k = 1, ndx ! stack
                  workx(k) = taus(k) * ucx(k) / max(hypot(ucx(k), ucy(k)), 1d-4) ! could use ucmag, but not guaranteed to exist
                  worky(k) = taus(k) * ucy(k) / max(hypot(ucx(k), ucy(k)), 1d-4)
               end do
            else
               do k = 1, ndx
                  call getkbotktop(k, kb, kt)
                  ux = ucx(kb); uy = ucy(kb)
                  um = max(hypot(ux, uy), 1d-4)
                  workx(k) = taus(k) * ux / um
                  worky(k) = taus(k) * uy / um
               end do
            end if
         else if (jamap_chezy_links > 0) then
            call gettaus(2, 1) ! Only update czs
         end if

         if (jawave > 0 .and. .not. flowWithoutWaves) then
            call gettauswave(jawaveswartdelwaq)
         end if
      end if

      if (jamap_chezy_links > 0) then
         do LL = 1, lnx
            if (frcu(LL) > 0d0) then
               call getcz(hu(LL), frcu(LL), ifrcutp(LL), czu(LL), LL) ! in gettaus czu is calculated but not stored
            end if
         end do
      end if
      !
      if (jamaptaucurrent > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausx, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausy, UNC_LOC_S, worky(1:ndx), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_taus, UNC_LOC_S, taus, jabndnd=jabndnd_)
         if (stm_included) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tausmax, UNC_LOC_S, sedtra%taub, jabndnd=jabndnd_) ! sedtra%taub=reconstruction of tausmax, or Soulsby-Clarke
         end if ! JRE+BJ to do: keep this one, or through moroutput
      end if

      if (jatidep > 0 .and. jamaptidep == 1) then
         if (jaselfal == 0) then
            do k = 1, Ndx
               workx(k) = tidep(1, k)
            end do
         else ! write potential without SAL and SAL potential
            do k = 1, Ndx
               workx(k) = tidep(1, k) - tidep(2, k)
            end do
         end if
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_tidep, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
      end if
      if (jaselfal > 0 .and. jamapselfal == 1) then
         do k = 1, Ndx
            workx(k) = tidep(2, k)
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_salp, UNC_LOC_S, workx(1:ndx), jabndnd=jabndnd_)
      end if

      if (jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_inttidesdiss, UNC_LOC_S, DissInternalTidesPerArea(1:ndx), jabndnd=jabndnd_)
      end if

      if (jamap_chezy_elements > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czs, UNC_LOC_S, czs, jabndnd=jabndnd_)
      end if
      if (jamap_chezy_links > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_czu, UNC_LOC_U, czu, jabndnd=jabndnd_)
      end if
      if (jamap_chezy_input > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfu, UNC_LOC_U, frcu, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfutyp, UNC_LOC_U, ifrcutp, jabndnd=jabndnd_)
      end if

      ! Roughness from trachytopes
      if (jamaptrachy > 0 .and. jatrt == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cftrt, UNC_LOC_L, cftrt(:, 2), jabndnd=jabndnd_)
      end if

      ! Calibration factor for roughness from trachytopes
      if (jamapcali > 0 .and. jacali == 1) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_cfcl, UNC_LOC_L, cfclval, jabndnd=jabndnd_)
      end if

      ! JRE debug variables
      if (jawritedebug /= 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg1d, UNC_LOC_U, debugarr1d(1:lnx), jabndnd=jabndnd_)

         if (allocated(debugarr2d)) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg2d, UNC_LOC_S, debugarr2d(1:ndxndxi, :), jabndnd=jabndnd_)
         end if

         if (allocated(debugarr3d)) then
            !ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_dbg3d, UNC_LOC_L, cfclval, jabndnd=jabndnd_)
         end if

      end if

      ! water quality bottom variables
      if (numwqbots > 0) then
         do j = 1, numwqbots
            do k = 1, ndxndxi
               call getkbotktop(k, kb, kt)
               workx(k) = wqbot(j, kb)
            end do
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb(:, j), UNC_LOC_S, workx(1:ndxndxi), jabndnd=jabndnd_)
            if (jamapwqbot3d == 1) then
!         also write 3D
               do kk = 1, ndxndxi
                  call getkbotktop(kk, kb, kt)
                  do k = kb, kt
                     workx(k) = wqbot(j, k)
                  end do
               end do
               ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqb3d(:, j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
            end if
         end do
      end if

      ! WAQ output
      if (jawaqproc > 0) then
         do j = 1, noout_map
            if (outvar(j) > 0) then
               workx = DMISS ! For proper fill values in z-model runs.
               if (kmx > 0) then
!               3D
                  do kk = 1, ndxndxi
                     call getkbotktop(kk, kb, kt)
                     do k = kb, kt
                        workx(k) = waqoutputs(j, k - kbx + 1)
                     end do
                  end do
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:, j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
               else
!               2D
                  do kk = 1, NdxNdxi
                     workx(kk) = waqoutputs(j, kk)
                  end do
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_waq(:, j), UNC_LOC_S, workx, jabndnd=jabndnd_)
               end if
            end if
         end do
         do j = 1, noout_statt
            jj = noout_user + j
            if (outvar(jj) > 0) then
               workx = DMISS ! For proper fill values in z-model runs.
               if (kmx > 0) then
!               3D
                  do kk = 1, ndxndxi
                     call getkbotktop(kk, kb, kt)
                     do k = kb, kt
                        workx(k) = waqoutputs(jj, k - kbx + 1)
                     end do
                  end do
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:, j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
               else
!               2D
                  do kk = 1, NdxNdxi
                     workx(kk) = waqoutputs(jj, kk)
                  end do
                  ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqst(:, j), UNC_LOC_S, workx, jabndnd=jabndnd_)
               end if
            end if
         end do
         if (comparereal(tim, ti_mape, eps10) == 0) then
            do j = 1, noout_state
               jj = noout_user + noout_statt + j
               if (outvar(jj) > 0) then
                  workx = DMISS ! For proper fill values in z-model runs.
                  if (kmx > 0) then
!                  3D
                     do kk = 1, ndxndxi
                        call getkbotktop(kk, kb, kt)
                        do k = kb, kt
                           workx(k) = waqoutputs(jj, k - kbx + 1)
                        end do
                     end do
                     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:, j), UNC_LOC_S3D, workx, jabndnd=jabndnd_)
                  else
!                  2D
                     do kk = 1, NdxNdxi
                        workx(kk) = waqoutputs(jj, kk)
                     end do
                     ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wqse(:, j), UNC_LOC_S, workx, jabndnd=jabndnd_)
                  end if
               end if
            end do
         end if
      end if

      if (janudge > 0 .and. jamapnudge > 0) then
!    nudging
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_tem, UNC_LOC_S3D, nudge_tem, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_sal, UNC_LOC_S3D, nudge_sal, jabndnd=jabndnd_)

         workx = DMISS
         do k = 1, ndkx
            if (nudge_tem(k) /= DMISS) then
               workx(k) = nudge_tem(k) - constituents(itemp, k)
            end if
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dtem, UNC_LOC_S3D, workx, jabndnd=jabndnd_)

         workx = DMISS
         do k = 1, ndkx
            if (nudge_tem(k) /= DMISS) then
               workx(k) = nudge_sal(k) - constituents(isalt, k)
            end if
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nudge_Dsal, UNC_LOC_S3D, workx, jabndnd=jabndnd_)
      end if

      if (javeg > 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_rnveg, UNC_LOC_S, rnveg, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_diaveg, UNC_LOC_S, diaveg, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_veg_stemheight, UNC_LOC_S, stemheight, jabndnd=jabndnd_)
      end if

      if (ndxi - ndx2d > 0 .and. jamapPure1D_debug /= 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_adve, UNC_LOC_U, adve(:), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_advi, UNC_LOC_U, advi(:), jabndnd=jabndnd_)
      end if

      if (ndxi - ndx2d > 0 .and. jaPure1D >= 3 .and. jamapPure1D_debug /= 0) then
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_1, UNC_LOC_U, q1d(1, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_q1d_2, UNC_LOC_U, q1d(2, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_volu1d, UNC_LOC_U, volu1D(:), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_1, UNC_LOC_U, au1d(1, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_au1d_2, UNC_LOC_U, au1d(2, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_1, UNC_LOC_U, wu1d(1, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_wu1d_2, UNC_LOC_U, wu1d(2, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_1, UNC_LOC_U, sar1d(1, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_sar1d_2, UNC_LOC_U, sar1d(2, :), jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_mom_1d, UNC_LOC_S, alpha_mom_1d, jabndnd=jabndnd_)
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_alpha_ene_1d, UNC_LOC_S, alpha_ene_1d, jabndnd=jabndnd_)
      end if

      if (ndxi - ndx2d > 0 .and. network%loaded) then
         if (jamapTimeWetOnGround > 0) then ! Cumulative time water above ground level
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_timewetground, UNC_LOC_S, time_wetground, jabndnd=jabndnd_)
         end if
         if (jamapFreeboard > 0) then ! freeboard
            ierr = nf90_put_var(mapids%ncid, mapids%id_freeboard(1), freeboard, start=(/1, mapids%id_tsp%idx_curtime/))
         end if
         if (jamapDepthOnGround > 0) then ! waterdepth that is above ground level
            ierr = nf90_put_var(mapids%ncid, mapids%id_hs_on_ground(1), hsOnGround, start=(/1, mapids%id_tsp%idx_curtime/))
         end if
         if (jamapVolOnGround > 0) then ! volume that is above ground level
            ierr = nf90_put_var(mapids%ncid, mapids%id_vol_on_ground(1), volOnGround, start=(/1, mapids%id_tsp%idx_curtime/))
         end if
         if (jamapTotalInflow1d2d > 0) then ! total 1d2d inflow
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCur1d2d, UNC_LOC_S, qCur1d2d, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTot1d2d, UNC_LOC_S, vTot1d2d, jabndnd=jabndnd_)
         end if
         if (jamapTotalInflowLat > 0) then ! total lateral inflow
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_qCurLat, UNC_LOC_S, qCurLat, jabndnd=jabndnd_)
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_vTotLat, UNC_LOC_S, vTotLat, jabndnd=jabndnd_)
         end if
      end if
      if (lnx1d > 0) then
         if (jamapS1Gradient > 0) then
            ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_s1Gradient, UNC_LOC_U, s1Gradient, jabndnd=jabndnd_)
         end if
      end if
      !
      ! Nearfield
      !
      if (jamapNearField == 1) then
         call realloc(work1d, ndkx, keepExisting=.false., fill=0.0d0)
         do isrc = numsrc - numsrc_nf + 1, numsrc
            !
            ! Sinks
            n = ksrc(1, isrc)
            if (n /= 0) then
               call getkbotktop(n, kbot_, ktop_)
               nkbot = kbot_
               nktop = ktop_
               do nk = kbot_, ktop_
                  if (zws(nk) < zsrc(1, isrc)) nkbot = nk
                  if (zws(nk) < zsrc2(1, isrc)) nktop = nk
               end do
               do nk = nkbot, nktop
                  work1d(nk) = work1d(nk) - qstss((1 + numconst) * (isrc - 1) + 1) / real(nktop - nkbot + 1, hp)
               end do
            end if
            !
            ! Sources
            n = ksrc(4, isrc)
            if (n /= 0) then
               call getkbotktop(n, kbot_, ktop_)
               nkbot = kbot_
               nktop = ktop_
               do nk = kbot_, ktop_
                  if (zws(nk) < zsrc(2, isrc)) nkbot = nk
                  if (zws(nk) < zsrc2(2, isrc)) nktop = nk
               end do
               do nk = nkbot, nktop
                  work1d(nk) = work1d(nk) + qstss((1 + numconst) * (isrc - 1) + 1) / real(nktop - nkbot + 1, hp)
               end do
            end if
         end do
         ierr = unc_put_var_map(mapids%ncid, mapids%id_tsp, mapids%id_nrfld, UNC_LOC_S3D, work1d, jabndnd=jabndnd_)
      end if
      if (timon) call timstop(handle_extra(73))
      if (timon) call timstop(handle_extra(70))

   end subroutine unc_write_map_filepointer_ugrid
!> Adds variable at nodes to map-file.
   function unc_put_var_map_nodes(ncid, id_tsp, id_var, values, jabndnd_) result(ierr)
      use network_data, only: kc, numk
      use m_missing, only: dmiss
      use fm_location_types, only: UNC_LOC_CN

      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      double precision, intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      integer, intent(in) :: jabndnd_

      integer :: ierr

      integer :: nn
      double precision, allocatable :: array_on_file(:)

      allocate (array_on_file(numk))
      array_on_file = dmiss

      ! re-mapping by edge nodes is needed, use kc as table
      do nn = 1, numk
         if (kc(nn) > 0) then
            array_on_file(kc(nn)) = values(nn)
         end if
      end do

      ierr = unc_put_var_map(ncid, id_tsp, id_var, UNC_LOC_CN, array_on_file, jabndnd=jabndnd_)
   end function unc_put_var_map_nodes

!> Writes map/flow data to an already opened netCDF dataset.
!! The netnode and -links have been written already.
   subroutine unc_write_map_filepointer(imapfile, tim, jaseparate) ! wrimap
      use m_flow
      use m_flowtimes
      use m_flowgeom
      use m_sobekdfm
      use m_heatfluxes
      use m_sferic
      use network_data
      use m_sediment
      use m_bedform
      use m_wind
      use m_flowparameters, only: jatrt, jacali
      use m_mass_balance_areas
      use m_fm_wq_processes
      use m_xbeach_data
      use m_transportdata
      use bedcomposition_module, only: bedcomp_getpointer_integer
      use m_alloc
      use m_missing
      use string_module, only: replace_multiple_spaces_by_single_spaces
      use netcdf_utils, only: ncu_append_atts
      use m_fm_icecover, only: ice_mapout, ice_af, ice_h, ice_p, ice_t, snow_h, snow_t, ja_icecover, ICECOVER_SEMTNER
      use m_gettaus
      use m_gettauswave
      use m_get_kbot_ktop

      implicit none

      integer, intent(in) :: imapfile
      real(kind=hp), intent(in) :: tim
      integer, optional, intent(in) :: jaseparate !< Whether this save is manual/by user (not part of the standard map write series)

      integer :: jaseparate_, idims(2)

      logical, dimension(2), save :: firststep = .true.

      integer, save :: ierr, ndim
      integer, dimension(2), save :: &
         !id_netcelldim, id_netcellmaxnodedim, id_netcellcontourptsdim, &
         id_laydim, id_wdim, &
         id_flowelemdim, &
         id_maxfracdim, &
         id_erolaydim, &
         id_flowlinkdim, &
         id_netlinkdim, &
         id_1d2ddim, &
         id_timedim, &
         id_time, id_timestep, &
         id_sbcx, id_sbcy, id_sbcx_reconstructed, id_sbcy_reconstructed, &
         id_sbwx, id_sbwy, id_sbwx_reconstructed, id_sbwy_reconstructed, &
         id_sswx, id_sswy, id_sswx_reconstructed, id_sswy_reconstructed, &
         id_sourse, id_sinkse, id_ws, &
         id_sxtot, id_sytot, id_rsedeq, id_umod, id_zumod, id_ustar, id_dzdn, id_dzdt, id_morbl, id_aks, id_rca, &
         id_bodsed, id_dpsed, id_msed, id_lyrfrac, id_thlyr, id_poros, id_nlyrdim, &
         id_sedtotdim, id_sedsusdim, id_rho, id_rhop, id_viu, id_diu, id_q1, id_spircrv, id_spirint, &
         id_q1main, &
         id_s1, id_taus, id_ucx, id_ucy, id_ucz, id_ucxa, id_ucya, id_unorm, id_ww1, id_sa1, id_tem1, id_sed, id_ero, id_s0, id_u0, id_cfcl, id_cftrt, id_czs, id_czu, &
         id_qsun, id_qeva, id_qcon, id_qlong, id_qfreva, id_qfrcon, id_qtot, &
         id_patm, id_ice_af, id_ice_h, id_ice_p, id_ice_t, id_snow_h, id_snow_t, id_tair, id_rhum, id_clou, id_E, id_R, id_H, id_D, id_DR, id_urms, id_thetamean, &
         id_cwav, id_cgwav, id_sigmwav, &
         id_ust, id_vst, id_windx, id_windy, id_windxu, id_windyu, id_numlimdt, id_hs, id_bl, id_zk, &
         id_1d2d_edges, id_1d2d_zeta1d, id_1d2d_crest_level, id_1d2d_b_2di, id_1d2d_b_2dv, id_1d2d_d_2dv, id_1d2d_q_zeta, id_1d2d_q_lat, &
         id_1d2d_cfl, id_1d2d_flow_cond, id_1d2d_sb, id_1d2d_s1_2d, id_1d2d_s0_2d, id_tidep, id_salp, id_inttidesdiss, &
         id_duneheight, id_dunelength, id_ksd, id_ksr, id_ksmr, id_ks, &
         id_taurat, id_dm, id_dg, id_dgsd, id_frac, id_mudfrac, id_sandfrac, id_fixfac, id_hidexp, id_mfluff, id_scrn, id_urmscc, id_Fxcc, id_Fycc, &
         id_sscx, id_sscy, id_sscx_reconstructed, id_sscy_reconstructed, &
         id_turkin1, id_tureps1, id_vicwwu, id_vicwws, id_swanbl, &
         id_rnveg, id_diaveg, id_veg_stemheight

      integer, dimension(:, :), allocatable, save :: id_dxx ! fractions
      double precision, dimension(:), allocatable :: dum
      double precision, dimension(:, :), allocatable :: poros
      double precision, dimension(:, :, :), allocatable :: frac
      double precision, dimension(:), allocatable :: toutput
      double precision, dimension(:, :), allocatable :: toutputx, toutputy
      double precision, dimension(:), allocatable :: rks

      integer, dimension(:), allocatable :: idum

      integer :: iid, i, j, jj, itim, k, kb, kt, kk, n, LL, Ltx, Lb, L, nm, nlayb, nrlay, nlaybL, nrlayLx, varid, ndims
      integer :: ndxndxi ! Either ndx or ndxi, depending on whether boundary nodes also need to be written.
      double precision, dimension(:), allocatable :: windx, windy
      double precision, dimension(:), allocatable :: numlimdtdbl ! TODO: WO/AvD: remove this once integer version of unc_def_map_var is available
      double precision :: vicc, dicc
      integer :: jaeulerloc

      double precision :: rhol
      character(16) :: dxname, zw_elem, zcc_elem, zwu_link, zu_link
      character(64) :: dxdescr
      character(10) :: transpunit
      character(len=255) :: tmpstr

      integer, dimension(:), allocatable :: flag_val
      character(len=10000) :: flag_mean

      double precision, dimension(:), pointer :: dens

      if (.not. allocated(id_dxx) .and. stm_included) allocate (id_dxx(1:stmpar%morpar%nxx, 1:2))

      ! If jaseparate_==1 or this map file was just opened for the first time:
      ! only write net+vardefs first time, and write subsequent flow snapshots in later calls.
      ! jaseparate_==2: write com file
      if (present(jaseparate)) then
         jaseparate_ = jaseparate
      else
         jaseparate_ = 0
      end if

      if (jaseparate_ == 0 .or. jaseparate_ == 1) then
         ! mapfile, store/use ids number 1
         iid = 1
         ndxndxi = ndxi
      elseif (jaseparate_ == 2) then
         ! comfile, store/use ids number 2
         iid = 2
         ndxndxi = ndx ! Com file, include boundary nodes
      else
         ! error
         iid = 0
      end if

      ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
      ! before in previous calls.
      ndim = 0
      ierr = nf90_inquire(imapfile, nDimensions=ndim)

      ! Only write net and flow geometry data the first time, or for a separate map file.
      if (ndim == 0) then

         call unc_write_net_filepointer(imapfile) ! Write standard net data as well

         if (jaseparate_ == 2) then
            call unc_write_flowgeom_filepointer(imapfile, jabndnd=1) ! Write time-independent flow geometry data, with boundary nodes
            ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
         else
            call unc_write_flowgeom_filepointer(imapfile) ! Write time-independent flow geometry data
            ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
         end if

         ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
         ierr = nf90_inq_dimid(imapfile, 'nNetLink', id_netlinkdim(iid))

         if (nbnd1d2d > 0) then
            ierr = nf90_def_dim(imapfile, 'nBnd1d2d', nbnd1d2d, id_1d2ddim(iid))
         end if

         ! Time
         ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim(iid))
         call check_error(ierr, 'def time dim')
         ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim(iid), id_time(iid))
         ierr = nf90_put_att(imapfile, id_time(iid), 'units', trim(Tudunitstr))
         ierr = nf90_put_att(imapfile, id_time(iid), 'standard_name', 'time')

         ! 3D
         if (kmx > 0) then
            call unc_append_3dflowgeom_def(imapfile) ! Append definition of time-independent 3d flow geometry data
            ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
            ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
         end if

         ! Size of latest timestep
         ierr = nf90_def_var(imapfile, 'timestep', nf90_double, id_timedim(iid), id_timestep(iid))
         ierr = nf90_put_att(imapfile, id_timestep(iid), 'units', 'seconds')
         ierr = nf90_put_att(imapfile, id_timestep(iid), 'standard_name', 'timestep')

         if (jamaps1 > 0 .or. jaseparate_ == 2) then
            ! Flow data on centres: water level at latest timestep
            ierr = nf90_def_var(imapfile, 's1', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_s1(iid))
            ierr = nf90_put_att(imapfile, id_s1(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_s1(iid), 'standard_name', 'sea_surface_height') ! sorry for inland water people
            ierr = nf90_put_att(imapfile, id_s1(iid), 'long_name', 'water level')
            ierr = nf90_put_att(imapfile, id_s1(iid), 'units', 'm')
            ierr = unc_add_gridmapping_att(imapfile, (/id_s1(iid)/), jsferic)
         end if

         if (jaseparate_ == 0 .or. jaseparate_ == 1) then ! to mapfile
            ! Flow data on centres: water level timestep before the latest timestep

            if (jamaps0 > 0) then
               ierr = nf90_def_var(imapfile, 's0', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_s0(iid))
               ierr = nf90_put_att(imapfile, id_s0(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_s0(iid), 'standard_name', 'sea_surface_height') ! sorry for inland water people
               ierr = nf90_put_att(imapfile, id_s0(iid), 'long_name', 'water level at previous timestep')
               ierr = nf90_put_att(imapfile, id_s0(iid), 'units', 'm')
               ierr = unc_add_gridmapping_att(imapfile, (/id_s0(iid)/), jsferic)
            end if

            idims(1) = id_flowelemdim(iid)
            idims(2) = id_timedim(iid)

            if (jamaphs > 0) then
               call definencvar(imapfile, id_hs(iid), nf90_double, idims, 'waterdepth', 'water depth', 'm', 'FlowElem_xcc FlowElem_ycc')
            end if

            if (jamapheatflux > 0 .and. jatem > 1) then ! Heat modelling only
               call definencvar(imapfile, id_tair(iid), nf90_double, idims, 'Tair', 'air temperature', 'degC', 'FlowElem_xcc FlowElem_ycc')
               call definencvar(imapfile, id_rhum(iid), nf90_double, idims, 'rhum', 'Relative humidity', ' ', 'FlowElem_xcc FlowElem_ycc')
               call definencvar(imapfile, id_clou(iid), nf90_double, idims, 'clou', 'cloudiness', ' ', 'FlowElem_xcc FlowElem_ycc')

               if (jatem == 5) then
                  call definencvar(imapfile, id_qsun(iid), nf90_double, idims, 'Qsun', 'solar influx', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                  call definencvar(imapfile, id_Qeva(iid), nf90_double, idims, 'Qeva', 'evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                  call definencvar(imapfile, id_Qcon(iid), nf90_double, idims, 'Qcon', 'sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                  call definencvar(imapfile, id_Qlong(iid), nf90_double, idims, 'Qlong', 'long wave back radiation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                  call definencvar(imapfile, id_Qfreva(iid), nf90_double, idims, 'Qfreva', 'free convection evaporative heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
                  call definencvar(imapfile, id_Qfrcon(iid), nf90_double, idims, 'Qfrcon', 'free convection sensible heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
               end if

               call definencvar(imapfile, id_Qtot(iid), nf90_double, idims, 'Qtot', 'total heat flux', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
            end if

            if (jamapnumlimdt > 0) then
               call definencvar(imapfile, id_numlimdt(iid), nf90_double, idims, 'numlimdt', 'number of times flow element was Courant limiting', '1', 'FlowElem_xcc FlowElem_ycc')
            end if

            if (jamaptaucurrent > 0) then
               ! Flow data on centres
               ierr = nf90_def_var(imapfile, 'taus', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_taus(iid))
               ierr = nf90_put_att(imapfile, id_taus(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_taus(iid), 'standard_name', 'taucurrent')
               ierr = nf90_put_att(imapfile, id_taus(iid), 'long_name', 'taucurrent in flow element')
               ierr = nf90_put_att(imapfile, id_taus(iid), 'units', 'N m-2')
            end if

            if (jamaptidep > 0 .and. jatidep > 0) then
               ierr = nf90_def_var(imapfile, 'TidalPotential', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_tidep(iid))
               ierr = nf90_put_att(imapfile, id_tidep(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_tidep(iid), 'standard_name', 'TidalPotential')
               ierr = nf90_put_att(imapfile, id_tidep(iid), 'long_name', 'Tidal Potential generated by celestial forces in flow element center')
               ierr = nf90_put_att(imapfile, id_tidep(iid), 'units', 'm2 s-2')
            end if
            if (jamapselfal > 0) then
               if (jaselfal > 0) then
                  ierr = nf90_def_var(imapfile, 'SALPotential', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_salp(iid))
                  ierr = nf90_put_att(imapfile, id_salp(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_salp(iid), 'standard_name', 'SALPotential')
                  ierr = nf90_put_att(imapfile, id_salp(iid), 'long_name', 'Self-attraction and loading Potential in flow element center')
                  ierr = nf90_put_att(imapfile, id_salp(iid), 'units', 'm2 s-2')
               end if
            end if

            if (jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss > 0) then
               ierr = nf90_def_var(imapfile, 'internal_tides_dissipation', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_IntTidesDiss(iid))
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid), 'standard_name', 'internal_tides_dissipation')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid), 'long_name', 'internal tides dissipation in flow element center')
               ierr = nf90_put_att(imapfile, id_inttidesdiss(iid), 'units', 'J s-1 m-2')
            end if

            if (kmx > 0) then
               !     3D
               if (jamapu1 > 0) then
                  ierr = nf90_def_var(imapfile, 'unorm', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_unorm(iid))
               end if
               if (jamapu0 > 0) then
                  ierr = nf90_def_var(imapfile, 'u0', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_u0(iid))
               end if
               if (jamapq1 > 0) then
                  ierr = nf90_def_var(imapfile, 'q1', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_q1(iid))
               end if
               if (jamapq1main > 0 .and. allocated(q1_main)) then
                  ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_q1main(iid))
               end if
               if (jamapviu > 0) then
                  ierr = nf90_def_var(imapfile, 'viu', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_viu(iid))
               end if
               if (jamapdiu > 0) then
                  ierr = nf90_def_var(imapfile, 'diu', nf90_double, (/id_laydim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_diu(iid))
               end if

               if (jamapucvec > 0) then
                  ! JRE Velocity vector needs to be written, irrespective of kmx, also for com file. Statements moved down outside if-clause
                  !    ierr = nf90_def_var(imapfile, 'ucx'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucx(iid)  )
                  !    ierr = nf90_def_var(imapfile, 'ucy'   , nf90_double, (/ id_flowelemdim(iid), id_timedim (iid)/) , id_ucy(iid)  )
                  ierr = nf90_def_var(imapfile, 'ucz', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_ucz(iid))

                  ! Depth-averaged cell-center velocities in 3D:
                  ierr = nf90_def_var(imapfile, 'ucxa', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ucxa(iid))
                  ierr = nf90_def_var(imapfile, 'ucya', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ucya(iid))

               end if
               if (jamapww1 > 0) then
                  ierr = nf90_def_var(imapfile, 'ww1', nf90_double, (/id_wdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_ww1(iid))
               end if
               if (jamaprho > 0) then
                  if (density_is_pressure_dependent()) then
                     ierr = nf90_def_var(imapfile, 'density', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_rho(iid))
                  else
                     ierr = nf90_def_var(imapfile, 'rho', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_rhop(iid))
                  end if
               end if
               !
               if (jamapucvec > 0) then
                  ierr = nf90_put_att(imapfile, id_ucz(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ucz(iid), 'standard_name', 'upward_sea_water_velocity')
                  ierr = nf90_put_att(imapfile, id_ucz(iid), 'long_name', 'upward velocity on flow element center')
                  ierr = nf90_put_att(imapfile, id_ucz(iid), 'units', 'm s-1')
                  ierr = nf90_put_att(imapfile, id_ucz(iid), '_FillValue', dmiss)

                  ierr = nf90_put_att(imapfile, id_ucxa(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  if (jsferic == 0) then
                     ierr = nf90_put_att(imapfile, id_ucxa(iid), 'standard_name', 'sea_water_x_velocity')
                  else
                     ierr = nf90_put_att(imapfile, id_ucxa(iid), 'standard_name', 'eastward_sea_water_velocity')
                  end if

                  ierr = nf90_put_att(imapfile, id_ucxa(iid), 'long_name', 'depth-averaged velocity on flow element center, x-component')
                  ierr = nf90_put_att(imapfile, id_ucxa(iid), 'units', 'm s-1')

                  ierr = nf90_put_att(imapfile, id_ucya(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  if (jsferic == 0) then
                     ierr = nf90_put_att(imapfile, id_ucya(iid), 'standard_name', 'sea_water_y_velocity')
                  else
                     ierr = nf90_put_att(imapfile, id_ucya(iid), 'standard_name', 'northward_sea_water_velocity')
                  end if
                  ierr = nf90_put_att(imapfile, id_ucya(iid), 'long_name', 'depth-averaged velocity on flow element center, y-component')
                  ierr = nf90_put_att(imapfile, id_ucya(iid), 'units', 'm s-1')
               end if
               if (jamapww1 > 0) then
                  ierr = nf90_put_att(imapfile, id_ww1(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ww1(iid), 'standard_name', 'upward_sea_water_velocity') ! same standard name allowed?
                  ierr = nf90_put_att(imapfile, id_ww1(iid), 'long_name', 'upward velocity on vertical interface') ! (upward normal or upward)?
                  ierr = nf90_put_att(imapfile, id_ww1(iid), 'units', 'm s-1')
                  ierr = nf90_put_att(imapfile, id_ww1(iid), '_FillValue', dmiss)
                  !?elevation
               end if
               if (jamaprho > 0) then
                  if (density_is_pressure_dependent()) then
                     ierr = nf90_put_att(imapfile, id_rho(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_rho(iid), 'standard_name', 'sea_water_density')
                     ierr = nf90_put_att(imapfile, id_rho(iid), 'long_name', 'flow mass density')
                     ierr = nf90_put_att(imapfile, id_rho(iid), 'units', 'kg m-3')
                     ierr = nf90_put_att(imapfile, id_rho(iid), '_FillValue', dmiss)
                  else
                     ierr = nf90_put_att(imapfile, id_rhop(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_rhop(iid), 'standard_name', 'sea_water_potential_density')
                     ierr = nf90_put_att(imapfile, id_rhop(iid), 'long_name', 'flow mass potential density')
                     ierr = nf90_put_att(imapfile, id_rhop(iid), 'units', 'kg m-3')
                     ierr = nf90_put_att(imapfile, id_rhop(iid), '_FillValue', dmiss)
                  end if
               end if
            end if ! kmx>0

            if (kmx == 0) then
               if (jamapu1 > 0) then
                  ierr = nf90_def_var(imapfile, 'unorm', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_unorm(iid))
               end if
               if (jamapu0 > 0) then
                  ierr = nf90_def_var(imapfile, 'u0', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_u0(iid))
               end if
               if (jamapq1 > 0) then
                  ierr = nf90_def_var(imapfile, 'q1', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_q1(iid))
               end if
               if (jamapq1main > 0 .and. allocated(q1_main)) then
                  ierr = nf90_def_var(imapfile, 'q1main', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_q1main(iid))
               end if
               if (jamapviu > 0) then
                  ierr = nf90_def_var(imapfile, 'viu', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_viu(iid))
               end if
               if (jamapdiu > 0) then
                  ierr = nf90_def_var(imapfile, 'diu', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_diu(iid))
               end if
            end if

            if (jamapu1 > 0) then
               ierr = nf90_put_att(imapfile, id_unorm(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_unorm(iid), 'long_name', 'normal component of sea_water_speed')
               ierr = nf90_put_att(imapfile, id_unorm(iid), 'units', 'm s-1')
               ierr = nf90_put_att(imapfile, id_unorm(iid), '_FillValue', dmiss)
            end if

            if (jamapu0 > 0) then
               ierr = nf90_put_att(imapfile, id_u0(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_u0(iid), 'long_name', 'normal component of sea_water_speed at previous timestep')
               ierr = nf90_put_att(imapfile, id_u0(iid), 'units', 'm s-1')
               ierr = nf90_put_att(imapfile, id_u0(iid), '_FillValue', dmiss)
            end if
            if (jamapq1 > 0) then
               ierr = nf90_put_att(imapfile, id_q1(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               !ierr = nf90_put_att(imapfile, id_q1(iid)   ,'standard_name', 'discharge') ! not CF
               ierr = nf90_put_att(imapfile, id_q1(iid), 'long_name', 'flow flux')
               ierr = nf90_put_att(imapfile, id_q1(iid), 'units', 'm3 s-1')
               ierr = nf90_put_att(imapfile, id_q1(iid), '_FillValue', dmiss)
            end if
            if (jamapq1main > 0 .and. allocated(q1_main)) then
               ierr = nf90_put_att(imapfile, id_q1main(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               !ierr = nf90_put_att(imapfile, id_q1main(iid)   ,'standard_name', 'discharge') ! not CF
               ierr = nf90_put_att(imapfile, id_q1main(iid), 'long_name', 'flow flux in main channel')
               ierr = nf90_put_att(imapfile, id_q1main(iid), 'units', 'm3 s-1')
               ierr = nf90_put_att(imapfile, id_q1main(iid), '_FillValue', dmiss)
            end if

            if (jamapviu > 0) then
               ierr = nf90_put_att(imapfile, id_viu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_viu(iid), 'long_name', 'horizontal viscosity')
               ierr = nf90_put_att(imapfile, id_viu(iid), 'units', 'm2 s-1')
               ierr = nf90_put_att(imapfile, id_viu(iid), '_FillValue', dmiss)
            end if
            if (jamapdiu > 0) then
               ierr = nf90_put_att(imapfile, id_diu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_diu(iid), 'long_name', 'horizontal diffusivity')
               ierr = nf90_put_att(imapfile, id_diu(iid), 'units', 'm2 s-1')
               ierr = nf90_put_att(imapfile, id_diu(iid), '_FillValue', dmiss)
            end if
         end if ! jaseparate =/ 2
         !
         if (kmx == 0) then
            if (jamapucvec > 0 .or. jaseparate_ == 2) then
               ierr = nf90_def_var(imapfile, 'ucx', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ucx(iid))
               ierr = nf90_def_var(imapfile, 'ucy', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ucy(iid))
            end if
         else
            if (jamapucvec > 0 .or. jaseparate_ == 2) then
               ierr = nf90_def_var(imapfile, 'ucx', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_ucx(iid))
               ierr = nf90_def_var(imapfile, 'ucy', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_ucy(iid))
            end if
         end if

         if (jamapucvec > 0 .or. jaseparate_ == 2) then
            ierr = nf90_put_att(imapfile, id_ucx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_ucx(iid), 'standard_name', 'sea_water_x_velocity')
            else
               ierr = nf90_put_att(imapfile, id_ucx(iid), 'standard_name', 'eastward_sea_water_velocity')
            end if

            if (jaeulervel == 0 .or. jaseparate_ == 2) then
               ierr = nf90_put_att(imapfile, id_ucx(iid), 'long_name', 'velocity on flow element center, x-component')
            else
               ierr = nf90_put_att(imapfile, id_ucx(iid), 'long_name', 'Eulerian velocity on flow element center, x-component')
            end if
            ierr = nf90_put_att(imapfile, id_ucx(iid), 'units', 'm s-1')
            ierr = nf90_put_att(imapfile, id_ucx(iid), '_FillValue', dmiss)

            ierr = nf90_put_att(imapfile, id_ucy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_ucy(iid), 'standard_name', 'sea_water_y_velocity')
            else
               ierr = nf90_put_att(imapfile, id_ucy(iid), 'standard_name', 'northward_sea_water_velocity')
            end if

            if (jaeulervel == 0 .or. jaseparate_ == 2) then
               ierr = nf90_put_att(imapfile, id_ucy(iid), 'long_name', 'velocity on flow element center, y-component')
            else
               ierr = nf90_put_att(imapfile, id_ucy(iid), 'long_name', 'Eulerian velocity on flow element center, y-component')
            end if
            ierr = nf90_put_att(imapfile, id_ucy(iid), 'units', 'm s-1')
            ierr = nf90_put_att(imapfile, id_ucy(iid), '_FillValue', dmiss)
         end if

         if (jaseparate_ /= 2) then
            if (jamapsal > 0 .and. jasal > 0) then
               if (kmx > 0) then !        3D
                  ierr = nf90_def_var(imapfile, 'sa1', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_sa1(iid))
               else
                  ierr = nf90_def_var(imapfile, 'sa1', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_sa1(iid))
               end if
               ierr = nf90_put_att(imapfile, id_sa1(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_sa1(iid), 'standard_name', 'sea_water_salinity')
               ierr = nf90_put_att(imapfile, id_sa1(iid), 'long_name', 'salinity')
               ierr = nf90_put_att(imapfile, id_sa1(iid), 'units', '1e-3')
               ierr = nf90_put_att(imapfile, id_sa1(iid), '_FillValue', dmiss)
            end if

            if (jamaptem > 0 .and. jatem > 0) then
               if (kmx > 0) then !        3D
                  ierr = nf90_def_var(imapfile, 'tem1', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_tem1(iid))
               else
                  ierr = nf90_def_var(imapfile, 'tem1', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_tem1(iid))
               end if
               ierr = nf90_put_att(imapfile, id_tem1(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_tem1(iid), 'standard_name', 'sea_water_temperature')
               ierr = nf90_put_att(imapfile, id_tem1(iid), 'long_name', 'temperature')
               ierr = nf90_put_att(imapfile, id_tem1(iid), 'units', 'degC')
               ierr = nf90_put_att(imapfile, id_tem1(iid), '_FillValue', dmiss)
            end if

!          tracers
            if (jamapconst > 0 .and. ITRA1 > 0) then
               do j = ITRA1, ITRAN
                  tmpstr = const_names(j)
                  ! Forbidden chars in NetCDF names: space, /, and more.
                  call replace_char(tmpstr, 32, 95)
                  call replace_char(tmpstr, 47, 95)
                  if (kmx > 0) then !        3D
                     ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_const(iid, j))
                  else
                     ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_const(iid, j))
                  end if
                  ierr = nf90_put_att(imapfile, id_const(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_const(iid, j), 'standard_name', trim(tmpstr))
                  ierr = nf90_put_att(imapfile, id_const(iid, j), 'long_name', trim(tmpstr))
                  if (const_units(j) /= ' ') then
                     tmpstr = const_units(j)
                  else
                     tmpstr = '-'
                  end if
                  ierr = nf90_put_att(imapfile, id_const(iid, j), 'units', tmpstr)
                  ierr = nf90_put_att(imapfile, id_const(iid, j), '_FillValue', dmiss)
               end do
            end if

!          water quality bottom variables
            if (numwqbots > 0) then
               call realloc(id_wqb, (/3, numwqbots/), keepExisting=.false., fill=0)
               do j = 1, numwqbots
                  tmpstr = wqbotnames(j)
                  ! Forbidden chars in NetCDF names: space, /, and more.
                  call replace_char(tmpstr, 32, 95)
                  call replace_char(tmpstr, 47, 95)
                  ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_wqb(iid, j))
                  ierr = nf90_put_att(imapfile, id_wqb(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_wqb(iid, j), 'standard_name', trim(tmpstr))
                  ierr = nf90_put_att(imapfile, id_wqb(iid, j), 'long_name', trim(tmpstr))
                  tmpstr = wqbotunits(j)
                  ierr = nf90_put_att(imapfile, id_wqb(iid, j), 'units', tmpstr)
                  ierr = nf90_put_att(imapfile, id_wqb(iid, j), '_FillValue', dmiss)
               end do
               if (jamapwqbot3d == 1) then
                  call realloc(id_wqb3d, (/3, numwqbots/), keepExisting=.false., fill=0)
                  do j = 1, numwqbots
                     tmpstr = wqbotnames(j)
                     ! Forbidden chars in NetCDF names: space, /, and more.
                     call replace_char(tmpstr, 32, 95)
                     call replace_char(tmpstr, 47, 95)
                     ierr = nf90_def_var(imapfile, trim(tmpstr)//'_3D', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_wqb3d(iid, j))
                     ierr = nf90_put_att(imapfile, id_wqb3d(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_wqb3d(iid, j), 'standard_name', trim(tmpstr))
                     ierr = nf90_put_att(imapfile, id_wqb3d(iid, j), 'long_name', trim(tmpstr))
                     tmpstr = wqbotunits(j)
                     ierr = nf90_put_att(imapfile, id_wqb3d(iid, j), 'units', tmpstr)
                     ierr = nf90_put_att(imapfile, id_wqb3d(iid, j), '_FillValue', dmiss)
                  end do
               end if
            end if

!          waq output
            if (jawaqproc > 0) then
               if (noout_map > 0) then
                  call realloc(id_waq, (/3, noout_map/), keepExisting=.false., fill=0)
                  do j = 1, noout_map
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_output_',I0)") j
                     if (kmx > 0) then !        3D
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_waq(iid, j))
                     else
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_waq(iid, j))
                     end if
                     tmpstr = trim(outputs%names(j))//' - '//trim(outputs%description(j))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = nf90_put_att(imapfile, id_waq(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_waq(iid, j), 'long_name', trim(outputs%names(j)))
                     ierr = nf90_put_att(imapfile, id_waq(iid, j), 'units', trim(outputs%units(j)))
                     ierr = nf90_put_att(imapfile, id_waq(iid, j), 'description', tmpstr)
                     ierr = nf90_put_att(imapfile, id_waq(iid, j), '_FillValue', dmiss)
                  end do
               end if
               if (noout_statt > 0) then
                  call realloc(id_wqst, (/3, noout_statt/), keepExisting=.false., fill=0)
                  do j = 1, noout_statt
                     jj = noout_user + j
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_stat_',I0)") j
                     if (kmx > 0) then !        3D
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_wqst(iid, j))
                     else
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_wqst(iid, j))
                     end if
                     tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = nf90_put_att(imapfile, id_wqst(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_wqst(iid, j), 'long_name', trim(outputs%names(jj)))
                     ierr = nf90_put_att(imapfile, id_wqst(iid, j), 'units', trim(outputs%units(jj)))
                     ierr = nf90_put_att(imapfile, id_wqst(iid, j), 'description', tmpstr)
                     ierr = nf90_put_att(imapfile, id_wqst(iid, j), '_FillValue', dmiss)
                  end do
               end if
               if (noout_state > 0) then
                  call realloc(id_wqse, (/3, noout_state/), keepExisting=.false., fill=0)
                  do j = 1, noout_state
                     jj = noout_user + noout_statt + j
                     tmpstr = ' '
                     write (tmpstr, "('water_quality_stat_',I0)") noout_statt + j
                     if (kmx > 0) then !        3D
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_laydim(iid), id_flowelemdim(iid)/), id_wqse(iid, j))
                     else
                        ierr = nf90_def_var(imapfile, tmpstr, nf90_double, (/id_flowelemdim(iid)/), id_wqse(iid, j))
                     end if
                     tmpstr = trim(outputs%names(jj))//' - '//trim(outputs%description(jj))//' in flow element'
                     call replace_multiple_spaces_by_single_spaces(tmpstr)
                     ierr = nf90_put_att(imapfile, id_wqse(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_wqse(iid, j), 'long_name', trim(outputs%names(jj)))
                     ierr = nf90_put_att(imapfile, id_wqse(iid, j), 'units', trim(outputs%units(jj)))
                     ierr = nf90_put_att(imapfile, id_wqse(iid, j), 'description', tmpstr)
                     ierr = nf90_put_att(imapfile, id_wqse(iid, j), '_FillValue', dmiss)
                  end do
               end if
            end if

            ! water quality mass balance areas
            if (nomba > 0) then
               ierr = nf90_def_var(imapfile, 'water_quality_mba', nf90_int, (/id_flowelemdim(iid)/), id_mba(iid))
               ierr = nf90_put_att(imapfile, id_mba(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_mba(iid), 'long_name', 'Water quality mass balance areas')
               ierr = unc_add_gridmapping_att(imapfile, (/id_mba(iid)/), jsferic)
               call realloc(flag_val, nomba, keepExisting=.false., fill=0)
               flag_mean = ' '
               do j = nomba, 1, -1
                  flag_val(j) = j
                  flag_mean = trim(mbaname(j))//' '//flag_mean
               end do
               ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_values', flag_val)
               ierr = nf90_put_att(imapfile, id_mba(iid), 'flag_meanings', flag_mean)
            end if

            if (jasecflow > 0 .and. jamapspir > 0) then
               if (kmx < 2) then
                  ierr = nf90_def_var(imapfile, 'spircrv', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_spircrv(iid))
                  ierr = nf90_put_att(imapfile, id_spircrv(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_spircrv(iid), 'long_name', 'streamline curvature')
                  ierr = nf90_put_att(imapfile, id_spircrv(iid), 'units', 'm-1')
               end if
               ierr = nf90_def_var(imapfile, 'spirint', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_spirint(iid))
               ierr = nf90_put_att(imapfile, id_spirint(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_spirint(iid), 'long_name', 'Spiral flow intensity')
               ierr = nf90_put_att(imapfile, id_spirint(iid), 'units', 'm/s')
            end if

            if (jamaptur > 0 .and. kmx > 0) then
               if (iturbulencemodel >= 3) then
                  ierr = nf90_def_var(imapfile, 'turkin1', nf90_double, (/id_wdim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_turkin1(iid))
                  ierr = nf90_put_att(imapfile, id_turkin1(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                  ierr = nf90_put_att(imapfile, id_turkin1(iid), 'standard_name', 'specific_turbulent_kinetic_energy_of_sea_water')
                  ierr = nf90_put_att(imapfile, id_turkin1(iid), 'long_name', 'turbulent kinetic energy')
                  ierr = nf90_put_att(imapfile, id_turkin1(iid), 'units', 'm2 s-2')
                  ierr = nf90_put_att(imapfile, id_turkin1(iid), '_FillValue', dmiss)

                  ierr = nf90_def_var(imapfile, 'vicwwu', nf90_double, (/id_wdim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_vicwwu(iid))
                  ierr = nf90_put_att(imapfile, id_vicwwu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                  ierr = nf90_put_att(imapfile, id_vicwwu(iid), 'long_name', 'turbulent vertical eddy viscosity at velocity points')
                  ierr = nf90_put_att(imapfile, id_vicwwu(iid), 'units', 'm2 s-1')
                  ierr = nf90_put_att(imapfile, id_vicwwu(iid), '_FillValue', dmiss)

                  ierr = nf90_def_var(imapfile, 'vicwws', nf90_double, (/id_wdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_vicwws(iid))
                  ierr = nf90_put_att(imapfile, id_vicwws(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_vicwws(iid), 'long_name', 'turbulent vertical eddy viscosity at pressure points')
                  ierr = nf90_put_att(imapfile, id_vicwws(iid), 'units', 'm2 s-1')
                  ierr = nf90_put_att(imapfile, id_vicwws(iid), '_FillValue', dmiss)

                  ierr = nf90_def_var(imapfile, 'tureps1', nf90_double, (/id_wdim(iid), id_flowlinkdim(iid), id_timedim(iid)/), id_tureps1(iid))
                  ierr = nf90_put_att(imapfile, id_tureps1(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                  ierr = nf90_put_att(imapfile, id_tureps1(iid), '_FillValue', dmiss)

                  if (iturbulencemodel == 3) then
                     ierr = nf90_put_att(imapfile, id_tureps1(iid), 'standard_name', 'specific_turbulent_kinetic_energy_dissipation_in_sea_water')
                     ierr = nf90_put_att(imapfile, id_tureps1(iid), 'long_name', 'turbulent energy dissipation')
                     ierr = nf90_put_att(imapfile, id_tureps1(iid), 'units', 'm2 s-3')
                  else if (iturbulencemodel == 4) then
                     ierr = nf90_put_att(imapfile, id_tureps1(iid), 'long_name', 'turbulent time scale')
                     ierr = nf90_put_att(imapfile, id_tureps1(iid), 'units', 's-1')
                  end if
               end if
            end if

            if (jamapsed > 0 .and. stm_included) then
               ierr = nf90_def_dim(imapfile, 'nSedTot', stmpar%lsedtot, id_sedtotdim(iid))
               ierr = nf90_def_dim(imapfile, 'nSedSus', stmpar%lsedsus, id_sedsusdim(iid))
               ierr = nf90_def_dim(imapfile, 'nBedLayers', stmpar%morlyr%settings%nlyr, id_nlyrdim(iid))
               !
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  transpunit = 'kg/(s m)'
               case (1)
                  transpunit = 'm3/(s m)'
               case (2)
                  transpunit = 'm3/(s m)'
               end select
               !
               ! fall velocity
               if (stmpar%lsedsus > 0) then
                  if (kmx > 0) then
                     ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_ws(iid))
                  else ! '2D' fall velocity, ref fm_erosed(), to check...
                     ierr = nf90_def_var(imapfile, 'ws', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_ws(iid))
                  end if
                  ierr = nf90_put_att(imapfile, id_ws(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ws(iid), 'long_name', 'Sediment settling velocity')
                  ierr = nf90_put_att(imapfile, id_ws(iid), 'units', 'm s-1')
                  !
                  ! equilibrium concentration, 2D only
                  if (kmx == 0) then
                     ierr = nf90_def_var(imapfile, 'rsedeq', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_rsedeq(iid))
                     ierr = nf90_put_att(imapfile, id_rsedeq(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_rsedeq(iid), 'long_name', 'Equilibrium sediment concentration')
                     ierr = nf90_put_att(imapfile, id_rsedeq(iid), 'units', 'kg m-3')
                  end if
                  !
                  ! reference height
                  if (stmpar%morpar%moroutput%aks) then
                     ierr = nf90_def_var(imapfile, 'aks', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_aks(iid))
                     ierr = nf90_put_att(imapfile, id_aks(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_aks(iid), 'long_name', 'Near-bed reference concentration height')
                     ierr = nf90_put_att(imapfile, id_aks(iid), 'units', 'm')

                     ierr = nf90_def_var(imapfile, 'rca', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_rca(iid))
                     ierr = nf90_put_att(imapfile, id_rca(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_rca(iid), 'long_name', 'Near-bed reference concentration')
                     ierr = nf90_put_att(imapfile, id_rca(iid), 'units', 'kg m-3')
                  end if

                  if (stmpar%morpar%moroutput%sourcesink) then
                     ierr = nf90_def_var(imapfile, 'sourse', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sourse(iid))
                     ierr = nf90_put_att(imapfile, id_sourse(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_sourse(iid), 'long_name', 'Source term suspended sediment fractions')
                     ierr = nf90_put_att(imapfile, id_sourse(iid), 'units', 'kg/(m3 s)')

                     ierr = nf90_def_var(imapfile, 'sinkse', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sinkse(iid))
                     ierr = nf90_put_att(imapfile, id_sinkse(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_sinkse(iid), 'long_name', 'Sink term suspended sediment fractions')
                     ierr = nf90_put_att(imapfile, id_sinkse(iid), 'units', 's-1')
                  end if

                  if (stmpar%morpar%moroutput%suvcor) then
                     ierr = nf90_def_var(imapfile, 'e_scrn', nf90_double, (/id_flowlinkdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_scrn(iid))
                     ierr = nf90_put_att(imapfile, id_scrn(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                     ierr = nf90_put_att(imapfile, id_scrn(iid), 'long_name', 'Near-bed transport correction in face-normal direction')
                     ierr = nf90_put_att(imapfile, id_scrn(iid), 'units', transpunit)

                     !ierr = nf90_def_var(imapfile, 'e_scrt' , nf90_double, (/ id_flowlinkdim(iid) , id_sedsusdim(iid) , id_timedim(iid) /) , id_scrt(iid))
                     !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'coordinates'  , 'FlowLink_xu FlowLink_yu')
                     !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'long_name'    , 'Near-bed transport correction face-tangential direction')
                     !ierr = nf90_put_att(imapfile, id_scrt(iid) ,  'units'        , transpunit)
                  end if
                  !
                  ! Suspended fractions
                  !
                  do j = ISED1, ISEDN
                     tmpstr = const_names(j)
                     ! Forbidden chars in NetCDF names: space, /, and more.
                     call replace_char(tmpstr, 32, 95)
                     call replace_char(tmpstr, 47, 95)
                     if (kmx > 0) then !        3D
                        ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/id_laydim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_const(iid, j))
                     else
                        ierr = nf90_def_var(imapfile, trim(tmpstr), nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_const(iid, j))
                     end if
                     ierr = nf90_put_att(imapfile, id_const(iid, j), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_const(iid, j), 'standard_name', trim(tmpstr)//' concentration')
                     ierr = nf90_put_att(imapfile, id_const(iid, j), 'long_name', trim(tmpstr)//' concentration')
                     ierr = nf90_put_att(imapfile, id_const(iid, j), 'units', 'kg m-3')
                  end do
               end if

               if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
                  ierr = nf90_def_var(imapfile, 'e_dzdn', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_dzdn(iid))
                  ierr = nf90_put_att(imapfile, id_dzdn(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                  ierr = nf90_put_att(imapfile, id_dzdn(iid), 'long_name', 'Bed slope, n-component')
                  ierr = nf90_put_att(imapfile, id_dzdn(iid), 'units', '-')

                  ierr = nf90_def_var(imapfile, 'e_dzdt', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_dzdt(iid))
                  ierr = nf90_put_att(imapfile, id_dzdt(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
                  ierr = nf90_put_att(imapfile, id_dzdt(iid), 'long_name', 'Bed slope, t-component')
                  ierr = nf90_put_att(imapfile, id_dzdt(iid), 'units', '-')
               end if

               if (stmpar%morpar%moroutput%umod) then
                  ierr = nf90_def_var(imapfile, 'umod', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_umod(iid))
                  ierr = nf90_put_att(imapfile, id_umod(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_umod(iid), 'long_name', 'Characteristic velocity magnitude in cell centre')
                  ierr = nf90_put_att(imapfile, id_umod(iid), 'units', 'm s-1')
               end if

               if (stmpar%morpar%moroutput%zumod) then
                  ierr = nf90_def_var(imapfile, 'zumod', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_zumod(iid))
                  ierr = nf90_put_att(imapfile, id_zumod(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_zumod(iid), 'long_name', 'Height above bed for characteristic velocity in cell centre')
                  ierr = nf90_put_att(imapfile, id_zumod(iid), 'units', 'm')
               end if

               if (stmpar%morpar%moroutput%ustar) then
                  ierr = nf90_def_var(imapfile, 'ustar', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ustar(iid))
                  ierr = nf90_put_att(imapfile, id_ustar(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ustar(iid), 'long_name', 'Bed shear velocity u* in cell centre')
                  ierr = nf90_put_att(imapfile, id_ustar(iid), 'units', 'm s-1')
               end if

               if (stmpar%morpar%moroutput%sbcuv) then
                  ierr = nf90_def_var(imapfile, 'sbcx', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbcx(iid))
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'long_name', 'bed load transport due to currents, x-component')
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbcy', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbcy(iid))
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'long_name', 'bed load transport due to currents, y-component')
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbcx_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbcx_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'long_name', 'bed load transport due to currents (reconstructed), x-component')
                  ierr = nf90_put_att(imapfile, id_sbcx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbcy_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbcy_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'long_name', 'bed load transport due to currents (reconstructed), y-component')
                  ierr = nf90_put_att(imapfile, id_sbcy(iid), 'units', transpunit)
               end if

               if (stmpar%morpar%moroutput%sbwuv) then
                  ierr = nf90_def_var(imapfile, 'sbwx', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbwx(iid))
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'long_name', 'bed load transport due to waves, x-component')
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbwy', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbwy(iid))
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'long_name', 'bed load transport due to waves, y-component')
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbwx_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbwx_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'long_name', 'bed load transport due to waves (reconstructed), x-component')
                  ierr = nf90_put_att(imapfile, id_sbwx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sbwy_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sbwy_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'long_name', 'bed load transport due to waves (reconstructed), y-component')
                  ierr = nf90_put_att(imapfile, id_sbwy(iid), 'units', transpunit)
               end if

               if (stmpar%morpar%moroutput%sswuv) then
                  ierr = nf90_def_var(imapfile, 'sswx', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sswx(iid))
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'long_name', 'suspended load transport due to waves, x-component')
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sswy', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sswy(iid))
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'long_name', 'suspended load transport due to waves, y-component')
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sswx_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sswx_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'long_name', 'suspended load transport due to waves (reconstructed), x-component')
                  ierr = nf90_put_att(imapfile, id_sswx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sswy_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sswy_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'long_name', 'suspended load transport due to waves (reconstructed), y-component')
                  ierr = nf90_put_att(imapfile, id_sswy(iid), 'units', transpunit)
               end if

               if (stmpar%morpar%moroutput%sscuv) then
                  ierr = nf90_def_var(imapfile, 'sscx', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sscx(iid))
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'long_name', 'suspended load transport due to currents, x-component')
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sscy', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sscy(iid))
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'long_name', 'suspended load transport due to currents, y-component')
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sscx_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sscx_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'long_name', 'suspended load transport due to currents (reconstructed), x-component')
                  ierr = nf90_put_att(imapfile, id_sscx(iid), 'units', transpunit)

                  ierr = nf90_def_var(imapfile, 'sscy_reconstructed', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_sscy_reconstructed(iid))
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'long_name', 'suspended load transport due to currents (reconstructed), y-component')
                  ierr = nf90_put_att(imapfile, id_sscy(iid), 'units', transpunit)
               end if

               ierr = nf90_def_var(imapfile, 'sxtot', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sxtot(iid))
               ierr = nf90_put_att(imapfile, id_sxtot(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_sxtot(iid), 'long_name', 'total sediment transport in flow cell center, x-component')
               ierr = nf90_put_att(imapfile, id_sxtot(iid), 'units', transpunit)

               ierr = nf90_def_var(imapfile, 'sytot', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_sytot(iid))
               ierr = nf90_put_att(imapfile, id_sytot(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_sytot(iid), 'long_name', 'total sediment transport in flow cell center, y-component')
               ierr = nf90_put_att(imapfile, id_sytot(iid), 'units', transpunit)

               ierr = nf90_def_var(imapfile, 'mor_bl', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_morbl(iid))
               ierr = nf90_put_att(imapfile, id_morbl(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_morbl(iid), 'long_name', 'Time-varying bottom level in flow cell center')
               ierr = nf90_put_att(imapfile, id_morbl(iid), 'units', 'm')

               select case (stmpar%morlyr%settings%iunderlyr)
               case (1)
                  ierr = nf90_def_var(imapfile, 'bodsed', nf90_double, (/id_sedtotdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_bodsed(iid))
                  ierr = nf90_put_att(imapfile, id_bodsed(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_bodsed(iid), 'long_name', 'available sediment in the bed in flow cell center')
                  ierr = nf90_put_att(imapfile, id_bodsed(iid), 'units', 'kg m-2')

                  ierr = nf90_def_var(imapfile, 'dpsed', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dpsed(iid))
                  ierr = nf90_put_att(imapfile, id_dpsed(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_dpsed(iid), 'long_name', 'sediment thickness in the bed in flow cell center')
                  ierr = nf90_put_att(imapfile, id_dpsed(iid), 'units', 'm')
               case (2)
                  ierr = nf90_def_var(imapfile, 'msed', nf90_double, (/id_sedtotdim(iid), id_nlyrdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_msed(iid))
                  ierr = nf90_put_att(imapfile, id_msed(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_msed(iid), 'long_name', 'available sediment in a layer of the bed in flow cell center')
                  ierr = nf90_put_att(imapfile, id_msed(iid), 'units', 'kg m-2')

                  ierr = nf90_def_var(imapfile, 'lyrfrac', nf90_double, (/id_sedtotdim(iid), id_nlyrdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_lyrfrac(iid))
                  ierr = nf90_put_att(imapfile, id_lyrfrac(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_lyrfrac(iid), 'long_name', 'volume fraction in a layer of the bed in flow cell center')
                  ierr = nf90_put_att(imapfile, id_lyrfrac(iid), 'units', '-')

                  ierr = nf90_def_var(imapfile, 'thlyr', nf90_double, (/id_nlyrdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_thlyr(iid))
                  ierr = nf90_put_att(imapfile, id_thlyr(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_thlyr(iid), 'long_name', 'thickness of a layer of the bed in flow cell center')
                  ierr = nf90_put_att(imapfile, id_thlyr(iid), 'units', 'm')

                  if (stmpar%morlyr%settings%iporosity > 0) then
                     ierr = nf90_def_var(imapfile, 'poros', nf90_double, (/id_nlyrdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_poros(iid))
                     ierr = nf90_put_att(imapfile, id_poros(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_poros(iid), 'long_name', 'porosity of a layer of the bed in flow cell center')
                     ierr = nf90_put_att(imapfile, id_poros(iid), 'units', '-')
                  end if
               end select

               if (stmpar%morpar%moroutput%taurat) then
                  ierr = nf90_def_var(imapfile, 'taurat', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_taurat(iid))
                  ierr = nf90_put_att(imapfile, id_taurat(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_taurat(iid), 'long_name', 'Excess bed shear ratio')
                  ierr = nf90_put_att(imapfile, id_taurat(iid), 'units', '-')
               end if
               if (stmpar%morpar%moroutput%dm) then
                  ierr = nf90_def_var(imapfile, 'dm', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dm(iid))
                  ierr = nf90_put_att(imapfile, id_dm(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_dm(iid), 'long_name', 'Arithmetic mean sediment diameter')
                  ierr = nf90_put_att(imapfile, id_dm(iid), 'units', 'm')
               end if
               if (stmpar%morpar%moroutput%dg) then
                  ierr = nf90_def_var(imapfile, 'dg', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dg(iid))
                  ierr = nf90_put_att(imapfile, id_dg(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_dg(iid), 'long_name', 'Geometric mean sediment diameter')
                  ierr = nf90_put_att(imapfile, id_dg(iid), 'units', 'm')
               end if
               if (stmpar%morpar%moroutput%dgsd) then
                  ierr = nf90_def_var(imapfile, 'dgsd', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dgsd(iid))
                  ierr = nf90_put_att(imapfile, id_dgsd(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_dgsd(iid), 'long_name', 'Geometric standard deviation of particle size mix')
                  ierr = nf90_put_att(imapfile, id_dgsd(iid), 'units', 'm')
               end if
               if (stmpar%morpar%moroutput%percentiles) then
                  do l = 1, stmpar%morpar%nxx
                     write (dxname, '(A,I2.2)') 'DXX', l
                     write (dxdescr, '(A,F4.1,A)') 'Sediment diameter percentile ', stmpar%morpar%xx(l) * 100d0, ' %'
                     ierr = nf90_def_var(imapfile, dxname, nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dxx(l, iid))
                     ierr = nf90_put_att(imapfile, id_dxx(l, iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                     ierr = nf90_put_att(imapfile, id_dxx(l, iid), 'long_name', dxdescr)
                     ierr = nf90_put_att(imapfile, id_dxx(l, iid), 'units', 'm')
                  end do
               end if
               if (stmpar%morpar%moroutput%frac) then
                  ierr = nf90_def_var(imapfile, 'frac', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_frac(iid))
                  ierr = nf90_put_att(imapfile, id_frac(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_frac(iid), 'long_name', 'Availability fraction in top layer')
                  ierr = nf90_put_att(imapfile, id_frac(iid), 'units', '-')
               end if
               if (stmpar%morpar%moroutput%mudfrac) then
                  ierr = nf90_def_var(imapfile, 'mudfrac', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_mudfrac(iid))
                  ierr = nf90_put_att(imapfile, id_mudfrac(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_mudfrac(iid), 'long_name', 'Mud fraction in top layer')
                  ierr = nf90_put_att(imapfile, id_mudfrac(iid), 'units', '-')
               end if
               if (stmpar%morpar%moroutput%sandfrac) then
                  ierr = nf90_def_var(imapfile, 'sandfrac', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_sandfrac(iid))
                  ierr = nf90_put_att(imapfile, id_sandfrac(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_sandfrac(iid), 'long_name', 'Sand fraction in top layer')
                  ierr = nf90_put_att(imapfile, id_sandfrac(iid), 'units', '-')
               end if
               if (stmpar%morpar%moroutput%fixfac) then
                  ierr = nf90_def_var(imapfile, 'fixfac', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_fixfac(iid))
                  ierr = nf90_put_att(imapfile, id_fixfac(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_fixfac(iid), 'long_name', 'Reduction factor due to limited sediment thickness')
                  ierr = nf90_put_att(imapfile, id_fixfac(iid), 'units', '-')
               end if
               if (stmpar%morpar%moroutput%hidexp) then
                  ierr = nf90_def_var(imapfile, 'hidexp', nf90_double, (/id_flowelemdim(iid), id_sedtotdim(iid), id_timedim(iid)/), id_hidexp(iid))
                  ierr = nf90_put_att(imapfile, id_hidexp(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_hidexp(iid), 'long_name', 'Hiding and exposure factor')
                  ierr = nf90_put_att(imapfile, id_hidexp(iid), 'units', '-')
               end if
               ! Fluff layers
               if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
                  ierr = nf90_def_var(imapfile, 'mfluff', nf90_double, (/id_flowelemdim(iid), id_sedsusdim(iid), id_timedim(iid)/), id_mfluff(iid))
                  ierr = nf90_put_att(imapfile, id_mfluff(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_mfluff(iid), 'long_name', 'Sediment mass in fluff layer')
                  ierr = nf90_put_att(imapfile, id_mfluff(iid), 'units', 'kg m-2 ')
               end if
            end if

            if (bfmpar%lfbedfrmout) then
               if (bfmpar%lfbedfrm) then
                  ! DUNEHEIGHT
                  ierr = nf90_def_var(imapfile, 'duneheight', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_duneheight(iid))
                  ierr = nf90_put_att(imapfile, id_duneheight(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_duneheight(iid), 'long_name', 'Time-varying dune height in flow cell centers')
                  ierr = nf90_put_att(imapfile, id_duneheight(iid), 'units', 'm')
                  ! DUNELENGTH
                  ierr = nf90_def_var(imapfile, 'dunelength', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_dunelength(iid))
                  ierr = nf90_put_att(imapfile, id_dunelength(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_dunelength(iid), 'long_name', 'Time-varying dune length in flow cell centers')
                  ierr = nf90_put_att(imapfile, id_dunelength(iid), 'units', 'm')
               end if
               if (bfmpar%lfbedfrmrou) then
                  call realloc(rks, ndx, keepExisting=.false., fill=0d0)
                  ! KSR
                  ierr = nf90_def_var(imapfile, 'ksr', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ksr(iid))
                  ierr = nf90_put_att(imapfile, id_ksr(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ksr(iid), 'long_name', 'Ripple roughness height in flow cell center')
                  ierr = nf90_put_att(imapfile, id_ksr(iid), 'units', 'm')
                  ! KSMR
                  ierr = nf90_def_var(imapfile, 'ksmr', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ksmr(iid))
                  ierr = nf90_put_att(imapfile, id_ksmr(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ksmr(iid), 'long_name', 'Mega-ripple roughness height in flow cell center')
                  ierr = nf90_put_att(imapfile, id_ksmr(iid), 'units', 'm')
                  ! KSD
                  ierr = nf90_def_var(imapfile, 'ksd', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ksd(iid))
                  ierr = nf90_put_att(imapfile, id_ksd(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ksd(iid), 'long_name', 'Dune roughness height in flow cell center')
                  ierr = nf90_put_att(imapfile, id_ksd(iid), 'units', 'm')
                  ! KS
                  ierr = nf90_def_var(imapfile, 'ks', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_ks(iid))
                  ierr = nf90_put_att(imapfile, id_ks(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ks(iid), 'long_name', 'Bedform roughness height in flow cell center')
                  ierr = nf90_put_att(imapfile, id_ks(iid), 'units', 'm')
               end if
            end if
            if (jased > 0 .and. .not. stm_included) then
               ierr = nf90_def_dim(imapfile, 'nFrac', mxgr, id_maxfracdim(iid))

               if (jaceneqtr == 1) then
                  ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
                  if (ierr /= nf90_noerr) then
                     ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
                  end if
               else
                  ierr = nf90_inq_dimid(imapfile, 'nNetNode', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
               end if

               ierr = nf90_def_var(imapfile, 'sed', nf90_double, (/id_maxfracdim(iid), id_flowelemdim(iid), id_timedim(iid)/), id_sed(iid))
               ierr = nf90_put_att(imapfile, id_sed(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_sed(iid), 'long_name', 'sediment concentration')
               ierr = nf90_put_att(imapfile, id_sed(iid), 'units', 'kg m-3')
               ierr = nf90_def_var(imapfile, 'ero', nf90_double, (/id_maxfracdim(iid), id_erolaydim(iid), id_timedim(iid)/), id_ero(iid))
               if (jaceneqtr == 1) then
                  ierr = nf90_put_att(imapfile, id_ero(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
                  ierr = nf90_put_att(imapfile, id_ero(iid), 'long_name', 'erodable layer thickness per size fraction in flow element center')
               else
                  ierr = nf90_put_att(imapfile, id_ero(iid), 'coordinates', 'NetNode_x NetNode_y')
                  ierr = nf90_put_att(imapfile, id_ero(iid), 'long_name', 'erodable layer thickness per size fraction at flow element corners')
               end if
               ierr = nf90_put_att(imapfile, id_ero(iid), 'standard_name', 'Erodable layer thickness') ! Not CF
               ierr = nf90_put_att(imapfile, id_ero(iid), 'units', 'm')

               if (jaceneqtr /= 1) then
                  idims(1) = id_erolaydim(iid)
                  call definencvar(imapfile, id_zk(iid), nf90_double, idims, 'netnode_bedlevel_zk', 'Flow element corner bedlevel (zk)', 'm', 'NetNode_x NetNode_y')
               end if
               idims(1) = id_flowelemdim(iid)
               call definencvar(imapfile, id_bl(iid), nf90_double, idims, 'flowelem_bedlevel_bl', 'Flow element center bedlevel (bl)', 'm', 'FlowElem_xcc FlowElem_ycc')

            end if

            ! JRE waves
            if (jawave == 4) then
               ierr = nf90_def_var(imapfile, 'E', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_E(iid))
               ierr = nf90_put_att(imapfile, id_E(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_E(iid), 'standard_name', 'sea_surface_bulk_wave_energy') ! not CF
               ierr = nf90_put_att(imapfile, id_E(iid), 'long_name', 'wave energy per square meter')
               ierr = nf90_put_att(imapfile, id_E(iid), 'units', 'J m-2')

               ierr = nf90_def_var(imapfile, 'R', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_R(iid))
               ierr = nf90_put_att(imapfile, id_R(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_R(iid), 'standard_name', 'sea_surface_bulk_roller_energy') ! not CF
               ierr = nf90_put_att(imapfile, id_R(iid), 'long_name', 'roller energy per square meter')
               ierr = nf90_put_att(imapfile, id_R(iid), 'units', 'J m-2')

               ierr = nf90_def_var(imapfile, 'DR', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_DR(iid))
               ierr = nf90_put_att(imapfile, id_DR(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_DR(iid), 'standard_name', 'sea_surface_bulk_roller_dissipation') ! not CF
               ierr = nf90_put_att(imapfile, id_DR(iid), 'long_name', 'roller energy dissipation per square meter')
               ierr = nf90_put_att(imapfile, id_DR(iid), 'units', 'W m-2')

               ierr = nf90_def_var(imapfile, 'D', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_D(iid))
               ierr = nf90_put_att(imapfile, id_D(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_D(iid), 'standard_name', 'sea_surface_wave_breaking_dissipation') ! not CF
               ierr = nf90_put_att(imapfile, id_D(iid), 'long_name', 'wave breaking energy dissipation per square meter')
               ierr = nf90_put_att(imapfile, id_D(iid), 'units', 'W m-2')
               ! JRE TO DO: change definition in unc file to correct one
               ierr = nf90_def_var(imapfile, 'H', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_H(iid))
               ierr = nf90_put_att(imapfile, id_H(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_H(iid), 'standard_name', 'sea_surface_wave_rms_height')
               ierr = nf90_put_att(imapfile, id_H(iid), 'long_name', 'Root mean square wave height based on wave energy')
               ierr = nf90_put_att(imapfile, id_H(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, 'urms_cc', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_urmscc(iid))
               ierr = nf90_put_att(imapfile, id_urmscc(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_urmscc(iid), 'standard_name', 'sea_surface_wave_orbital_velocity')
               ierr = nf90_put_att(imapfile, id_urmscc(iid), 'long_name', 'Root mean square orbital velocity on flow centers')
               ierr = nf90_put_att(imapfile, id_urmscc(iid), 'units', 'm/s')

               ierr = nf90_def_var(imapfile, 'Fx_cc', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_Fxcc(iid))
               ierr = nf90_put_att(imapfile, id_Fxcc(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_Fxcc(iid), 'standard_name', 'sea_surface_wave_force_east')
               ierr = nf90_put_att(imapfile, id_Fxcc(iid), 'long_name', 'Wave induced flow forcing in cell centre, east component')
               ierr = nf90_put_att(imapfile, id_Fxcc(iid), 'units', 'kg m s-2')

               ierr = nf90_def_var(imapfile, 'Fy_cc', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_Fycc(iid))
               ierr = nf90_put_att(imapfile, id_Fycc(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_Fycc(iid), 'standard_name', 'sea_surface_wave_force_north')
               ierr = nf90_put_att(imapfile, id_Fycc(iid), 'long_name', 'Wave induced flow forcing in cell centre, north component')
               ierr = nf90_put_att(imapfile, id_Fycc(iid), 'units', 'kg m s-2')

               ierr = nf90_def_var(imapfile, 'thetamean', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_thetamean(iid))
               ierr = nf90_put_att(imapfile, id_thetamean(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_thetamean(iid), 'standard_name', 'sea_surface_wave_from_direction') ! not CF
               ierr = nf90_put_att(imapfile, id_thetamean(iid), 'long_name', 'mean wave angle')
               ierr = nf90_put_att(imapfile, id_thetamean(iid), 'units', 'deg')

               ierr = nf90_def_var(imapfile, 'cwav', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_cwav(iid))
               ierr = nf90_put_att(imapfile, id_cwav(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_cwav(iid), 'standard_name', 'sea_surface_wave_phase_celerity') ! not CF
               ierr = nf90_put_att(imapfile, id_cwav(iid), 'long_name', 'phase celerity')
               ierr = nf90_put_att(imapfile, id_cwav(iid), 'units', 'm s-1')

               ierr = nf90_def_var(imapfile, 'cgwav', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_cgwav(iid))
               ierr = nf90_put_att(imapfile, id_cgwav(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_cgwav(iid), 'standard_name', 'sea_surface_wave_group_celerity') ! not CF
               ierr = nf90_put_att(imapfile, id_cgwav(iid), 'long_name', 'group celerity')
               ierr = nf90_put_att(imapfile, id_cgwav(iid), 'units', 'm s-1')

               ierr = nf90_def_var(imapfile, 'sigmwav', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_sigmwav(iid))
               ierr = nf90_put_att(imapfile, id_sigmwav(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_sigmwav(iid), 'standard_name', 'sea_surface_wave_mean_frequency') ! not CF
               ierr = nf90_put_att(imapfile, id_sigmwav(iid), 'long_name', 'mean wave frequency')
               ierr = nf90_put_att(imapfile, id_sigmwav(iid), 'units', 'rad s-1')

               !if ( (windmodel.eq.1) .and. (jawsource.eq.1) ) then
               !
               !   ierr = nf90_def_var(imapfile, 'SwE',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwE(iid))
               !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'standard_name', 'source_term_wind_on_E')                          ! not CF
               !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'long_name'    , 'source term wind on wave energy')
               !   ierr = nf90_put_att(imapfile, id_SwE(iid),   'units'        , 'J m-2 s-1')
               !
               !   ierr = nf90_def_var(imapfile, 'SwT',  nf90_double, (/ id_flowelemdim(iid), id_timedim(iid) /) , id_SwT(iid))
               !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'coordinates'  , 'FlowElem_xcc FlowElem_ycc')
               !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'standard_name', 'source_term_wind_on_T')                          ! not CF
               !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'long_name'    , 'source term wind on wave period')
               !   ierr = nf90_put_att(imapfile, id_SwT(iid),   'units'        , 's s-1')
               !
               !endif
            end if

            if (NUMCONST == 0) then
               ierr = unc_add_gridmapping_att(imapfile, (/id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid)/), jsferic) ! add id_ucz(iid)?
            else
               if (allocated(idum)) then
                  deallocate (idum)
               end if
               allocate (idum(7 + NUMCONST))
               idum(1:7) = (/id_s1(iid), id_taus(iid), id_ucx(iid), id_ucy(iid), id_unorm(iid), id_sa1(iid), id_sed(iid)/)
               do j = 1, NUMCONST
                  idum(7 + j) = id_const(iid, j)
               end do
               ierr = unc_add_gridmapping_att(imapfile, idum, jsferic)
            end if
            if (kmx > 0) then
               if (density_is_pressure_dependent()) then
                  ierr = unc_add_gridmapping_att(imapfile, (/id_ucz(iid), id_ucxa(iid), id_ucya(iid), id_ww1(iid), id_rho(iid)/), jsferic)
               else
                  ierr = unc_add_gridmapping_att(imapfile, (/id_ucz(iid), id_ucxa(iid), id_ucya(iid), id_ww1(iid), id_rhop(iid)/), jsferic)
               end if
            end if

            if (jamaptrachy > 0 .and. jatrt == 1) then
               ! Roughness data on net-links
               ierr = nf90_def_var(imapfile, 'cftrt', nf90_double, (/id_netlinkdim(iid), id_timedim(iid)/), id_cftrt(iid))
               if (ifrctypuni == 0) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'long_name', 'Chezy roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'units', 'm0.5s-1') ! WO: does not follow standard ? (which accepts only integral powers?)
               elseif (ifrctypuni == 1) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'long_name', 'Manning roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'units', 'sm-0.333') ! WO: does not follow standard ? (which accepts only integral powers?)
               elseif ((ifrctypuni == 2) .or. (ifrctypuni == 3)) then
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'long_name', 'White-Colebrook roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'units', 'm')
               else
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'long_name', 'roughness from trachytopes')
                  ierr = nf90_put_att(imapfile, id_cftrt(iid), 'units', ' ')
               end if
            end if

            if (jamapcali > 0 .and. jacali == 1) then
               ! Calibration factor for roughness data on net-links
               ierr = nf90_def_var(imapfile, 'cfcl', nf90_double, (/id_netlinkdim(iid), id_timedim(iid)/), id_cfcl(iid))
               ierr = nf90_put_att(imapfile, id_cfcl(iid), 'long_name', 'Calibration factor for roughness')
               ierr = nf90_put_att(imapfile, id_cfcl(iid), 'units', ' ')
            end if

            if (jamap_chezy_elements > 0) then
               ! Chezy data on flow-nodes
               ierr = nf90_def_var(imapfile, 'czs', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_czs(iid))
               ierr = nf90_put_att(imapfile, id_czs(iid), 'long_name', 'Chezy roughness')
               ierr = nf90_put_att(imapfile, id_czs(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
               ierr = nf90_put_att(imapfile, id_czs(iid), 'units', 'm0.5s-1') ! WO: does not follow standard ? (which accepts only integral powers?)
            end if
            if (jamap_chezy_links > 0) then
               ! Chezy data on flow-links
               ierr = nf90_def_var(imapfile, 'czu', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_czu(iid))
               ierr = nf90_put_att(imapfile, id_czu(iid), 'long_name', 'Chezy roughness on flow links')
               ierr = nf90_put_att(imapfile, id_czu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
               ierr = nf90_put_att(imapfile, id_czu(iid), 'units', 'm0.5s-1')
            end if

            ! 1D2D boundaries
            if (nbnd1d2d > 0) then
               ierr = nf90_def_var(imapfile, '1d2d_flowlinknrs', nf90_int, (/id_1d2ddim(iid)/), id_1d2d_edges(iid))
               ierr = nf90_put_att(imapfile, id_czs(iid), 'long_name', 'flow link numbers of the open 1D2D boundary links')

               ierr = nf90_def_var(imapfile, '1d2d_zeta', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_zeta1d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid), 'standard_name', 'sea_surface_height_above_geoid')
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid), 'long_name', '1D water level next to each 1d2d boundary link')
               ierr = nf90_put_att(imapfile, id_1d2d_zeta1d(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, '1d2d_crest_level', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_crest_level(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid), 'standard_name', 'sea_surface_height_above_geoid')
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid), 'long_name', 'crest level of 1d2d boundary link')
               ierr = nf90_put_att(imapfile, id_1d2d_crest_level(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, '1d2d_b_2di', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_b_2di(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid), 'standard_name', 'b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid), 'long_name', 'coefficient for 1d2d interface b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2di(iid), 'units', '-')

               ierr = nf90_def_var(imapfile, '1d2d_b_2dv', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_b_2dv(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid), 'standard_name', 'b_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid), 'long_name', 'coefficient for 1d2d interface b_2di')
               ierr = nf90_put_att(imapfile, id_1d2d_b_2dv(iid), 'units', '-')

               ierr = nf90_def_var(imapfile, '1d2d_d_2dv', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_d_2dv(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid), 'standard_name', 'd_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid), 'long_name', 'coefficient for 1d2d interface d_2dv')
               ierr = nf90_put_att(imapfile, id_1d2d_d_2dv(iid), 'units', '-')

               ierr = nf90_def_var(imapfile, '1d2d_qzeta', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_q_zeta(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid), 'standard_name', 'q_zeta_1d2d')
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid), 'long_name', 'q_zeta_1d2d')
               ierr = nf90_put_att(imapfile, id_1d2d_q_zeta(iid), 'units', 'm2 s-1')

               ierr = nf90_def_var(imapfile, '1d2d_q_lat', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_q_lat(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid), 'standard_name', 'q_lat')
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid), 'long_name', 'q_lat')
               ierr = nf90_put_att(imapfile, id_1d2d_q_lat(iid), 'units', 'm3 s-1')

               ierr = nf90_def_var(imapfile, '1d2d_cfl', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_cfl(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid), 'standard_name', 'cfl')
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid), 'long_name', 'wave flow courant')
               ierr = nf90_put_att(imapfile, id_1d2d_cfl(iid), 'units', '-')

               ierr = nf90_def_var(imapfile, '1d2d_sb', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_sb(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid), 'standard_name', '1d2d_sb')
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid), 'long_name', 'water levels in boundary points')
               ierr = nf90_put_att(imapfile, id_1d2d_sb(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, '1d2d_s0_2d', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_s0_2d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid), 'standard_name', '1d2d_s0_2d')
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid), 'long_name', 'water levels on interface at previous time step')
               ierr = nf90_put_att(imapfile, id_1d2d_s0_2d(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, '1d2d_s1_2d', nf90_double, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_s1_2d(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid), 'standard_name', '1d2d_s1_2d')
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid), 'long_name', 'water levels on interface at current time step')
               ierr = nf90_put_att(imapfile, id_1d2d_s1_2d(iid), 'units', 'm')

               ierr = nf90_def_var(imapfile, '1d2d_flow_cond', nf90_int, (/id_1d2ddim(iid), id_timedim(iid)/), id_1d2d_flow_cond(iid))
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid), 'standard_name', 'flow_condition')
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid), 'long_name', 'flow Condition 0: closed, 1: free 1d to 2d, 2: free 2d to 1d, 3: submerged')
               ierr = nf90_put_att(imapfile, id_1d2d_flow_cond(iid), 'units', '-')

            end if
         end if

         if (jamapwind > 0 .and. japatm > 0) then
            call definencvar(imapfile, id_patm(iid), nf90_double, idims, 'Patm', 'Atmospheric Pressure', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         end if

         if (ice_mapout) then
            call definencvar(imapfile, id_ice_af(iid), nf90_double, idims, 'ice_af', 'Fraction of the surface area covered by floating ice', '1', 'FlowElem_xcc FlowElem_ycc')
            call definencvar(imapfile, id_ice_h(iid), nf90_double, idims, 'ice_h', 'Thickness of floating ice cover', 'm', 'FlowElem_xcc FlowElem_ycc')
            call definencvar(imapfile, id_ice_p(iid), nf90_double, idims, 'ice_p', 'Pressure exerted by the floating ice cover', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
            if (ja_icecover == ICECOVER_SEMTNER) then
               call definencvar(imapfile, id_ice_t(iid), nf90_double, idims, 'ice_t', 'Temperature of the floating ice cover', 'degC', 'FlowElem_xcc FlowElem_ycc')
               call definencvar(imapfile, id_snow_h(iid), nf90_double, idims, 'snow_h', 'Thickness of the snow layer', 'm', 'FlowElem_xcc FlowElem_ycc')
               call definencvar(imapfile, id_snow_t(iid), nf90_double, idims, 'snow_t', 'Temperature of the snow layer', 'degC', 'FlowElem_xcc FlowElem_ycc')
            end if
         end if

         if ((jamapwind > 0 .or. jamapwindstress > 0 .or. jaseparate_ == 2) .and. jawind /= 0) then
            if (jawindstressgiven == 0 .or. jaseparate_ == 2) then
               ierr = nf90_def_var(imapfile, 'windx', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_windx(iid))
               ierr = nf90_def_var(imapfile, 'windy', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_windy(iid))
            else
               ierr = nf90_def_var(imapfile, 'windstressx', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_windx(iid))
               ierr = nf90_def_var(imapfile, 'windstressy', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_windy(iid))
            end if

            ierr = nf90_put_att(imapfile, id_windx(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            if (jawindstressgiven == 0 .or. jaseparate_ == 2) then
               if (jsferic == 0) then
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'standard_name', 'x_wind')
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'long_name', 'velocity of air on flow element center, x-component')
               else
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'standard_name', 'eastward_wind')
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'long_name', 'eastward air velocity on flow element center, x-component')
               end if
               ierr = nf90_put_att(imapfile, id_windx(iid), 'units', 'm s-1')
            else
               if (jsferic == 0) then
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'standard_name', 'x_windstress')
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'long_name', 'windstress on flow element center, x-component')
               else
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'standard_name', 'eastward_windstress')
                  ierr = nf90_put_att(imapfile, id_windx(iid), 'long_name', 'eastward windstress on flow element center, x-component')
               end if
               ierr = nf90_put_att(imapfile, id_windx(iid), 'units', 'N m-2')
            end if

            ierr = nf90_put_att(imapfile, id_windy(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            if (jawindstressgiven == 0 .or. jaseparate_ == 2) then
               if (jsferic == 0) then
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'standard_name', 'y_wind')
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'long_name', 'velocity of air on flow element center, y-component')
               else
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'standard_name', 'northward_wind')
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'long_name', 'northward air velocity on flow element center, y-component')
               end if
               ierr = nf90_put_att(imapfile, id_windy(iid), 'units', 'm s-1')
            else
               if (jsferic == 0) then
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'standard_name', 'y_windstress')
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'long_name', 'windstress air on flow element center, y-component')
               else
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'standard_name', 'northward_windstress')
                  ierr = nf90_put_att(imapfile, id_windy(iid), 'long_name', 'northward windstress on flow element center, y-component')
               end if
               ierr = nf90_put_att(imapfile, id_windy(iid), 'units', 'N m-2')
            end if
         end if

         if (jamapwind > 0 .and. jawind /= 0 .and. jawindstressgiven == 0) then
            ! Also wind on flow links
            ierr = nf90_def_var(imapfile, 'windxu', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_windxu(iid))
            ierr = nf90_def_var(imapfile, 'windyu', nf90_double, (/id_flowlinkdim(iid), id_timedim(iid)/), id_windyu(iid))

            ierr = nf90_put_att(imapfile, id_windxu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_windxu(iid), 'long_name', 'velocity of air on flow links, x-component')
               ierr = nf90_put_att(imapfile, id_windxu(iid), 'standard_name', 'x_velocity_wind')
            else
               ierr = nf90_put_att(imapfile, id_windxu(iid), 'long_name', 'eastward air velocity on flow links, x-component')
               ierr = nf90_put_att(imapfile, id_windxu(iid), 'standard_name', 'eastward_wind')
            end if
            ierr = nf90_put_att(imapfile, id_windxu(iid), 'units', 'm s-1')

            ierr = nf90_put_att(imapfile, id_windyu(iid), 'coordinates', 'FlowLink_xu FlowLink_yu')
            if (jsferic == 0) then
               ierr = nf90_put_att(imapfile, id_windyu(iid), 'long_name', 'velocity of air on flow links, y-component')
               ierr = nf90_put_att(imapfile, id_windyu(iid), 'standard_name', 'y_velocity_wind')
            else
               ierr = nf90_put_att(imapfile, id_windyu(iid), 'long_name', 'northward air velocity on flow links, y-component')
               ierr = nf90_put_att(imapfile, id_windyu(iid), 'standard_name', 'northward_wind')
            end if
            ierr = nf90_put_att(imapfile, id_windyu(iid), 'units', 'm s-1')
         end if
         !
         ierr = unc_add_gridmapping_att(imapfile, (/id_windx(iid), id_windy(iid), id_windxu(iid), id_windyu(iid), nf90_global/), jsferic)

         if (javeg > 0) then
            ierr = nf90_def_var(imapfile, 'rnveg', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_rnveg(iid))
            ierr = nf90_put_att(imapfile, id_rnveg(iid), 'long_name', 'Stem density of vegetation')
            ierr = nf90_put_att(imapfile, id_rnveg(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_rnveg(iid), 'units', 'm-2')

            ierr = nf90_def_var(imapfile, 'diaveg', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_diaveg(iid))
            ierr = nf90_put_att(imapfile, id_diaveg(iid), 'long_name', 'Stem diameter of vegetation')
            ierr = nf90_put_att(imapfile, id_diaveg(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_diaveg(iid), 'units', 'm')

            ierr = nf90_def_var(imapfile, 'veg_stemheight', nf90_double, (/id_flowelemdim(iid), id_timedim(iid)/), id_veg_stemheight(iid))
            ierr = nf90_put_att(imapfile, id_veg_stemheight(iid), 'long_name', 'Stem height of vegetation')
            ierr = nf90_put_att(imapfile, id_veg_stemheight(iid), 'coordinates', 'FlowElem_xcc FlowElem_ycc')
            ierr = nf90_put_att(imapfile, id_veg_stemheight(iid), 'units', 'm')
         end if

         ! For all 3D variables, expand the coordinate attribute with a vertical coordinate
         ierr = nf90_inq_varid(imapfile, 'LayCoord_cc', varid)
         if (ierr == NF90_NOERR) then
            zcc_elem = 'LayCoord_cc'
            zw_elem = 'LayCoord_w'
            zu_link = 'LayCoord_cc' ! z/sigma coords are the same for u-positions and cc-positions.
            zwu_link = 'LayCoord_w'
         else
            zcc_elem = 'FlowElem_zcc'
            zw_elem = 'FlowElem_zw'
            zu_link = '' ! To be added, Issue UNST-4880
            zwu_link = ''
         end if
         if (nf90_inquire(imapfile, nVariables=varid) == NF90_NOERR) then
            do while (varid > 0)
               if (nf90_inquire_variable(imapfile, varid, ndims=ndims) == NF90_NOERR) then
                  call realloc(idum, ndims, keepexisting=.false.)
                  if (nf90_inquire_variable(imapfile, varid, dimids=idum) == NF90_NOERR) then
                     if (any(idum == id_wdim(iid)) .and. any(idum == id_flowelemdim(iid))) then
                        ierr = ncu_append_atts(imapfile, varid, 'coordinates', trim(zw_elem))
                     end if
                     if (any(idum == id_laydim(iid)) .and. any(idum == id_flowelemdim(iid))) then
                        ierr = ncu_append_atts(imapfile, varid, 'coordinates', trim(zcc_elem))
                     end if
                     if (any(idum == id_wdim(iid)) .and. any(idum == id_flowlinkdim(iid))) then
                        ierr = ncu_append_atts(imapfile, varid, 'coordinates', trim(zwu_link))
                     end if
                     if (any(idum == id_laydim(iid)) .and. any(idum == id_flowelemdim(iid))) then
                        ierr = ncu_append_atts(imapfile, varid, 'coordinates', trim(zu_link))
                     end if
                  end if
               end if
               varid = varid - 1
            end do
         end if

         ierr = nf90_enddef(imapfile)

         ! 1D2D boundaries
         if (nbnd1d2d > 0 .and. jaseparate_ /= 2) then
            if (allocated(idum)) then
               deallocate (idum)
            end if
            allocate (idum(nbnd1d2d))
            do i = 1, nbnd1d2d
               idum(i) = kbnd1d2d(3, i) ! Flow link nrs
            end do
            ierr = nf90_put_var(imapfile, id_1d2d_edges(iid), idum)
            deallocate (idum)
         end if

         if (nomba > 0) then
            ierr = nf90_put_var(imapfile, id_mba(iid), mbadef(1:NdxNdxi))
         end if

         firststep(iid) = .false.

      end if

      ! End of writing time-independent flow geometry data.
      ! -- Inquire id's belonging to map file ------------------------
      if (firststep(iid) .and. ndim > 0) then ! TODO: AvD: UNST-530
         !
         !
         ! this step is necessary because if a snapshot_map.nc file is written
         ! in between two map file outputs the saved id's may have changed
         !
         firststep(iid) = .false.
         !
         ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim(iid))
         if (ierr /= nf90_noerr) then
            ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_flowelemdim(iid))
         end if

         ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim(iid))
         !
         ! Time
         ierr = nf90_inq_dimid(imapfile, 'time', id_timedim(iid))
         ierr = nf90_inq_varid(imapfile, 'time', id_time(iid))
         !
         if (kmx > 0) then
            ierr = nf90_inq_dimid(imapfile, 'laydim', id_laydim(iid))
            ierr = nf90_inq_dimid(imapfile, 'wdim', id_wdim(iid))
         end if
         !
         ! Size of latest timestep

         ! Why ask for id_*, they are in a save statement no?

         ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep(iid))
         ierr = nf90_inq_varid(imapfile, 'taus', id_taus(iid))
         !
         if (kmx > 0) then !  3D
            ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
            ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
            ierr = nf90_inq_varid(imapfile, 'ucz', id_ucz(iid))
            ierr = nf90_inq_varid(imapfile, 'ucxa', id_ucxa(iid))
            ierr = nf90_inq_varid(imapfile, 'ucya', id_ucya(iid))
            ierr = nf90_inq_varid(imapfile, 'ww1', id_ww1(iid))
            if (density_is_pressure_dependent()) then
               ierr = nf90_inq_varid(imapfile, 'density', id_rho(iid))
            else
               ierr = nf90_inq_varid(imapfile, 'rho', id_rhop(iid))
            end if
            if (iturbulencemodel >= 3) then
               ierr = nf90_inq_varid(imapfile, 'turkin1', id_turkin1(iid))
               ierr = nf90_inq_varid(imapfile, 'tureps1', id_tureps1(iid))
               ierr = nf90_inq_varid(imapfile, 'vicwwu', id_vicwwu(iid))
            end if
         else
            ierr = nf90_inq_varid(imapfile, 'ucx', id_ucx(iid))
            ierr = nf90_inq_varid(imapfile, 'ucy', id_ucy(iid))
            ierr = nf90_inq_varid(imapfile, 'spircrv', id_spircrv(iid))
            ierr = nf90_inq_varid(imapfile, 'spirint', id_spirint(iid))
         end if
         !
         if (jasal > 0) then
            ierr = nf90_inq_varid(imapfile, 'sa1', id_sa1(iid))
         end if

         if (jatem > 0) then
            ierr = nf90_inq_varid(imapfile, 'tem1', id_tem1(iid))
         end if

         if (ITRA1 > 0) then
            do j = ITRA1, ITRAN
               tmpstr = const_names(j)
               ! Forbidden chars in NetCDF names: space, /, and more.
               call replace_char(tmpstr, 32, 95)
               call replace_char(tmpstr, 47, 95)
               ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid, j))
            end do
         end if

         !
         if (stm_included) then
            ierr = nf90_inq_varid(imapfile, 'nSedTot', id_sedtotdim(iid))
            ierr = nf90_inq_varid(imapfile, 'nSedSus', id_sedsusdim(iid))
            ierr = nf90_inq_varid(imapfile, 'nBedLayers', id_nlyrdim(iid))

            if (stmpar%lsedsus > 0) then
               ierr = nf90_inq_varid(imapfile, 'ws', id_ws(iid))
               !
               ! equilibrium concentration, 2D only
               if (kmx == 0) then
                  ierr = nf90_inq_varid(imapfile, 'rsedeq', id_rsedeq(iid))
               end if

               if (stmpar%morpar%moroutput%sourcesink) then
                  ierr = nf90_inq_varid(imapfile, 'sourse', id_sourse(iid))
                  ierr = nf90_inq_varid(imapfile, 'sinkse', id_sinkse(iid))
               end if

               if (stmpar%morpar%moroutput%suvcor) then
                  ierr = nf90_inq_varid(imapfile, 'e_scrn', id_scrn(iid))
                  !ierr = nf90_inq_varid(imapfile, 'e_scrt', id_scrt(iid))
               end if

               if (stmpar%morpar%moroutput%aks) then
                  ierr = nf90_inq_varid(imapfile, 'aks', id_aks(iid))
                  ierr = nf90_inq_varid(imapfile, 'rca', id_rca(iid))
               end if
               !
               ! Suspended fractions
               if (stmpar%lsedsus > 0) then
                  do j = ISED1, ISEDN
                     tmpstr = const_names(j)
                     ! Forbidden chars in NetCDF names: space, /, and more.
                     call replace_char(tmpstr, 32, 95)
                     call replace_char(tmpstr, 47, 95)
                     ierr = nf90_inq_varid(imapfile, trim(tmpstr), id_const(iid, j))
                  end do
               end if
            end if

            if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
               ierr = nf90_inq_varid(imapfile, 'e_dzdn', id_dzdn(iid))
               ierr = nf90_inq_varid(imapfile, 'e_dzdt', id_dzdt(iid))
            end if

            if (stmpar%morpar%moroutput%umod) then
               ierr = nf90_inq_varid(imapfile, 'umod', id_umod(iid))
            end if

            if (stmpar%morpar%moroutput%zumod) then
               ierr = nf90_inq_varid(imapfile, 'zumod', id_zumod(iid))
            end if

            if (stmpar%morpar%moroutput%ustar) then
               ierr = nf90_inq_varid(imapfile, 'ustar', id_ustar(iid))
            end if

            if (stmpar%morpar%moroutput%sbcuv) then
               ierr = nf90_inq_varid(imapfile, 'sbcx', id_sbcx(iid))
               ierr = nf90_inq_varid(imapfile, 'sbcy', id_sbcy(iid))
               ierr = nf90_inq_varid(imapfile, 'sbcx_reconstructed', id_sbcx_reconstructed(iid))
               ierr = nf90_inq_varid(imapfile, 'sbcy_reconstructed', id_sbcy_reconstructed(iid))
            end if

            if (stmpar%morpar%moroutput%sbwuv) then
               ierr = nf90_inq_varid(imapfile, 'sbwx', id_sbwx(iid))
               ierr = nf90_inq_varid(imapfile, 'sbwy', id_sbwy(iid))
               ierr = nf90_inq_varid(imapfile, 'sbwx_reconstructed', id_sbwx_reconstructed(iid))
               ierr = nf90_inq_varid(imapfile, 'sbwy_reconstructed', id_sbwy_reconstructed(iid))
            end if

            if (stmpar%morpar%moroutput%sswuv) then
               ierr = nf90_inq_varid(imapfile, 'sswx', id_sswx(iid))
               ierr = nf90_inq_varid(imapfile, 'sswy', id_sswy(iid))
               ierr = nf90_inq_varid(imapfile, 'sswx_reconstructed', id_sswx_reconstructed(iid))
               ierr = nf90_inq_varid(imapfile, 'sswy_reconstructed', id_sswy_reconstructed(iid))
            end if

            if (stmpar%morpar%moroutput%sscuv) then
               ierr = nf90_inq_varid(imapfile, 'sscx', id_sscx(iid))
               ierr = nf90_inq_varid(imapfile, 'sscy', id_sscy(iid))
               ierr = nf90_inq_varid(imapfile, 'sscx_reconstructed', id_sscx_reconstructed(iid))
               ierr = nf90_inq_varid(imapfile, 'sscy_reconstructed', id_sscy_reconstructed(iid))
            end if

            ierr = nf90_inq_varid(imapfile, 'sxtot', id_sxtot(iid))
            ierr = nf90_inq_varid(imapfile, 'sytot', id_sytot(iid))

            ierr = nf90_inq_varid(imapfile, 'mor_bl', id_morbl(iid))

            select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ierr = nf90_inq_varid(imapfile, 'bodsed', id_bodsed(iid))
               ierr = nf90_inq_varid(imapfile, 'dpsed', id_dpsed(iid))
            case (2)
               ierr = nf90_inq_varid(imapfile, 'msed', id_msed(iid))
               ierr = nf90_inq_varid(imapfile, 'lyrfrac', id_lyrfrac(iid))
               ierr = nf90_inq_varid(imapfile, 'thlyr', id_thlyr(iid))
               if (stmpar%morlyr%settings%iporosity > 0) then
                  ierr = nf90_inq_varid(imapfile, 'poros', id_poros(iid))
               end if
            end select
            !
            if (stmpar%morpar%moroutput%taurat) then
               ierr = nf90_inq_varid(imapfile, 'taurat', id_taurat(iid))
            end if
            if (stmpar%morpar%moroutput%dm) then
               ierr = nf90_inq_varid(imapfile, 'dm', id_dm(iid))
            end if
            if (stmpar%morpar%moroutput%dg) then
               ierr = nf90_inq_varid(imapfile, 'dg', id_dg(iid))
            end if
            if (stmpar%morpar%moroutput%dgsd) then
               ierr = nf90_inq_varid(imapfile, 'dgsd', id_dgsd(iid))
            end if
            if (stmpar%morpar%moroutput%percentiles) then
               do l = 1, stmpar%morpar%nxx
                  write (dxname, '(A,I2.2)') 'DXX', l
                  ierr = nf90_inq_varid(imapfile, dxname, id_dxx(l, iid))
               end do
            end if
            if (stmpar%morpar%moroutput%frac) then
               ierr = nf90_inq_varid(imapfile, 'frac', id_frac(iid))
            end if
            if (stmpar%morpar%moroutput%mudfrac) then
               ierr = nf90_inq_varid(imapfile, 'mudfrac', id_mudfrac(iid))
            end if
            if (stmpar%morpar%moroutput%sandfrac) then
               ierr = nf90_inq_varid(imapfile, 'sandfrac', id_sandfrac(iid))
            end if
            if (stmpar%morpar%moroutput%fixfac) then
               ierr = nf90_inq_varid(imapfile, 'fixfac', id_fixfac(iid))
            end if
            if (stmpar%morpar%moroutput%hidexp) then
               ierr = nf90_inq_varid(imapfile, 'hidexp', id_hidexp(iid))
            end if
            ! Fluff layers
            if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
               ierr = nf90_inq_varid(imapfile, 'mfluff', id_mfluff(iid))
            end if
         end if

         if (bfmpar%lfbedfrmout) then
            if (bfmpar%lfbedfrm) then
               ierr = nf90_inq_varid(imapfile, 'duneheight', id_duneheight(iid))
               ierr = nf90_inq_varid(imapfile, 'dunelength', id_dunelength(iid))
            end if
            if (bfmpar%lfbedfrmrou) then
               ierr = nf90_inq_varid(imapfile, 'ksr', id_ksr(iid))
               ierr = nf90_inq_varid(imapfile, 'ksmr', id_ksmr(iid))
               ierr = nf90_inq_varid(imapfile, 'ksd', id_ksd(iid))
               ierr = nf90_inq_varid(imapfile, 'ks', id_ks(iid))
            end if
         end if
         !
         if (jased > 0 .and. .not. stm_included) then
            ierr = nf90_inq_dimid(imapfile, 'nFrac', id_maxfracdim(iid))
            if (jaceneqtr == 1) then
               ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
               if (ierr /= nf90_noerr) then
                  ierr = nf90_inq_dimid(imapfile, 'nFlowElemWithBnd', id_erolaydim(iid))
               end if
            else
               ierr = nf90_inq_dimid(imapfile, 'nNetNode', id_erolaydim(iid)) ! Note: points to an existing dimension (either nNetNode, or nFlowElem)
            end if
            !
            ierr = nf90_inq_varid(imapfile, 'sed', id_sed(iid))
            !
            ierr = nf90_inq_varid(imapfile, 'ero', id_ero(iid))
         end if
         !
         ! JRE - XBeach
         if (jawave == 4) then
            ierr = nf90_inq_varid(imapfile, 'E', id_E(iid))
            ierr = nf90_inq_varid(imapfile, 'R', id_R(iid))
            ierr = nf90_inq_varid(imapfile, 'H', id_H(iid))
            ierr = nf90_inq_varid(imapfile, 'D', id_D(iid))
            ierr = nf90_inq_varid(imapfile, 'DR', id_DR(iid))
            ierr = nf90_inq_varid(imapfile, 'urms', id_urms(iid))
            ierr = nf90_inq_varid(imapfile, 'urms_cc', id_urmscc(iid))
            ierr = nf90_inq_varid(imapfile, 'ust', id_ust(iid))
            ierr = nf90_inq_varid(imapfile, 'vst', id_vst(iid))
            ierr = nf90_inq_varid(imapfile, 'Fx_cc', id_Fxcc(iid))
            ierr = nf90_inq_varid(imapfile, 'Fy_cc', id_Fycc(iid))

            ierr = nf90_inq_varid(imapfile, 'thetamean', id_thetamean(iid))
            ierr = nf90_inq_varid(imapfile, 'cwav', id_cwav(iid))
            ierr = nf90_inq_varid(imapfile, 'cgwav', id_cgwav(iid))
            ierr = nf90_inq_varid(imapfile, 'sigmwav', id_sigmwav(iid))

            !if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
            !   ierr = nf90_inq_varid(imapfile, 'SwE'  , id_SwE(iid))
            !   ierr = nf90_inq_varid(imapfile, 'SwT'  , id_SwT(iid))
            !endif

         end if

         ! 1D2D boundaries
         if (nbnd1d2d > 0) then
            ierr = nf90_inq_varid(imapfile, '1d2d_flowlinknrs', id_1d2d_edges(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_zeta', id_1d2d_zeta1d(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_crest_level', id_1d2d_crest_level(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_b_2di', id_1d2d_b_2di(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_b_2dv', id_1d2d_b_2dv(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_d_2dv', id_1d2d_d_2dv(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_q_zeta', id_1d2d_q_zeta(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_q_lat', id_1d2d_q_lat(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_cfl', id_1d2d_cfl(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_sb', id_1d2d_sb(iid))
            ierr = nf90_inq_varid(imapfile, 'id_1d2d_s0_2d', id_1d2d_s0_2d(iid))
            ierr = nf90_inq_varid(imapfile, 'id_1d2d_s1_2d', id_1d2d_s1_2d(iid))
            ierr = nf90_inq_varid(imapfile, '1d2d_flow_cond', id_1d2d_flow_cond(iid))
         end if

         if (jamaptidep == 1 .and. jatidep > 0) then
            if (jaselfal == 0) then
               ierr = nf90_inq_varid(imapfile, 'TidalPotential', id_tidep(iid))
            else
               ierr = nf90_inq_varid(imapfile, 'TidalPotential_no_SAL', id_tidep(iid))
            end if
            if (jaselfal > 0) then
               ierr = nf90_inq_varid(imapfile, 'SALPotential', id_salp(iid))
            end if
         end if

         if (jamapIntTidesDiss == 1 .and. jaFrcInternalTides2D > 0) then
            ierr = nf90_inq_varid(imapfile, 'internal_tides_dissipation', id_inttidesdiss(iid))
         end if

         !
         ! Flow data on edges
         ierr = nf90_inq_varid(imapfile, 'unorm', id_unorm(iid))
         !
         ! Flow data on edges
         ierr = nf90_inq_varid(imapfile, 'u0', id_u0(iid))
         ierr = nf90_inq_varid(imapfile, 'q1', id_q1(iid))
         ierr = nf90_inq_varid(imapfile, 'viu', id_viu(iid))
         ierr = nf90_inq_varid(imapfile, 'diu', id_diu(iid))
         !
         if (jawind /= 0) then
            ierr = nf90_inq_varid(imapfile, 'windx', id_windx(iid))
            ierr = nf90_inq_varid(imapfile, 'windy', id_windy(iid))
         end if

         if (jaseparate_ == 2 .and. javeg > 0) then
            ierr = nf90_inq_varid(imapfile, 'rnveg', id_rnveg(iid))
            ierr = nf90_inq_varid(imapfile, 'diaveg', id_diaveg(iid))
            ierr = nf90_inq_varid(imapfile, 'veg_stemheight', id_veg_stemheight(iid))
         end if
      end if

      ! -- Start data writing (flow data) ------------------------
      if (jaseparate_ == 1) then
         itim = 1
         firststep(iid) = .true.
      elseif (jaseparate_ == 2) then
         itim = 1
      else
         it_map = it_map + 1
         itim = it_map ! Increment time dimension index
      end if

      ! Time
      ierr = nf90_put_var(imapfile, id_time(iid), tim, (/itim/))
      ierr = nf90_put_var(imapfile, id_timestep(iid), dts, (/itim/))

      !
      ! Transform uxy/ucy into Eulerian velocities,
      ! only when the user asks for it and only if we are not writing to com-file
      !
      jaeulerloc = 0
      if (jaeulervel == 1 .and. jaseparate_ /= 2 .and. jawave > 0) then
         jaeulerloc = 1
      end if
      !
      call getucxucyeulmag(ndkx, workx, worky, ucmag, jaeulerloc, 0)
      !
      !  Hack to pass time varying bottom levels to SWAN
      !  Also needed for morphostatic runs in 3D
      !
      if (jaseparate_ == 2) then
         ! JRE: was _zcc, but this has laydim included as dimension, which does not work in 3D
         ierr = nf90_inq_varid(imapfile, 'FlowElem_bl', id_swanbl(iid))
         ierr = nf90_put_var(imapfile, id_swanbl(iid), -bl, (/1, itim/), (/ndxndxi, 1/))
      end if
      !
      ! Water level
      if (jamaps1 > 0 .or. jaseparate_ == 2) then
         ierr = nf90_put_var(imapfile, id_s1(iid), s1, (/1, itim/), (/ndxndxi, 1/))
      end if
      !
      if (jamapucvec > 0 .or. jaseparate_ == 2) then
         if (kmx == 0) then
            ierr = nf90_put_var(imapfile, id_ucx(iid), workx, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_ucy(iid), worky, (/1, itim/), (/ndxndxi, 1/))
         end if
      end if

      if (kmx > 0) then
         call unc_append_3dflowgeom_put(imapfile, jaseparate_, itim) ! needed for 3D wave coupling on comfile: Flowelem_zw
         if (jamapucvec > 0 .or. jaseparate_ == 2) then
            do kk = 1, ndxndxi
               work1(:, kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk, kb, kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb, kt
                  work1(k - kb + nlayb, kk) = workx(k)
               end do
            end do
            ierr = nf90_put_var(imapfile, id_ucx(iid), work1(1:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx, ndxndxi, 1/))

            do kk = 1, ndxndxi
               work1(:, kk) = dmiss ! For proper fill values in z-model runs.
               call getkbotktop(kk, kb, kt)
               call getlayerindices(kk, nlayb, nrlay)
               do k = kb, kt
                  work1(k - kb + nlayb, kk) = worky(k)
               end do
            end do
            ierr = nf90_put_var(imapfile, id_ucy(iid), work1(1:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx, ndxndxi, 1/))
         end if
      end if

      if (jaseparate_ /= 2) then
         if (jamaps0 > 0) then
            ierr = nf90_put_var(imapfile, id_s0(iid), s0, (/1, itim/), (/ndxndxi, 1/))
         end if

         if (jamaphs > 0) then
            ierr = nf90_put_var(imapfile, id_hs(iid), hs, (/1, itim/), (/ndxndxi, 1/))
         end if
         ! Tau current and chezy roughness
         if (jamaptaucurrent > 0 .or. jamap_chezy_elements > 0 .or. jamap_chezy_links > 0) then
            if (jawave == 0) then ! Else, get taus from subroutine tauwave (taus = f(taucur,tauwave))
               call gettaus(1, 1) ! Update taus and czs
            else if (jamap_chezy_links > 0) then
               call gettaus(2, 1) ! Only update czs
            end if
            if (jawave > 0 .and. .not. flowWithoutWaves) then
               call gettauswave(jawaveswartdelwaq)
            end if
         end if
         !
         if (jamap_chezy_links > 0) then
            do LL = 1, lnx
               if (frcu(LL) > 0d0) then
                  call getcz(hu(LL), frcu(LL), ifrcutp(LL), czu(LL), LL)
               end if
            end do
         end if
         !
         if (jamaptaucurrent > 0) then
            ierr = nf90_put_var(imapfile, id_taus(iid), taus, (/1, itim/), (/ndxndxi, 1/))
         end if
         !
         if (jamap_chezy_elements > 0) then
            ierr = nf90_put_var(imapfile, id_czs(iid), czs, (/1, itim/), (/ndxndxi, 1/))
         end if
         if (jamap_chezy_links > 0) then
            ierr = nf90_put_var(imapfile, id_czu(iid), czu, (/1, itim/), (/lnx, 1/))
         end if

         ! Velocities
         if (kmx > 0) then
!         3D
            if (jamapucvec > 0) then
               call reconstructucz(0)
               !
               do kk = 1, ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = ucz(k)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_ucz(iid), work1(1:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx, ndxndxi, 1/))

               ierr = nf90_put_var(imapfile, id_ucxa(iid), ucxq(1:ndxndxi), start=(/1, itim/), count=(/ndxndxi, 1/))
               ierr = nf90_put_var(imapfile, id_ucya(iid), ucyq(1:ndxndxi), start=(/1, itim/), count=(/ndxndxi, 1/))
            end if

            if (jamapww1 > 0) then
               do kk = 1, ndxndxi
                  work0(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb - 1, kt
                     work0(k - kb + nlayb, kk) = ww1(k)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_ww1(iid), work0(0:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx + 1, ndxndxi, 1/))
            end if

            if (jamapu1 > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb, Ltx
                     work1(L - Lb + nlaybL, LL) = u1(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_unorm(iid), work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
            end if

            if (jamapu0 > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb, Ltx
                     work1(L - Lb + nlaybL, LL) = u0(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_u0(iid), work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
            end if

            if (jamapq1 > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb, Ltx
                     work1(L - Lb + nlaybL, LL) = q1(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_q1(iid), work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
            end if

            if (jamapviu > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  if (javiusp == 1) then ! user specified part
                     vicc = viusp(LL)
                  else
                     vicc = vicouv
                  end if
                  do L = Lb, Ltx
                     work1(L - Lb + nlaybL, LL) = viu(L) + vicc
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_viu(iid), work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
            end if

            if (jamapdiu > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  if (jadiusp == 1) then
                     dicc = diusp(LL)
                  else
                     dicc = dicouv
                  end if
                  do L = Lb, Ltx
                     work1(L - Lb + nlaybL, LL) = viu(L) / 0.7 + dicc
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_diu(iid), work1(1:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx, lnx, 1/))
            end if

            if (jamaprho > 0) then
               do kk = 1, ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = rho(k)
                  end do
               end do
               if (density_is_pressure_dependent()) then
                  ierr = nf90_put_var(imapfile, id_rho(iid), work1(1:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx, ndxndxi, 1/))
               else
                  ierr = nf90_put_var(imapfile, id_rhop(iid), work1(1:kmx, 1:ndxndxi), start=(/1, 1, itim/), count=(/kmx, ndxndxi, 1/))
               end if
            end if

            if (jamaptur > 0 .and. iturbulencemodel >= 3) then
               do LL = 1, lnx
                  work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb - 1, Ltx
                     work0(L - Lb + nlaybL, LL) = turkin1(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_turkin1(iid), work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))
               do LL = 1, lnx
                  work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb - 1, Ltx
                     work0(L - Lb + nlaybL, LL) = tureps1(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_tureps1(iid), work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))
               do LL = 1, lnx
                  work0(:, LL) = dmiss ! For proper fill values in z-model runs.
                  call getLbotLtopmax(LL, Lb, Ltx)
                  call getlayerindicesLmax(LL, nlaybL, nrlayLx)
                  do L = Lb - 1, Ltx
                     work0(L - Lb + nlaybL, LL) = vicwwu(L)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_vicwwu(iid), work0(0:kmx, 1:lnx), start=(/1, 1, itim/), count=(/kmx + 1, lnx, 1/))
               work0 = dmiss
               do kk = 1, ndxi
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb - 1, kt
                     work0(k - kb + nlayb, kk) = vicwws(k)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_vicwws(iid), work0(0:kmx, 1:ndxi), (/1, 1, itim/), (/kmx + 1, ndxi, 1/))
            end if

         end if

         if (jasecflow > 0 .and. jamapspir > 0) then
            ierr = nf90_put_var(imapfile, id_spirint(iid), spirint, (/1, itim/), (/ndxndxi, 1/))
            if (kmx == 0) then
               ierr = nf90_put_var(imapfile, id_spircrv(iid), spircrv, (/1, itim/), (/ndxndxi, 1/))
            end if
         end if

         if (kmx == 0) then
            if (jamapu1 > 0) then
               ierr = nf90_put_var(imapfile, id_unorm(iid), u1, (/1, itim/), (/lnx, 1/))
            end if

            if (jamapu0 > 0) then
               ierr = nf90_put_var(imapfile, id_u0(iid), u0, (/1, itim/), (/lnx, 1/))
            end if

            if (jamapq1 > 0) then
               ierr = nf90_put_var(imapfile, id_q1(iid), q1, (/1, itim/), (/lnx, 1/))
            end if

            if (jamapviu > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss
                  if (javiusp == 1) then ! user specified part
                     vicc = viusp(LL)
                  else
                     vicc = vicouv
                  end if
                  work1(1, LL) = viu(LL) + vicc
               end do
               ierr = nf90_put_var(imapfile, id_viu(iid), work1(1:1, 1:lnx), (/1, itim/), (/lnx, 1/))
            end if

            if (jamapdiu > 0) then
               do LL = 1, lnx
                  work1(:, LL) = dmiss
                  if (jadiusp == 1) then
                     dicc = diusp(LL)
                  else
                     dicc = dicouv
                  end if
                  work1(1, LL) = viu(LL) * 0.7 + dicc
               end do
               ierr = nf90_put_var(imapfile, id_diu(iid), work1(1:1, 1:lnx), (/1, itim/), (/lnx, 1/))
            end if
         end if

      end if

      if (jaseparate_ /= 2) then

         ! Salinity
         if (jamapsal > 0 .and. jasal > 0) then
            if (kmx > 0) then
!            3D
               !do kk=1,ndxndxi
               !   call getkbotktop(kk,kb,kt)
               !   ierr = nf90_put_var(imapfile, id_sa1(iid), sa1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
               !enddo
               do kk = 1, ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = constituents(isalt, k)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_sa1(iid), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
            else
               do k = 1, ndxndxi
                  sa1(k) = constituents(isalt, k)
               end do
               ierr = nf90_put_var(imapfile, id_sa1(iid), sa1, (/1, itim/), (/ndxndxi, 1/))
            end if
         end if

         if (jamaptem > 0 .and. jatem > 0) then
            if (kmx > 0) then ! 3D
               !do kk=1,ndxndxi
               !   call getkbotktop(kk,kb,kt)
               !   ierr = nf90_put_var(imapfile, id_tem1(iid), tem1(kb:kt), (/ 1, kk, itim /), (/ kt-kb+1, 1, 1 /))
               !enddo
               do kk = 1, ndxndxi
                  work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                  call getkbotktop(kk, kb, kt)
                  call getlayerindices(kk, nlayb, nrlay)
                  do k = kb, kt
                     work1(k - kb + nlayb, kk) = constituents(itemp, k)
                  end do
               end do
               ierr = nf90_put_var(imapfile, id_tem1(iid), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
            else
               do k = 1, ndxndxi
                  tem1(k) = constituents(itemp, k)
               end do
               ierr = nf90_put_var(imapfile, id_tem1(iid), tem1, (/1, itim/), (/ndxndxi, 1/))
            end if
         end if

!      tracers
         if (jamapconst > 0 .and. ITRA1 > 0) then ! Note: numtracers is only counting tracer boundaries. SPvdP: now also includes tracers with initial conditions only
            allocate (dum(NdxNdxi))

            do j = ITRA1, ITRAN
               if (kmx > 0) then
!               3D
                  do kk = 1, ndxndxi
                     work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                     call getkbotktop(kk, kb, kt)
                     call getlayerindices(kk, nlayb, nrlay)
                     do k = kb, kt
                        work1(k - kb + nlayb, kk) = constituents(j, k)
                     end do
                  end do
                  ierr = nf90_put_var(imapfile, id_const(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
                  !   if ( ierr.ne.0 ) exit  ! probably newly added tracer in the GUI
               else
                  do kk = 1, NdxNdxi
                     dum(kk) = constituents(j, kk)
                  end do
                  ierr = nf90_put_var(imapfile, id_const(iid, j), dum, (/1, itim/), (/NdxNdxi, 1/))
               end if
            end do

            if (allocated(dum)) deallocate (dum)
         end if

         ! water quality bottom variables outputs
         if (numwqbots > 0) then
            allocate (dum(NdxNdxi))
            do j = 1, numwqbots
               do kk = 1, NdxNdxi
                  call getkbotktop(kk, kb, kt)
                  dum(kk) = wqbot(j, kb)
               end do
               ierr = nf90_put_var(imapfile, id_wqb(iid, j), dum, (/1, itim/), (/NdxNdxi, 1/))
            end do
            if (jamapwqbot3d == 1) then
               do j = 1, numwqbots
                  do kk = 1, ndxndxi
                     work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                     call getkbotktop(kk, kb, kt)
                     call getlayerindices(kk, nlayb, nrlay)
                     do k = kb, kt
                        work1(k - kb + nlayb, kk) = wqbot(j, k)
                     end do
                  end do
                  ierr = nf90_put_var(imapfile, id_wqb3d(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
               end do
            end if
            if (allocated(dum)) deallocate (dum)
         end if

         ! WAQ extra outputs
         if (jawaqproc > 0) then
            do j = 1, noout_map
               if (outvar(j) > 0) then
                  work1 = DMISS ! For proper fill values in z-model runs.
                  if (kmx > 0) then
!                  3D
                     do kk = 1, ndxndxi
                        work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                        call getkbotktop(kk, kb, kt)
                        call getlayerindices(kk, nlayb, nrlay)
                        do k = kb, kt
                           work1(k - kb + nlayb, kk) = waqoutputs(j, k - kbx + 1)
                        end do
                     end do
                     ierr = nf90_put_var(imapfile, id_waq(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
                  else
                     call realloc(dum, NdxNdxi, keepExisting=.false.)
                     do kk = 1, NdxNdxi
                        dum(kk) = waqoutputs(j, kk)
                     end do
                     ierr = nf90_put_var(imapfile, id_waq(iid, j), dum, (/1, itim/), (/NdxNdxi, 1/))
                     if (allocated(dum)) then
                        deallocate (dum)
                     end if
                  end if
               end if
            end do
            do j = 1, noout_statt
               jj = noout_user + j
               if (outvar(jj) > 0) then
                  work1 = DMISS ! For proper fill values in z-model runs.
                  if (kmx > 0) then
!                  3D
                     do kk = 1, ndxndxi
                        work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                        call getkbotktop(kk, kb, kt)
                        call getlayerindices(kk, nlayb, nrlay)
                        do k = kb, kt
                           work1(k - kb + nlayb, kk) = waqoutputs(jj, k - kbx + 1)
                        end do
                     end do
                     ierr = nf90_put_var(imapfile, id_wqst(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
                  else
                     call realloc(dum, NdxNdxi, keepExisting=.false.)
                     do kk = 1, NdxNdxi
                        dum(kk) = waqoutputs(jj, kk)
                     end do
                     ierr = nf90_put_var(imapfile, id_wqst(iid, j), dum, (/1, itim/), (/NdxNdxi, 1/))
                     if (allocated(dum)) then
                        deallocate (dum)
                     end if
                  end if
               end if
            end do
            if (comparereal(tim, ti_mape, eps10) == 0) then
               do j = 1, noout_state
                  jj = noout_user + noout_statt + j
                  if (outvar(jj) > 0) then
                     work1 = DMISS ! For proper fill values in z-model runs.
                     if (kmx > 0) then
!                     3D
                        do kk = 1, ndxndxi
                           work1(:, kk) = dmiss ! For proper fill values in z-model runs.
                           call getkbotktop(kk, kb, kt)
                           call getlayerindices(kk, nlayb, nrlay)
                           do k = kb, kt
                              work1(k - kb + nlayb, kk) = waqoutputs(jj, k - kbx + 1)
                           end do
                        end do
                        ierr = nf90_put_var(imapfile, id_wqse(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1/), (/kmx, ndxndxi, 1/))
                     else
                        call realloc(dum, NdxNdxi, keepExisting=.false.)
                        do kk = 1, NdxNdxi
                           dum(kk) = waqoutputs(jj, kk)
                        end do
                        ierr = nf90_put_var(imapfile, id_wqse(iid, j), dum, (/1/), (/NdxNdxi, 1/))
                        if (allocated(dum)) then
                           deallocate (dum)
                        end if
                     end if
                  end if
               end do
            end if
         end if

         if (jased > 0 .and. stm_included) then
            if (stmpar%lsedsus > 0) then
               if (kmx > 0) then
                  do kk = 1, ndxndxi
                     call getkbotktop(kk, kb, kt)
                     ierr = nf90_put_var(imapfile, id_ws(iid), mtd%ws(kb:kt, :), (/1, kk, 1, itim/), (/kt - kb + 1, 1, stmpar%lsedsus, 1/))
                  end do
               else
                  ierr = nf90_put_var(imapfile, id_ws(iid), mtd%ws, (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               end if
               !
               ! equilibrium concentration, 2D only
               if (kmx == 0) then
                  ierr = nf90_put_var(imapfile, id_rsedeq(iid), sedtra%rsedeq(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               end if

               if (stmpar%morpar%moroutput%sourcesink) then
                  ierr = nf90_put_var(imapfile, id_sourse(iid), sedtra%sourse(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
                  ierr = nf90_put_var(imapfile, id_sinkse(iid), sedtra%sinkse(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               end if

               if (stmpar%morpar%moroutput%suvcor) then
                  ierr = nf90_put_var(imapfile, id_scrn(iid), sedtra%e_scrn(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
                  !ierr = nf90_put_var(imapfile, id_scrt(iid) , sedtra%e_scrt(1:ndxndxi, :),  (/ 1, 1, itim /), (/ ndxndxi, stmpar%lsedsus, 1 /))
               end if

               if (stmpar%morpar%moroutput%aks) then
                  ierr = nf90_put_var(imapfile, id_aks(iid), sedtra%aks(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
                  ierr = nf90_put_var(imapfile, id_rca(iid), sedtra%rca(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               end if
               !
               ! Suspended fractions
               call realloc(dum, NdxNdxi, keepExisting=.false.)
               do j = ISED1, ISEDN
                  if (kmx > 0) then
                     !            3D
                     do kk = 1, ndxndxi
                        call getkbotktop(kk, kb, kt)
                        do k = kb, kt
                           ! TODO: UNST-976, incorrect for Z-layers:
                           work1(k - kb + 1, kk) = constituents(j, k)
                        end do
                     end do
                     ierr = nf90_put_var(imapfile, id_const(iid, j), work1(1:kmx, 1:ndxndxi), (/1, 1, itim/), (/kmx, ndxndxi, 1/))
                  else
                     do kk = 1, NdxNdxi
                        dum(kk) = constituents(j, kk)
                     end do
                     ierr = nf90_put_var(imapfile, id_const(iid, j), dum, (/1, itim/), (/NdxNdxi, 1/))
                  end if
               end do
               if (allocated(dum)) deallocate (dum)
            end if

            if (stmpar%morpar%moroutput%dzduuvv) then ! bedslope
               ierr = nf90_put_var(imapfile, id_dzdn(iid), sedtra%e_dzdn, (/1, itim/), (/lnxi, 1/))
               ierr = nf90_put_var(imapfile, id_dzdt(iid), sedtra%e_dzdt, (/1, itim/), (/lnxi, 1/))
            end if

            if (stmpar%morpar%moroutput%umod) then
               ierr = nf90_put_var(imapfile, id_umod(iid), sedtra%umod, (/1, itim/), (/ndxndxi, 1/))
            end if

            if (stmpar%morpar%moroutput%zumod) then
               ierr = nf90_put_var(imapfile, id_zumod(iid), sedtra%zumod, (/1, itim/), (/ndxndxi, 1/))
            end if

            if (stmpar%morpar%moroutput%ustar) then
               ierr = nf90_put_var(imapfile, id_ustar(iid), sqrt(sedtra%ust2), (/1, itim/), (/ndxndxi, 1/))
            end if

            if (stmpar%morpar%moroutput%sbcuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sbcx(:, l) = sedtra%sbcx(:, l) / rhol
                  sedtra%sbcy(:, l) = sedtra%sbcy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sbcx(iid), sedtra%sbcx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sbcy(iid), sedtra%sbcy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sbwuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sbwx(:, l) = sedtra%sbwx(:, l) / rhol
                  sedtra%sbwy(:, l) = sedtra%sbwy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sbwx(iid), sedtra%sbwx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sbwy(iid), sedtra%sbwy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sswuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sswx(:, l) = sedtra%sswx(:, l) / rhol
                  sedtra%sswy(:, l) = sedtra%sswy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sswx(iid), sedtra%sswx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sswy(iid), sedtra%sswy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sscuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedsus
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
                  case (2)
                     rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
                  end select
                  toutputx(:, l) = sedtra%sscx(:, sedtot2sedsus(l)) / rhol ! mapping necessary because dim(sscx)=lsedtot
                  toutputy(:, l) = sedtra%sscy(:, sedtot2sedsus(l)) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sscx(iid), toutputx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               ierr = nf90_put_var(imapfile, id_sscy(iid), toutputy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
            end if

            ! Get cell centre transport values, removed from fm_erosed and fm_bott3d, and calculated here and stored in sscx, sscy, sbcx, sbcy, sbwx, sbwy, sswx, sswy
            call reconstructsedtransports()

            if (stmpar%morpar%moroutput%sbcuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sbcx(:, l) = sedtra%sbcx(:, l) / rhol
                  sedtra%sbcy(:, l) = sedtra%sbcy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sbcx_reconstructed(iid), sedtra%sbcx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sbcy_reconstructed(iid), sedtra%sbcy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sbwuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sbwx(:, l) = sedtra%sbwx(:, l) / rhol
                  sedtra%sbwy(:, l) = sedtra%sbwy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sbwx_reconstructed(iid), sedtra%sbwx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sbwy_reconstructed(iid), sedtra%sbwy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sswuv) then
               do l = 1, stmpar%lsedtot
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(l)
                  case (2)
                     rhol = stmpar%sedpar%rhosol(l)
                  end select
                  sedtra%sswx(:, l) = sedtra%sswx(:, l) / rhol
                  sedtra%sswy(:, l) = sedtra%sswy(:, l) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sswx_reconstructed(iid), sedtra%sswx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
               ierr = nf90_put_var(imapfile, id_sswy_reconstructed(iid), sedtra%sswy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if

            if (stmpar%morpar%moroutput%sscuv) then
               call realloc(toutputx, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedsus/), keepExisting=.false., fill=-999d0)
               do l = 1, stmpar%lsedsus
                  select case (stmpar%morpar%moroutput%transptype)
                  case (0)
                     rhol = 1d0
                  case (1)
                     rhol = stmpar%sedpar%cdryb(sedtot2sedsus(sedtot2sedsus(l)))
                  case (2)
                     rhol = stmpar%sedpar%rhosol(sedtot2sedsus(sedtot2sedsus(l)))
                  end select
                  toutputx(:, l) = sedtra%sscx(:, sedtot2sedsus(l)) / rhol ! mapping necessary because dim(sscx)=lsedtot
                  toutputy(:, l) = sedtra%sscy(:, sedtot2sedsus(l)) / rhol
               end do
               ierr = nf90_put_var(imapfile, id_sscx_reconstructed(iid), toutputx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
               ierr = nf90_put_var(imapfile, id_sscy_reconstructed(iid), toutputy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedsus, 1/))
            end if

            do l = 1, stmpar%lsedtot
               call realloc(toutputx, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               call realloc(toutputy, (/ndx, stmpar%lsedtot/), keepExisting=.false., fill=-999d0)
               select case (stmpar%morpar%moroutput%transptype)
               case (0)
                  rhol = 1d0
               case (1)
                  rhol = stmpar%sedpar%cdryb(l)
               case (2)
                  rhol = stmpar%sedpar%rhosol(l)
               end select
               toutputx(:, l) = sedtra%sxtot(:, l) / rhol
               toutputy(:, l) = sedtra%sytot(:, l) / rhol
            end do
            ierr = nf90_put_var(imapfile, id_sxtot(iid), toutputx(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            ierr = nf90_put_var(imapfile, id_sytot(iid), toutputy(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))

            if (stmpar%morpar%bedupd) then
               ierr = nf90_put_var(imapfile, id_morbl(iid), bl(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if

            select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ierr = nf90_put_var(imapfile, id_bodsed(iid), stmpar%morlyr%state%bodsed(:, 1:ndxndxi), (/1, 1, itim/), (/stmpar%lsedtot, ndxndxi, 1/))
               ierr = nf90_put_var(imapfile, id_dpsed(iid), stmpar%morlyr%state%dpsed(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            case (2)
               !
               ! Calculate values for lyrfrac and porosity
               !
               ! lyrfrac
               if (.not. allocated(frac)) allocate (frac(stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndx))
               frac = -999d0
               if (stmpar%morlyr%settings%iporosity == 0) then
                  dens => stmpar%sedpar%cdryb
               else
                  dens => stmpar%sedpar%rhosol
               end if
               do k = 1, stmpar%morlyr%settings%nlyr
                  do nm = 1, ndxndxi
                     if (stmpar%morlyr%state%thlyr(k, nm) > 0.0_fp) then
                        do l = 1, stmpar%lsedtot
                           frac(l, k, nm) = stmpar%morlyr%state%msed(l, k, nm) / (dens(l) * stmpar%morlyr%state%svfrac(k, nm) * &
                                                                                  stmpar%morlyr%state%thlyr(k, nm))
                        end do
                     else
                        frac(:, k, nm) = 0d0
                     end if
                  end do
               end do
               !
               if (stmpar%morlyr%settings%iporosity > 0) then
                  if (.not. allocated(poros)) allocate (poros(1:stmpar%morlyr%settings%nlyr, 1:ndx))
                  poros = 1d0 - stmpar%morlyr%state%svfrac
               end if
               !
               ! Avoid stack overflow issues with large models
               do l = 1, stmpar%lsedtot
                  ierr = nf90_put_var(imapfile, id_msed(iid), stmpar%morlyr%state%msed(l, :, 1:ndxndxi), (/l, 1, 1, itim/), (/1, stmpar%morlyr%settings%nlyr, ndxndxi, 1/))
                  ierr = nf90_put_var(imapfile, id_lyrfrac(iid), frac(l, :, 1:ndxndxi), (/l, 1, 1, itim/), (/1, stmpar%morlyr%settings%nlyr, ndxndxi, 1/))
               end do
               ierr = nf90_put_var(imapfile, id_thlyr(iid), stmpar%morlyr%state%thlyr(:, 1:ndxndxi), (/1, 1, itim/), (/stmpar%morlyr%settings%nlyr, ndxndxi, 1/))
               if (stmpar%morlyr%settings%iporosity > 0) then
                  ierr = nf90_put_var(imapfile, id_poros(iid), poros(:, 1:ndxndxi), (/1, 1, itim/), (/stmpar%morlyr%settings%nlyr, ndxndxi, 1/))
               end if
            end select

            if (stmpar%morpar%moroutput%taurat) then
               ierr = nf90_put_var(imapfile, id_taurat(iid), sedtra%taurat(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if
            if (stmpar%morpar%moroutput%dm) then
               ierr = nf90_put_var(imapfile, id_dm(iid), sedtra%dm(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))

            end if
            if (stmpar%morpar%moroutput%dg) then
               ierr = nf90_put_var(imapfile, id_dg(iid), sedtra%dg(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
            if (stmpar%morpar%moroutput%dgsd) then
               ierr = nf90_put_var(imapfile, id_dgsd(iid), sedtra%dgsd(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
            if (stmpar%morpar%moroutput%percentiles) then ! JRE to do: check with Arthur
               call realloc(dum, ndxndxi, keepExisting=.false.)
               do l = 1, stmpar%morpar%nxx
                  do kk = 1, NdxNdxi
                     dum(kk) = sedtra%dxx(kk, l)
                  end do
                  ierr = nf90_put_var(imapfile, id_dxx(l, iid), dum, (/1, itim/), (/ndxndxi, 1/))
               end do
            end if
            if (stmpar%morpar%moroutput%frac) then
               ierr = nf90_put_var(imapfile, id_frac(iid), sedtra%frac(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if
            if (stmpar%morpar%moroutput%mudfrac) then
               ierr = nf90_put_var(imapfile, id_mudfrac(iid), sedtra%mudfrac(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
            if (stmpar%morpar%moroutput%sandfrac) then
               ierr = nf90_put_var(imapfile, id_sandfrac(iid), sedtra%sandfrac(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
            if (stmpar%morpar%moroutput%fixfac) then
               ierr = nf90_put_var(imapfile, id_fixfac(iid), sedtra%fixfac(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if
            if (stmpar%morpar%moroutput%hidexp) then
               ierr = nf90_put_var(imapfile, id_hidexp(iid), sedtra%hidexp(1:ndxndxi, :), (/1, 1, itim/), (/ndxndxi, stmpar%lsedtot, 1/))
            end if
            ! Fluff layers
            if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0) then
               do l = 1, stmpar%lsedsus
                  call realloc(toutput, ndx, keepExisting=.false., fill=-999d0)
                  toutput = stmpar%morpar%flufflyr%mfluff(l, 1:ndx)
                  ierr = nf90_put_var(imapfile, id_mfluff(iid), toutput(1:ndxndxi), (/1, l, itim/), (/ndxndxi, 1, 1/))
               end do
            end if
         end if ! stm

         ! Bedform pars
         if (bfmpar%lfbedfrmout) then
            if (bfmpar%lfbedfrm) then
               ierr = nf90_put_var(imapfile, id_duneheight(iid), bfmpar%duneheight(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
               ierr = nf90_put_var(imapfile, id_dunelength(iid), bfmpar%dunelength(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
            if (bfmpar%lfbedfrmrou) then
               ierr = nf90_put_var(imapfile, id_ksr(iid), bfmpar%rksr(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
               ierr = nf90_put_var(imapfile, id_ksmr(iid), bfmpar%rksmr(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
               ierr = nf90_put_var(imapfile, id_ksd(iid), bfmpar%rksd(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))

               do k = 1, ndxndxi
                  rks(k) = sqrt(bfmpar%rksr(k)**2 + bfmpar%rksmr(k)**2 + bfmpar%rksd(k)**2)
               end do
               ierr = nf90_put_var(imapfile, id_ks(iid), rks(1:ndxndxi), (/1, itim/), (/ndxndxi, 1/))
            end if
         end if
         ! Sediment Herman
         if (jased > 0 .and. .not. stm_included) then
            ierr = nf90_put_var(imapfile, id_sed(iid), sed, (/1, 1, itim/), (/mxgr, ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_ero(iid), grainlay, (/1, 1, itim/), (/mxgr, size(grainlay, 2), 1/))

            ierr = nf90_put_var(imapfile, id_bl(iid), bl, (/1, itim/), (/ndxndxi, 1/))
            if (jaceneqtr /= 1) then
               ierr = nf90_put_var(imapfile, id_zk(iid), zk, (/1, itim/), (/numk, 1/))
            end if

            ! TODO: AvD: size(grainlay,2) is always correct (mxn), but we have a problem if jaceneqtr==2 and mxn/=numk,
            ! because then the dimension for ero is set to nNetNode, and coordinate attribute refers to NetNode_x
            ! (both length numk), whereas ero itself is shorter than numk.
         end if

         ! 1D2D boundaries
         if (nbnd1d2d > 0) then
            ierr = nf90_put_var(imapfile, id_1d2d_zeta1d(iid), zbnd1d2d1, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_crest_level(iid), zcrest1d2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_b_2di(iid), b_2di, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_b_2dv(iid), b_2dv, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_d_2dv(iid), d_2dv, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_q_zeta(iid), qzeta_1d2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_q_lat(iid), qlat_1d2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_cfl(iid), cfl, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_sb(iid), sb_1d2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_s1_2d(iid), s1_2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_s0_2d(iid), s0_2d, (/1, itim/), (/nbnd1d2d, 1/))
            ierr = nf90_put_var(imapfile, id_1d2d_flow_cond(iid), FlowCond, (/1, itim/), (/nbnd1d2d, 1/))
         end if

         if (jatidep > 0 .and. jamaptidep == 1) then
            if (jaselfal == 0) then
               do k = 1, Ndx
                  workx(k) = tidep(1, k)
               end do
               ierr = nf90_put_var(imapfile, id_tidep(iid), workx, (/1, itim/), (/ndxndxi, 1/))
            else ! write potential without SAL and SAL potential
               do k = 1, Ndx
                  workx(k) = tidep(1, k) - tidep(2, k)
!                worky(k) = tidep(2,k)
               end do
               ierr = nf90_put_var(imapfile, id_tidep(iid), workx, (/1, itim/), (/ndxndxi, 1/))
!             ierr = nf90_put_var(imapfile, id_salp(iid),  worky,  (/ 1, itim /), (/ ndxndxi, 1 /))
            end if
         end if
         if (jaselfal > 0 .and. jamapselfal == 1) then
            do k = 1, Ndx
               worky(k) = tidep(2, k)
            end do
            ierr = nf90_put_var(imapfile, id_salp(iid), worky, (/1, itim/), (/ndxndxi, 1/))
         end if

         if (jaFrcInternalTides2D > 0 .and. jamapIntTidesDiss == 1) then
            ierr = nf90_put_var(imapfile, id_inttidesdiss(iid), DissInternalTidesPerArea, (/1, itim/), (/ndxndxi, 1/))
         end if
      end if

      if (jawind > 0 .and. ((jamapwind > 0 .and. jawindstressgiven == 0) .or. (jaseparate_ == 2))) then
         allocate (windx(ndxndxi), windy(ndxndxi), stat=ierr)
         if (ierr /= 0) call aerr('windx/windy', ierr, ndxndxi)
         !windx/y is not set to 0.0 for flownodes without links !
         windx = 0.0d0
         windy = 0.0d0
         do n = 1, ndxndxi
            !
            ! Currently, wx/y is defined on the links
            ! TO DO: EC-module should not be asked for wind components on the links but on the cells
            !
            if (nd(n)%lnx > 0) then
               do i = 1, nd(n)%lnx
                  windx(n) = windx(n) + wx(abs(nd(n)%ln(i)))
                  windy(n) = windy(n) + wy(abs(nd(n)%ln(i)))
               end do
               windx(n) = windx(n) / nd(n)%lnx
               windy(n) = windy(n) / nd(n)%lnx
            else
               j = 1
            end if
         end do
         ierr = nf90_put_var(imapfile, id_windx(iid), windx, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_windy(iid), windy, (/1, itim/), (/ndxndxi, 1/))
         deallocate (windx, stat=ierr)
         ierr = nf90_put_var(imapfile, id_windxu(iid), wx, (/1, itim/), (/lnx, 1/))
         ierr = nf90_put_var(imapfile, id_windyu(iid), wy, (/1, itim/), (/lnx, 1/))
      end if

      if (jamapwind > 0 .and. japatm > 0) then
         ierr = nf90_put_var(imapfile, id_patm(iid), Patm, (/1, itim/), (/ndxndxi, 1/))
      end if

      if (ice_mapout) then
         ierr = nf90_put_var(imapfile, id_ice_af(iid), ice_af, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_ice_h(iid), ice_h, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_ice_p(iid), ice_p, (/1, itim/), (/ndxndxi, 1/))
         if (ja_icecover == ICECOVER_SEMTNER) then
            ierr = nf90_put_var(imapfile, id_ice_t(iid), ice_t, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_snow_h(iid), snow_h, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_snow_t(iid), snow_t, (/1, itim/), (/ndxndxi, 1/))
         end if
      end if

      if (jamapheatflux > 0 .and. jatem > 1) then ! Heat modelling only
         ierr = nf90_put_var(imapfile, id_tair(iid), Tair, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_rhum(iid), Rhum, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_clou(iid), Clou, (/1, itim/), (/ndxndxi, 1/))

         if (jatem == 5) then
            ierr = nf90_put_var(imapfile, id_qsun(iid), Qsunmap, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_qeva(iid), Qevamap, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_qcon(iid), Qconmap, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_qlong(iid), Qlongmap, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_qfreva(iid), Qfrevamap, (/1, itim/), (/ndxndxi, 1/))
            ierr = nf90_put_var(imapfile, id_qfrcon(iid), Qfrconmap, (/1, itim/), (/ndxndxi, 1/))
         end if
         ierr = nf90_put_var(imapfile, id_qtot(iid), Qtotmap, (/1, itim/), (/ndxndxi, 1/))
      end if
      call realloc(numlimdtdbl, ndxndxi, keepExisting=.false.)
      numlimdtdbl = dble(numlimdt) ! To prevent stack overflow. TODO: remove once integer version is available.
      ierr = nf90_put_var(imapfile, id_numlimdt(iid), numlimdtdbl, (/1, itim/), (/ndxndxi, 1/))
      deallocate (numlimdtdbl)

      ! Roughness from trachytopes
      if (jatrt == 1) then
         ierr = nf90_put_var(imapfile, id_cftrt(iid), cftrt(:, 2), (/1, itim/), (/numl, 1/))
      end if

      ! Roughness calibration factors
      if (jacali == 1) then
         ierr = nf90_put_var(imapfile, id_cfcl(iid), cfclval, (/1, itim/), (/numl, 1/))
      end if

      ! JRE - XBeach
      if (jawave == 4) then
         ierr = nf90_put_var(imapfile, id_E(iid), E, (/1, itim/), (/ndxndxi, 1/)) ! direction integrated
         ierr = nf90_put_var(imapfile, id_R(iid), R, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_H(iid), H, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_urmscc(iid), uorb, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_Fxcc(iid), Fx_cc, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_Fycc(iid), Fy_cc, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_D(iid), D, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_DR(iid), DR, (/1, itim/), (/ndxndxi, 1/))

         ierr = nf90_put_var(imapfile, id_sigmwav(iid), sigmwav, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_cwav(iid), cwav, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_cgwav(iid), cgwav, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_thetamean(iid), 270d0 - thetamean * 180d0 / pi, (/1, itim/), (/ndxndxi, 1/))
         !if ( (windmodel .eq. 1) .and. (jawsource .eq. 1) ) then
         !   ierr = nf90_put_var(imapfile, id_SwE(iid), SwE, (/ 1, itim /), (/ ndxndxi, 1 /))
         !   ierr = nf90_put_var(imapfile, id_SwT(iid), SwT, (/ 1, itim /), (/ ndxndxi, 1 /))
         !endif
      end if

!   deallocate
      if (NUMCONST > 0) then
         if (allocated(idum)) deallocate (idum)
      end if

      if (jaseparate_ == 2 .and. javeg > 0) then
         ierr = nf90_put_var(imapfile, id_rnveg(iid), rnveg, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_diaveg(iid), diaveg, (/1, itim/), (/ndxndxi, 1/))
         ierr = nf90_put_var(imapfile, id_veg_stemheight(iid), stemheight, (/1, itim/), (/ndxndxi, 1/))
      end if

   end subroutine unc_write_map_filepointer

!> Writes the unstructured net to a netCDF file.
!! If file exists, it will be overwritten.
   subroutine unc_write_net(filename, janetcell, janetbnd, jaidomain, jaiglobal_s, iconventions, md_ident)
      character(len=*), intent(in) :: filename !< output filename
      integer, optional, intent(in) :: janetcell !< write additional network cell information (1) or not (0). Default: 0.
      integer, optional, intent(in) :: janetbnd !< write additional network boundary information (1) or not (0). Default: 0.
      integer, optional, intent(in) :: jaidomain !< write subdomain numbers (1) or not (0, default)
      integer, optional, intent(in) :: jaiglobal_s !< write global netcell number (1) or not (0, default)
      integer, optional, intent(in) :: iconventions !< type output convention (default: UNC_CONV_CFOLD = 1)
      character(len=*), optional, intent(in) :: md_ident !< identifier of the model (default: blank)

      type(t_unc_mapids) :: mapids
      type(t_ug_meta) :: meta
      integer :: inetfile, ierr, janetcell_loc, janetbnd_loc, jaidomain_loc, jaiglobal_s_loc, iconv

      janetcell_loc = 0
      janetbnd_loc = 0
      jaidomain_loc = 0
      jaiglobal_s_loc = 0
      iconv = UNC_CONV_CFOLD

      if (present(janetcell)) then
         janetcell_loc = janetcell
      end if
      if (present(janetbnd)) then
         janetbnd_loc = janetbnd
      end if
      if (present(jaidomain)) then
         jaidomain_loc = jaidomain
      end if
      if (present(jaiglobal_s)) then
         jaiglobal_s_loc = jaiglobal_s
      end if
      if (present(iconventions)) then
         iconv = iconventions
      end if

      ierr = unc_create(filename, 0, inetfile)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create net file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      if (iconv == UNC_CONV_UGRID) then
         mapids%ncid = inetfile
         meta = ug_meta_fm
         if (present(md_ident)) then
            meta%modelname = md_ident
         end if
         ierr = ug_addglobalatts(mapids%ncid, meta)
         call unc_write_net_ugrid2(mapids%ncid, mapids%id_tsp, janetcell=janetcell_loc, jaidomain=jaidomain_loc, jaiglobal_s=jaiglobal_s_loc)
      else
         call unc_write_net_filepointer(inetfile, janetcell=janetcell_loc, janetbnd=janetbnd_loc, jaidomain=jaidomain_loc, jaiglobal_s=jaiglobal_s_loc)
      end if

      ierr = unc_close(inetfile)
   end subroutine unc_write_net

!> Writes the unstructured net in UGRID format to an already opened netCDF dataset.
   subroutine unc_write_net_filepointer(inetfile, janetcell, janetbnd, jaidomain, jaiglobal_s)
      use network_data
      use m_alloc
      use m_polygon
      use m_sferic
      use m_missing
      use m_partitioninfo
      use geometry_module, only: get_startend, normaloutchk
      use gridoperations
      use m_copynetboundstopol

      integer, intent(in) :: inetfile

      integer, optional, intent(in) :: janetcell !< write additional network cell information (1) or not (0). Default: 1.
      integer, optional, intent(in) :: janetbnd !< write additional network boundary information (1) or not (0). Default: 1.
      integer, optional, intent(in) :: jaidomain !< write subdomain numbers (1) or not (0, default)
      integer, optional, intent(in) :: jaiglobal_s !< write global netcell numbers (1) or not (0, default)
      integer :: janetcell_
      integer :: janetbnd_
      integer :: jaidomain_
      integer :: jaiglobal_s_
      integer, allocatable :: kn3(:), ibndlink(:)

      integer :: ierr
      integer :: id_netnodedim, id_netlinkptsdim, & !< Dimensions
                 id_bndlinkdim, &
                 id_encinstdim, id_encptsdim, id_encpartdim, &
                 id_netnodex, id_netnodey, id_netnodez, & !< Node variables
                 id_netlink, id_netlinktype, & !< Link variables
                 id_bndlink, & !< Boundary variables
                 id_enc_container, id_encx, id_ency, & !< Grid enclosure variables
                 id_enc_nodecount, id_enc_partnodecount, &
                 id_enc_interiorring, &
                 id_idomain, id_iglobal_s !< Netelem variables
      type(t_unc_netelem_ids) :: ids_netelem
      integer :: id_mesh2d
      integer :: jaInDefine
      integer :: k, L, nv, numbnd, maxbnd, numencparts, numencpts
      double precision, allocatable :: polc(:)
      integer, dimension(:), allocatable :: kn1write
      integer, dimension(:), allocatable :: kn2write
      integer :: istart, iend, ipoint, ipoly, numpoints, iorient, iinterior
      integer :: netstat_store

      call readyy('Writing net data', 0d0)

      ! Defaults for extended information:
      janetcell_ = 1
      janetbnd_ = 1
      jaidomain_ = 0
      jaiglobal_s_ = 0

      if (present(janetcell)) then
         janetcell_ = janetcell
      end if
      if (present(janetbnd)) then
         janetbnd_ = janetbnd
      end if
      if (present(jaidomain)) then
         jaidomain_ = jaidomain
      end if
      if (present(jaiglobal_s)) then
         jaiglobal_s_ = jaiglobal_s
      end if

      ! hk: this should not be done here anymore
      ! if (janetcell_ /= 0) then
      !    if (size(lnn) < numl .or. netstat == NETSTAT_CELLS_DIRTY ) then
      !       call setnodadm(0)
      !       call findcells(0)
      !    endif
      ! endif

      if (janetbnd_ /= 0) then
         numbnd = 0
         maxbnd = ceiling(sqrt(real(numl))) ! First estimate of numbnd
         allocate (ibndlink(maxbnd))
         do L = 1, numl
            if (lnn(L) < 2 .and. kn(3, L) == 2) then
               numbnd = numbnd + 1
               if (numbnd > maxbnd) then
                  maxbnd = max(NUMBND, nint(1.2 * maxbnd))
                  call realloc(ibndlink, maxbnd)
               end if
               ibndlink(numbnd) = L
            end if
         end do

         ! Start detecting grid enclosure
         call savepol()
         if (jampi > 0) then ! .and. jawave == 3) then
            netstat_store = netstat
            netstat = NETSTAT_OK
            call generate_partition_pol_from_idomain(ierr, myrank=my_rank) ! UNST-1937: strictly domain, no ghostcells
            netstat = netstat_store
         else
            call copynetboundstopol(0, 0, 1, 0)
         end if
         call realloc(iistart, maxpoly, keepExisting=.false.)
         call realloc(iiend, maxpoly, keepExisting=.false.)
         ipoint = 1
         ipoly = 0
         numpoints = 0
         do while (ipoint <= NPL)
            ipoly = ipoly + 1
            if (ipoly > maxpoly) then
               maxpoly = ceiling(maxpoly * 1.1)
               call realloc(iistart, maxpoly, keepExisting=.true.)
               call realloc(iiend, maxpoly, keepExisting=.true.)
            end if

            !        get polygon start and end pointer respectively
            call get_startend(NPL - ipoint + 1, xpl(ipoint:NPL), ypl(ipoint:NPL), istart, iend, dmiss)
            istart = istart + ipoint - 1
            iend = iend + ipoint - 1

            if (istart >= iend .or. iend > NPL) exit ! done

            iistart(ipoly) = istart
            iiend(ipoly) = iend
            numpoints = numpoints + (iend - istart + 1)

!           advance pointer
            ipoint = iend + 2
         end do
         npoly = ipoly
      end if

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      jaInDefine = 0
      ierr = nf90_redef(inetfile)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         call mess(LEVEL_ERROR, 'Could not put header in net file.')
         call check_error(ierr)
         return
      end if

      if (janetcell_ /= 0) then
         ! Determine max nr. of vertices in NetElems (netcells)
         nv = 0
         do k = 1, nump1d2d
            nv = max(nv, netcell(k)%n)
         end do
      end if

      ! Dimensions
      ierr = nf90_def_dim(inetfile, 'nNetNode', numk, id_netnodedim)
      ierr = nf90_def_dim(inetfile, 'nNetLink', numl, ids_netelem%id_netlinkdim)
      ierr = nf90_def_dim(inetfile, 'nNetLinkPts', 2, id_netlinkptsdim)
      ierr = nf90_def_dim(inetfile, 'nNetElem', nump1d2d, ids_netelem%id_netelemdim)
      if (janetbnd_ /= 0 .and. numbnd > 0) then
         ierr = nf90_def_dim(inetfile, 'nBndLink', numbnd, id_bndlinkdim)

         numencpts = numpoints
         numencparts = npoly
         ! Define the enclosure variables for each mesh
         ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosureInstance', 1, id_encinstdim) ! Just one enclosure
         ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosurePoints', numencpts, id_encptsdim)
         ierr = nf90_def_dim(inetfile, 'nmesh2d_EnclosureParts', numencparts, id_encpartdim)

         ierr = nf90_def_var(inetfile, 'mesh2d_enc_x', nf90_double, id_encptsdim, id_encx)
         ierr = nf90_def_var(inetfile, 'mesh2d_enc_y', nf90_double, id_encptsdim, id_ency)
         ierr = unc_addcoordatts(inetfile, id_encx, id_ency, jsferic)
         ierr = nf90_put_att(inetfile, id_encx, 'cf_role', 'geometry_x_node')
         ierr = nf90_put_att(inetfile, id_ency, 'cf_role', 'geometry_y_node')

         ierr = nf90_def_var(inetfile, 'mesh2d_enc_node_count', nf90_int, id_encinstdim, id_enc_nodecount)
         ierr = nf90_put_att(inetfile, id_enc_nodecount, 'long_name', 'count of coordinates in each instance geometry')

         ierr = nf90_def_var(inetfile, 'mesh2d_enc_part_node_count', nf90_int, id_encpartdim, id_enc_partnodecount)
         ierr = nf90_put_att(inetfile, id_enc_partnodecount, 'long_name', 'count of nodes in each geometry part')

         ierr = nf90_def_var(inetfile, 'mesh2d_enc_interior_ring', nf90_int, id_encpartdim, id_enc_interiorring)
         ierr = nf90_put_att(inetfile, id_enc_interiorring, 'long_name', 'type of each geometry part')

         ierr = nf90_def_var(inetfile, 'mesh2d_enclosure_container', nf90_float, id_enc_container)
         ierr = nf90_put_att(inetfile, id_enc_container, 'geometry_type', 'multipolygon')
         ierr = nf90_put_att(inetfile, id_enc_container, 'node_count', 'mesh2d_enc_node_count')
         ierr = nf90_put_att(inetfile, id_enc_container, 'node_coordinates', 'mesh2d_enc_x mesh2d_enc_y')
         !ierr = nf90_put_att(inetfile, id_enc_container,     'crs',              'crs')
         ierr = nf90_put_att(inetfile, id_enc_container, 'part_node_count', 'mesh2d_enc_part_node_count')
         ierr = nf90_put_att(inetfile, id_enc_container, 'interior_ring', 'mesh2d_enc_interior_ring')
      end if

      if (janetcell_ /= 0 .and. nump1d2d > 0) then
         ierr = nf90_def_dim(inetfile, 'nNetElemMaxNode', nv, ids_netelem%id_netelemmaxnodedim)
         ierr = nf90_def_dim(inetfile, 'nNetLinkContourPts', 4, ids_netelem%id_netlinkcontourptsdim) ! Momentum control volume a la Perot: rectangle around xu/yu
      end if

!    ierr = nf90_def_dim(inetfile, 'nNetElemLink',    numl,   id_netelemlinkdim)
!    ierr = nf90_def_dim(inetfile, 'nNetElemLinkPts', 2,      id_netelemlinkptsdim)

      ierr = nf90_def_var(inetfile, 'Mesh2D', nf90_int, id_mesh2d)
      ierr = nf90_put_att(inetfile, id_mesh2d, 'cf_role', 'mesh_topology')
      ierr = nf90_put_att(inetfile, id_mesh2d, 'node_coordinates', 'NetNode_x NetNode_y')
      ierr = nf90_put_att(inetfile, id_mesh2d, 'node_dimension', 'nNetNode')
      ierr = nf90_put_att(inetfile, id_mesh2d, 'edge_node_connectivity', 'NetLink')
      ierr = nf90_put_att(inetfile, id_mesh2d, 'edge_dimension', 'nNetLink')
      ierr = nf90_put_att(inetfile, NF90_GLOBAL, 'Conventions', 'UGRID-0.9')

      ! Coordinates
      ierr = nf90_def_var(inetfile, 'NetNode_x', nf90_double, id_netnodedim, id_netnodex)
      ierr = nf90_def_var(inetfile, 'NetNode_y', nf90_double, id_netnodedim, id_netnodey)
      ierr = unc_addcoordatts(inetfile, id_netnodex, id_netnodey, jsferic)

      ierr = unc_addcoordmapping(inetfile, jsferic)

    !! Add mandatory lon/lat coords too (only if jsferic==0)
      !ierr = unc_add_lonlat_vars(inetfile, 'NetNode', '', (/ id_netnodedim /), id_netnodelon, id_netnodelat, jsferic)

      ierr = nf90_def_var(inetfile, 'NetNode_z', nf90_double, id_netnodedim, id_netnodez)
      ierr = nf90_put_att(inetfile, id_netnodez, 'units', 'm')
      ierr = nf90_put_att(inetfile, id_netnodez, 'positive', 'up')
      ierr = nf90_put_att(inetfile, id_netnodez, 'standard_name', 'sea_floor_depth')
      ierr = nf90_put_att(inetfile, id_netnodez, 'long_name', 'bed level at net nodes (flow element corners)') !!  at flow element''s corner  / net node
      ierr = nf90_put_att(inetfile, id_netnodez, 'coordinates', 'NetNode_x NetNode_y')
      ierr = nf90_put_att(inetfile, id_netnodez, 'mesh', 'Mesh2D')
      ierr = nf90_put_att(inetfile, id_netnodez, 'location', 'node')

!    ierr = unc_add_gridmapping_att(inetfile, (/ id_netnodex, id_netnodey, id_netnodez /), jsferic)

      ! Netlinks
      ierr = nf90_def_var(inetfile, 'NetLink', nf90_int, (/id_netlinkptsdim, ids_netelem%id_netlinkdim/), id_netlink)
      ierr = nf90_put_att(inetfile, id_netlink, 'standard_name', 'netlink')
      ierr = nf90_put_att(inetfile, id_netlink, 'long_name', 'link between two netnodes')
      ierr = nf90_put_att(inetfile, id_netlink, 'start_index', 1)

      ierr = nf90_def_var(inetfile, 'NetLinkType', nf90_int, ids_netelem%id_netlinkdim, id_netlinktype)
      ierr = nf90_put_att(inetfile, id_netlinktype, 'long_name', 'type of netlink')
      ierr = nf90_put_att(inetfile, id_netlinktype, 'valid_range', (/1, 7/))
      ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_values', (/1, 2, 3, 4, 5, 7/))
      ierr = nf90_put_att(inetfile, id_netlinktype, 'flag_meanings', 'link_between_1D_nodes link_between_2D_nodes embedded_1D2D_link longitudinal_1D2D_link vertically_stacked_1D2D_link roof_gutter_1D2D_link')

      if (janetcell_ /= 0 .and. nump1d2d > 0) then
         ierr = unc_def_net_elem(inetfile, ids_netelem, '', id_mesh2d)
      else
         ierr = nf90_put_att(inetfile, id_mesh2d, 'topology_dimension', 1)
      end if

      if (janetbnd_ /= 0 .and. numbnd > 0) then
         ! List of boundary netlinks
         ierr = nf90_def_var(inetfile, 'BndLink', nf90_int, id_bndlinkdim, id_bndlink)
         ierr = nf90_put_att(inetfile, id_bndlink, 'long_name', 'netlinks that compose the net boundary')
      end if

      if (jaidomain_ /= 0) then
         ierr = unc_def_idomain(inetfile, id_idomain, ids_netelem%id_netelemdim)
      end if

      if (jaiglobal_s_ /= 0) then
         ierr = unc_def_iglobal(inetfile, id_iglobal_s, ids_netelem%id_netelemdim)
      end if

      ierr = nf90_enddef(inetfile)
      call readyy('Writing net data', .05d0)

      ! Write the actual data
      ierr = nf90_put_var(inetfile, id_netnodex, xk(1:numk))
      call readyy('Writing net data', .25d0)
      ierr = nf90_put_var(inetfile, id_netnodey, yk(1:numk))
      call readyy('Writing net data', .45d0)
      ierr = nf90_put_var(inetfile, id_netnodez, zk(1:numk))
      call readyy('Writing net data', .65d0)

      !   ierr = nf90_put_var(inetfile, id_netlink,     kn, count=(/ 2, numl /), map=(/ 1, 3 /))
      allocate (kn1write(numL))
      allocate (kn2write(numL))
      do L = 1, numL
         kn1write(L) = kn(1, L)
         kn2write(L) = kn(2, L)
      end do
      ierr = nf90_put_var(inetfile, id_netlink, kn1write, count=(/1, numl/), start=(/1, 1/))
      ierr = nf90_put_var(inetfile, id_netlink, kn2write, count=(/1, numl/), start=(/2, 1/))
      deallocate (kn1write)
      deallocate (kn2write)
      call readyy('Writing net data', .85d0)

      ! AvD: TODO: if jsferic==0, then use proj.4 to convert x/y and write lon/lat for netnodes too.

      ! An array slice cannot be passed to netcdf C-library (risk of stack overflow), so use copy.
      allocate (kn3(numl))
      do L = 1, numl
         if (kn(3, L) >= 1 .and. kn(3, L) <= 7) then
            kn3(L) = kn(3, L) ! TODO: UNST-715: in rare cases, this will incorrectly change 1D net links into 2D net links.
         else
            kn3(L) = 2 ! e.g. thind dams, so thin dams become 2D again
         end if
      end do

      ierr = nf90_put_var(inetfile, id_netlinktype, kn3)
      deallocate (kn3)
      call readyy('Writing net data', 1d0)

      if (janetbnd_ /= 0 .and. numbnd > 0) then
         ! Write boundary links
         ierr = nf90_put_var(inetfile, id_bndlink, ibndlink, count=(/numbnd/))

         ! Write grid enclosure
         ipoint = 1
         do ipoly = 1, npoly
            ! Points
            istart = iistart(ipoly)
            iend = iiend(ipoly)
            nv = iend - istart + 1
            allocate (polc(nv)) ! temp variable necessary here to get actual values in file
            polc = xpl(istart:iend)
            ierr = nf90_put_var(inetfile, id_encx, polc, start=(/ipoint/), count=(/nv/))
            polc = ypl(istart:iend)
            ierr = nf90_put_var(inetfile, id_ency, polc, start=(/ipoint/), count=(/nv/))
            deallocate (polc)
            ipoint = ipoint + nv

            ! Current part
            ierr = nf90_put_var(inetfile, id_enc_partnodecount, (/nv/), start=(/ipoly/), count=(/1/))

            ! Re-determine the orientation of this polygon again (currently no nice way to get it back from copynetboundstopol())
            call polorientation(xpl(istart:iend), ypl(istart:iend), iend - istart + 1, iend - istart + 1, iorient)
            ! NCSG requires 1/0 var to denote interior/outer ring respectively. Additionally, we always write outer as CCW, and inner as CW.
            if (iorient == 1) then
               iinterior = 0
            else
               iinterior = 1
            end if
            ierr = nf90_put_var(inetfile, id_enc_interiorring, (/iinterior/), start=(/ipoly/), count=(/1/))
         end do
         ierr = nf90_put_var(inetfile, id_enc_nodecount, numencpts)
         ierr = nf90_sync(inetfile)
         call restorepol()

      end if
      !
      if (jaidomain_ /= 0) then
         ierr = nf90_put_var(inetfile, id_idomain, idomain, count=(/nump1d2d/)) !!!!!!!!!!!!!!
      end if

      if (jaiglobal_s_ /= 0) then
         ierr = nf90_put_var(inetfile, id_iglobal_s, iglobal_s, count=(/nump1d2d/))
      end if

      if (janetcell_ /= 0 .and. nump1d2d > 0) then
         ierr = unc_write_net_elem(inetfile, ids_netelem)
      end if

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 1) then
         ierr = nf90_redef(inetfile)
      end if

      if (allocated(ibndlink)) then
         deallocate (ibndlink)
      end if
      if (allocated(kn3)) then
         deallocate (kn3)
      end if

      call readyy('Writing net data', -1d0)
   end subroutine unc_write_net_filepointer

!> helper function to define the net elements in an open NetCDF file
   function unc_def_net_elem(inetfile, ids, prefix, id_mesh2d) result(ierr)
      use m_missing, only: dmiss, intmiss
      use m_sferic, only: jsferic
      integer, intent(in) :: inetfile !< file id NetCDF file
      type(t_unc_netelem_ids), intent(inout) :: ids !< struct holding variable ids
      character(len=*), intent(in) :: prefix !< String prefix for the NetlinkContour variable names that will be defined.
      integer, intent(in), optional :: id_mesh2d !< id for mesh2d (is case of UGRID-0.8)
      integer :: ierr !< function result

      ierr = 0
      if (present(id_mesh2d)) then
         ierr = nf90_put_att(inetfile, id_mesh2d, 'topology_dimension', 2)
         ierr = nf90_put_att(inetfile, id_mesh2d, 'face_node_connectivity', 'NetElemNode')
         ierr = nf90_put_att(inetfile, id_mesh2d, 'face_dimension', 'nNetElem')
      end if
      !
      ! Netcells
      ! Netcell-to-netnode mapping
      ierr = nf90_def_var(inetfile, 'NetElemNode', nf90_int, (/ids%id_netelemmaxnodedim, ids%id_netelemdim/), ids%id_netelemnode)
      if (ierr /= 0) goto 999
      ierr = nf90_put_att(inetfile, ids%id_netelemnode, 'long_name', 'mapping from net cell to net nodes (counterclockwise)')
      ierr = nf90_put_att(inetfile, ids%id_netelemnode, 'start_index', 1)
      ierr = nf90_put_att(inetfile, ids%id_netelemnode, '_FillValue', intmiss)

      ierr = nf90_def_var(inetfile, 'NetElemLink', nf90_int, (/ids%id_netelemmaxnodedim, ids%id_netelemdim/), ids%id_netelemlink)
      if (ierr /= 0) goto 999
      ierr = nf90_put_att(inetfile, ids%id_netelemlink, 'long_name', 'mapping from net cell to its net links (counterclockwise)')
      ierr = nf90_put_att(inetfile, ids%id_netelemlink, 'short_name', 'netcell()%LIN')

      ierr = nf90_def_var(inetfile, trim(prefix)//'NetLinkContour_x', nf90_double, (/ids%id_netlinkcontourptsdim, ids%id_netlinkdim/), ids%id_netlinkcontourx)
      if (ierr /= 0) goto 999
      ierr = nf90_def_var(inetfile, trim(prefix)//'NetLinkContour_y', nf90_double, (/ids%id_netlinkcontourptsdim, ids%id_netlinkdim/), ids%id_netlinkcontoury)
      if (ierr /= 0) goto 999
      ierr = unc_addcoordatts(inetfile, ids%id_netlinkcontourx, ids%id_netlinkcontoury, jsferic)
      ierr = nf90_put_att(inetfile, ids%id_netlinkcontourx, 'long_name', 'list of x-contour points of momentum control volume surrounding each net/flow link')
      ierr = nf90_put_att(inetfile, ids%id_netlinkcontoury, 'long_name', 'list of y-contour points of momentum control volume surrounding each net/flow link')
      ierr = nf90_put_att(inetfile, ids%id_netlinkcontourx, '_FillValue', dmiss)
      ierr = nf90_put_att(inetfile, ids%id_netlinkcontoury, '_FillValue', dmiss)

      ierr = nf90_def_var(inetfile, 'NetLink_xu', nf90_double, (/ids%id_netlinkdim/), ids%id_netlinkxu)
      if (ierr /= 0) goto 999
      ierr = nf90_def_var(inetfile, 'NetLink_yu', nf90_double, (/ids%id_netlinkdim/), ids%id_netlinkyu)
      if (ierr /= 0) goto 999
      ierr = unc_addcoordatts(inetfile, ids%id_netlinkxu, ids%id_netlinkyu, jsferic)
      ierr = nf90_put_att(inetfile, ids%id_netlinkxu, 'long_name', 'x-coordinate of net link center (velocity point)')
      ierr = nf90_put_att(inetfile, ids%id_netlinkyu, 'long_name', 'y-coordinate of net link center (velocity point)')

      return

999   continue ! label for NetCDF errors
      call check_error(ierr, 'unc_def_net_elem')

   end function unc_def_net_elem

!> helper function to write the net elements to an already open NetCDF file
   function unc_write_net_elem(inetfile, ids) result(ierr)
      use network_data
      use m_missing, only: intmiss

      integer, intent(in) :: inetfile !< id of the NetCDF file
      type(t_unc_netelem_ids), intent(in) :: ids !< struct holding variable ids
      integer :: ierr !< function result

      integer, allocatable :: netcellnod(:, :), netcelllin(:, :)
      real(kind=hp), allocatable :: xtt(:, :), ytt(:, :), xut(:), yut(:)
      integer :: k, nv, nv1

      ierr = 0

      ! Write net cells
      ierr = nf90_inquire_dimension(inetfile, ids%id_netelemmaxnodedim, len=nv)
      if (ierr /= 0) goto 999
      allocate (netcellnod(nv, nump1d2d), netcelllin(nv, nump1d2d), stat=ierr)
      if (ierr /= 0) goto 888
      netcellnod = intmiss
      netcelllin = intmiss
      do k = 1, nump1d2d
         nv1 = netcell(k)%n
         netcellnod(1:nv1, k) = netcell(k)%nod
         netcelllin(1:nv1, k) = netcell(k)%lin
      end do
      ierr = nf90_put_var(inetfile, ids%id_netelemnode, netcellnod)
      if (ierr == 0) ierr = nf90_put_var(inetfile, ids%id_netelemlink, netcelllin)
      if (ierr /= 0) goto 999
      deallocate (netcellnod, netcelllin)

      call readyy('Writing net data', .65d0)
      allocate (xtt(4, numl), ytt(4, numl), xut(numl), yut(numl), stat=ierr)
      if (ierr /= 0) goto 888
      call fill_netlink_geometry(xtt, ytt, xut, yut)
      ierr = nf90_put_var(inetfile, ids%id_netlinkcontourx, xtt, (/1, 1/), (/4, numl/))
      if (ierr == 0) ierr = nf90_put_var(inetfile, ids%id_netlinkcontoury, ytt, (/1, 1/), (/4, numl/))
      if (ierr == 0) ierr = nf90_put_var(inetfile, ids%id_netlinkxu, xut)
      if (ierr == 0) ierr = nf90_put_var(inetfile, ids%id_netlinkyu, yut)
      if (ierr /= 0) goto 999

      deallocate (xtt, ytt, xut, yut)

      return

888   continue ! label for allocation errors
      call mess(LEVEL_ERROR, 'Allocation error in unc_write_net_elem')
      return

999   continue ! label for NetCDF errors
      call check_error(ierr, 'unc_write_net_elem')
   end function unc_write_net_elem

!> helper function to fill coordinate arrays (see below)
   subroutine fill_netlink_geometry(xtt, ytt, xut, yut)
      use network_data
      use m_missing, only: dmiss, dxymis
      use m_sferic, only: jsferic, jasfer3D, rd2dg, ra
      use m_flowgeom, only: xz, yz
      use geometry_module, only: normaloutchk

      real(kind=hp), intent(out) :: xtt(:, :) !< array with x-contour points of momentum control volume surrounding each net/flow link
      real(kind=hp), intent(out) :: ytt(:, :) !< array with y-contour points of momentum control volume surrounding each net/flow link
      real(kind=hp), intent(out) :: xut(:) !< array with x-coordinate of net link center (velocity point)
      real(kind=hp), intent(out) :: yut(:) !< array with y-coordinate of net link center (velocity point)

      integer :: n1, l, k1, k2, kt, ja
      real(kind=hp) :: xzn, yzn, x3, y3, x4, y4, dis, xp, yp, rl, xn, yn
      real(kind=hp), parameter :: half = 0.5_hp

      do L = 1, numl
         xut(L) = half * (xk(kn(1, L)) + xk(kn(2, L)))
         yut(L) = half * (yk(kn(1, L)) + yk(kn(2, L)))
      end do

      do L = 1, numl1d
         xtt(:, L) = dmiss
         ytt(:, L) = dmiss
      end do

      do L = numl1d + 1, numl
         xtt(:, L) = 0d0
         ytt(:, L) = 0d0
         if (lnn(L) >= 1) then
            K1 = kn(1, L)
            k2 = kn(2, L)

            ! 'left' half
            n1 = lne(1, L)
            if (n1 < 0) n1 = -n1
            x3 = xk(k1)
            y3 = yk(k1)
            x4 = xk(k2)
            y4 = yk(k2)
            xzn = xz(n1)
            yzn = yz(n1)

            ! Normal distance from circumcenter to net link:
            call dlinedis2(xzn, yzn, x3, y3, x4, y4, ja, dis, xp, yp, rl)
            if (jsferic == 1) then
               dis = rd2dg * dis / ra ! convert m to degrees; valid if not to close to one of the poles
            end if

            ! Note: we're only in net-mode, not yet in flow-mode, so we can NOT assume that net node 3->4 have a 'rightward' flow node orientation 1->2
            ! Instead, compute outward normal, and swap points 3 and 4 if necessary, such that we locally achieve the familiar k3->k4 + n1->n2 orientation.
            ! Outward normal vector of net link (cell 1 considered 'inward'):
            call normaloutchk(x3, y3, x4, y4, xzw(n1), yzw(n1), xn, yn, ja, jsferic, jasfer3D, dmiss, dxymis)
            if (ja == 1) then ! normal was flipped, so swap net point 3 and 4 locally, to construct counterclockwise cell hereafter.
               kt = k1
               k1 = k2
               k2 = kt
               xp = x3
               yp = y3
               x3 = x4
               y3 = y4
               x4 = xp
               y4 = yp
            end if

            xtt(1, L) = x4 - dis * xn
            ytt(1, L) = y4 - dis * yn
            xtt(2, L) = x3 - dis * xn
            ytt(2, L) = y3 - dis * yn

            if (lnn(l) == 2) then
               ! second half
               n1 = lne(2, L)
               if (n1 < 0) n1 = -n1
               xzn = xz(n1)
               yzn = yz(n1)
               call dlinedis2(xzn, yzn, x3, y3, x4, y4, ja, dis, xp, yp, rl)
               if (jsferic == 1) then
                  dis = rd2dg * dis / ra ! convert m to degrees; valid if not to close to one of the poles
               end if
               xtt(3, L) = x3 + dis * xn
               ytt(3, L) = y3 + dis * yn
               xtt(4, L) = x4 + dis * xn
               ytt(4, L) = y4 + dis * yn
            else
               ! closed net boundary, no second half cell, just close off with netlink
               xtt(3, L) = x3
               ytt(3, L) = y3
               xtt(4, L) = x4
               ytt(4, L) = y4
            end if
         else
            ! No surrounding cells: leave missing fill values in file.
            xtt(:, L) = dmiss
            ytt(:, L) = dmiss
         end if
      end do
   end subroutine fill_netlink_geometry

!> helper function to define idomain
   function unc_def_idomain(inetfile, id_idomain, id_netelemdim) result(ierr)
      use m_partitioninfo, only: ndomains
      integer, intent(in) :: inetfile, id_netelemdim
      integer, intent(out) :: id_idomain
      integer :: ierr

      ierr = nf90_def_var(inetfile, 'idomain', nf90_int, (/id_netelemdim/), id_idomain)
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_idomain, 'long_name', 'partition subdomain numbers')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_idomain, 'short_name', 'idomain')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_idomain, 'valid_max', ndomains) ! the total number of subdomains
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_idomain, 'mesh', 'Mesh2D')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_idomain, 'location', 'face')
   end function unc_def_idomain

!> helper function to define iglobal_s
   function unc_def_iglobal(inetfile, id_iglobal_s, id_netelemdim) result(ierr)
      use m_partitioninfo, only: Nglobal_s
      integer, intent(in) :: inetfile, id_netelemdim
      integer, intent(out) :: id_iglobal_s
      integer :: ierr

      ierr = nf90_def_var(inetfile, 'iglobal_s', nf90_int, (/id_netelemdim/), id_iglobal_s)
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_iglobal_s, 'long_name', 'global netcell numbers')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_iglobal_s, 'short_name', 'iglobal_s')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_iglobal_s, 'valid_max', Nglobal_s)
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_iglobal_s, 'mesh', 'Mesh2D')
      if (ierr == NF90_NOERR) ierr = nf90_put_att(inetfile, id_iglobal_s, 'location', 'face')
   end function unc_def_iglobal

! NOTE: AvD: this routine below is a temporary working function that should replace unc_write_ugrid soon.
! It should contain two things:
! * io_ugrid-based writing of all basic net data (nodes/edges/faces)
! * AND NetLinkContour-related variables (see the original unc_write_net_filepointer routine)
!> Writes the unstructured network in UGRID format to an already opened netCDF dataset.
   subroutine unc_write_net_ugrid2(ncid, id_tsp, janetcell, jaidomain, jaiglobal_s)
      use network_data, xe_no => xe, ye_no => ye
      use m_flowgeom, only: ndx2d, ndxi
      use m_partitioninfo, only: idomain, ndomains, iglobal_s, Nglobal_s
      use m_sferic, only: jsferic
      use m_missing, only: dmiss
      use netcdf
      use m_alloc
      use geometry_module
      use m_save_ugrid_state
      use gridoperations
      use fm_location_types

      implicit none

      integer, intent(in) :: ncid !< NetCDF file id
      type(t_unc_timespace_id), intent(inout) :: id_tsp !< struct holding NetCDF ids
      integer, optional, intent(in) :: janetcell !< write net cell (1) or not (0, default)
      integer, optional, intent(in) :: jaidomain !< write subdomain numbers (1) or not (0, default)
      integer, optional, intent(in) :: jaiglobal_s !< write global netcell numbers (1) or not (0, default)

      integer :: janetcell_, jaidomain_, jaiglobal_s_
      integer :: id_netnodedim
      type(t_unc_netelem_ids) :: ids_netelem

      integer :: nn
      integer, allocatable :: edge_nodes(:, :), face_nodes(:, :), edge_type(:), contacts(:, :)

      integer :: ierr
      integer :: i, k, k1, k2, numl2d, numk1d, numk2d, nump1d, L, Lnew, nv, n1, n2, n
      integer :: jaInDefine
      integer :: id_zf

      real(kind=hp), allocatable :: xn(:), yn(:), zn(:), xe(:), ye(:), zf(:)

      integer :: n1dedges, n1d2dcontacts, start_index
      integer, allocatable :: contacttype(:), idomain1d(:), iglobal_s1d(:)

      call readyy('Writing net data', 0d0)

      jaInDefine = 0
      n1d2dcontacts = 0
      start_index = 1

      if (present(janetcell)) then
         janetcell_ = janetcell
      else
         janetcell_ = 0
      end if
      if (present(jaidomain)) then
         jaidomain_ = jaidomain
      else
         jaidomain_ = 0
      end if
      if (present(jaiglobal_s)) then
         jaiglobal_s_ = jaiglobal_s
      else
         jaiglobal_s_ = 0
      end if

      !We need the cells for the face_nodes
      if (janetcell_ /= 0) then
         if (size(lnn) < numl .or. netstat == NETSTAT_CELLS_DIRTY) then
            call setnodadm(0)
            call findcells(0)
            call find1dcells()
         end if
      end if

      if (NUMK <= 0) then
         call mess(LEVEL_WARN, 'No grid points in model, will not write net geometry.')
         return
      end if

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         call mess(LEVEL_ERROR, 'Could not put header in net geometry file.')
         call check_error(ierr)
         return
      end if

      if (jsferic == 1) then
         crs%epsg_code = 4326
      end if

      ! 1D network geometry
      NUMK1D = 0
      if (numl1d > 0) then

         ! count 1d mesh nodes, edges and 1d2d contacts
         n1dedges = 0
         n1d2dcontacts = 0
         KC(:) = 0
         NUMK1D = 0
         do L = 1, numl1d
            if (janetcell_ == 0 .or. (kn(3, L) == 1 .or. kn(3, L) == 6)) then
               ! Regular 1D net link, or: when no cells, all 1D2D-type net links will also be included with both start and end node.
               n1dedges = n1dedges + 1

               K1 = KN(1, L)
               K2 = KN(2, L)
               if (KC(K1) == 0) then
                  NUMK1D = NUMK1D + 1
                  KC(K1) = 1
               end if
               if (KC(K2) == 0) then
                  NUMK1D = NUMK1D + 1
                  KC(K2) = 1
               end if
            else
               ! 1D2D-type net links, with cell info available.
               n1d2dcontacts = n1d2dcontacts + 1

               N1 = abs(lne(1, L))
               N2 = abs(lne(2, L))
               if (N1 > nump .and. N2 <= nump) then ! First point of 1D link is 1D cell
                  K1 = netcell(N1)%nod(1)
                  if (KC(K1) == 0) then
                     NUMK1D = NUMK1D + 1
                     KC(K1) = 1
                  end if
               else if (N2 > nump .and. N1 <= nump) then ! Second point of 1D link is 1D cell
                  K2 = netcell(N2)%nod(1)
                  if (KC(K2) == 0) then
                     NUMK1D = NUMK1D + 1
                     KC(K2) = 1
                  end if
               else
                  n1d2dcontacts = n1d2dcontacts - 1
               end if

            end if
         end do

         ! Allocate  nodes
         numk1d = max(numk1d, nump1d2d - nump) ! See: UNST-6524. Ugly fix to prevent a crash in case of invalid 1d2dlinks
         call realloc(xn, NUMK1D)
         call realloc(yn, NUMK1D)
         call realloc(zn, NUMK1D)
         if (jaidomain_ > 0) then
            call realloc(idomain1d, NUMK1D, fill=-999)
         end if
         if (jaiglobal_s_ > 0) then
            call realloc(iglobal_s1d, NUMK1D, fill=-999)
         end if

         ! Allocate edges
         call realloc(edge_nodes, (/2, n1dedges/), fill=-999)
         call realloc(edge_type, n1dedges, fill=-999, keepExisting=.false.)
         call realloc(xe, n1dedges, fill=dmiss, keepExisting=.false.)
         call realloc(ye, n1dedges, fill=dmiss, keepExisting=.false.)

         ! Allocate contacts
         call realloc(contacts, (/2, n1d2dcontacts/), fill=-999)
         call realloc(contacttype, n1d2dcontacts, keepExisting=.false., fill=0)

         ! Assign values to 1D mesh nodes and edges, and 1d2d contacts
         n1dedges = 0
         n1d2dcontacts = 0
         NUMK1D = 0
         KC(:) = 0
         nump1d = nump1d2d - nump
         if (janetcell_ == 1 .and. nump1d > 0) then
            ! Determine 1D net nodes directly from 1D net cells
            do N1 = nump + 1, nump1d2d
               k1 = netcell(N1)%nod(1)

               numk1d = numk1d + 1
               xn(numk1d) = xk(k1)
               yn(numk1d) = yk(k1)
               zn(numk1d) = zk(k1)
               kc(k1) = -numk1d ! Remember new node number
            end do

            do L = 1, NUML1D
               if (kn(3, L) == 1 .or. kn(3, L) == 6) then
                  n1dedges = n1dedges + 1
                  K1 = KN(1, L)
                  K2 = KN(2, L)

                  edge_nodes(1, n1dedges) = abs(KC(K1))
                  edge_nodes(2, n1dedges) = abs(KC(K2))
                  edge_type(n1dedges) = KN(3, L)

                  xe(n1dedges) = .5d0 * (xk(K1) + xk(K2)) ! TODO: AvD: make this sferic+3D-safe
                  ye(n1dedges) = .5d0 * (yk(K1) + yk(K2)) ! TODO: AvD: make this sferic+3D-safe

               else if (kn(3, L) == 3 .or. kn(3, L) == 4 .or. kn(3, L) == 5 .or. kn(3, L) == 7) then ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
                  ! 1D2D-type net links, with cell info available.

                  N1 = abs(lne(1, L))
                  N2 = abs(lne(2, L))

                  n1d2dcontacts = n1d2dcontacts + 1
                  if (N1 > nump .and. N2 <= nump) then ! First point of 1D link is 1D cell
                     contacts(1, n1d2dcontacts) = abs(KC(netcell(N1)%nod(1))) ! cell -> orig node -> new node
                     contacts(2, n1d2dcontacts) = N2 ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
                  else if (N2 > nump .and. N1 <= nump) then ! First point of 1D link is 1D cell
                     contacts(1, n1d2dcontacts) = abs(KC(netcell(N2)%nod(1))) ! cell -> orig node -> new node
                     contacts(2, n1d2dcontacts) = N1 ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
                  else
                     n1d2dcontacts = n1d2dcontacts - 1
                     cycle
                  end if

                  contacttype(n1d2dcontacts) = kn(3, L)
               end if
            end do

         else ! not directly 1D netcell based, indirectly 1D netlink based

            do L = 1, NUML1D
               if (janetcell_ == 0 .or. (kn(3, L) == 1 .or. kn(3, L) == 6)) then
                  n1dedges = n1dedges + 1

                  K1 = KN(1, L)
                  K2 = KN(2, L)
                  if (KC(K1) == 0) then
                     NUMK1D = NUMK1D + 1
                     xn(NUMK1D) = xk(K1)
                     yn(NUMK1D) = yk(K1)
                     zn(NUMK1D) = zk(K1)
                     KC(K1) = -NUMK1D ! Remember new node number
                  end if
                  if (KC(K2) == 0) then
                     NUMK1D = NUMK1D + 1
                     xn(NUMK1D) = xk(K2)
                     yn(NUMK1D) = yk(K2)
                     zn(NUMK1D) = zk(K2)
                     KC(K2) = -NUMK1D ! Remember new node number
                  end if

                  edge_nodes(1, n1dedges) = abs(KC(KN(1, L)))
                  edge_nodes(2, n1dedges) = abs(KC(KN(2, L)))
                  edge_type(n1dedges) = KN(3, L)

                  xe(n1dedges) = .5d0 * (xk(K1) + xk(K2)) ! TODO: AvD: make this sferic+3D-safe
                  ye(n1dedges) = .5d0 * (yk(K1) + yk(K2)) ! TODO: AvD: make this sferic+3D-safe

               else if (kn(3, L) == 3 .or. kn(3, L) == 4 .or. kn(3, L) == 5 .or. kn(3, L) == 7) then ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
                  ! 1D2D-type net links, with cell info available.
                  n1d2dcontacts = n1d2dcontacts + 1

                  N1 = abs(lne(1, L))
                  N2 = abs(lne(2, L))

                  if (N1 > nump) then ! First point of 1D link is 1D cell
                     K1 = netcell(N1)%nod(1)
                     if (KC(K1) == 0) then
                        NUMK1D = NUMK1D + 1
                        xn(NUMK1D) = xk(K1)
                        yn(NUMK1D) = yk(K1)
                        zn(NUMK1D) = zk(K1)
                        KC(K1) = -NUMK1D ! Remember new node number
                     end if

                     contacts(1, n1d2dcontacts) = abs(KC(netcell(N1)%nod(1))) ! cell -> orig node -> new node
                     contacts(2, n1d2dcontacts) = N2 ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
                  end if

                  if (N2 > nump) then ! First point of 1D link is 1D cell
                     K2 = netcell(N2)%nod(1)
                     if (KC(K2) == 0) then
                        NUMK1D = NUMK1D + 1
                        xn(NUMK1D) = xk(K2)
                        yn(NUMK1D) = yk(K2)
                        zn(NUMK1D) = zk(K2)
                        KC(K2) = -NUMK1D ! Remember new node number
                     end if
                     contacts(1, n1d2dcontacts) = abs(KC(netcell(N2)%nod(1))) ! cell -> orig node -> new node
                     contacts(2, n1d2dcontacts) = N1 ! 2D cell number in network_data is the same in UGRID mesh2d numbering (see below).
                  end if

                  contacttype(n1d2dcontacts) = kn(3, L)
               end if
            end do
         end if ! not netcell-based

         if (jaidomain_ > 0) then
            do n = nump + 1, nump1d2d ! Loop over all 1D netcells
               k1 = netcell(n)%nod(1) ! original 1D netnode index
               k = abs(KC(k1)) ! This will only be k /= n if the reconstructed 1D netlink-based numbering in file differs from the input 1D netcell numbering.
               idomain1d(k) = idomain(n) ! the node ordering in idomain1d is the same as in xn,yn,zn
            end do
         end if

         if (jaiglobal_s_ > 0) then
            do n = nump + 1, nump1d2d
               k1 = netcell(n)%nod(1)
               k = abs(KC(k1)) ! This will only be k /= n if the reconstructed 1D netlink-based numbering in file differs from the input 1D netcell numbering.
               iglobal_s1d(k) = iglobal_s(n)
            end do
         end if

         if (.not. allocated(face_nodes)) then
            allocate (face_nodes(0, 0))
         end if
         if (associated(meshgeom1d%ngeopointx)) then
            if (meshgeom1d%numnode >= 0) then ! TODO: LC:  check the number of mesh nodes has not changed
               ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, numk1d, n1dedges, 0, 0, &
                                           edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:1), yzw(1:1), &
                                           crs, -999, dmiss, start_index, -999, -999, null(), null(), & ! Indexing is 1 based
                                           id_tsp%network1d, network1dname, meshgeom1d%nnodex, meshgeom1d%nnodey, nnodeids, nnodelongnames, &
                                           meshgeom1d%nedge_nodes(1, :), meshgeom1d%nedge_nodes(2, :), nbranchids, nbranchlongnames, meshgeom1d%nbranchlengths, meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranches, &
                                           meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, meshgeom1d%ngeometry, &
                                           meshgeom1d%nbranchorder, &
                                           nodeids=nodeids, nodelongnames=nodelongnames, nodebranchidx=meshgeom1d%nodebranchidx, nodeoffsets=meshgeom1d%nodeoffsets, edgebranchidx=meshgeom1d%edgebranchidx, edgeoffsets=meshgeom1d%edgeoffsets)
            else
               call mess(LEVEL_ERROR, 'Could not put header in net geometry file.')
               return
            end if
         else
            ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, numk1d, n1dedges, 0, 0, &
                                        edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:1), yzw(1:1), &
                                        crs, -999, dmiss, start_index)
         end if

      !! TODO: AvD: hier verder
      !! Determine max nr of vertices and contour points
         !
      !! NOTE: numk2d = numk - numk1d does not necessarily hold, if input grid illegally connected a 2D net link and 1D netlink to one and the same net node.
      !! Count 2D net nodes
         !numNodes   = ndx1d
         !numContPts = 0
         !do i=1,ndx1d
         !   numNodes   = max(numNodes,   size(netcell(ndx2d + i)%NOD))
         !   numContPts = max(numContPts, size(netcell(ndx2d + i)%NOD))
         !enddo
         !
         !if ( allocated(work2) ) deallocate( work2 )
         !allocate( work2(numContPts,ndx1d) ) ; work2 = dmiss
         !
         !ierr = nf90_def_dim(mapids%ncid, 'nmesh1d_FlowElemContourPts', numContPts,    id_flowelemcontourptsdim)
         !
      !! Flow elem contours (plot help)
      !! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
         !ierr = nf90_def_var(mapids%ncid, 'mesh1d_FlowElemContour_x', nf90_double, (/ id_flowelemcontourptsdim, mapids%id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontourx)
         !ierr = nf90_def_var(mapids%ncid, 'mesh1d_FlowElemContour_y', nf90_double, (/ id_flowelemcontourptsdim, mapids%id_tsp%meshids1d%dimids(mdim_node) /), id_flowelemcontoury)
         !ierr = unc_addcoordatts(mapids%ncid, id_flowelemcontourx, id_flowelemcontoury, jsferic)
         !ierr = nf90_put_att(mapids%ncid, id_flowelemcontourx, 'long_name',     'list of x-coordinates forming flow element')
         !ierr = nf90_put_att(mapids%ncid, id_flowelemcontoury, 'long_name',     'list of y-coordinafltes forming flow element')
         !ierr = nf90_put_att(mapids%ncid, id_flowelemcontourx, '_FillValue', dmiss)
         !ierr = nf90_put_att(mapids%ncid, id_flowelemcontoury, '_FillValue', dmiss)
         !
         !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids1d%varids(mid_nodex), 'bounds', 'mesh1d_FlowElemContour_x')
         !ierr = nf90_put_att(mapids%ncid, mapids%id_tsp%meshids1d%varids(mid_nodey), 'bounds', 'mesh1d_FlowElemContour_y')
         !
         !ierr = nf90_enddef(mapids%ncid)
         !
         !do i=1,ndx1d
         !   nn = size(nd(ndx2d + i)%x)
         !   do n = 1,nn
         !      work2(n,i)=nd(ndx2d + i)%x(n)
         !   enddo
         !enddo
         !ierr = nf90_put_var(mapids%ncid, id_flowelemcontourx, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
         !
         !do i=1,ndx1d
         !   nn = size(nd(ndx2d + i)%x)
         !   do n = 1,nn
         !      work2(n,i)=nd(ndx2d + i)%y(n)
         !   enddo
         !enddo
         !ierr = nf90_put_var(mapids%ncid, id_flowelemcontoury, work2(1:numContPts,1:ndx1d), (/ 1, 1 /), (/ numContPts, ndx1d /) )
         !ierr = nf90_redef(mapids%ncid)
         !
         !deallocate( work2 )
         !
         ! Add edge type variable (edge-flowlink relation)
         call write_edge_type_variable(ncid, id_tsp%meshids1d, mesh1dname, edge_type)

         if (numk1d > 0) then
            ierr = ug_inq_varid(ncid, id_tsp%meshids1d, 'node_z', id_tsp%id_netnodez(1)) ! TODO: AvD: keep this here as long as ug itself does not WRITE the zk data.
            ! TODO: AvD: move cell_measure  'point' to io_ugrid, or not? Check comm with G.Lang.

            ierr = nf90_enddef(ncid)
            ierr = nf90_put_var(ncid, id_tsp%id_netnodez(1), zn)

            ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.
         end if

         deallocate (xn)
         deallocate (yn)
         deallocate (edge_nodes)
         deallocate (edge_type)
      end if ! 1D network geometry

      call readyy('Writing net data', 0.3d0)

      numl2d = numl - numl1d
      if (numl2d > 0) then ! 2D net geometry
         call realloc(edge_nodes, (/2, numl2d/), fill=-999, keepExisting=.false.)
         call realloc(edge_type, numl2d, fill=-999, keepExisting=.false.)
         call realloc(xe, numl2d, fill=dmiss, keepExisting=.false.)
         call realloc(ye, numl2d, fill=dmiss, keepExisting=.false.)

         ! All 2D net links
         ! Count first:
         KC(:) = 0
         NUMK2D = 0
         do L = NUML1D + 1, NUML
            K1 = KN(1, L)
            K2 = KN(2, L)
            if (KC(K1) == 0) then
               NUMK2D = NUMK2D + 1
               KC(K1) = 1
            end if
            if (KC(K2) == 0) then
               NUMK2D = NUMK2D + 1
               KC(K2) = 1
            end if
         end do

         ! Nodes Allocate
         call realloc(xn, NUMK2D)
         call realloc(yn, NUMK2D)
         call realloc(zn, NUMK2D)

         k = 0
         KC(:) = 0
         do L = NUML1D + 1, NUML
            LNEW = L - NUML1D
            K1 = KN(1, L)
            K2 = KN(2, L)
            if (KC(K1) == 0) then
               k = k + 1
               xn(k) = xk(K1)
               yn(k) = yk(K1)
               zn(k) = zk(K1)
               KC(K1) = -k ! Remember new node number
            end if
            if (KC(K2) == 0) then
               k = k + 1
               xn(k) = xk(K2)
               yn(k) = yk(K2)
               zn(k) = zk(K2)
               KC(K2) = -k ! Remember new node number
            end if

            edge_nodes(1, Lnew) = abs(KC(KN(1, L)))
            edge_nodes(2, Lnew) = abs(KC(KN(2, L)))
            edge_type(Lnew) = KN(3, L) ! TODO: AvD: later, restore UG_EDGE_TYPE params, these have gotten lost.

            xe(Lnew) = .5d0 * (xk(K1) + xk(K2)) ! TODO: AvD: make this sferic+3D-safe
            ye(Lnew) = .5d0 * (yk(K1) + yk(K2)) ! TODO: AvD: make this sferic+3D-safe
         end do

         if (allocated(iglobal_s) .and. size(blcell) > 0) then
            call realloc(zf, nump, fill=dmiss, keepExisting=.false.)
            do i = 1, nump
               zf(i) = blcell(iglobal_s(i))
            end do
         end if

         ! Determine max nr of vertices and contour points
         nv = 0
         do i = 1, NUMP ! 2D cells
            nv = max(nv, netcell(i)%n)
         end do

         ! Note: AvD: numk may be larger than nr of cell corners. Will cause problems when writing output data on corners (mismatch in dimensions), not crucial now.
         call realloc(face_nodes, (/nv, NUMP/), fill=-999)
         do i = 1, NUMP
            nn = size(netcell(i)%NOD)
            do k = 1, nn
               face_nodes(k, i) = abs(KC(netcell(i)%NOD(k))) ! Use permuted 2D node numbers
            end do
         end do

         ! TODO: AvD: lnx1d+1:lnx includes open bnd links, which may *also* be 1D boundaries (don't want that in mesh2d)
         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids2d, mesh2dname, 2, UG_LOC_EDGE + UG_LOC_FACE, numk2d, numl2d, nump, nv, &
                                     edge_nodes, face_nodes, null(), null(), null(), xn, yn, xe, ye, xzw(1:nump), yzw(1:nump), &
                                     crs, -999, dmiss, start_index)

         ! Add edge type variable (edge-flowlink relation)
         call write_edge_type_variable(ncid, id_tsp%meshids2d, mesh2dname, edge_type)

         if (numk2d > 0) then
            ierr = ug_inq_varid(ncid, id_tsp%meshids2d, 'node_z', id_tsp%id_netnodez(2)) ! TODO: AvD: keep this here as long as ug itself does not WRITE the zk data.

            if (allocated(zf)) then
               ierr = ug_def_var(ncid, id_zf, (/id_tsp%meshids2d%dimids(mdim_face)/), nf90_double, UG_LOC_FACE, &
                                 mesh2dname, 'face_z', face_z_stdname, 'z-coordinate of mesh faces', 'm', '', '', crs, dfill=dmiss)
            end if

            ierr = nf90_enddef(ncid)
            ierr = nf90_put_var(ncid, id_tsp%id_netnodez(2), zn)
            if (allocated(zf)) then
               ierr = nf90_put_var(ncid, id_zf, zf)
            end if
         end if

         ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.

         !define 1d2dcontacts only after mesh2d is completly defined
         if (n1d2dcontacts > 0) then
            ierr = ug_def_mesh_contact(ncid, id_tsp%meshcontacts, trim(contactname), n1d2dcontacts, id_tsp%meshids1d, id_tsp%meshids2d, UG_LOC_NODE, UG_LOC_FACE, start_index)
            ierr = nf90_enddef(ncid)
            ! Put the contacts
            ierr = ug_put_mesh_contact(ncid, id_tsp%meshcontacts, contacts(1, :), contacts(2, :), contacttype)
            ierr = nf90_redef(ncid) ! TODO: AvD: I know that all this redef is slow. Split definition and writing soon.
         end if

         deallocate (xn)
         deallocate (yn)
         deallocate (zn)
         deallocate (face_nodes)
         deallocate (edge_nodes)
         deallocate (edge_type)
      else
         nv = 1
      end if

      call readyy('Writing net data', 0.5d0)

      ! Dimensions
      ierr = nf90_def_dim(ncid, 'nNetNode', numk, id_netnodedim)
      ierr = nf90_def_dim(ncid, 'nNetLink', numl, ids_netelem%id_netlinkdim)
      ierr = nf90_def_dim(ncid, 'nNetElem', nump1d2d, ids_netelem%id_netelemdim)

      if (janetcell_ /= 0 .and. nump1d2d > 0) then
         ierr = nf90_def_dim(ncid, 'nNetElemMaxNode', nv, ids_netelem%id_netelemmaxnodedim)
         ierr = nf90_def_dim(ncid, 'n'//trim(mesh2dname)//'_NetLinkContourPts', 4, ids_netelem%id_netlinkcontourptsdim) ! Momentum control volume a la Perot: rectangle around xu/yu
         ierr = unc_def_net_elem(ncid, ids_netelem, trim(mesh2dname)//'_')
      else if (jaidomain_ /= 0 .or. jaiglobal_s_ /= 0) then
         ierr = nf90_def_dim(ncid, 'nNetElem', nump1d2d, ids_netelem%id_netelemdim)
      end if

      ! Define partitioned model variables:
      ndx2d = nump ! Needed to use helper routine unc_def_var_map below.
      ndxi = nump1d2d
      if (jaidomain_ > 0) then
         ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemdomain(:), nf90_int, UNC_LOC_S, 'netelem_domain', '', 'domain number of netcell', '', 0, cell_method='point', jabndnd=0, ivalid_max=ndomains)
      end if
      if (jaiglobal_s_ > 0) then
         ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemglobalnr(:), nf90_int, UNC_LOC_S, 'netelem_globalnr', '', 'global netcell number', '', 0, cell_method='point', jabndnd=0, ivalid_max=Nglobal_s)
      end if

      ierr = nf90_enddef(ncid)

      ! -- Start data writing (time-independent data) ------------
      if (janetcell_ /= 0 .and. nump1d2d > 0) then
         ierr = unc_write_net_elem(ncid, ids_netelem)
      end if

      ! Write partitioned model variables:
      if (numk1d > 0) then
         if (jaidomain_ > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemdomain(1), idomain1d(1:numk1d))
         end if
         if (jaiglobal_s_ > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemglobalnr(1), iglobal_s1d(1:numk1d))
         end if
      end if

      if (nump > 0) then
         if (jaidomain_ > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemdomain(2), idomain(1:nump))
         end if
         if (jaiglobal_s_ > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemglobalnr(2), iglobal_s(1:nump))
         end if
      end if
      call readyy('Writing net data', 0.9d0)

      ! TODO: AvD:
      ! * in WAVE: handle the obsolete 'nFlowElemWithBnd'/'nFlowElem' difference
      ! * for WAVE: add FlowElem_zcc back in com file.
      ! * for parallel: add 'FlowElemDomain', 'FlowLinkDomain', 'FlowElemGlobalNr'

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 1) then
         ierr = nf90_redef(ncid)
      end if

      !call readyy('Writing flow geometry data',-1d0)
      call readyy('Writing net data', -1d0)
      return

888   continue
      ! Possible error.

   end subroutine unc_write_net_ugrid2

!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
   subroutine unc_read_net_ugrid(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
      use network_data
      use m_save_ugrid_state
      use io_netcdf
      use odugrid
      use netcdf
      use netcdf_utils, only: ncu_get_att
      use m_sferic
      use m_missing
      use unstruc_messages
      use MessageHandling
      use dfm_error
      use m_alloc
      use gridoperations
      use m_partitioninfo, only: jampi, my_rank

      use unstruc_channel_flow
      use m_cross_helper
      use m_1d_networkreader
      use m_flow1d_reader
      use m_profiles
      use fm_location_types

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(inout) :: numk_keep !< Number of netnodes to keep in existing net (0 to replace all).
      integer, intent(inout) :: numl_keep !< Number of netlinks to keep in existing net (0 to replace all).
      integer, intent(out) :: numk_read !< Number of new netnodes read from file.
      integer, intent(out) :: numl_read !< Number of new netlinks read from file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)

      integer :: ioncid, iconvtype, start_index, networkIndex
      integer :: im, nmesh, i, L, numk_last, numl_last
      integer :: ncid, id_netnodez
      integer, allocatable :: kn12(:, :), kn3(:) ! Placeholder arrays for the edge_nodes and edge_types
      double precision :: convversion, zk_fillvalue, altsign
      type(t_ug_meshgeom) :: meshgeom

      ! 1d2d links
      integer :: ncontacts, ncontactmeshes, koffset1dmesh
      integer, allocatable :: mesh1indexes(:), mesh2indexes(:), contacttype(:)
      character(len=80), allocatable :: contactslongnames(:)
      logical :: includeArrays
      logical :: do_edgelengths, need_edgelengths
      double precision, allocatable :: xface(:), yface(:)
      integer :: nodesOnBranchVertices
      character(len=:), allocatable :: tmpstring
      integer :: n1, n2, ibr_n1, ibr_n2, ibr
      double precision :: off1, off2
      integer :: numerr, threshold_abort_current

      numk_read = 0
      numl_read = 0
      start_index = 1
      numk_last = numk_keep
      numl_last = numl_keep
      includeArrays = .true.
      do_edgelengths = .false.
      networkIndex = 0
      koffset1dmesh = 0
      numerr = 0

      allocate (character(len=0) :: tmpstring)
      ierr = ionc_open(filename, NF90_NOWRITE, ioncid, iconvtype, convversion)

      if (ierr /= ionc_noerr .or. iconvtype /= IONC_CONV_UGRID .or. convversion < 1.0) then ! NOTE: no check on conventions version number (yet?)
         ! No valid UGRID, not a problem, call site will fall back to trying old format.
         call mess(LEVEL_DEBUG, 'unc_read_net_ugrid: net file '''//trim(filename)//''' is not UGRID. No problem, will fall back to old format reader.')
         ierr = DFM_EFILEFORMAT
         goto 999
      end if

      if (numk_keep == 0 .and. numl_keep == 0) then
         ! This is to allow more than one call to loadNetwork/unc_read_net_ugrid. Remove any previously read network state.
         call default_save_ugrid_state()
         call dealloc(network)
         network%loaded = .false.
         network%initialized = .false.
      else
         continue ! TODO: I don't think we support reading and appending a new 1D ugrid network to the currently loaded one.
      end if

      ! Old convention, with overlapping points
      if (allocated(mesh1dNodeIds)) then
         deallocate (mesh1dNodeIds)
      end if
      if (allocated(mesh1dUnmergedToMerged)) then
         deallocate (mesh1dUnmergedToMerged)
      end if
      !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)

      ! UNST-2510: Based on _net.nc version either read with or without duplicatie points on connection nodes.
      nodesOnBranchVertices = 1
      ierr = ionc_get_ncid(ioncid, ncid)
      tmpstring = ''
      ierr = ncu_get_att(ncid, nf90_global, 'Conventions', tmpstring)
      if (ierr == NF90_ENOTATT) then
         nodesOnBranchVertices = 0 ! New format.
         call mess(LEVEL_DEBUG, 'No NetCDF Conventions found. Defaulting to current format (>= "CF-1.8 UGRID-1.0 Deltares-0.10") for '''//trim(filename)//'''.')
      elseif (ierr == nf90_noerr) then
         i = index(tmpstring, 'Deltares')
         if (i > 0 .and. tmpstring(i + 9:i + 11) == '0.8') then
            nodesOnBranchVertices = 1 ! Old format
            call mess(LEVEL_DEBUG, 'Detected old format for 1D ("'//trim(tmpstring)//'") in '''//trim(filename)//'''.')
         else
            nodesOnBranchVertices = 0 ! New format
            call mess(LEVEL_DEBUG, 'Detected new format for 1D ("'//trim(tmpstring)//'") in '''//trim(filename)//'''.')
         end if
      end if
      deallocate (tmpstring)

      ! Construct network with with old files (nodesOnBranchVertices). In this function 1d edge nodes (kn array) are also set
      if (nodesOnBranchVertices == 1) then
         ierr = read_1d_mesh_convention_one(ioncid, numk_keep, numl_keep, numk_last, numl_last)
         ! TODO: AvD: do we need the two lines below here?
         numk_read = numk_last
         numl_read = numl_last
      end if

      ierr = ionc_get_coordinate_reference_system(ioncid, crs)
      ! ierr = ionc_get_crs(ioncid, crs) ! TODO: make this API routine.
      ! TODO: also get the %crs item from the ionc dataset, store it in unstruc, AND, use that one in unc_write_flowgeom_ugrid.
      if (ierr /= ionc_noerr) then
         call mess(LEVEL_WARN, 'ionc_get_coordinate_system: No epsg_code found in UGRID net file '''//trim(filename)//'''.')
         goto 999
      end if
      select case (crs%epsg_code)
      case (4326) ! WGS84
         jsferic = 1
      case default
         jsferic = 0
         jasfer3D = 0
      end select

      !
      ! Prepare for multiple (partial) meshes
      !
      ierr = ionc_get_mesh_count(ioncid, nmesh)
      if (ierr /= ionc_noerr) then
         call mess(LEVEL_WARN, 'unc_read_net_ugrid: No grids found in UGRID net file '''//trim(filename)//'''.')
         goto 999
      end if

      !------------------------------------------------------------!
      ! meshes
      !------------------------------------------------------------!
      do im = 1, nmesh

         ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom)

         if (meshgeom%dim == 1 .and. networkIndex > 0) then
            !Save meshgeom for later writing of the 1d network names
            ! Retrieve the 1d geometry twice one time in meshgeom to process in this subroutine, one time in meshgeom1d, that can be used later during initialisation.
            ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, &
                                     nodeids, nodelongnames, network1dname, mesh1dname)
            if (ierr /= IONC_NOERR) then
               ! mesh1d could not be read (possibly network topology missing in file)
               write (msgbuf, '(a,a,a,i0,a,i0,a)') 'unc_read_net_ugrid: Could not read 1D mesh from file ''', trim(filename), &
                  ''', for mesh #', im, ', error code: ', ierr, '.'
               call warn_flush()
               goto 999
            end if

            ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom1d, start_index, includeArrays, nbranchids, nbranchlongnames, nnodeids, nnodelongnames, &
                                     nodeids, nodelongnames, network1dname, mesh1dname)
            mesh1dname = meshgeom1d%meshname
            if (nodesOnBranchVertices == 1) then
               !1d edge nodes (kn array) set above
               cycle
            end if
         elseif (meshgeom%dim == 1 .and. networkIndex <= 0) then
            ! 1D mesh without network topology (i.e., direct xk/yk)
            if (meshgeom%numnode < 0) then
               cycle
            end if
            ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays)
            mesh1dname = meshgeom%meshname
         elseif (meshgeom%dim == 2) then
            !Else 2d/3d mesh
            if (meshgeom%numnode < 0 .or. meshgeom%numface < 0) then
               cycle
            end if
            ierr = ionc_get_meshgeom(ioncid, im, networkIndex, meshgeom, start_index, includeArrays)
            mesh2dname = meshgeom%meshname
            !Variable to store the coordinates of face centres
            allocate (xface(meshgeom%numface)) ! TODO: LC: this is only used when there are mesh contacts. Also: have these not already been read into meshgeom%facex/y?
            allocate (yface(meshgeom%numface))
            ierr = ionc_get_face_coordinates(ioncid, im, xface, yface)
            call read_mesh2d_face_z(ioncid, im, meshgeom%numface)
         else
            ! Only support 1D network and 2D grid
            write (msgbuf, '(a,i0,a,i0,a)') 'unc_read_net_ugrid: unsupported topology dimension ', meshgeom%dim, &
               ' in file '''//trim(filename)//' for mesh #', im, '.'
            call warn_flush()
            cycle
         end if

         do_edgelengths = (meshgeom%dim == 1 .and. networkIndex > 0)
         need_edgelengths = allocated(dxe) .or. do_edgelengths ! Either from a previous meshgeom, or now for the first time.

         !increasenetw
         call increasenetw(numk_last + meshgeom%numnode, numl_last + meshgeom%numedge, also_dxe=need_edgelengths) ! increases XK, YK, KN, optionally dxe
         if (meshgeom%dim == 2 .or. networkIndex <= 0) then
            ! 2D, or 1D without network topology
            ierr = ionc_get_node_coordinates(ioncid, im, XK(numk_last + 1:numk_last + meshgeom%numnode), YK(numk_last + 1:numk_last + meshgeom%numnode)) ! TODO: LC: this duplicates with the above get_meshgeom with includearrays=.true.
         else if (networkIndex > 0) then
            ! 1d part
            koffset1dmesh = numk_last
            ierr = odu_get_xy_coordinates(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%ngeopointx, meshgeom%ngeopointy, &
                                          meshgeom%nbranchgeometrynodes, meshgeom%nbranchlengths, jsferic, meshgeom%nodeX, meshgeom%nodeY)
            XK(numk_last + 1:numk_last + meshgeom%numnode) = meshgeom%nodeX
            YK(numk_last + 1:numk_last + meshgeom%numnode) = meshgeom%nodeY
            network%numk = meshgeom%numnode
            ! construct network and administrate

            ! continue on errors during this subroutine
            threshold_abort_current = threshold_abort
            threshold_abort = LEVEL_FATAL

            ierr = construct_network_from_meshgeom(network, meshgeom, nbranchids, nbranchlongnames, nnodeids, &
                                                   nnodelongnames, nodeids, nodelongnames, network1dname, mesh1dname, nodesOnBranchVertices, jampi, my_rank)

            ! check if any errors have occurred, if so raise a fatal error
            if (getMaxErrorLevel() == LEVEL_ERROR) then
               call SetMessage(LEVEL_FATAL, 'Errors have occurred during the initialisation. Check the previous messages')
            end if
            threshold_abort = threshold_abort_current

            ! get the edge nodes, usually not available (needs to be generated)

            if (meshgeom%numedge == -1) then
               ierr = ggeo_count_or_create_edge_nodes(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%nedge_nodes(1, :), meshgeom%nedge_nodes(2, :), meshgeom%nbranchlengths, start_index, meshgeom%numedge)
               call reallocP(meshgeom%edge_nodes, (/2, meshgeom%numedge/), keepExisting=.false.)
               meshgeom%edge_nodes = 0
               ierr = ggeo_count_or_create_edge_nodes(meshgeom%nodebranchidx, meshgeom%nodeoffsets, meshgeom%nedge_nodes(1, :), meshgeom%nedge_nodes(2, :), meshgeom%nbranchlengths, start_index, meshgeom%numedge, meshgeom%edge_nodes)
            end if
            network%numl = meshgeom%numedge
         end if

         if (ierr /= ionc_noerr) then
            write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not read x/y node coordinates from mesh #', im, ' in UGRID net file '''//trim(filename)//'''.'
            call warn_flush()
            goto 999
         end if

         ierr = ionc_get_ncid(ioncid, ncid)
         if (ierr /= ionc_noerr) then
            write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not get direct access to UGRID NetCDF net file '''//trim(filename)//'''.'
            call warn_flush()
            goto 999
         end if

         ! zk values on nodes
         ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_NODE, 'sea_floor_depth_below_geoid', id_netnodez)
         if (ierr == ionc_noerr) then
            altsign = -1d0 ! altitude as depths
         else
            ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_NODE, 'altitude', id_netnodez)
            if (ierr == ionc_noerr) then
               altsign = 1d0 ! altitude as altitudes
            else
               ! NOTE: AvD: As long as there's no proper standard_name, try some possible variable names for reading in net node z values:
               altsign = 1d0 ! altitude as altitudes
               ierr = ionc_inq_varid(ioncid, im, 'NetNode_z', id_netnodez)
               if (ierr /= ionc_noerr) then
                  ierr = ionc_inq_varid(ioncid, im, 'node_z', id_netnodez)
               end if
            end if
         end if

         if (ierr == nf90_noerr) then
            ierr = nf90_get_var(ncid, id_netnodez, ZK(numk_last + 1:numk_last + meshgeom%numnode))
            call check_error(ierr, 'z values')
            if (ierr == nf90_noerr) then
               ZK(numk_last + 1:numk_last + meshgeom%numnode) = altsign * ZK(numk_last + 1:numk_last + meshgeom%numnode)
            end if

            ! Replace the missing/fill values read from file by the kernel's dmiss missing value.
            ierr = nf90_get_att(ncid, id_netnodez, '_FillValue', zk_fillvalue)
            ! TODO: LC: should we not check for nf90 constant default double-fill-value here as a fallback?
            if (ierr == nf90_noerr) then
               if (zk_fillvalue /= dmiss) then
                  where (ZK(numk_last + 1:numk_last + meshgeom%numnode) == altsign * zk_fillvalue) ZK(numk_last + 1:numk_last + meshgeom%numnode) = dmiss
               end if
            else
               ierr = ionc_noerr
            end if
         else
            ZK(numk_last + 1:numk_last + meshgeom%numnode) = dmiss
         end if

         !
         ! 3. Net links. Just append the edges from the mesh(es) as netlinks, later setnodadm() at call site will group them by 1D and 2D.
         !
         if (allocated(kn12)) then
            deallocate (kn12)
         end if
         allocate (kn12(2, meshgeom%numedge))

         if (allocated(kn3)) then
            deallocate (kn3)
         end if
         allocate (kn3(meshgeom%numedge))

         if (meshgeom%dim /= 1) then
            ! TODO: LC: these have already been read into meshgeom%edge_nodes, so maybe just copy it here?
            ierr = ionc_get_edge_nodes(ioncid, im, kn12, 1) !unstruct requires 1 based indexes
         else
            kn12 = meshgeom%edge_nodes
            ierr = ionc_noerr
         end if

         if (ierr /= ionc_noerr) then
            write (msgbuf, '(a,i0,a)') 'unc_read_net_ugrid: Could not read edge-node connectivity from mesh #', im, ' in UGRID net file '''//trim(filename)//'''.'
            call warn_flush()
            goto 999
         end if

         ! TODO: AvD: replace by read-in edge_type
         ! NOTE: AvD: even meshgeom%dim is not entirely suitable, because if a net file was saved without cell info, then we currently write topology_dimension=1, whereas we actually intend to have kn(3,:)=2.
         kn3(:) = meshgeom%dim ! was 2, Needs to be read from file at some point

         ! Backwards compatibility
         !ierr = nf90_inq_varid(ncid, 'NetLinkType', id_netlinktype)
         !if (ierr == nf90_noerr) then
         !   ierr = nf90_get_var(ncid, id_netlinktype, kn3, count = (/ meshgeom%numedge /))
         !endif

         ! ierr = ionc_inq_varid(ioncid, im, 'kn3', iv)
         !ierr = ionc_inq_varid(ioncid, im, 'kn3', iv)
         !ierr = nf90_get_var(..., iv, kn3, count=meshgeom%numedge)

         do L = 1, meshgeom%numedge
            ! Append the netlink table, and also increment netnode numbers in netlink array to ensure unique ids.
            kn(1:2, numl_last + L) = numk_last + kn12(:, L)
            kn(3, numl_last + L) = kn3(L)

            ! Determine edge (==netlink) lengths, IF present in the file.
            if (do_edgelengths) then
               ibr = meshgeom%edgebranchidx(L)
               if (ibr <= 0 .or. ibr > meshgeom%nbranches) then
                  cycle
               end if
               n1 = meshgeom%edge_nodes(1, L)
               n2 = meshgeom%edge_nodes(2, L)
               if (n1 <= 0 .or. n1 > meshgeom%numnode) then
                  write (msgbuf, '(a,a,a,a,a,i0,a,i0,a)') 'Error while reading ''', trim(filename), ''', mesh ''', trim(meshgeom%meshname), &
                     ''' : edge_node table for edge #', L, ' contains invalid node nr ', n1, '.'
                  call warn_flush()
                  ierr = DFM_WRONGINPUT
                  goto 999
               end if
               if (n2 <= 0 .or. n2 > meshgeom%numnode) then
                  write (msgbuf, '(a,a,a,a,a,i0,a,i0,a)') 'Error while reading ''', trim(filename), ''', mesh ''', trim(meshgeom%meshname), &
                     ''' : edge_node table for edge #', L, ' contains invalid node nr ', n2, '.'
                  call warn_flush()
                  ierr = DFM_WRONGINPUT
                  goto 999
               end if

               ibr_n1 = meshgeom%nodebranchidx(n1)
               ibr_n2 = meshgeom%nodebranchidx(n2)
               if (ibr_n1 /= ibr .and. ibr_n2 /= ibr) then
                  off1 = 0d0 ! Start of branch
                  off2 = meshgeom%nbranchlengths(ibr) ! End of branch
               else if (ibr_n1 == ibr .and. ibr_n2 /= ibr) then
                  off1 = meshgeom%nodeoffsets(n1)
                  if (meshgeom%nodeoffsets(n1) < meshgeom%edgeoffsets(L)) then
                     off2 = meshgeom%nbranchlengths(ibr) ! End of branch
                  else
                     off2 = 0d0 ! Start of branch
                  end if
               else if (ibr_n1 /= ibr .and. ibr_n2 == ibr) then
                  off2 = meshgeom%nodeoffsets(n2)
                  if (meshgeom%nodeoffsets(n2) < meshgeom%edgeoffsets(L)) then
                     off1 = meshgeom%nbranchlengths(ibr) ! End of branch
                  else
                     off1 = 0d0 ! Start of branch
                  end if
               else if (ibr_n1 == ibr .and. ibr_n2 == ibr) then
                  off1 = meshgeom%nodeoffsets(n1)
                  off2 = meshgeom%nodeoffsets(n2)
               end if
               dxe(numl_last + L) = abs(off2 - off1)
            end if
         end do

         numk_read = numk_read + meshgeom%numnode
         numk_last = numk_last + meshgeom%numnode

         numl_read = numl_read + meshgeom%numedge
         numl_last = numl_last + meshgeom%numedge

      end do

      !------------------------------------------------------------!
      ! 1d2d contacts
      !------------------------------------------------------------!
      ierr = ionc_get_contact_topo_count(ioncid, ncontactmeshes)
      if (ierr /= ionc_noerr) then
         call mess(LEVEL_WARN, 'ug_get_contact_topo_count: No mesh contacts found in UGRID net file '''//trim(filename)//'''.')
         goto 999
      end if

      contactnlinks = 0
      do im = 1, ncontactmeshes

         ierr = ionc_get_contacts_count_ugrid(ioncid, im, ncontacts)

         call realloc(mesh1indexes, ncontacts, keepExisting=.false.)
         call realloc(mesh2indexes, ncontacts, keepExisting=.false.)
         call realloc(contactslongnames, ncontacts, keepExisting=.false.)
         call realloc(contacttype, ncontacts, keepExisting=.false.)

         call realloc(hashlist_contactids%id_list, contactnlinks + ncontacts, keepExisting=.true.) ! Remember contactids for later use.
         call realloc(contactnetlinks, contactnlinks + ncontacts, keepExisting=.true.) ! Remember contact netlink numbers for later use.
         call realloc(contact1d2didx, (/2, contactnlinks + ncontacts/), keepExisting=.true.) ! Remember contact connectivity table for later use.

         ierr = ionc_get_mesh_contact_ugrid(ioncid, im, mesh1indexes, mesh2indexes, hashlist_contactids%id_list(contactnlinks + 1:contactnlinks + ncontacts), contactslongnames, contacttype, 1)
         hashlist_contactids%id_count = contactnlinks + ncontacts

         ierr = ionc_get_contact_name(ioncid, im, contactname)

         numerr = 0
         call increasenetw(numk_last + ncontacts, numl_last + ncontacts)
         do l = 1, ncontacts
            if (contacttype(L) < 3) then
               numerr = numerr + 1
               if (numerr <= maxerrprint) then
                  write (msgbuf, '(a,a,a,i0,a,i0,a)') 'Error while reading net file ''', trim(filename), ''', contact type of link ', &
                     L, ' is not valid: ', contacttype(L), '. Should be >= 3.'
                  call warn_flush()
               elseif (numerr == maxerrprint + 1) then
                  call mess(LEVEL_WARN, 'Skipping more errors of this type...')
               end if

               cycle
            end if

            XK(numk_last + l) = xface(mesh2indexes(l))
            YK(numk_last + l) = yface(mesh2indexes(l))

            contact1d2didx(1, contactnlinks + L) = mesh1indexes(L)
            contact1d2didx(2, contactnlinks + L) = mesh2indexes(L)
            if (nodesOnBranchVertices == 1) then
               kn(1, numl_last + l) = mesh1dUnmergedToMerged(mesh1indexes(l))
            else
               kn(1, numl_last + l) = mesh1indexes(l)
            end if
            kn(2, numl_last + l) = numk_last + l
            kn(3, numl_last + l) = contacttype(l)
            contactnetlinks(contactnlinks + L) = numl_last + L
         end do

         if (numerr > 0) then
            write (msgbuf, '(a,a,a,i0,a)') 'Error(s) while reading net file ''', trim(filename), ''', ', numerr, &
               ' 1D2D links contained errors. See previous warnings.'
            call err_flush()
         end if

         ! Set the ZK to dmiss
         ZK(numk_last + 1:numk_last + ncontacts) = dmiss

         numk_read = numk_read + ncontacts
         numk_last = numk_last + ncontacts

         numl_read = numl_read + ncontacts
         numl_last = numl_last + ncontacts

         contactnlinks = contactnlinks + ncontacts
      end do

      call hashfill(hashlist_contactids)

      ! Success
888   continue
      ierr = ionc_close(ioncid)
      ierr = dfm_noerr
      return

999   continue
      ! Some error occurred (error code previously set)
      ! Try to close+cleanup the data set anyway.
      i = ionc_close(ioncid) ! Don't overwrite actual ierr.

   end subroutine unc_read_net_ugrid

!> read mesh2d_face_z and store result in module variable blcell.
!! Also keep standard name used in face_z_stdname
   subroutine read_mesh2d_face_z(ioncid, im, numface)
      use io_netcdf
      use netcdf
      use m_alloc
      integer, intent(in) :: ioncid, im, numface

      integer :: i, ierr, id_netnodef, ncid
      character(len=*), parameter :: std_names(2) = (/'sea_floor_depth_below_geoid', 'altitude                   '/)

      ierr = huge(ierr)
      do i = 1, size(std_names)
         ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_FACE, std_names(i), id_netnodef)
         face_z_stdname = trim(std_names(i))
         if (ierr == nf90_noerr) exit
      end do
      if (ierr == nf90_noerr) then
         call realloc(blcell, numface, keepExisting=.false.)
         ierr = ionc_get_ncid(ioncid, ncid)
         ierr = nf90_get_var(ncid, id_netnodef, blcell)
         ! TODO: for future use inside setbedlevelfromnetfile(),
         !       make sure to store vertical orientation of variable.
         ! TODO: handle _FillValue correctly, and make sure that the mesh writing
         !       stays consistent with the fill/dmiss value that we use here.
      else
         call realloc(blcell, 0)
      end if
   end subroutine read_mesh2d_face_z

!> Reads the net data from a NetCDF file.
!! Processing is done elsewhere.
   subroutine unc_read_net(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
      use network_data
      use m_sferic
      use m_missing
      use dfm_error
      use gridoperations
      use netcdf_utils, only: ncu_get_att, ncu_get_var_attset

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(inout) :: numk_keep !< Number of netnodes to keep in existing net.
      integer, intent(inout) :: numl_keep !< Number of netlinks to keep in existing net.
      integer, intent(out) :: numk_read !< Number of new netnodes read from file.
      integer, intent(out) :: numl_read !< Number of new netlinks read from file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)

      logical :: stringsequalinsens

      character(len=:), allocatable :: coordsyscheck
      integer, dimension(:), allocatable :: kn3read
      integer, dimension(:), allocatable :: kn1read
      integer, dimension(:), allocatable :: kn2read

      integer :: inetfile, &
                 id_netnodedim, id_netlinkdim, & !< Dimensions
                 id_netnodex, id_netnodey, id_netnodez, & ! Node variables
                 id_netlink, id_netlinktype, & !< Link variables
                 id_crsvar

      integer :: L
      double precision :: zk_fillvalue

      call readyy('Reading net data', 0d0)

      call prepare_error('Could not read NetCDF file '''//trim(filename)//'''. Details follow:')

      nerr_ = 0
      allocate (character(len=0) :: coordsyscheck)

      !
      ! Try and read as new UGRID NetCDF format
      !
      call unc_read_net_ugrid(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
      if (ierr == dfm_noerr) then
         ! UGRID successfully read, we're done.
         return
      else
         ! No UGRID, but just try to use the 'old' format now.
         continue
      end if

      ierr = unc_open(filename, nf90_nowrite, inetfile)
      call check_error(ierr, 'file '''//trim(filename)//'''')
      if (nerr_ > 0) return

      ! Get nr of nodes and edges
      ierr = nf90_inq_dimid(inetfile, 'nNetNode', id_netnodedim)
      call check_error(ierr, 'nNetNode')
      ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
      call check_error(ierr, 'nNetLink')
      if (nerr_ > 0) return

      ierr = nf90_inquire_dimension(inetfile, id_netnodedim, len=numk_read)
      call check_error(ierr, 'node count')
      ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl_read)
      call check_error(ierr, 'link count')
      if (nerr_ > 0) return

      call readyy('Reading net data', .05d0)

      ierr = nf90_inq_varid(inetfile, 'projected_coordinate_system', id_crsvar)
      if (ierr /= nf90_noerr) then
         ierr = nf90_inq_varid(inetfile, 'wgs84', id_crsvar)
      end if
      if (ierr == nf90_noerr) then
         ierr = nf90_inquire_variable(inetfile, id_crsvar, name=crs%varname)
         ierr = nf90_get_var(inetfile, id_crsvar, crs%epsg_code)
         if (crs%epsg_code == nf90_fill_int) then
            ierr = nf90_get_att(inetfile, id_crsvar, 'epsg', crs%epsg_code)
            !if (ierr /= nf90_noerr) then
            !   ierr = nf90_get_att(datasets(ioncid)%ncid, id_crsvar, 'epsg_code', tmpstring)
            !   read(tmpstring, '(a,i0)') dummy, datasets(ioncid)%crs%epsg_code
            !endif
         end if
         ierr = ncu_get_var_attset(inetfile, id_crsvar, crs%attset)
      end if

! Prepare net vars for new data and fill with values from file
      call increasenetw(numk_keep + numk_read, numl_keep + numl_read)
      call readyy('Reading net data', .1d0)

      ierr = nf90_inq_varid(inetfile, 'NetNode_x', id_netnodex)
      call check_error(ierr, 'x coordinates')

      ierr = nf90_inq_varid(inetfile, 'NetNode_y', id_netnodey)
      call check_error(ierr, 'y coordinates')

      ierr = nf90_inq_varid(inetfile, 'NetLink', id_netlink)
      call check_error(ierr, 'netlinks')
      ierr = nf90_inq_varid(inetfile, 'NetLinkType', id_netlinktype)
      call check_error(ierr, 'netlinktypes')
      if (nerr_ > 0) return

      ierr = nf90_get_var(inetfile, id_netnodex, XK(numk_keep + 1:numk_keep + numk_read))
      call check_error(ierr, 'x values')
      call readyy('Reading net data', .3d0)
      ierr = nf90_get_var(inetfile, id_netnodey, YK(numk_keep + 1:numk_keep + numk_read))
      call check_error(ierr, 'y values')
      call readyy('Reading net data', .5d0)

      ierr = nf90_inq_varid(inetfile, 'NetNode_z', id_netnodez)
      if (ierr == nf90_noerr) then
         ierr = nf90_get_var(inetfile, id_netnodez, ZK(numk_keep + 1:numk_keep + numk_read))
         call check_error(ierr, 'z values')

         ierr = nf90_get_att(inetfile, id_netnodez, '_FillValue', zk_fillvalue)
         if (ierr == nf90_noerr) then
            if (zk_fillvalue /= dmiss) then
               where (ZK(numk_keep + 1:numk_keep + numk_read) == zk_fillvalue) ZK(numk_keep + 1:numk_keep + numk_read) = dmiss
            end if
         else
            ierr = nf90_noerr
         end if
      else
         ZK(numk_keep + 1:numk_keep + numk_read) = dmiss
      end if
      call readyy('Reading net data', .7d0)

      coordsyscheck = ''
      ierr = ncu_get_att(inetfile, id_netnodex, 'standard_name', coordsyscheck)
      if (stringsequalinsens(coordsyscheck, 'longitude')) then
         jsferic = 1
      else
         jsferic = 0
         jasfer3D = 0
      end if
      if (jsferic == 1) then
         crs%epsg_code = 4326
      end if
      deallocate (coordsyscheck)

      ! An array slice cannot be passed to netcdf C-library (risk of stack overflow), so use placeholder.
      allocate (kn3read(numl_read))
      allocate (kn2read(numl_read))
      allocate (kn1read(numl_read))

!    ierr = nf90_get_var(inetfile, id_netlink,     kn(:,numl_keep+1:numl_keep+numl_read), count = (/ 2, numl_read /), map=(/ 1, 3 /))
      ierr = nf90_get_var(inetfile, id_netlink, kn1read, count=(/1, numl_read/))
      ierr = nf90_get_var(inetfile, id_netlink, kn2read, count=(/1, numl_read/), start=(/2, 1/))
      call check_error(ierr, 'netlink nodes')
      do L = numL_keep + 1, numL_keep + numL_read
         kn(1, L) = kn1read(L - numL_keep)
         kn(2, L) = kn2read(L - numL_keep)
      end do

      ierr = nf90_get_var(inetfile, id_netlinktype, kn3read, count=(/numl_read/))
      call check_error(ierr, 'netlink type')

      kn(3, numl_keep + 1:numl_keep + numl_read) = kn3read
      ! Repair invalid kn3 codes (e.g. 0, always set to default 2==2D, i.e., don't read in thin dam codes)
      do L = numl_keep + 1, numl_keep + numl_read
         if (kn(3, L) < 1) then
            kn(3, L) = 2
         end if
      end do

      deallocate (kn3read)
      deallocate (kn1read)
      deallocate (kn2read)
      call readyy('Reading net data', .95d0)

      ! Increment netnode numbers in netlink array to ensure unique ids.
      KN(1:2, numl_keep + 1:numl_keep + numl_read) = KN(1:2, numl_keep + 1:numl_keep + numl_read) + numk_keep
      call readyy('Reading net data', 1d0)

      ierr = unc_close(inetfile)
      call readyy('Reading net data', -1d0)

   end subroutine unc_read_net

!> print MD5 checksum for net file, based on netlinks only.
!! Only in case of loglevel is debug or all.
   subroutine md5_net_file(numlstart, numlcount)
      use network_data, only: kn, numl
      use md5_checksum
      use unstruc_messages

      integer, optional, intent(in) :: numlstart !< Start index of links to start checking. Optional, default: 1.
      integer, optional, intent(in) :: numlcount !< Total count of links to check. Optional, default: numl.

      integer :: L
      integer :: ierr
      integer, allocatable :: kn1d(:)
      character(len=md5length) :: checksum
      character(len=2*md5length) :: checksum_hex
      logical :: success
      integer :: numlstart_
      integer :: numlcount_

      if (present(numlstart)) then
         numlstart_ = numlstart
      else
         numlstart_ = 1
      end if

      if (present(numlcount)) then
         numlcount_ = numlcount
      else
         numlcount_ = numl
      end if

      if (loglevel_StdOut <= LEVEL_DEBUG .or. loglevel_file <= LEVEL_DEBUG) then
         allocate (kn1d(3 * numlcount_), stat=ierr)
         if (ierr /= 0) then
            call mess(LEVEL_FATAL, 'memory allocation error in md5_net_file with size = ', 3 * numlcount_)
         end if
         do L = numlstart_, numlstart_ + numlcount_ - 1
            kn1d(3 * (L - numlstart_) + 1) = KN(1, L)
            kn1d(3 * (L - numlstart_) + 2) = KN(2, L)
            kn1d(3 * (L - numlstart_) + 3) = KN(3, L)
         end do
         call md5intarr(kn1d, checksum, success)
         if (success) then
            call checksum2hex(checksum, checksum_hex)
            call mess(LEVEL_DEBUG, 'MD5 checksum KN = '//checksum_hex)
         else
            call mess(LEVEL_DEBUG, 'could not generate MD5 checksum')
         end if
         deallocate (kn1d)
      end if

   end subroutine md5_net_file

!> Assigns the information, that has been read from a restart file and stored in array1, to a 2D array2.
   subroutine assign_restart_data_to_local_array(array1, array2, iloc, kmx, loccount, jamergedmap, iloc_own, write_only_bottom_layer, target_shift)
      use m_get_kbot_ktop

      double precision, allocatable, intent(in) :: array1(:) !< Array that contains information read from a restart file
      double precision, allocatable, intent(inout) :: array2(:, :) !< Target 2D array
      integer, intent(in) :: iloc !< Index of one dimension of the 2D array
      integer, intent(in) :: kmx !< Number of layers
      integer, intent(in) :: loccount !< Spatial count in file to read (e.g. ndxi_own)
      integer, intent(in) :: jamergedmap !< Whether input is from a merged map file (i.e. needs shifting or not) (1/0)
      integer, intent(in) :: iloc_own(:) !< Mapping array from the unique own (i.e. non-ghost) nodes/links to the actual ndxi/lnx numbering. Should be filled from index 1:loccount (e.g. 1:ndxi_own).
      logical, optional, intent(in) :: write_only_bottom_layer !< Whether to only perform the assignment of the 2D bottom layer (e.g. for waq bottom variables)
      integer, optional, intent(in) :: target_shift !< shift of the index where the array is to be written (1:ndx), default = 0
      integer :: i, kloc, k, kb, kt, target_shift_
      integer :: i_target
      logical :: write_only_bottom_layer_

      write_only_bottom_layer_ = .false.
      if (present(write_only_bottom_layer)) then
         write_only_bottom_layer_ = write_only_bottom_layer
      end if

      target_shift_ = 0
      if (present(target_shift)) then
         target_shift_ = target_shift
      end if

      do i = 1, loccount
         i_target = i + target_shift_
         if (jamergedmap == 1) then
            kloc = iloc_own(i_target)
         else
            kloc = i_target
         end if

         call getkbotktop(kloc, kb, kt)
         if (write_only_bottom_layer_) then
            array2(iloc, kb) = array1(kloc)
         else
            do k = kb, kt ! When model is 2D, then there is one iteration since k == kb == kt == kloc
               array2(iloc, k) = array1(k)
            end do
         end if
      end do
   end subroutine assign_restart_data_to_local_array

!> Reads a single array variable from a map file, and optionally,
!! if it was a merged map file from parallel run, reshift the read
!! values to the actual own 1:ndxi / 1:lnx numbering.
!!
!! Details for merged-map file as single restart file for parallel models:
!! In the current parallel model, 1:ndxi contains mainly own nodes, but also several ghost nodes.
!! A merged-map file contains only unique nodes, in long blocks per partition, concatenated in one long
!! domain-global array for each quantity. The nf90_var_get will only read the block for the current rank,
!! and that will yield only 'own' nodes, not ghostnodes. All these values need to be 'spread' into the current
!! s1/u1, etc. arrays, with some empty ghost values in between here and there.
!! The calling routine should later call update_ghosts, such that ghost locations are filled as well.
   function get_var_and_shift(ncid, varname, targetarr, tmparr, loctype, kmx, locstart, loccount, it_read, jamergedmap, iloc_own, iloc_merge, target_shift) result(ierr)
      use dfm_error
      use fm_location_types
      use m_get_kbot_ktop
      
      integer, intent(in) :: ncid !< Open NetCDF data set
      character(len=*), intent(in) :: varname !< Variable name in file.
      double precision, intent(inout) :: targetarr(:) !< Data will be stored in this array.
      double precision, intent(inout) :: tmparr(:) !< Temporary work array where file data will be first read before shifting.
      integer, intent(in) :: loctype !< Loc type (UNC_LOC_S, etc.)
      integer, intent(in) :: kmx !< Number of layers (0 if 2D)
      integer, intent(in) :: locstart !< Spatial index in file where to start reading (e.g. kstart)
      integer, intent(in) :: loccount !< Spatial count in file to read (e.g. ndxi_own)
      integer, intent(in) :: it_read !< Time index in file to read
      integer, intent(in) :: jamergedmap !< Whether input is from a merged map file (i.e. needs shifting or not) (1/0)
      integer, intent(in) :: iloc_own(:) !< Mapping array from the unique own (i.e. non-ghost) nodes/links to the actual ndxi/lnx numbering. Should be filled from index 1:loccount (e.g. 1:ndxi_own).
      integer, intent(in) :: iloc_merge(:) !< Mapping array from the unique own (i.e. non-ghost) nodes/links to the global/merged ndxi/lnx numbering. Should be filled from index 1:loccount (e.g. 1:ndxi_own).
      integer, intent(in), optional :: target_shift !< shift of the index where the array is to be written (1:ndx), default = 0
      integer :: ierr !< Result, DFM_NOERR if successful
      integer :: id_var
      integer :: i, ib, it, is, imap, numDims, d1, d2, nlayb, nrlay
      double precision, allocatable :: tmparray1D(:), tmparray2D(:, :)
      integer, dimension(nf90_max_var_dims) :: rhdims, tmpdims
      integer :: jamerged_dif
      integer :: target_shift_
      integer :: i_target

      ierr = DFM_NOERR

      target_shift_ = 0
      if (present(target_shift)) then
         target_shift_ = target_shift
      end if

      if (size(iloc_merge) == 1 .and. iloc_merge(1) == -999) then
         jamerged_dif = 0 !the partition is the same, iloc_merge does not function
      else
         jamerged_dif = 1
      end if

      ierr = nf90_inq_varid(ncid, varname, id_var)
      if (ierr /= 0) goto 999
      if (kmx == 0 .or. loctype == UNC_LOC_S .or. loctype == UNC_LOC_U) then
         if (jamergedmap /= 1) then
            ierr = nf90_get_var(ncid, id_var, targetarr(target_shift_ + 1:target_shift_ + loccount), start=(/locstart, it_read/), count=(/loccount, 1/))
         else
            if (jamerged_dif == 1) then
               ! Firstly read all the data from the file to tmparray1D, this avoinds calling nf90 subroutine in the loop
               ierr = nf90_inquire_variable(ncid, id_var, ndims=numDims, dimids=rhdims)
               do i = 1, numDims - 1
                  ierr = nf90_inquire_dimension(ncid, rhdims(i), len=tmpdims(i))
                  if (ierr /= nf90_noerr) goto 999
               end do
               if (numDims == 2 .or. (numDims == 1 .and. it_read == 1)) then
                  d1 = tmpdims(1)
                  if (allocated(tmparray1D)) then
                     deallocate (tmparray1D)
                  end if
                  allocate (tmparray1D(d1))
                  ierr = nf90_get_var(ncid, id_var, tmparray1D, start=(/1, it_read/), count=(/d1, 1/))
                  if (ierr /= nf90_noerr) goto 999
               else
                  call mess(LEVEL_WARN, 'get_var_and_shift: rank of the array  '''//trim(varname)//''' can only be 2 for 2D models (time+space).')
                  goto 999
               end if
               ! Then assign the data based on the mapping
               do i = 1, loccount
                  i_target = i + target_shift_
                  imap = iloc_merge(i_target)
                  targetarr(iloc_own(i_target)) = tmparray1D(imap)
               end do
            else
               ierr = nf90_get_var(ncid, id_var, tmparr(1:loccount), start=[locstart, it_read], count=[loccount, 1])
               if (ierr /= nf90_noerr) goto 999
               do i = 1, loccount
                  i_target = i + target_shift_
                  targetarr(iloc_own(i_target)) = tmparr(i)
               end do
            end if
         end if
      else
         ! 3D array, firstly read the whole array from the nc file, and then assign the data.
         ierr = nf90_inquire_variable(ncid, id_var, ndims=numDims, dimids=rhdims)
         do i = 1, numDims - 1
            ierr = nf90_inquire_dimension(ncid, rhdims(i), len=tmpdims(i))
            if (ierr /= nf90_noerr) goto 999
         end do
         if (numDims == 3) then
            d1 = tmpdims(1); d2 = tmpdims(2)
            if (allocated(tmparray2D)) then
               deallocate (tmparray2D)
            end if
            allocate (tmparray2D(d1, d2))
            ierr = nf90_get_var(ncid, id_var, tmparray2D, start=(/1, 1, it_read/), count=(/d1, d2, 1/))
            ! TODO: consider using loccount etc.: ierr = nf90_get_var(ncid, id_var, tmparray2D, start = (/1, locstart, it_read/), count = (/d1, loccount, 1/))

            if (ierr /= nf90_noerr) goto 999
         else
            call mess(LEVEL_WARN, 'get_var_and_shift: rank of the array  '''//trim(varname)//''' can only be 3 for 3D models (time+2D space+layers).')
            goto 999
         end if

         do i = 1, loccount
            i_target = i + target_shift_
            if (jamergedmap /= 1) then
               is = i_target
            else
               if (jamerged_dif == 1) then
                  is = iloc_own(i_target)
                  imap = iloc_merge(i_target)
               else
                  is = iloc_own(i_target)
               end if
            end if

            if (loctype == UNC_LOC_S3D .or. loctype == UNC_LOC_W) then
               call getkbotktop(is, ib, it) ! TODO: AvD: double check whether this original 3D restart reading was working at all with kb, kt! (no kbotktopmax here?? lbotltopmax)
               call getlayerindices(is, nlayb, nrlay)
            else if (loctype == UNC_LOC_U3D .or. loctype == UNC_LOC_WU) then
               call getLbotLtopmax(is, ib, it)
               call getlayerindicesLmax(is, nlayb, nrlay)
               !call getlayerindices(is, nlayb, nrlay)
               ! UNST-976: TODO: does NOT work for links yet. We need some setlbotltop call up in read_map, similar to sethu behavior.
               !if (layertype .ne. 1 .and. jawarn < 100)  then
               !    call mess(LEVEL_WARN, 'get_var_and_shift: reading 3D flow link data from '''//trim(varname)//''' is badly supported for z-layer models.')
               !    jawarn = jawarn + 1
               !endif
               !nlayb = 1
               !nrlay = it-ib+1 ! For now, sigma defaults
               !goto 999
            end if
!         call getLbotLtopmax(LL,Lb,Lt)
            if (loctype == UNC_LOC_WU .or. loctype == UNC_LOC_W) then
               ib = ib - 1
               nrlay = nrlay + 1
            end if

            if (jamerged_dif == 1) then
               targetarr(ib:it) = tmparray2D(nlayb:nlayb + nrlay - 1, imap)
            else
               targetarr(ib:it) = tmparray2D(nlayb:nlayb + nrlay - 1, locstart - 1 + i)
            end if
         end do
      end if

      if (allocated(tmparray1D)) then
         deallocate (tmparray1D)
      end if
      if (allocated(tmparray2D)) then
         deallocate (tmparray2D)
      end if
999   continue

   end function get_var_and_shift

!> Reads the flow data from a map or a rst file.
!! Processing is done elsewhere.
!subroutine unc_read_map(filename, numk_keep, numl_keep, numk_read, numl_read, ierr)
   subroutine unc_read_map_or_rst(filename, ierr)
      use time_module, only: datetimestring_to_seconds, seconds_to_datetimestring
      use m_flow
      use m_flowtimes
      use m_transport, only: NUMCONST, ISALT, ITEMP, ISED1, ISEDN, ITRA1, ITRAN, constituents, itrac2const, const_names, ifrac2const
      use m_fm_wq_processes
      use fm_external_forcings_data, only: numtracers, trnames, ibnd_own, ndxbnd_own
      use m_sediment
      use bedcomposition_module
      use m_flowgeom
      use dfm_error
      use m_partitioninfo
      use m_alloc
      use m_timer
      use m_turbulence
      use m_CrossSections
      use m_save_ugrid_state, only: mesh1dname
      use unstruc_channel_flow, only: network
      use m_GlobalParameters
      use netcdf_utils, only: ncu_get_att
      use m_structures_saved_parameters
      use m_initsedtra, only: initsedtra
      use m_fixedweirs, only: weirdte, nfxwL
      use fm_location_types
      use m_gettaus

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)

      character(len=:), allocatable :: refdat_map !< Date time string read from map file.
      real(kind=hp) :: trefdat_map, trefdat_rst
      character(len=:), allocatable :: convformat

      type(t_unc_merged) :: um !< struct holding all data for ugrid merged map/rst files

      real(fp), dimension(:, :, :), pointer :: msed
      real(fp), dimension(:, :), pointer :: thlyr
      real(fp), dimension(:, :), pointer :: svfrac

      integer :: imapfile, &
                 id_timedim, &
                 id_bnddim, &
                 id_time, &
                 id_timestep, &
                 id_tsalbnd, &
                 id_zsalbnd, &
                 id_ttembnd, &
                 id_ztembnd, &
                 id_tsedbnd, &
                 id_zsedbnd, &
                 id_sedtotdim, &
                 id_sedsusdim, &
                 id_nlyrdim, &
                 id_1dflowlinkdim, &
                 id_msed, &
                 id_mfluff, &
                 id_thlyr, &
                 id_lyrfrac, &
                 id_bodsed, &
                 id_blbnd, id_s0bnd, id_s1bnd, &
                 id_squbnd, id_sqibnd, &
                 id_morft, &
                 id_jmax, id_ncrs, id_flowelemcrsz, id_flowelemcrsn, &
                 id_ucxqbnd, id_ucyqbnd

      integer :: id_tmp
      integer :: layerfrac, layerthk
      integer, allocatable :: id_ttrabnd(:), id_ztrabnd(:)
      integer, allocatable :: id_tsedfracbnd(:), id_zsedfracbnd(:)

      integer :: it_read, nt_read, ndxi_read, lnx_read, L
      integer :: sedtot_read, sedsus_read, nlyr_read
      integer :: kloc, kk, itmp, i, iconst, iwqbot, nm, Lf, j, k
      integer :: iostat
      logical :: mdu_has_date
      integer, allocatable :: maptimes(:)
      logical :: file_exists
      double precision, allocatable :: max_threttim(:)
      double precision, allocatable :: tmpvar(:, :)
      double precision, allocatable :: tmpvar1(:), tmpvar1D(:)
      double precision, allocatable :: tmpvar2(:, :, :)
      double precision, allocatable :: tmp_s1(:), tmp_bl(:), tmp_s0(:)
      double precision, allocatable :: tmp_sqi(:), tmp_squ(:)
      double precision, allocatable :: tmp_ucxq(:), tmp_ucyq(:)
      double precision, allocatable :: rst_bodsed(:, :), rst_mfluff(:, :), rst_thlyr(:, :)
      double precision, allocatable :: rst_msed(:, :, :)
      integer, allocatable :: itmpvar(:)
      real(fp) :: mfracsum, poros, sedthick
      real(fp), dimension(stmpar%lsedtot) :: mfrac
      integer :: numpart, lmerge = 0, lugrid = 0
      integer :: kstart, lstart, kstart_bnd
      integer :: jamergedmap_same_bu
      integer :: tmp_loc
      integer :: numl1d
      logical :: is_wq_bot_3d

      character(len=8) :: numformat
      character(len=2) :: numtrastr, numsedfracstr
      character(len=255) :: tmpstr, tmpstr1

      integer :: jmax, ndx1d, nCrs
      double precision, dimension(:, :), allocatable :: work1d_z, work1d_n

      ierr = DFM_GENERICERROR

      numformat = '(I2.2)'
      call realloc(tmpvar1, 0)
      call realloc(um%inode_own, 0)
      call realloc(um%ilink_own, 0)
      allocate (character(len=0) :: convformat)
      allocate (character(len=0) :: refdat_map)

      call readyy('Reading map data', 0d0)

      call prepare_error('Could not read NetCDF restart file '''//trim(filename)//'''. Details follow:')
      nerr_ = 0
      inquire (file=filename, exist=file_exists)
      if (.not. file_exists) then
         call mess(LEVEL_FATAL, 'The specified file for the restart has not been found. Check your .mdu file.')
         call readyy('Reading map data', -1d0)
         return
      end if
      ierr = unc_open(filename, nf90_nowrite, imapfile)
      call check_error(ierr, 'file '''//trim(filename)//'''')
      if (nerr_ > 0) goto 999

      !-- Sequential model, or parallel? If parallel: merged-map file, or separate partition-map files?
      ! First check whether the restart NetCDF file contains a fully merged model, or is just a partition.
      um%jamergedmap = 0
      um%jamergedmap_same = 1

      ! do not support a rst file of UGRID format
      convformat = ''
      ierr = ncu_get_att(imapfile, nf90_global, 'Conventions', convformat)
      lugrid = index(convformat, 'UGRID-1')
      deallocate (convformat)
      if (lugrid > 0) then
         call mess(LEVEL_ERROR, 'The specified restart file is of UGRID format, which is not supported.')
         call readyy('Reading map data', -1d0)
         go to 999
      end if

      lmerge = index(filename, '_merged')
      if (lmerge > 0) then
         um%jamergedmap = 1
      end if

      if (jampi == 1) then
         ierr = nf90_get_att(imapfile, nf90_global, 'NumPartitionsInFile', numpart)
         if (ierr /= nf90_noerr .and. um%jamergedmap == 1) then
            call mess(LEVEL_ERROR, 'The merged restart file is not correct.')
            call readyy('Reading map data', -1d0)
            go to 999
         end if
         if (ierr == nf90_noerr) then
            if (numpart /= numranks) then
               write (msgbuf, '(a,i0,a,a,a,i0,a)') 'Partitions are different in model (', numranks, ') and in restart file `', &
                  trim(filename), ''' (', numpart, ').'
               call warn_flush() ! Error handled on call site.
               um%jamergedmap_same = 0
            end if
            if (um%jamergedmap_same == 1) then ! If rst file is a merged file, and the partitions do not change
               nerr_ = 0

               ierr = nf90_inq_varid(imapfile, 'partitions_face_count', id_tmp)
               call check_error(ierr, 'inquiring partitions_face_count')
               ierr = nf90_get_var(imapfile, id_tmp, ndxi_read, start=(/my_rank + 1/))
               call check_error(ierr, 'getting partitions_face_count')

               ierr = nf90_inq_varid(imapfile, 'partitions_edge_count', id_tmp)
               call check_error(ierr, 'inquiring partitions_edge_count')
               ierr = nf90_get_var(imapfile, id_tmp, lnx_read, start=(/my_rank + 1/))
               call check_error(ierr, 'getting partitions_edge_count')

               ierr = nf90_inq_varid(imapfile, 'partitions_face_start', id_tmp)
               call check_error(ierr, 'getting partitions_face_start')
               ierr = nf90_get_var(imapfile, id_tmp, kstart, start=(/my_rank + 1/))
               call check_error(ierr, 'getting partitions_face_start')

               ierr = nf90_inq_varid(imapfile, 'partitions_edge_start', id_tmp)
               call check_error(ierr, 'getting partitions_edge_start')
               ierr = nf90_get_var(imapfile, id_tmp, lstart, start=(/my_rank + 1/))
               call check_error(ierr, 'getting partitions_edge_start')

               ! Ask file for the dimension of its own boundary points
               ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', id_bnddim)
               if (ierr == 0) then
                  ierr = nf90_inq_varid(imapfile, 'partitions_facebnd_start', id_tmp)
                  call check_error(ierr, 'getting partitions_facebnd_start')
                  ierr = nf90_get_var(imapfile, id_tmp, kstart_bnd, start=(/my_rank + 1/))
                  call check_error(ierr, 'getting partitions_facebnd_start')

                  ierr = nf90_inq_varid(imapfile, 'partitions_facebnd_count', id_tmp)
                  call check_error(ierr, 'getting partitions_facebnd_count')
                  ierr = nf90_get_var(imapfile, id_tmp, um%nbnd_read, start=(/my_rank + 1/))
                  call check_error(ierr, 'getting partitions_facebnd_count')

                  jaoldrstfile = 0
               else
                  call mess(LEVEL_INFO, 'The restart file does not contain info. on boundaries.')
                  ierr = 0
                  um%nbnd_read = 0
                  jaoldrstfile = 1
               end if

               if (nerr_ > 0) then
                  write (msgbuf, '(a,a,a)') 'Could not read partition start/count info from file `', trim(filename), '''.'
                  call warn_flush() ! Error handled on call site.
                  ierr = DFM_WRONGINPUT
                  goto 999
               end if
            end if
         else ! No merged-map file: no problem, we'll assume that each rank got its own unique restart file, &
            ! so just read data from start.
            kstart = 1
            lstart = 1
            kstart_bnd = 1
         end if
      else ! Sequential model: just read all data from restart file.
         kstart = 1
         lstart = 1
         kstart_bnd = 1
      end if
      if (jampi == 0 .and. um%jamergedmap == 1) then
         call mess(LEVEL_INFO, 'Restart a sequential run with a merged rst file.')
         um%jamergedmap_same = 0
      end if

      if (jampi > 0 .and. kmx > 0 .and. um%jamergedmap == 1) then
         ! In the case of 3D parallel restarting with a merged rst file,
         ! some variables on ghost cells are not communicated but filled using kd-tree
         um%jafillghost = 1
      else
         um%jafillghost = 0
      end if

      ! allocate inode_merge, ilink_merge and ibnd_merge in all restart situations. When the partition is the same, these two arrays do not function,
      ! but they help to simplify the codes when calling "get_var_and_shift".
      call realloc(um%inode_merge, 1, keepExisting=.false., fill=-999)
      call realloc(um%ilink_merge, 1, keepExisting=.false., fill=-999)
      call realloc(um%ibnd_merge, 1, keepExisting=.false., fill=-999)

      jamergedmap_same_bu = um%jamergedmap_same
      success = unc_read_merged_map(um, imapfile, filename, ierr)
      if (jamergedmap_same_bu /= um%jamergedmap_same) then
         ! number of partitions is the same, but the partition itself differs
         success = unc_read_merged_map(um, imapfile, filename, ierr)
      end if
      if (.not. success) goto 999
      if (ierr /= 0) return

      call readyy('Reading map data', 0.05d0)

      ! Choose latest timestep
      ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
      call check_error(ierr, 'time')
      ierr = nf90_inquire_dimension(imapfile, id_timedim, len=nt_read)
      call check_error(ierr, 'time')
      if (nt_read == 0) then
         call qnerror('There do not exist any time data in file ', trim(filename), ' ')
         call readyy('Reading map data', -1d0)
         return
      end if

      call readyy('Reading map data', 0.10d0)

      iostat = 0
      call datetimestring_to_seconds(restartdatetime(1:14), refdat, trefdat_rst, iostat) ! result: refdatnew in seconds  w.r.t. absolute MDU refdat
      mdu_has_date = (iostat == 0)

      if (nt_read == 1 .and. .not. mdu_has_date) then
         ! Restart from rst or map file, no particular filename,
         ! only a single time snapshot in file: directly use it.
         it_read = 1
      else
         ! Restart from *_map.nc and select time snapshot based on MDU RestartDateTime value.
         if (.not. mdu_has_date) then
            call mess(LEVEL_WARN, 'Missing RestartDateTime in MDU file. Will not read from map file '''//trim(filename)//'''.')
            ierr = DFM_WRONGINPUT
            goto 999
         end if

         allocate (maptimes(nt_read), STAT=ierr)
         ! Read reference time of the underlying computation
         ! Seconds since yyyy-dd-mm HH:MM:SS
         ! 123456789012345678901234567890123
         ierr = nf90_inq_varid(imapfile, 'time', id_time)
         refdat_map = ''
         ierr = ncu_get_att(imapfile, id_time, "units", refdat_map)
         tmpstr = ' '
         tmpstr = refdat_map(15:18)//refdat_map(20:21)//refdat_map(23:24)//refdat_map(26:27)//refdat_map(29:30)//refdat_map(32:33)
         call datetimestring_to_seconds(trim(tmpstr), refdat, trefdat_map, iostat) ! result: refdatold in seconds  w.r.t. absolute t0
         deallocate (refdat_map)

         ! Read map times
         ierr = nf90_inq_varid(imapfile, 'time', id_time)
         ierr = nf90_get_var(imapfile, id_time, maptimes)
         call check_error(ierr, 'time')
         call readyy('Reading map data', 0.20d0)

         ! Find last map time <= restartdatetime
         it_read = 0
         do L = nt_read, 1, -1
            if (maptimes(L) + trefdat_map <= trefdat_rst) then
               it_read = L
               exit
            end if
         end do

         ! If no map time was found <= restartdatetime, issue warning
         if (it_read == 0) then
            ! TODO: warning
            ! And stop, because no suitable restart time found.
            call mess(LEVEL_WARN, 'No suitable restart time found in '''//trim(filename)//''' for requested RestartDateTime='//trim(restartdatetime)//'.')
            ierr = DFM_WRONGINPUT
            goto 999
         end if
         if (maptimes(it_read) + trefdat_map /= trefdat_rst) then
            call seconds_to_datetimestring(tmpstr, refdat, maptimes(it_read) + trefdat_map)
            call mess(LEVEL_WARN, 'Could not find exact restart datetime in '''//trim(filename)// &
                      ''', now selected: '//tmpstr)
            ! And proceed, because this is still a good restart time.
         end if
      end if

      ! Read size of latest timestep
      ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep)
      ierr = nf90_get_var(imapfile, id_timestep, dt_init, start=(/it_read/))
      call check_error(ierr, 'timestep')
      dts = dt_init
      dti = 1d0 / dts

      ! Read following variables no matter if it is the same or different partitions

      if (kmx > 0) then
         tmp_loc = UNC_LOC_S3D
      else
         tmp_loc = UNC_LOC_S
      end if

      ! Read waterlevels (flow elem)
      ierr = get_var_and_shift(imapfile, 's1', s1, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, um%inode_own, &
                               um%inode_merge)
      call check_error(ierr, 'waterlevels')
      call readyy('Reading map data', 0.30d0)

      ! Read waterlevels old (flow elem)
      ierr = get_var_and_shift(imapfile, 's0', s0, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, um%inode_own, &
                               um%inode_merge)

      call check_error(ierr, 'waterlevels old')
      call readyy('Reading map data', 0.35d0)

      ! Read chezy roughness (flow elem)
      call gettaus(2, 1) ! It can happen that `czs` is not allocated at this point (e.g., if `jamap_chezy_elements = 0`)
      ierr = get_var_and_shift(imapfile, 'czs', czs, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, um%inode_own, &
                               um%inode_merge)

      call check_error(ierr, 'chezy roughness')
      call readyy('Reading map data', 0.375d0)

      ! Read bedlevels (flow elem)
      if (jaoldrstfile == 1) then
         call mess(LEVEL_INFO, 'The restart file is of an old version, therefore no bedlevel info is read')
      else if (jarstignorebl == 1) then
         call mess(LEVEL_INFO, 'Ignoring bedlevel information on restart file')
      else if (jased > 0) then
         ierr = get_var_and_shift(imapfile, 'FlowElem_bl', bl, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, 1, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         call check_error(ierr, 'FlowElem_bl')
      end if
      ! Read normal velocities (flow link)
      ierr = get_var_and_shift(imapfile, 'unorm', u1, tmpvar1, UNC_LOC_U3D, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)
      call check_error(ierr, 'normal velocities')
      call readyy('Reading map data', 0.40d0)

      ! Read normal velocities old (flow link)
      ierr = get_var_and_shift(imapfile, 'u0', u0, tmpvar1, UNC_LOC_U3D, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)
      call check_error(ierr, 'normal velocities old')
      call readyy('Reading map data', 0.45d0)

      ! Read discharges (flow link)
      ierr = get_var_and_shift(imapfile, 'q1', q1, tmpvar1, UNC_LOC_U3D, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)
      call check_error(ierr, 'discharges')
      call readyy('Reading map data', 0.50d0)

      ! Read upwinded flow depth (flow link)
      ierr = get_var_and_shift(imapfile, 'hu', hu, tmpvar1, UNC_LOC_U, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)

      ! Read qa (flow link), optional: only from rst file, so no error check
      ierr = get_var_and_shift(imapfile, 'qa', qa, tmpvar1, UNC_LOC_U3D, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)

      ! Read ucxq (flow elem), optional: only from rst file, so no error check
      ucxyq_read_rst = .true.

      ierr = get_var_and_shift(imapfile, 'ucxq', ucxq, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, 1, um%jamergedmap, &
                               um%inode_own, um%inode_merge)

      if (ierr /= nf90_noerr) then
         ucxyq_read_rst = .false.
      end if

      ! Read ucxq (flow elem), optional: only from rst file, so no error check
      ierr = get_var_and_shift(imapfile, 'ucyq', ucyq, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, 1, um%jamergedmap, &
                               um%inode_own, um%inode_merge)

      if (ierr /= nf90_noerr) then
         ucxyq_read_rst = .false.
      end if

      ! Read rho (flow elem), optional: only from rst file and when sediment and `idens` is true, so no error check
      rho_read_rst = .true.

      ierr = get_var_and_shift(imapfile, 'rho', rho, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, 1, um%jamergedmap, &
                               um%inode_own, um%inode_merge)

      if (ierr /= nf90_noerr) then
         rho_read_rst = .false.
      end if

      ! Read rhowat (flow elem), optional: only from rst file and when sediment and `idens` is true, so no error check
      if (stm_included) then
         ierr = get_var_and_shift(imapfile, 'rhowat', rhowat, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, 1, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
      end if

      !Read Coriolis Adams-Bashford (flow link)
      if (Corioadamsbashfordfac > 0d0) then
         !We check on it because if the factor is 0, `fvcoro` is not allocated.
         ierr = get_var_and_shift(imapfile, 'fvcoro', fvcoro, tmpvar1, UNC_LOC_U3D, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                                  um%ilink_own, um%ilink_merge)
      end if

      if (um%jamergedmap_same == 1) then
         ! Read info. on waterlevel boundaries
         if (um%nbnd_read > 0 .and. jaoldrstfile == 0) then
            call realloc(tmp_s1, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_s0, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_bl, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_squ, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_sqi, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_ucxq, um%nbnd_read, stat=ierr, keepExisting=.false.)
            call realloc(tmp_ucyq, um%nbnd_read, stat=ierr, keepExisting=.false.)

            ierr = nf90_inq_varid(imapfile, 's0_bnd', id_s0bnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_s0bnd, tmp_s0, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 's0_bnd')
            end if

            ierr = nf90_inq_varid(imapfile, 's1_bnd', id_s1bnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_s1bnd, tmp_s1, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 's1_bnd')
            end if

            if (jarstignorebl == 0) then
               ierr = nf90_inq_varid(imapfile, 'bl_bnd', id_blbnd)
               if (ierr == 0) then
                  ierr = nf90_get_var(imapfile, id_blbnd, tmp_bl, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
                  call check_error(ierr, 'bl_bnd')
               end if
            end if

            ierr = nf90_inq_varid(imapfile, 'sqi_bnd', id_sqibnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_sqibnd, tmp_sqi, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 'sqi_bnd')
            end if

            ierr = nf90_inq_varid(imapfile, 'squ_bnd', id_squbnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_squbnd, tmp_squ, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 'squ_bnd')
            end if

            ierr = nf90_inq_varid(imapfile, 'ucxq_bnd', id_ucxqbnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_ucxqbnd, tmp_ucxq, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 'ucxq_bnd')
            end if

            ierr = nf90_inq_varid(imapfile, 'ucyq_bnd', id_ucyqbnd)
            if (ierr == 0) then
               ierr = nf90_get_var(imapfile, id_ucyqbnd, tmp_ucyq, start=(/kstart_bnd, it_read/), count=(/um%nbnd_read, 1/))
               call check_error(ierr, 'ucyq_bnd')
            end if

            if (ierr == 0) then
               ! Read rho_bnd (bnd elem), optional: only from rst file and when sediment and `idens` is true, so no error check
               ierr = get_var_and_shift(imapfile, 'rho_bnd', rho, tmpvar1, tmp_loc, kmx, kstart_bnd, um%nbnd_read, it_read, &
                                        um%jamergedmap, ibnd_own, um%ibnd_merge, ndxi)

               if (stm_included) then
                  ! Read rhowat_bnd (bnd elem), optional: only from rst file and when sediment and `idens` is true, so no error check
                  ierr = get_var_and_shift(imapfile, 'rhowat_bnd', rhowat, tmpvar1, tmp_loc, kmx, kstart_bnd, um%nbnd_read, it_read, &
                                           um%jamergedmap, ibnd_own, um%ibnd_merge, ndxi)
               end if
            end if

            if (nerr_ /= 0) goto 999

            if (jampi == 0) then
               do i = 1, um%nbnd_read
                  kk = ln(1, lnxi + i)
                  s0(kk) = tmp_s0(i)
                  s1(kk) = tmp_s1(i)
                  if (jarstignorebl == 0) then
                     bl(kk) = tmp_bl(i)
                  end if
                  squ(kk) = tmp_squ(i)
                  sqi(kk) = tmp_sqi(i)
                  ucxq(kk) = tmp_ucxq(i)
                  ucyq(kk) = tmp_ucyq(i)
               end do
            else
               do i = 1, um%nbnd_read ! u and z bnd
                  Lf = lnxi + ibnd_own(i) ! boundary flow link
                  kk = ln(1, Lf) ! boundary flow node (the external one)
                  s0(kk) = tmp_s0(i)
                  s1(kk) = tmp_s1(i)
                  if (jarstignorebl == 0) then
                     bl(kk) = tmp_bl(i)
                  end if
                  squ(kk) = tmp_squ(i)
                  sqi(kk) = tmp_sqi(i)
                  ucxq(kk) = tmp_ucxq(i)
                  ucyq(kk) = tmp_ucyq(i)
               end do
            end if
         end if
         call readyy('Reading map data', 0.60d0)

      else ! restart with different partitions
         ! Read info. on waterlevel boundaries
         if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
            call realloc(tmp_s1, ndx - ndxi, stat=ierr, keepExisting=.false.)
            call realloc(tmp_s0, ndx - ndxi, stat=ierr, keepExisting=.false.)
            if (jarstignorebl == 0) then
               call realloc(tmp_bl, ndx - ndxi, stat=ierr, keepExisting=.false.)
            end if
            call realloc(tmp_squ, ndx - ndxi, stat=ierr, keepExisting=.false.)
            call realloc(tmp_sqi, ndx - ndxi, stat=ierr, keepExisting=.false.)
            call realloc(tmp_ucxq, ndx - ndxi, stat=ierr, keepExisting=.false.)
            call realloc(tmp_ucyq, ndx - ndxi, stat=ierr, keepExisting=.false.)

            ierr = get_var_and_shift(imapfile, 's0_bnd', tmp_s0, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 's0_bnd')
            ierr = get_var_and_shift(imapfile, 's1_bnd', tmp_s1, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 's1_bnd')
            if (jarstignorebl == 0) then
               ierr = get_var_and_shift(imapfile, 'bl_bnd', tmp_bl, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                        um%jamergedmap, ibnd_own, um%ibnd_merge)
               call check_error(ierr, 'bl_bnd')
            end if
            ierr = get_var_and_shift(imapfile, 'squ_bnd', tmp_squ, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 'squ_bnd')
            ierr = get_var_and_shift(imapfile, 'sqi_bnd', tmp_sqi, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 'sqi_bnd')
            ierr = get_var_and_shift(imapfile, 'ucxq_bnd', tmp_ucxq, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 'ucxq_bnd')
            ierr = get_var_and_shift(imapfile, 'ucyq_bnd', tmp_ucyq, tmpvar1, UNC_LOC_S, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            call check_error(ierr, 'ucyq_bnd')
            ! Read rho_bnd (bnd elem), optional: only from rst file and when sediment and `idens` is true, so no error check
            ierr = get_var_and_shift(imapfile, 'rho_bnd', rho, tmpvar1, tmp_loc, kmx, kstart, ndxbnd_own, it_read, &
                                     um%jamergedmap, ibnd_own, um%ibnd_merge)
            if (stm_included) then
               ! Read rhowat_bnd (bnd elem), optional: only from rst file and when sediment and `idens` is true, so no error check
               ierr = get_var_and_shift(imapfile, 'rhowat_bnd', rhowat, tmpvar1, tmp_loc, kmx, kstart, ndxbnd_own, it_read, &
                                        um%jamergedmap, ibnd_own, um%ibnd_merge)
            end if
            do i = 1, ndxbnd_own
               j = ibnd_own(i)
               Lf = lnxi + j
               kk = ln(1, Lf)
               s0(kk) = tmp_s0(j)
               s1(kk) = tmp_s1(j)
               if (jarstignorebl == 0) then
                  bl(kk) = tmp_bl(j)
               end if
               squ(kk) = tmp_squ(j)
               sqi(kk) = tmp_sqi(j)
               ucxq(kk) = tmp_ucxq(j)
               ucyq(kk) = tmp_ucyq(j)
            end do
         end if
      end if

      ! compute kbot and ktop after reading s1 from rst/map file
      if (kmx > 0) then
         call setkbotktop(1)
      end if

      ! For 3D model
      if (kmx > 0) then
         ierr = get_var_and_shift(imapfile, 'ww1', ww1, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         call check_error(ierr, 'ww1')
       !! qa
         !ierr = get_var_and_shift(imapfile, 'qa', qa,  tmpvar1, UNC_LOC_U3D, kmx, Lstart, lnx_own,  it_read, jamergedmap, &
         !                         ilink_own, ilink_merge)
         !call check_error(ierr, 'qa')
         ! qw
         ierr = get_var_and_shift(imapfile, 'qw', qw, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         call check_error(ierr, 'qw')
         ! unorm_averaged
         ierr = get_var_and_shift(imapfile, 'unorm_averaged', u1(1:lnx), tmpvar1, UNC_LOC_U, 1, Lstart, um%lnx_own, it_read, &
                                  um%jamergedmap, um%ilink_own, um%ilink_merge)
         call check_error(ierr, 'unorm_averaged')
         ! sqi
         ierr = get_var_and_shift(imapfile, 'sqi', sqi, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         ! squ
         ierr = get_var_and_shift(imapfile, 'squ', squ, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)

         ! ghost
         if (um%jafillghost == 1) then
            ierr = get_var_and_shift(imapfile, 'qw', qw, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_ghost, it_read, um%jamergedmap, &
                                     um%inode_ghost, um%inodeghost_merge)
            call check_error(ierr, 'qw_ghost_cells')
            ierr = get_var_and_shift(imapfile, 'unorm_averaged', u1(1:lnx), tmpvar1, UNC_LOC_U, 1, Lstart, um%lnx_ghost, it_read, &
                                     um%jamergedmap, um%ilink_ghost, um%ilinkghost_merge)
            call check_error(ierr, 'unorm_averaged_ghost_cells')
         end if
         call readyy('Reading map data', 0.75d0)

         ! turbulence variables
         if (iturbulencemodel >= 3) then
            ! vicwwu
            ierr = get_var_and_shift(imapfile, 'vicwwu', vicwwu, tmpvar1, UNC_LOC_WU, kmx, Lstart, um%lnx_own, it_read, &
                                     um%jamergedmap, um%ilink_own, um%ilink_merge)
            call check_error(ierr, 'vicwwu')
            call readyy('Reading map data', 0.76d0)

            ! tureps1
            ierr = get_var_and_shift(imapfile, 'tureps1', tureps1, tmpvar1, UNC_LOC_WU, kmx, Lstart, um%lnx_own, it_read, &
                                     um%jamergedmap, um%ilink_own, um%ilink_merge)
            call check_error(ierr, 'tureps1')
            call readyy('Reading map data', 0.77d0)

            ! turkin1
            ierr = get_var_and_shift(imapfile, 'turkin1', turkin1, tmpvar1, UNC_LOC_WU, kmx, Lstart, um%lnx_own, it_read, &
                                     um%jamergedmap, um%ilink_own, um%ilink_merge)
            call check_error(ierr, 'turkin1')
            call readyy('Reading map data', 0.78d0)

            ! ghost
            if (um%jafillghost == 1) then
               ierr = get_var_and_shift(imapfile, 'tureps1', tureps1, tmpvar1, UNC_LOC_WU, kmx, Lstart, um%lnx_ghost, it_read, &
                                        um%jamergedmap, um%ilink_ghost, um%ilinkghost_merge)
               call check_error(ierr, 'tureps1_ghost_cells')
               ierr = get_var_and_shift(imapfile, 'turkin1', turkin1, tmpvar1, UNC_LOC_WU, kmx, Lstart, um%lnx_ghost, it_read, &
                                        um%jamergedmap, um%ilink_ghost, um%ilinkghost_merge)
               call check_error(ierr, 'turkin1_ghost_cells')
            end if
         end if
      else
         ! Read squ and sqi, optional: only from rst file, so no error check
         ierr = get_var_and_shift(imapfile, 'squ', squ, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         ierr = get_var_and_shift(imapfile, 'sqi', sqi, tmpvar1, UNC_LOC_W, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         sq = sqi - squ
      end if
      call readyy('Reading map data', 0.80d0)

      ! Read fixed weirs
      ierr = get_var_and_shift(imapfile, 'weirdte', map_fixed_weir_energy_loss, tmpvar1, UNC_LOC_U, kmx, Lstart, um%lnx_own, it_read, um%jamergedmap, &
                               um%ilink_own, um%ilink_merge)
      do L = 1, lnx
         if (iadv(L) == 24 .or. iadv(L) == 25) then
            weirdte(nfxwL(L)) = map_fixed_weir_energy_loss(L)
         end if
      end do

      ! Read secondary flow
      if (jasecflow > 0) then
         ierr = get_var_and_shift(imapfile, 'spirint', spirint, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, um%inode_own, &
                                  um%inode_merge)
         call check_error(ierr, 'spirint')
      end if

      ! Read the salinity (flow elem)
      if (jasal > 0) then
         if (kmx > 0) then
            tmp_loc = UNC_LOC_S3D
         else
            tmp_loc = UNC_LOC_S
         end if
         ierr = get_var_and_shift(imapfile, 'sa1', sa1, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'unc_read_map_or_rst: cannot read variable sa1 from the specified restart file. Skip reading this variable.')
         else
            call assign_restart_data_to_local_array(sa1, constituents, isalt, kmx, um%ndxi_own, um%jamergedmap, um%inode_own)
         end if
      end if

      call readyy('Reading map data', 0.90d0)

      ! Read the temperature (flow elem)
      if (jatem > 0) then
         if (kmx > 0) then
            tmp_loc = UNC_LOC_S3D
         else
            tmp_loc = UNC_LOC_S
         end if
         ierr = get_var_and_shift(imapfile, 'tem1', tem1, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                  um%inode_own, um%inode_merge)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'unc_read_map_or_rst: cannot read variable tem1 from the specified restart file. Skip reading this variable.')
         else
            call assign_restart_data_to_local_array(tem1, constituents, itemp, kmx, um%ndxi_own, um%jamergedmap, um%inode_own)
         end if
      end if

      ! Read the tracers
      if (ITRA1 > 0) then
         call realloc(tmpvar1D, ndkx, keepExisting=.false., fill=0.0d0)
         do iconst = ITRA1, ITRAN
            i = iconst - ITRA1 + 1
            tmpstr = const_names(iconst)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr, 32, 95)
            call replace_char(tmpstr, 47, 95)
!         tracer exists in restart file
            if (kmx > 0) then
               tmp_loc = UNC_LOC_S3D
            else
               tmp_loc = UNC_LOC_S
            end if
            ierr = get_var_and_shift(imapfile, trim(tmpstr), tmpvar1D, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                     um%inode_own, um%inode_merge)
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_WARN, 'unc_read_map_or_rst: cannot read variable '''//trim(tmpstr)//''' from the specified restart file. Skip reading this variable.')
            else
               call assign_restart_data_to_local_array(tmpvar1D, constituents, iconst, kmx, um%ndxi_own, um%jamergedmap, um%inode_own)
            end if !ierr
         end do !iconst
      end if !ITRA1

      ! Read the water quality bottom variables
      is_wq_bot_3d = jahiswqbot3d .or. jamapwqbot3d
      if (numwqbots > 0) then
         call realloc(tmpvar1D, ndkx, keepExisting=.false., fill=0.0d0)
         do iwqbot = 1, numwqbots
            tmpstr = wqbotnames(iwqbot)
            ! Forbidden chars in NetCDF names: space, /, and more.
            call replace_char(tmpstr, 32, 95)
            call replace_char(tmpstr, 47, 95)
            if (is_wq_bot_3d) then
               tmp_loc = UNC_LOC_S3D
               tmpstr1 = trim(tmpstr)//'_3D'
            else
               tmp_loc = UNC_LOC_S
               tmpstr1 = tmpstr
            end if
            ierr = get_var_and_shift(imapfile, trim(tmpstr1), tmpvar1D, tmpvar1, tmp_loc, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                     um%inode_own, um%inode_merge)
            if (ierr /= nf90_noerr) then
               call mess(LEVEL_WARN, 'unc_read_map_or_rst: cannot read variable '''//trim(tmpstr1)//''' from the specified restart file. Skip reading this variable.')
            else
               call assign_restart_data_to_local_array(tmpvar1D, wqbot, iwqbot, kmx, um%ndxi_own, um%jamergedmap, um%inode_own,.not. is_wq_bot_3d)
            end if
         end do
      end if

      ! JRE to do
      if (jased > 0 .and. stm_included) then
         if (um%jamergedmap == 1) then
            call mess(LEVEL_WARN, 'read_map: Morphology data not present in merged map file. Ignoring for now.')
         end if
         msed => stmpar%morlyr%state%msed
         thlyr => stmpar%morlyr%state%thlyr
         svfrac => stmpar%morlyr%state%svfrac
         layerfrac = imiss
         layerthk = imiss
         !
         ! Check dimensions for consistency
         !
         ierr = nf90_inq_dimid(imapfile, 'nSedTot', id_sedtotdim) ! Accept any errors, we may have a hydrodynamic restart only. Is allowed.
         ierr = nf90_inq_dimid(imapfile, 'nSedSus', id_sedsusdim)
         ierr = nf90_inq_dimid(imapfile, 'nBedLayers', id_nlyrdim)

         ierr = nf90_inquire_dimension(imapfile, id_sedtotdim, len=sedtot_read)
         if (ierr /= nf90_noerr) then
            sedtot_read = 0 ! Set the total number of sediment fractions to zero if it is not found in the _rst.nc file
            if (stmpar%lsedtot > 0) then
               write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no sediment fractions, but model does. Continuing anyway.'
               call msg_flush()
            end if
         end if

         ierr = nf90_inquire_dimension(imapfile, id_sedsusdim, len=sedsus_read)
         if (ierr /= nf90_noerr) then
            sedsus_read = 0 ! Set the suspended sediment fraction to zero if it is not found in the _rst.nc file
            if (stmpar%lsedsus > 0) then
               write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no suspended sediment fractions, but model does. Continuing anyway.'
               call msg_flush()
            end if
         end if

         ierr = nf90_inquire_dimension(imapfile, id_nlyrdim, len=nlyr_read)
         if (ierr /= nf90_noerr) then
            nlyr_read = 0 ! Set the bed layer count to zero if it is not found in the _rst.nc file
            if (stmpar%morlyr%settings%nlyr > 0) then
               write (msgbuf, '(a)') 'Restart file '''//trim(filename)//''' contains no bed composition layers, but model does. Continuing anyway.'
               call msg_flush()
            end if
         end if
         !
         if ((sedtot_read /= stmpar%lsedtot .and. sedtot_read > 0 .and. stmpar%lsedtot > 0) .or. &
             (sedsus_read /= stmpar%lsedsus .and. sedsus_read >= 0 .and. stmpar%lsedsus >= 0)) then
            write (msgbuf, '(a)') 'Mismatch in number of sediment fractions in morphology restart file. Not reading restart data at all.'
            call warn_flush()
            ierr = DFM_WRONGINPUT
            goto 999
         end if

         !
         ! Read morphology data:
         !

         ! fraction concentrations
         if (stmpar%lsedsus > 0 .and. sedsus_read == stmpar%lsedsus) then

            !internal cells
            call read_sediment(constituents, '', imapfile, kstart, um%ndxi_own, it_read, um, 0)
            !boundary cells
            if (um%nbnd_read > 0) then
               call read_sediment(constituents, '_bnd', imapfile, 1, um%nbnd_read, it_read, um, um%ndxi_own)
            end if !um%nbnd_read > 0
            sed = constituents(ISED1:ISEDN, :)
         end if !lsedsus

         ! morbl
         if (jarstignorebl == 0) then
            ierr = get_var_and_shift(imapfile, 'mor_bl', bl, tmpvar1, UNC_LOC_S, kmx, kstart, um%ndxi_own, it_read, um%jamergedmap, &
                                     um%inode_own, um%inode_merge)
         end if

         ! morphological time
         ierr = nf90_inq_varid(imapfile, 'morft', id_morft)
         ierr = nf90_get_var(imapfile, id_morft, stmpar%morpar%morft0, start=(/it_read/))
         stmpar%morpar%morft = stmpar%morpar%morft0

         ! mfluff
         if (stmpar%morpar%flufflyr%iflufflyr > 0 .and. stmpar%lsedsus > 0 .and. sedsus_read == stmpar%lsedsus) then
            ierr = nf90_inq_varid(imapfile, 'mfluff', id_mfluff)
            if (ierr == nf90_noerr) then
               if (allocated(tmpvar)) then
                  deallocate (tmpvar)
               end if
               if (allocated(rst_mfluff)) then
                  deallocate (rst_mfluff)
               end if
               allocate (tmpvar(sedsus_read, ndxi))
               allocate (rst_mfluff(stmpar%lsedsus, ndxi))
               ierr = nf90_get_var(imapfile, id_mfluff, tmpvar(1:sedsus_read, 1:um%ndxi_own), start=(/1, kstart, it_read/), count=(/sedsus_read, ndxi, 1/))
               do kk = 1, ndxi
                  if (um%jamergedmap == 1) then
                     kloc = um%inode_own(kk)
                     rst_mfluff(:, kloc) = tmpvar(:, um%inode_merge(kk))
                  else
                     kloc = kk
                     rst_mfluff(:, kloc) = tmpvar(:, kk)
                  end if
               end do
               call check_error(ierr, 'mfluff')
               stmpar%morpar%flufflyr%mfluff(:, 1:ndxi) = rst_mfluff(:, 1:ndxi)
            end if
         end if

         ! Bed composition
         if (stmpar%morlyr%settings%iunderlyr > 0 .and. nlyr_read == stmpar%morlyr%settings%nlyr) then
            select case (stmpar%morlyr%settings%iunderlyr)
            case (1)
               ! bodsed
               if (allocated(tmpvar)) then
                  deallocate (tmpvar)
               end if
               if (allocated(rst_bodsed)) then
                  deallocate (rst_bodsed)
               end if
               allocate (tmpvar(sedtot_read, ndxi))
               allocate (rst_bodsed(sedtot_read, ndxi))
               ierr = nf90_inq_varid(imapfile, 'bodsed', id_bodsed)
               ierr = nf90_get_var(imapfile, id_bodsed, tmpvar(1:sedtot_read, 1:um%ndxi_own), start=(/1, kstart, it_read/), count=(/sedtot_read, ndxi, 1/))
               do kk = 1, ndxi
                  if (um%jamergedmap == 1) then
                     kloc = um%inode_own(kk)
                     rst_bodsed(:, kloc) = tmpvar(:, um%inode_merge(kk))
                  else
                     kloc = kk
                     rst_bodsed(:, kloc) = tmpvar(:, kk)
                  end if
               end do
               call check_error(ierr, 'bodsed')
               stmpar%morlyr%state%bodsed(:, 1:ndxi) = rst_bodsed(:, 1:ndxi)
               call bedcomp_use_bodsed(stmpar%morlyr)
               layerfrac = 2
            case (2)
               ! msed (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
               if (allocated(tmpvar2)) then
                  deallocate (tmpvar2)
               end if
               if (allocated(rst_msed)) then
                  deallocate (rst_msed)
               end if
               call realloc(tmpvar2, (/sedtot_read, nlyr_read, ndxi/), keepExisting=.false.)
               call realloc(rst_msed, (/sedtot_read, nlyr_read, ndxi/), keepExisting=.false.)
               !
               ierr = nf90_inq_varid(imapfile, 'msed', id_msed)
               if (ierr == nf90_noerr) then
                  layerfrac = 0
                  do l = 1, sedtot_read
                     ierr = nf90_get_var(imapfile, id_msed, tmpvar2(l, 1:nlyr_read, 1:um%ndxi_own), start=(/l, 1, kstart, it_read/), count=(/1, nlyr_read, ndxi, 1/))
                  end do
                  !
                  do kk = 1, ndxi
                     if (um%jamergedmap == 1) then
                        kloc = um%inode_own(kk)
                        rst_msed(:, :, kloc) = tmpvar2(:, :, um%inode_merge(kk))
                     else
                        kloc = kk
                        rst_msed(:, :, kloc) = tmpvar2(:, :, kk)
                     end if
                  end do
                  call check_error(ierr, 'msed')
               else
                  !
                  ierr = nf90_inq_varid(imapfile, 'lyrfrac', id_lyrfrac)
                  if (ierr == nf90_noerr) then
                     !
                     ! lyrfrac (/ id_sedtotdim(iid) , id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
                     if (allocated(tmpvar2)) then
                        deallocate (tmpvar2)
                     end if
                     call realloc(tmpvar2, (/sedtot_read, nlyr_read, ndxi/), keepExisting=.false.)
                     !
                     do l = 1, sedtot_read
                        ierr = nf90_get_var(imapfile, id_lyrfrac, tmpvar2(l, 1:nlyr_read, 1:um%ndxi_own), start=(/l, 1, kstart, it_read/), count=(/1, nlyr_read, ndxi, 1/))
                     end do
                     do kk = 1, ndxi
                        if (um%jamergedmap == 1) then
                           kloc = um%inode_own(kk)
                           rst_msed(:, :, kloc) = tmpvar2(:, :, um%inode_merge(kk))
                        else
                           kloc = kk
                           rst_msed(:, :, kloc) = tmpvar2(:, :, kk) ! no typo, see restart_lyrs.f90
                        end if
                     end do
                     layerfrac = 1
                  end if
                  call check_error(ierr, 'lyrfrac')
               end if
               !
               ierr = nf90_inq_varid(imapfile, 'thlyr', id_thlyr)
               if (ierr == nf90_noerr) then
                  layerthk = 1
                  !
                  ! thlyr (/ id_nlyrdim(iid) , id_flowelemdim(iid) , id_timedim(iid) /)
                  if (allocated(tmpvar)) then
                     deallocate (tmpvar)
                  end if
                  if (allocated(rst_thlyr)) then
                     deallocate (rst_thlyr)
                  end if
                  call realloc(tmpvar, (/nlyr_read, ndxi/), keepExisting=.false.)
                  call realloc(rst_thlyr, (/nlyr_read, ndxi/), keepExisting=.false.)
                  !
                  ierr = nf90_get_var(imapfile, id_thlyr, tmpvar(1:nlyr_read, 1:um%ndxi_own), start=(/1, kstart, it_read/), count=(/nlyr_read, ndxi, 1/))
                  do kk = 1, ndxi
                     if (um%jamergedmap == 1) then
                        kloc = um%inode_own(kk)
                        rst_thlyr(:, kloc) = tmpvar(:, um%inode_merge(kk))
                     else
                        kloc = kk
                        rst_thlyr(:, kloc) = tmpvar(:, kk)
                     end if
                  end do
               end if
               call check_error(ierr, 'thlyr')
               !
               ! Organize layer administration
               if ((layerfrac >= 0) .and. (layerthk >= 0)) then
                  if (stmpar%morlyr%settings%nlyr >= nlyr_read) then
                     ! copy first layer
                     thlyr(1, 1:ndxi) = rst_thlyr(1, 1:ndxi)
                     msed(1:stmpar%lsedtot, 1, 1:ndxi) = rst_msed(1:stmpar%lsedtot, 1, 1:ndxi)
                     !
                     do k = 2, 1 + stmpar%morlyr%settings%nlyr - nlyr_read
                        thlyr(k, 1:ndxi) = 0.0_fp
                        msed(1:stmpar%lsedtot, k, 1:ndxi) = 0.0_fp
                     end do
                     !
                     ! copy remaining layers
                     !
                     thlyr(stmpar%morlyr%settings%nlyr - nlyr_read + 2:stmpar%morlyr%settings%nlyr, 1:ndxi) = rst_thlyr(2:nlyr_read, 1:ndxi)
                     msed(1:stmpar%lsedtot, stmpar%morlyr%settings%nlyr - nlyr_read + 2:stmpar%morlyr%settings%nlyr, 1:ndxi) = rst_msed(1:stmpar%lsedtot, 2:nlyr_read, 1:ndxi)
                  else
                     !
                     ! more layers in restart file than in simulation
                     !
                     ! copy the first nlyr layers
                     !
                     thlyr(1:stmpar%morlyr%settings%nlyr, 1:ndxi) = rst_thlyr(1:stmpar%morlyr%settings%nlyr, 1:ndxi)
                     msed(1:stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndxi) = rst_msed(1:stmpar%lsedtot, 1:stmpar%morlyr%settings%nlyr, 1:ndxi)
                     !
                     !
                     ! add contents of other layers to last layer
                     !
                     do k = stmpar%morlyr%settings%nlyr + 1, nlyr_read
                        thlyr(stmpar%morlyr%settings%nlyr, 1:ndxi) = thlyr(stmpar%morlyr%settings%nlyr, 1:ndxi) &
                                                                         & + rst_thlyr(k, 1:ndxi)
                        do l = 1, stmpar%lsedtot
                           msed(l, stmpar%morlyr%settings%nlyr, 1:ndxi) = msed(l, stmpar%morlyr%settings%nlyr, 1:ndxi) &
                                                                        & + rst_msed(l, k, 1:ndxi)
                        end do
                     end do
                  end if
                  !
                  if (layerfrac == 1) then
                     !
                     ! msed contains volume fractions
                     if (stmpar%morlyr%settings%iporosity == 0) then
                        do l = 1, stmpar%lsedtot
                           do k = 1, stmpar%morlyr%settings%nlyr
                              do nm = 1, ndxi
                                 msed(l, k, nm) = msed(l, k, nm) * thlyr(k, nm) * stmpar%sedpar%cdryb(l)
                              end do
                           end do
                        end do
                     else
                        do k = 1, stmpar%morlyr%settings%nlyr
                           do nm = 1, ndxi
                              !
                              ! determine mass fractions
                              mfracsum = 0.0_fp
                              do l = 1, stmpar%lsedtot
                                 mfrac(l) = msed(l, k, nm) * stmpar%sedpar%rhosol(l)
                                 mfracsum = mfracsum + mfrac(l)
                              end do
                              if (mfracsum > 0.0_fp) then
                                 do l = 1, stmpar%lsedtot
                                    mfrac(l) = mfrac(l) / mfracsum
                                 end do
                                 !
                                 ! obtain porosity and sediment thickness without pores
                                 !
                                 call getporosity(stmpar%morlyr, mfrac, poros)
                                 sedthick = thlyr(k, nm) * (1.0_fp - poros)
                              else
                                 sedthick = 0.0_fp
                                 poros = 0.0_fp
                              end if
                              !
                              ! convert volume fractions to sediment mass
                              !
                              do l = 1, stmpar%lsedtot
                                 msed(l, k, nm) = msed(l, k, nm) * sedthick * stmpar%sedpar%rhosol(l)
                              end do
                              svfrac(k, nm) = 1.0_fp - poros
                           end do
                        end do
                     end if
                  else
                     if (stmpar%morlyr%settings%iporosity > 0) then
                        do nm = 1, ndxi
                           sedthick = 0.0_fp
                           do l = 1, stmpar%lsedtot
                              sedthick = sedthick + msed(l, k, nm) / stmpar%sedpar%rhosol(l)
                           end do
                           svfrac(k, nm) = sedthick / thlyr(k, nm)
                        end do
                     end if
                  end if
               end if

               ! sedshort, preload, porosity, dpsed?
            end select
         end if

         ! Read 1D cross sections
         ndx1d = ndxi - ndx2d
         if (ndx1d > 0 .and. stm_included) then
            if (stmpar%morpar%bedupd) then
               if (jarstignorebl == 0) then
                  ierr = nf90_inq_dimid(imapfile, trim(mesh1dname)//'_crs_maxdim', id_jmax)
                  if (ierr == 0) ierr = nf90_inquire_dimension(imapfile, id_jmax, len=jmax)
                  ierr = nf90_inq_dimid(imapfile, trim(mesh1dname)//'_ncrs', id_ncrs)
                  if (ierr == 0) ierr = nf90_inquire_dimension(imapfile, id_ncrs, len=nCrs)
                  if (allocated(work1d_z)) then
                     deallocate (work1d_z, work1d_n)
                  end if
                  allocate (work1d_z(1:jmax, 1:nCrs), work1d_n(1:jmax, 1:nCrs))
                  ierr = nf90_inq_varid(imapfile, trim(mesh1dname)//'_mor_crs_z', id_flowelemcrsz)
                  ierr = nf90_get_var(imapfile, id_flowelemcrsz, work1d_z(1:jmax, 1:nCrs), start=(/1, 1/), count=(/jmax, nCrs/))
                  do i = 1, nCrs
                     do j = 1, network%crs%cross(i)%tabdef%levelscount
                        network%crs%cross(i)%tabdef%height(j) = work1d_z(j, i)
                     end do
                     network%crs%cross(i)%bedlevel = work1d_z(1, i)
                  end do
                  ierr = nf90_inq_varid(imapfile, trim(mesh1dname)//'_mor_crs_n', id_flowelemcrsn)
                  if (ierr == 0) ierr = nf90_get_var(imapfile, id_flowelemcrsn, work1d_n(1:jmax, 1:nCrs), start=(/1, 1/), count=(/jmax, nCrs/))
                  if (ierr == 0) then
                     do i = 1, nCrs
                        do j = 1, network%crs%cross(i)%tabdef%levelscount
                           network%crs%cross(i)%tabdef%flowwidth(j) = work1d_n(j, i)
                        end do
                     end do
                  end if
               end if
            end if
         end if
         ! Update the D50 based on the info in the restart file
         call initsedtra(sedtra, stmpar%sedpar, stmpar%trapar, stmpar%morpar, stmpar%morlyr, rhomean, ag, vismol, 1, ndx, ndx, stmpar%lsedsus, stmpar%lsedtot)
      end if

      ! Read Thatcher-Harleman boundary data
      ! TODO: AvD: UNST-994: no TH data in merged files yet. Replace the 1 indices below by a prop kstart later.
      if (allocated(threttim)) then
         if (um%jamergedmap == 1) then
            call mess(LEVEL_WARN, 'read_map: Thatcher-Harlemann data not present in merged map file. Ignoring for now.')
         end if
         allocate (max_threttim(NUMCONST))
         max_threttim = maxval(threttim, dim=2)
         if (jasal > 0) then
            if (max_threttim(ISALT) > 0d0) then
               ierr = nf90_inq_varid(imapfile, 'tsalbnd', id_tsalbnd)
               ierr = nf90_get_var(imapfile, id_tsalbnd, thtbnds(1:nbnds), start=(/1, it_read/), count=(/nbnds, 1/))
               ierr = nf90_inq_varid(imapfile, 'zsalbnd', id_zsalbnd)
               ierr = nf90_get_var(imapfile, id_zsalbnd, thzbnds(1:nbnds * kmxd), start=(/1, it_read/), count=(/nbnds * kmxd, 1/))
            end if
         end if
         if (jatem > 0) then
            if (max_threttim(ITEMP) > 0d0) then
               ierr = nf90_inq_varid(imapfile, 'ttembnd', id_ttembnd)
               ierr = nf90_get_var(imapfile, id_ttembnd, thtbndtm(1:nbndtm), start=(/1, it_read/), count=(/nbndtm, 1/))
               ierr = nf90_inq_varid(imapfile, 'ztembnd', id_ztembnd)
               ierr = nf90_get_var(imapfile, id_ztembnd, thzbndtm(1:nbndtm * kmxd), start=(/1, it_read/), count=(/nbndtm * kmxd, 1/))
            end if
         end if
         if (jased > 0 .and. .not. stm_included) then
            if (max_threttim(ISED1) > 0d0) then
               ierr = nf90_inq_varid(imapfile, 'tsedbnd', id_tsedbnd)
               ierr = nf90_get_var(imapfile, id_tsedbnd, thtbndsd(1:nbndsd), start=(/1, it_read/), count=(/nbndsd, 1/))
               ierr = nf90_inq_varid(imapfile, 'zsedbnd', id_zsedbnd)
               ierr = nf90_get_var(imapfile, id_zsedbnd, thzbndsd(1:nbndsd * kmxd), start=(/1, it_read/), count=(/nbndsd * kmxd, 1/))
            end if
         end if
         if (numfracs > 0) then ! sediment fractions stm model
            if (.not. allocated(id_tsedfracbnd)) allocate (id_tsedfracbnd(numfracs))
            if (.not. allocated(id_zsedfracbnd)) allocate (id_zsedfracbnd(numfracs))
            do i = 1, numfracs
               iconst = ifrac2const(i)
               if (iconst == 0) cycle
               if (max_threttim(iconst) > 0d0) then
                  write (numsedfracstr, numformat) i
                  ierr = nf90_inq_varid(imapfile, 'tsedfracbnd'//numsedfracstr, id_tsedfracbnd(i))
                  ierr = nf90_get_var(imapfile, id_tsedfracbnd(i), bndsf(i)%tht(1:nbndsf(i)), start=(/1, it_read/), count=(/nbndsf(i), 1/))
                  ierr = nf90_inq_varid(imapfile, 'zsedfracbnd'//numsedfracstr, id_zsedfracbnd(i))
                  ierr = nf90_get_var(imapfile, id_zsedfracbnd(i), bndsf(i)%thz(1:nbndsf(i) * kmxd), start=(/1, it_read/), count=(/nbndsf(i) * kmxd, 1/))
               end if
            end do
         end if
         if (numtracers > 0) then
            if (.not. allocated(id_ttrabnd)) allocate (id_ttrabnd(numtracers))
            if (.not. allocated(id_ztrabnd)) allocate (id_ztrabnd(numtracers))
            do i = 1, numtracers
               iconst = itrac2const(i)
               if (max_threttim(iconst) > 0d0) then
                  write (numtrastr, numformat) i
                  ierr = nf90_inq_varid(imapfile, 'ttrabnd'//numtrastr, id_ttrabnd(i))
                  ierr = nf90_get_var(imapfile, id_ttrabnd(i), bndtr(i)%tht(1:nbndtr(i)), start=(/1, it_read/), &
                                      count=(/nbndtr(i), 1/))
                  ierr = nf90_inq_varid(imapfile, 'ztrabnd'//numtrastr, id_ztrabnd(i))
                  ierr = nf90_get_var(imapfile, id_ztrabnd(i), bndtr(i)%thz(1:nbndtr(i) * kmxd), start=(/1, it_read/), &
                                      count=(/nbndtr(i) * kmxd, 1/))
               end if
            end do
         end if
         call check_error(ierr, 'Thatcher-Harleman boundaries')
      end if

      ! Read structure
      call read_structures_from_rst(imapfile, filename, it_read)

      call process_structures_saved_parameters(READ_DATA_FROM_FILE, imapfile)

      call readyy('Reading map data', 0.95d0)

      ! Read hysteresis_for_summerdike
      if (lnx1d > 0 .and. network%loaded) then
         ierr = nf90_inq_dimid(imapfile, 'n1DFlowLink', id_1dflowlinkdim)
         if (ierr == nf90_noerr) then
            ierr = nf90_inquire_dimension(imapfile, id_1dflowlinkdim, len=numl1d)
            if (ierr == nf90_noerr) then
               if (numl1d == network%numl) then
                  call realloc(tmpvar1D, numl1d, keepExisting=.false., fill=0d0) ! We use this array becasue get_var_and_shift requires a double precision array
                  ierr = get_var_and_shift(imapfile, 'hysteresis_for_summerdike', tmpvar1D, tmpvar1, UNC_LOC_U, kmx, Lstart, numl1d, it_read, um%jamergedmap, &
                                           um%ilink_own, um%ilink_merge)
                  ! Convert to logic value and fill in hysteresis_for_summerdike
                  call realloc(itmpvar, numl1d, keepExisting=.false., fill=0)
                  itmpvar = int(tmpvar1D)
                  call convert_hysteresis_summerdike(.false., itmpvar)
               else
                  write (msgbuf, '(a,i0,a,i0,a)') 'Number of 1D links: in the restart file ', numl1d, ',  in model: ', lnx1d, '.'
                  call warn_flush()
                  call qnerror('Number of 1D links read from the restart file unequal to number of 1d links in model.', ' ', ' ')
               end if
            else
               write (msgbuf, '(a)') 'unc_read_map_or_rst: cannot read the number of 1D links.'
               call warn_flush()
            end if
         end if
      end if

      ! Check if the orientation of each flowlink in the current model is the same with the link in the rst file
      ! If not, reverse the velocity that is read from rst file
      ! Check only when parallel restart with different partitions. ToDo: check for all the restart scenarios
      !if (jamergedmap_same == 0 .and. jampi == 1 ) then
      !   ! Read link/interface between two flow elements (flow link) from the merged file
      !   allocate(ln_read(2,lnx_own))
      !   allocate(itmp2D(2,lnx_merge))
      !   ierr = nf90_inq_varid(imapfile, 'FlowLink', id_flowlink)
      !   ierr = nf90_get_var(imapfile, id_flowlink, itmp2D)
      !   do L = 1, lnx_own
      !      LL = ilink_merge(L)
      !      ln_read(:,L) = itmp2D(:,LL)
      !   enddo
      !   call check_error(ierr, 'FlowLink')
      !
      !endif

      !-- Synchronisation to other domains, only for merged-map input
      if (jampi == 1 .and. um%jamergedmap == 1) then
         !-- S/S3D --
         if (jatimer == 1) call starttimer(IUPDSALL)

         call update_ghosts(ITYPE_SALL, 1, Ndx, s1, ierr)
         call update_ghosts(ITYPE_SALL, 1, Ndx, s0, ierr)

         if (numconst > 0) then

            if (kmx == 0) then ! 2D
               call update_ghosts(ITYPE_Sall, NUMCONST, Ndx, constituents, ierr)
            else ! 3D
               call update_ghosts(ITYPE_Sall3D, NUMCONST, Ndkx, constituents, ierr)
            end if

         end if

         if (jatimer == 1) call stoptimer(IUPDSALL)

         !-- U/U3D --
         if (jatimer == 1) call starttimer(IUPDU)

         if (kmx == 0) then ! 2D
            call update_ghosts(ITYPE_U, 1, Lnx, u1, ierr)
            call update_ghosts(ITYPE_U, 1, Lnx, u0, ierr)
            call update_ghosts(ITYPE_U, 1, Lnx, q1, ierr)
         else ! 3D
            call update_ghosts(ITYPE_U3D, 1, Lnkx, u1, ierr)
            call update_ghosts(ITYPE_U3D, 1, Lnkx, u0, ierr)
            call update_ghosts(ITYPE_U3D, 1, Lnkx, q1, ierr)
            call update_ghosts(ITYPE_U3D, 1, Lnkx, qa, ierr)
         end if

         if (jatimer == 1) call stoptimer(IUPDU)

         if (ierr /= 0) then
            ierr = DFM_MODELNOTINITIALIZED
            goto 999
         end if

      end if ! jampi .and. jamapmerged

      call readyy('Reading map data', 1.00d0)

      ierr = DFM_NOERR
      ! Close the netcdf-file _map.nc
999   continue
      itmp = unc_close(imapfile)
      call readyy('Reading map data', -1d0)
      if (allocated(maptimes)) then
         deallocate (maptimes)
      end if
      if (allocated(max_threttim)) then
         deallocate (max_threttim)
      end if
      if (allocated(id_ttrabnd)) then
         deallocate (id_ttrabnd)
      end if
      if (allocated(id_ztrabnd)) then
         deallocate (id_ztrabnd)
      end if
      if (allocated(id_tsedfracbnd)) then
         deallocate (id_tsedfracbnd)
      end if
      if (allocated(id_zsedfracbnd)) then
         deallocate (id_zsedfracbnd)
      end if
      if (allocated(tmpvar)) then
         deallocate (tmpvar)
      end if
      if (allocated(tmpvar1)) then
         deallocate (tmpvar1)
      end if
      if (allocated(tmpvar2)) then
         deallocate (tmpvar2)
      end if

   end subroutine unc_read_map_or_rst

!> helper routine for unc_read_map_or_rst
   function unc_read_merged_map(um, imapfile, filename, ierr) result(success)
      use m_alloc, only: realloc
      use m_samples, only: Ns
      use dfm_error, only: DFM_GENERICERROR
      use m_partitioninfo, only: jampi, my_rank, idomain, ighostlev, sdmn, link_ghostdata, reduce_key, reduce_int_sum
      use m_flowgeom, only: ndxi, lnx, ln, ndx
      use fm_external_forcings_data, only: ibnd_own, kbndz, ndxbnd_own, jaoldrstfile
      use m_wrisam

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(in) :: imapfile
      integer, intent(inout) :: ierr
      type(t_unc_merged), intent(inout) :: um !< struct holding all data for ugrid merged map/rst files
      logical :: success !< function result

      integer :: kk, ll
      integer :: MSAM
      integer :: ndxi_merge, lnx_merge, ndxbnd_merge, jaghost
      integer :: ndxi_all, lnx_all !< total numbers of nodes/links among all partitions
      integer :: id_xu, id_yu, id_xbnd, id_ybnd, id_xzw, id_yzw, id_flowelemdim, id_flowlinkdim
      real(kind=hp), allocatable :: xmc(:), ymc(:), xuu(:), yuu(:), xbnd_read(:), ybnd_read(:)
      integer, allocatable :: inode_owninv(:) !< Mapping from actual node index to its own index
      character(len=32) :: tmpstr
      integer, allocatable :: inode_merge2own(:) !< Mapping flownodes from merged file to subdomain own index

      success = .false.

      if (um%jamergedmap == 1) then
         ! If rst file is a merged-map or merged-rst file, read only a domain's own flow nodes and links.
         if (um%jamergedmap_same == 1) then ! If the partitions of the model are the same with the rst file
            um%ndxi_own = 0
            um%lnx_own = 0
            call realloc(um%inode_own, ndxi, keepExisting=.false.)
            call realloc(um%ilink_own, lnx, keepExisting=.false.)

            if (um%nbnd_read > 0 .and. jaoldrstfile == 0 .and. jampi == 0) then
               do kk = 1, um%nbnd_read
                  ibnd_own(kk) = kk
               end do
            end if

            if (um%jafillghost == 0) then
               do kk = 1, ndxi
                  if (idomain(kk) == my_rank) then
                     um%ndxi_own = um%ndxi_own + 1
                     um%inode_own(um%ndxi_own) = kk
                  end if
               end do

               do LL = 1, lnx
                  call link_ghostdata(my_rank, idomain(ln(1, LL)), idomain(ln(2, LL)), jaghost, um%idmn_ghost, &
                                      ighostlev(ln(1, LL)), ighostlev(ln(2, LL)))
                  if (jaghost /= 1) then
                     um%lnx_own = um%lnx_own + 1
                     um%ilink_own(um%lnx_own) = LL
                  end if
               end do
            else ! Using merged rst file in 3D problems need to fill in some variables on ghost cells
               um%ndxi_ghost = 0
               um%lnx_ghost = 0
               call realloc(um%inode_ghost, ndxi, keepExisting=.false.)
               call realloc(um%inodeghost_merge, ndxi, keepExisting=.false.)
               call realloc(um%ilink_ghost, lnx, keepExisting=.false.)
               call realloc(um%ilinkghost_merge, lnx, keepExisting=.false.)

               do kk = 1, ndxi
                  if (idomain(kk) == my_rank) then
                     um%ndxi_own = um%ndxi_own + 1
                     um%inode_own(um%ndxi_own) = kk
                  else
                     um%ndxi_ghost = um%ndxi_ghost + 1
                     um%inode_ghost(um%ndxi_ghost) = kk
                  end if
               end do

               do LL = 1, lnx
                  call link_ghostdata(my_rank, idomain(ln(1, LL)), idomain(ln(2, LL)), jaghost, um%idmn_ghost, &
                                      ighostlev(ln(1, LL)), ighostlev(ln(2, LL)))
                  if (jaghost /= 1) then
                     um%lnx_own = um%lnx_own + 1
                     um%ilink_own(um%lnx_own) = LL
                  else
                     um%lnx_ghost = um%lnx_ghost + 1
                     um%ilink_ghost(um%lnx_ghost) = LL
                  end if
               end do

               ! prepare for kd-tree search
               ! Read coordinates of flow elem circumcenters from merged map file
               ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
               call check_error(ierr, 'nFlowElem')
               ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=ndxi_merge)
               call check_error(ierr, 'Flow Elem count')
               ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
               call check_error(ierr, 'center of mass x-coordinate')
               ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
               call check_error(ierr, 'center of mass y-coordinate')

               call realloc(xmc, ndxi_merge, keepExisting=.false.)
               call realloc(ymc, ndxi_merge, keepExisting=.false.)
               ierr = nf90_get_var(imapfile, id_xzw, xmc)
               ierr = nf90_get_var(imapfile, id_yzw, ymc)

               Ns = 0
               call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, um%ndxi_ghost, um%inode_ghost, um%inodeghost_merge, 1, 1)

               ! read coordinates of flow links center
               ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
               call check_error(ierr, 'nFlowLink')
               ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=lnx_merge)
               call check_error(ierr, 'link count')
               ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
               ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
               if (nerr_ > 0) return

               call realloc(xuu, lnx_merge, keepExisting=.false.)
               call realloc(yuu, lnx_merge, keepExisting=.false.)
               ierr = nf90_get_var(imapfile, id_xu, xuu)
               ierr = nf90_get_var(imapfile, id_yu, yuu)

               call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, um%lnx_ghost, um%ilink_ghost, um%ilinkghost_merge, 0, 1)
            end if
         else ! If the partitions in the model are different comparing with the rst file
            um%ndxi_own = 0
            um%lnx_own = 0
            call realloc(um%inode_own, ndxi, keepExisting=.false.)
            call realloc(um%inode_merge, ndxi, keepExisting=.false.)
            call realloc(um%ilink_own, lnx, keepExisting=.false.)
            call realloc(um%ilink_merge, lnx, keepExisting=.false.)
            call realloc(inode_owninv, ndxi, keepExisting=.false., fill=-999)

            if (jampi == 0) then ! Restart a sequential run with a merged rst file
               ndxbnd_own = ndx - ndxi
               if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
                  call realloc(ibnd_own, ndxbnd_own, keepExisting=.false.)
               end if
            end if
            if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
               call realloc(um%ibnd_merge, ndxbnd_own, keepExisting=.false.)
            end if

        !!! Set inode_own, ilink_own, etc
            ! when sequential run
            if (jampi == 0) then
               ! node
               um%ndxi_own = ndxi
               ndxi_all = um%ndxi_own
               do kk = 1, ndxi
                  um%inode_own(kk) = kk
               end do
               ! bnd
               um%nbnd_read = ndx - ndxi ! Sequential restart, nbnd_read is assumed to be ndx-ndxi
               if (um%nbnd_read > 0 .and. jaoldrstfile == 0) then
                  do kk = 1, um%nbnd_read
                     ibnd_own(kk) = kk
                  end do
               end if
               ! link
               um%lnx_own = lnx
               lnx_all = um%lnx_own
               do LL = 1, lnx
                  um%ilink_own(LL) = LL
               end do
            else
               ! parallel
               if (um%jafillghost == 0) then
                  ! only consider non-ghost flow nodes
                  do kk = 1, ndxi
                     if (idomain(kk) == my_rank) then
                        um%ndxi_own = um%ndxi_own + 1
                        um%inode_own(um%ndxi_own) = kk
                        inode_owninv(kk) = um%ndxi_own
                     end if
                  end do
                  ! link
                  do LL = 1, lnx
                     call link_ghostdata(my_rank, idomain(ln(1, LL)), idomain(ln(2, LL)), jaghost, um%idmn_ghost, &
                                         ighostlev(ln(1, LL)), ighostlev(ln(2, LL)))
                     if (jaghost /= 1) then
                        um%lnx_own = um%lnx_own + 1
                        um%ilink_own(um%lnx_own) = LL
                     end if
                  end do
               else ! need to fill ghosts
                  um%ndxi_ghost = 0
                  um%lnx_ghost = 0
                  call realloc(um%inode_ghost, ndxi, keepExisting=.false.)
                  call realloc(um%inodeghost_merge, ndxi, keepExisting=.false.)
                  call realloc(um%ilink_ghost, lnx, keepExisting=.false.)
                  call realloc(um%ilinkghost_merge, lnx, keepExisting=.false.)

                  do kk = 1, ndxi
                     if (idomain(kk) == my_rank) then
                        um%ndxi_own = um%ndxi_own + 1
                        um%inode_own(um%ndxi_own) = kk
                        inode_owninv(kk) = um%ndxi_own
                     else
                        um%ndxi_ghost = um%ndxi_ghost + 1
                        um%inode_ghost(um%ndxi_ghost) = kk
                     end if
                  end do

                  do LL = 1, lnx
                     call link_ghostdata(my_rank, idomain(ln(1, LL)), idomain(ln(2, LL)), jaghost, um%idmn_ghost, &
                                         ighostlev(ln(1, LL)), ighostlev(ln(2, LL)))
                     if (jaghost /= 1) then
                        um%lnx_own = um%lnx_own + 1
                        um%ilink_own(um%lnx_own) = LL
                     else
                        um%lnx_ghost = um%lnx_ghost + 1
                        um%ilink_ghost(um%lnx_ghost) = LL
                     end if
                  end do
               end if
               ! compute global number of nodes/lnx of all subdomains
               call reduce_int_sum(um%ndxi_own, ndxi_all)
               call reduce_int_sum(um%lnx_own, lnx_all)
            end if

       !! read and prepare for inode_merge and ilink_merge, etc
            ! Read coordinates of flow elem circumcenters from merged map file
            ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
            call check_error(ierr, 'nFlowElem')
            ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=ndxi_merge)
            call check_error(ierr, 'Flow Elem count')

            ! Check if global number of nodes in the merged rst file is equal to that in the model
            if (ndxi_all /= ndxi_merge) then
               write (msgbuf, '(a,i0,a,i0,a)') 'Global number of nodes among all partitions: in the merged restart file ', ndxi_merge, ',  in model: ', ndxi_all, '.'
               call warn_flush()
               call qnerror('Global number of nodes read from the merged restart file unequal to global number of nodes in model,' &
                            //' therefore some nodes may not be found', ' ', ' ')
            end if

            ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
            call check_error(ierr, 'center of mass x-coordinate')
            ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
            call check_error(ierr, 'center of mass y-coordinate')
            if (ndxbnd_own > 0) then
               ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', um%id_bnddim)
               if (ierr == 0) then
                  jaoldrstfile = 0
                  call check_error(ierr, 'nFlowElemBnd')
                  ierr = nf90_inquire_dimension(imapfile, um%id_bnddim, len=ndxbnd_merge)
                  call check_error(ierr, 'Flow Elem bnd count')
                  ierr = nf90_inq_varid(imapfile, 'FlowElem_xbnd', id_xbnd)
                  call check_error(ierr, 'x-coordinate of boundary points')
                  ierr = nf90_inq_varid(imapfile, 'FlowElem_ybnd', id_ybnd)
                  call check_error(ierr, 'y-coordinate of boundary points')
               else
                  jaoldrstfile = 1
                  ierr = 0
               end if
            end if

            if (ierr == nf90_noerr) then
               ! centers of mass were stored in rst/map file, so directly read them
               call realloc(xmc, ndxi_merge, keepExisting=.false.)
               call realloc(ymc, ndxi_merge, keepExisting=.false.)
               ierr = nf90_get_var(imapfile, id_xzw, xmc)
               call check_error(ierr, 'xmc')
               ierr = nf90_get_var(imapfile, id_yzw, ymc)
               call check_error(ierr, 'ymc')
               if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
                  call realloc(xbnd_read, ndxbnd_merge, keepExisting=.false.)
                  call realloc(ybnd_read, ndxbnd_merge, keepExisting=.false.)
                  ierr = nf90_get_var(imapfile, id_xbnd, xbnd_read)
                  call check_error(ierr, 'xbnd_read')
                  ierr = nf90_get_var(imapfile, id_ybnd, ybnd_read)
                  call check_error(ierr, 'ybnd_read')
               end if
            end if

            if (nerr_ > 0) return

            call realloc(inode_merge2own, ndxi_merge, keepExisting=.false., fill=-999)
            call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, um%ndxi_own, um%inode_own, um%inode_merge, 1, 1, inode_merge2own)

            if (ndxbnd_own > 0 .and. jaoldrstfile == 0) then
               ! For parallel run, 'ibnd_own' and 'ndxbnd_own' has been determined in function 'flow_initexternalforcings'
               call find_flownodesorlinks_merge(ndxbnd_merge, xbnd_read, ybnd_read, ndx - ndxi, ndxbnd_own, ibnd_own, um%ibnd_merge, 2, 1)
            end if
            ! read coordinates of flow links center
            ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
            call check_error(ierr, 'nFlowLink')
            ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=lnx_merge)
            call check_error(ierr, 'link count')
            ! ! Check if global number of links in the merged rst file is equal to that in the model
            if (lnx_all /= lnx_merge) then
               write (msgbuf, '(a,i0,a,i0,a)') 'Global number of links among all partitions: in the merged restart file ', lnx_merge, ',  in model: ', lnx_all, '.'
               call warn_flush()
               call qnerror('Global number of links read from the merged restart file unequal to global number of links in model, ' &
                            //' therefore some links may not be found', ' ', ' ')
            end if
            ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
            call check_error(ierr, 'FlowLink_xu')
            ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
            call check_error(ierr, 'FlowLink_yu')

            call realloc(xuu, lnx_merge, keepExisting=.false.)
            call realloc(yuu, lnx_merge, keepExisting=.false.)
            ierr = nf90_get_var(imapfile, id_xu, xuu)
            call check_error(ierr, 'xuu')
            ierr = nf90_get_var(imapfile, id_yu, yuu)
            call check_error(ierr, 'yuu')

            if (nerr_ > 0) return

            call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, um%lnx_own, um%ilink_own, um%ilink_merge, 0, 1)

            if (um%jafillghost == 1) then
               call find_flownodesorlinks_merge(ndxi_merge, xmc, ymc, ndxi, um%ndxi_ghost, um%inode_ghost, um%inodeghost_merge, 1, 1)
               call find_flownodesorlinks_merge(lnx_merge, xuu, yuu, lnx, um%lnx_ghost, um%ilink_ghost, um%ilinkghost_merge, 0, 1)
            end if

            if (jampi == 0) then
               if (Ns > 0) then
                  call newfil(MSAM, 'rst_error.xyz')
                  call wrisam(MSAM)
                  !         delete samples
                  Ns = 0
                  call mess(LEVEL_ERROR, 'restart error, unfound nodes/links are written to sample files rst_error.xyz')
               end if
            else
               if (Ns > 0) then
                  call newfil(MSAM, 'rst_error_'//sdmn//'.xyz')
                  call wrisam(MSAM)
                  call mess(LEVEL_WARN, 'restart error, unfound nodes/links are written to sample files rst_error_'//sdmn//'.xyz')
               end if

!         get maximum number of flownodes with error over all subdomains
               call reduce_key(Ns)

               if (Ns > 0) then
                  call mess(LEVEL_ERROR, 'restart error, please check sample files rst_error_NNNN.xyz')
               end if

               !      delete samples
               Ns = 0
            end if

            deallocate (xmc, ymc, xuu, yuu)
            if (ndxbnd_own > 0 .and. jaoldrstfile == 0) deallocate (xbnd_read, ybnd_read)
         end if
      else ! If rst file is a non-merged rst file
         ! NOTE: intentional: if jampi==1, but rst file is a normal separate rst file
         !       *per* partition, just read all ndxi/lnx, including ghost nodes/links (as before)
         um%ndxi_own = ndxi
         um%lnx_own = lnx

         nerr_ = 0

         ! Ask file for dimension id of nodes and edges
         ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim) ! Intentional: read a map/rst is *without* boundary nodes.
         ! (so don't read nFlowElemWithBnd)
         call check_error(ierr, 'nFlowElem')
         ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
         call check_error(ierr, 'nFlowLink')
         if (nerr_ > 0) return

         ! Ask for dimensions of nodes and edges, ergo: the number of netnodes and netlinks
         ierr = nf90_inquire_dimension(imapfile, id_flowelemdim, len=um%ndxi_read)
         call check_error(ierr, 'elem count')
         ierr = nf90_inquire_dimension(imapfile, id_flowlinkdim, len=um%lnx_read)
         call check_error(ierr, 'link count')
         if (nerr_ > 0) return

         ! Ask file for the dimension of its own boundary points
         ierr = nf90_inq_dimid(imapfile, 'nFlowElemBnd', um%id_bnddim)
         if (ierr == 0) then
            ierr = nf90_inquire_dimension(imapfile, um%id_bnddim, len=um%nbnd_read)
            call check_error(ierr, 'FlowElem_bnd count')
            jaoldrstfile = 0
         else
            call mess(LEVEL_INFO, 'The restart file does not contain waterlevel info. on boundaries')
            ierr = 0
            um%nbnd_read = 0
            jaoldrstfile = 1
         end if
      end if

      ! check if restarting a model with Riemann boundary conditions
      if (allocated(kbndz)) then
         if (any(kbndz(4, :) == 5) .and. jaoldrstfile == 1) then
            call mess(LEVEL_WARN, 'When restarting a model with Riemann boundary conditions, the restart file should be ' &
                      //'a *_rst file which contains waterlevel info. on boundaries. Otherwise FM still runs but the results are ' &
                      //'not accurate.')
         end if
      end if

      if (um%jamergedmap_same == 1) then
         if (um%ndxi_read /= um%ndxi_own .or. um%lnx_read /= um%lnx_own) then
            if (jampi > 0 .and. um%jamergedmap == 1) then
               success = .true.
               um%jamergedmap_same = 0
               return
            end if
            tmpstr = ''
            if (jampi == 1) then
               write (tmpstr, '(a,i0,a)') 'my_rank=', my_rank, ': '
            end if
            write (msgbuf, '(a,i0,a,i0,a)') trim(tmpstr)//'#nodes in file: ', um%ndxi_read, ', #nodes in model: ', um%ndxi_own, '.'
            call warn_flush()
            write (msgbuf, '(a,i0,a,i0,a)') trim(tmpstr)//'#links in file: ', um%lnx_read, ', #links in model: ', um%lnx_own, '.'
            call warn_flush()
            call qnerror('Error reading '''//trim(filename)//''': Number of nodes/links read unequal to nodes/links in model', ' ', ' ')
            ierr = DFM_GENERICERROR
            call readyy('Reading map data', -1d0)
            return
         end if
         if (um%nbnd_read > 0 .and. jaoldrstfile == 0) then
            if ((jampi == 0 .and. um%nbnd_read /= ndx - ndxi) .or. (jampi > 0 .and. um%nbnd_read /= ndxbnd_own)) then
               if (jampi > 0 .and. um%jamergedmap == 1) then
                  success = .true.
                  um%jamergedmap_same = 0
                  return
               end if
               write (msgbuf, '(a,i0,a,i0,a)') '#boundary points in file: ', um%nbnd_read, ', #boundary points in model: ', ndx - ndxi, '.'
               call warn_flush()
               call qnerror('Number of boundary points unequal to those in model', ' ', ' ')
               ierr = DFM_GENERICERROR
               call readyy('Reading map data', -1d0)
               return
            end if
         end if

         if (um%jamergedmap == 0) then
          !! check if the flownodes/flowlinks numbering index is the same
            ! only check when sequential restart and parallel restart with its own rst file.
            ! TODO: check also for other restart situations
            ! Read coordinates of flownodes
            call realloc(xmc, um%ndxi_read, keepExisting=.false.)
            call realloc(ymc, um%ndxi_read, keepExisting=.false.)
            ierr = nf90_inq_varid(imapfile, 'FlowElem_xzw', id_xzw)
            if (ierr == nf90_noerr) call check_error(ierr, 'center of mass x-coordinate')
            if (ierr == nf90_noerr) ierr = nf90_inq_varid(imapfile, 'FlowElem_yzw', id_yzw)
            if (ierr == nf90_noerr) call check_error(ierr, 'center of mass y-coordinate')

            if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_xzw, xmc)
            if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_yzw, ymc)

            if (ierr == nf90_noerr) then
               ! check flownodes numbering with rst file
               call check_flownodesorlinks_numbering_rst(ndxi, 1, xmc, ymc, ierr)
               if (ierr /= nf90_noerr) then
                  return
               end if
            else
               call mess(LEVEL_WARN, 'Skip checking flownodes numbering when restart, ' &
                         //'because flownodes coordinates are missing in rst file.')
            end if

            ! Read coordinates of flowlinks
            call realloc(xuu, um%lnx_read, keepExisting=.false.)
            call realloc(yuu, um%lnx_read, keepExisting=.false.)
            ierr = nf90_inq_varid(imapfile, 'FlowLink_xu', id_xu)
            call check_error(ierr, 'velocity point x-coordinate')
            ierr = nf90_inq_varid(imapfile, 'FlowLink_yu', id_yu)
            call check_error(ierr, 'velocity point y-coordinate')

            if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_xu, xuu)
            if (ierr == nf90_noerr) ierr = nf90_get_var(imapfile, id_yu, yuu)

            if (ierr == nf90_noerr) then
               ! Check flowlinks numbering with rst file
               call check_flownodesorlinks_numbering_rst(lnx, 0, xuu, yuu, ierr)
               if (ierr /= nf90_noerr) then
                  return
               end if
            else
               ierr = 0 ! as it a warning
               call mess(LEVEL_WARN, 'Skip checking flowlinks numbering when restart, ' &
                         //'because flowlinks coordinates are missing in rst file.')
            end if
         end if
      end if

      success = .true.
   end function unc_read_merged_map

! Write input coordinates of all structures of input structuretype to open history file
   subroutine unc_write_struc_input_coordinates(ihisfile, structuretype)
      use m_structures
      use m_globalparameters
      use simple_geometry, only: sgeom_def_geometry_variables

      integer, intent(in) :: structuretype ! Structure type, see: m_globalparameters
      integer, intent(in) :: ihisfile ! Handle to already open history file

      double precision, allocatable :: geomXStrucInput(:), geomYStrucInput(:)
      integer, allocatable :: nNodesStrucInput(:)
      integer :: ierr, nNodeTot, nstruc
      integer :: id_Strucgendim_input, id_Strucgeom_input_node_count, id_Strucgeom_input_node_coordx, id_Strucgeom_input_node_coordy
      character(len=255) :: structstring
      integer :: jahis

      ierr = 0
      select case (structuretype)
      case (ST_WEIR)
         structstring = 'weir'
         jahis = jahisweir
      case (ST_UNI_WEIR)
         structstring = 'uniweir'
         jahis = jahisuniweir
      case (ST_CULVERT)
         structstring = 'culvert'
         jahis = jahisculv
      case (ST_BRIDGE)
         structstring = 'bridge'
         jahis = jahisbridge
      case (ST_PUMP)
         structstring = 'pump'
         jahis = jahispump
      case (ST_ORIFICE)
         structstring = 'orifice'
         jahis = jahisorif
      case (ST_GATE)
         structstring = 'gate'
         jahis = jahisgate
      case (ST_GENERAL_ST)
         structstring = 'general_structure'
         jahis = jahiscgen
      case default
         return
      end select

      if (jahis > 0) then
         call get_input_coordinates_of_structure(structuretype, geomXStrucInput, geomYStrucInput, nNodesStrucInput, nNodeTot, nstruc)
         if (nstruc > 0) then

            ierr = nf90_redef(ihisfile)
            ierr = nf90_def_dim(ihisfile, trim(structstring)//'_input', nstruc, id_strucgendim_input)
            ierr = sgeom_def_geometry_variables(ihisfile, trim(structstring)//'_input_geom', structstring, 'line', nNodeTot, id_strucgendim_input, &
                                                id_strucgeom_input_node_count, id_strucgeom_input_node_coordx, id_strucgeom_input_node_coordy)
            ierr = nf90_enddef(ihisfile)

            ierr = nf90_put_var(ihisfile, id_Strucgeom_input_node_coordx, geomXStrucInput, start=(/1/), count=(/nNodeTot/))
            ierr = nf90_put_var(ihisfile, id_Strucgeom_input_node_coordy, geomYStrucInput, start=(/1/), count=(/nNodeTot/))
            ierr = nf90_put_var(ihisfile, id_Strucgeom_input_node_count, nNodesStrucInput, start=(/1/), count=(/nstruc/))

            if (allocated(geomXStrucInput)) then
               deallocate (geomXStrucInput)
            end if
            if (allocated(geomYStrucInput)) then
               deallocate (geomYStrucInput)
            end if
            if (allocated(nNodesStrucInput)) then
               deallocate (nNodesStrucInput)
            end if

         end if
      end if

   end subroutine unc_write_struc_input_coordinates

!> Writes the unstructured flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
   subroutine unc_write_flowgeom(filename)
      character(len=*), intent(in) :: filename

      integer :: igeomfile, ierr

      ierr = unc_create(filename, 0, igeomfile)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      call unc_write_flowgeom_filepointer(igeomfile) ! UNC_CONV_CFOLD

      ierr = unc_close(igeomfile)
   end subroutine unc_write_flowgeom

!> Writes the unstructured network and flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
   subroutine unc_write_net_flowgeom(filename)
      character(len=*), intent(in) :: filename

      integer :: igeomfile, ierr

      ierr = unc_create(filename, 0, igeomfile)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      call unc_write_net_filepointer(igeomfile) ! Write standard net data as well
      call unc_write_flowgeom_filepointer(igeomfile) ! UNC_CONV_CFOLD

      ierr = unc_close(igeomfile)
   end subroutine unc_write_net_flowgeom

!> Writes the unstructured network and flow geometry to a netCDF file.
!! If file exists, it will be overwritten.
! TODO: consider removing/replacing by flowgeom_ugrid, IF network data writing is not necessary anymore as a separate call. AvD
   subroutine unc_write_net_flowgeom_ugrid(filename)
      implicit none

      character(len=*), intent(in) :: filename

      integer :: ierr
      type(t_unc_mapids) :: geomids

      ierr = unc_create(filename, 0, geomids%ncid)
      if (ierr /= nf90_noerr) then
         call mess(LEVEL_ERROR, 'Could not create flow geometry file '''//trim(filename)//'''.')
         call check_error(ierr)
         return
      end if

      call unc_write_flowgeom_filepointer_ugrid(geomids%ncid, geomids%id_tsp) ! UNC_CONV_UGRID

      ierr = unc_close(geomids%ncid)
   end subroutine unc_write_net_flowgeom_ugrid

!> Fills the given arrays for all edges in the 2D mesh, ordered as follows: first internal flow links, then boundary flow links, then closed net links.
   subroutine get_2d_edge_data(edge_nodes, edge_faces, edge_type, xue, yue, edge_mapping_table, reverse_edge_mapping_table)
      use network_data
      use m_flowgeom

      implicit none

      integer, intent(out) :: edge_nodes(:, :) !< Edge node connectivity array to be filled.
      integer, pointer, intent(in) :: edge_faces(:, :) !< Edge face connectivity array to be filled (uses -999 as fill value).
      integer, intent(out) :: edge_type(:) !< Edge type array to be filled.
      real(kind=dp), intent(out) :: xue(:) !< Edge x coordinate array to be filled.
      real(kind=dp), intent(out) :: yue(:) !< Edge y coordinate array to be filled.
      integer, optional, intent(out) :: edge_mapping_table(:) !< Mapping from original edges to ordered edges (first flow links, then closed edges). To be filled if present.
      integer, optional, intent(out) :: reverse_edge_mapping_table(:) !< Mapping from ordered edges (first flow links, then closed edges) to original edges. To be filled if present.

      integer :: is, i, L, Lf !< Counters.
      logical :: is_lne2ln_allocated, is_edge_faces_associated, is_edge_mapping_table_present, is_reverse_edge_mapping_table_present

      is_lne2ln_allocated = allocated(lne2ln)
      is_edge_faces_associated = associated(edge_faces)
      is_edge_mapping_table_present = present(edge_mapping_table)
      is_reverse_edge_mapping_table_present = present(reverse_edge_mapping_table)

      ! set LC mask to 0
      LC = 0

      ! Write all edges that are 2D internal flow links.
      i = 0
      ! Lf is flow link number.
      do Lf = lnx1d + 1, lnxi

         L = ln2lne(Lf)
         if (LC(L) /= 0) then
            cycle
         end if
         LC(L) = 1

         ! i is edge number.
         i = i + 1

         edge_nodes(1:2, i) = lncn(1:2, Lf)
         if (is_edge_faces_associated) edge_faces(1:2, i) = ln(1:2, Lf)

         edge_type(i) = UG_EDGETYPE_INTERNAL
         xue(i) = xu(Lf)
         yue(i) = yu(Lf)

         if (is_edge_mapping_table_present) edge_mapping_table(L - numl1d) = i
         if (is_reverse_edge_mapping_table_present) reverse_edge_mapping_table(i) = L - numl1d
      end do

      ! Write all edges that are 2D boundary flow links.
      ! Lf is flow link number.
      do Lf = lnx1Db + 1, lnx

         L = ln2lne(Lf)
         if (LC(L) /= 0) then
            cycle
         end if
         LC(L) = 1

         ! i is edge number.
         i = i + 1

         edge_nodes(1:2, i) = lncn(1:2, Lf)
         if (is_edge_faces_associated) then
            ! NOTE: the internal face intentionally gets placed on index 1,
            ! even though the flow link has it on index 2 by definition.
            edge_faces(1, i) = ln(2, Lf)
            edge_faces(2, i) = -999
         end if

         edge_type(i) = UG_EDGETYPE_BND
         xue(i) = xu(Lf)
         yue(i) = yu(Lf)

         if (is_edge_mapping_table_present) edge_mapping_table(L - numl1d) = i
         if (is_reverse_edge_mapping_table_present) reverse_edge_mapping_table(i) = L - numl1d
      end do

      ! Write all remaining edges, which are closed.
      ! Loop over all 2D net links, which includes both 2D flow links and closed 2D net links.
      ! L is net link number
      if (is_lne2ln_allocated) then
         do L = NUML1D + 1, NUML

            ! Lf is flow link number.
            Lf = lne2ln(L)

            if (Lf <= 0) then ! If this net link does not have a flow link (i.e. closed net link).

               if (LC(L) /= 0) then
                  cycle
               end if
               LC(L) = 1

               ! i is edge number.
               i = i + 1
               edge_nodes(1:2, i) = KN(1:2, L)
               if (lnn(L) < 2) then
                  edge_type(i) = UG_EDGETYPE_BND_CLOSED
               else if (kn(3, L) == 0) then
                  edge_type(i) = UG_EDGETYPE_INTERNAL_CLOSED
               end if

               if (is_edge_faces_associated) then
                  do is = 1, 2
                     if (lne(is, L) > 0) then
                        edge_faces(is, i) = lne(is, L)
                     else
                        edge_faces(is, i) = -999
                     end if
                  end do
               end if

               ! Edge coordinate is in the middle of the net link.
               xue(i) = .5d0 * (xk(kn(1, L)) + xk(kn(2, L)))
               yue(i) = .5d0 * (yk(kn(1, L)) + yk(kn(2, L)))

               if (is_edge_mapping_table_present) edge_mapping_table(L - numl1d) = i
               if (is_reverse_edge_mapping_table_present) reverse_edge_mapping_table(i) = L - numl1d
            end if

         end do
      end if

      ! restore the mask
      LC = 0

   end subroutine get_2d_edge_data

!> Sets layer info in the given variables. Only call this if layers present.
!! The returned layer_type and arrays can be passed to io_ugrid writing routines.
   subroutine get_layer_data_ugrid(layer_count, layer_type, layer_zs, interface_zs)
      use m_alloc
      use m_missing
      use m_flow, only: laytyp, LAYTP_SIGMA, LAYTP_Z, zslay, numtopsig
      use io_ugrid, only: LAYERTYPE_OCEANSIGMA, LAYERTYPE_Z, LAYERTYPE_OCEAN_SIGMA_Z

      implicit none

      integer, intent(in) :: layer_count !< Number of layers.
      integer, intent(out) :: layer_type !< UGRID layer type (sigma or z or sigma-z) to be determined, one of io_ugrid's constants.
      real(kind=dp), dimension(:), intent(out) :: layer_zs !< Vertical layer center coordinates array to be filled.
      real(kind=dp), dimension(:), intent(out) :: interface_zs !< Vertical layer interface coordinates array to be filled.
      character(len=255) :: message !< Temporary variable for writing log messages.
      real(kind=dp) :: dsig
      integer :: i

      ! Create rank 1 array with vertical layer coordinates (not per flow node).
      ! NOTE: dfm sigma values (positive upwards, bedlevel=0, eta=1) will be converted
      !       to CF-compliant ocean sigma (positive upwards, bedlevel=-1, eta=0) coordinates.
      if (laytyp(1) == LAYTP_SIGMA) then
         interface_zs(1:layer_count + 1) = zslay(0:layer_count, 1) - 1d0
         layer_type = LAYERTYPE_OCEANSIGMA
      elseif (laytyp(1) == LAYTP_Z) then
         if (numtopsig == 0) then
            ! Fixed z coordinates.
            interface_zs(1:layer_count + 1) = zslay(0:layer_count, 1)
            layer_type = LAYERTYPE_Z
         else
            ! sigma over z coordinates
            ! NOTE: the sigma levels below are only correct for janumtopsiguniform == 1
            dsig = 1d0 / numtopsig
            interface_zs(1:layer_count + 1 - numtopsig) = zslay(0:layer_count - numtopsig, 1)
            interface_zs(layer_count + 2 - numtopsig:layer_count + 1) = dsig*[(i, i=1, numtopsig)] - 1d0
            layer_type = LAYERTYPE_OCEAN_SIGMA_Z
         end if
      else
         write (message, *) 'Unsupported layer type: ', laytyp(1), '. Layer coordinate variables will not be written.'
         call mess(LEVEL_WARN, trim(message))
         layer_type = -1
         return
      end if

      ! Layer center coordinates.
      layer_zs(1:layer_count) = .5d0 * (interface_zs(1:layer_count) + interface_zs(2:layer_count + 1))

      if (laytyp(1) == LAYTP_Z .and. numtopsig > 0) then
         ! Repair the first sigma layer center in a z-sigma combined layering, which was wrongly calculated in above expression.
         layer_zs(layer_count + 1 - numtopsig) = .5d0 * dsig - 1d0
      end if

   end subroutine get_layer_data_ugrid

!> Writes the unstructured flow geometry in UGRID format to an already opened netCDF dataset.
   subroutine unc_write_flowgeom_filepointer_ugrid(ncid, id_tsp, jabndnd, jafou, ja2D)

      use m_flowgeom
      use network_data
      use m_sferic
      use m_missing
      use netcdf
      use m_partitioninfo
      use m_flow, only: kmx, mxlaydefs, laymx, numtopsig, s1max
      use m_alloc
      use dfm_error
      use m_save_ugrid_state !stores the contactname and other saved ugrid names
      use m_CrossSections
      use m_flowparameters, only: jafullgridoutput
      use m_flowtimes, only: handle_extra
      use Timers
      use m_modelbounds
      use io_netcdf_acdd, only: ionc_add_geospatial_bounds
      use fm_location_types
      implicit none

      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(inout) :: id_tsp !< Set of time and space related variable id's
      integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.
      logical, optional, intent(in) :: jaFou !< Whether this flowgeom writing is part of a Fourier file or not (affects 3D layer writing)
      logical, optional, intent(in) :: ja2D !< Whether to include the 2D grid (default = .true.)

      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last 2/3D node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1D node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.
      integer :: ndx1d !< Number of internal 1D nodes.

      integer :: nn
      integer, allocatable :: edge_nodes(:, :), face_nodes(:, :), edge_type(:), contacts(:, :)
      integer, dimension(:, :), pointer :: edge_faces => null()
      integer :: layer_count, layer_type
   !! Geometry options
      integer, parameter :: LAYERTYPE_OCEAN_SIGMA = 1 !< Dimensionless vertical ocean sigma coordinate.
      integer, parameter :: LAYERTYPE_Z = 2 !< Vertical coordinate for fixed z-layers.
      integer, parameter :: LAYERTYPE_OCEAN_SIGMA_Z = 3 !< Combined Z-Sigma layers
      real(kind=dp), dimension(:), pointer :: layer_zs => null(), interface_zs => null()
      character(len=10) :: waterlevelname, bldepthname
      logical :: jafou_
      logical :: ja2D_
!   type(t_crs) :: pj

      integer :: ierr
      integer :: i, numContPts, numNodes, n, numl2d, L
      integer :: jaInDefine
      integer :: n1dedges, n1d2dcontacts, numk2d, start_index
      integer, allocatable :: contacttype(:)

      ! re-mapping of 1d mesh coordinates for UGrid
      double precision, allocatable :: xue(:), yue(:)
      ! re-mapping of 2d mesh coordinates for UGrid
      double precision, allocatable :: x2dn(:), y2dn(:), z2dn(:)
      integer :: netNodeReMappedIndex, nnSize

      jaInDefine = 0
      n1d2dcontacts = 0
      n1dedges = 0
      start_index = 1

      if (ndxi <= 0) then
         call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
         return
      end if

      if (timon) call timstrt("unc_write_flowgeom_filepointer_ugrid", handle_extra(69))
      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      ! Include boundary cells in output (ndx) or not (ndxi)
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      if (present(jaFou)) then
         jafou_ = jaFou
      else
         jafou_ = .false.
      end if

      if (present(ja2D)) then
         ja2D_ = ja2D
      else
         ja2D_ = .true.
      end if

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
         call check_error(ierr)
         return
      end if

      if (jsferic == 1) then
         crs%epsg_code = 4326
      end if

      ! Get layer info.
      if (kmx <= 0) then ! If no layers present.
         layer_count = 0
         layer_type = -1
         ! Leave layer_zs and interface_zs unallocated, since they will not be used in this case.
      else ! If layers present.
         if (mxlaydefs > 1) then
            call mess(LEVEL_WARN, 'Multiple layer definitions cannot be handled for layer variables. Layer variables will not be written.')
            ierr = DFM_NOTIMPLEMENTED
            goto 888
         else
            layer_count = laymx(1)
            call reallocP(layer_zs, layer_count, fill=dmiss, keepExisting=.false.)
            call reallocP(interface_zs, layer_count + 1, fill=dmiss, keepExisting=.false.)
            call get_layer_data_ugrid(layer_count, layer_type, layer_zs, interface_zs)
         end if
      end if

      n1d2dcontacts = 0

      call unc_write_1D_flowgeom_ugrid(id_tsp, ncid, jabndnd_, jafou_, ja2D_, contacts, contacttype, n1d2dcontacts)
      numk2d = 0
      ndx1d = ndxi - ndx2d
      if (ndx2d > 0 .and. ja2D_) then ! 2D flow geometry
         numl2d = numl - numl1d
         numk2d = (numk - n1d2dcontacts) - ndx1d
         call realloc(edge_nodes, (/2, numl2d/), fill=-999, keepExisting=.false.)
         call reallocP(edge_faces, (/2, numl2d/), fill=-999)
         call realloc(edge_type, numl2d, fill=-999, keepExisting=.false.)
         call realloc(xue, numl2d, fill=dmiss, keepExisting=.false.)
         call realloc(yue, numl2d, fill=dmiss, keepExisting=.false.)
         call realloc(x2dn, numk2d, fill=dmiss, keepExisting=.false.)
         call realloc(y2dn, numk2d, fill=dmiss, keepExisting=.false.)
         call realloc(z2dn, numk2d, fill=dmiss, keepExisting=.false.)
         call get_2d_edge_data(edge_nodes, edge_faces, edge_type, xue, yue)

         ! Determine max nr of vertices and contour points
         numNodes = 0
         numContPts = 0 ! TODO: AvD: contour points equals nodes here, remove, OR move to 1D
         do i = 1, ndxndxi
            numNodes = max(numNodes, size(nd(i)%nod))
            numContPts = max(numContPts, size(nd(i)%x))
         end do

         ! Note: AvD: for cell corners, we write *all* net nodes (numk). This may also be '1D' nodes, but that is not problematic: they will simply not be referenced in face_nodes/edge_nodes.
         ! Note: AvD: numk may be larger than nr of cell corners. Will cause problems when writing output data on corners (mismatch in dimensions), not crucial now.
         call realloc(face_nodes, (/numNodes, ndx2d/), fill=-999)

         ! re-mapping by edge nodes is needed, use kc as table
         kc = 0
         netNodeReMappedIndex = 0
         do l = 1, numl2d
            nn = edge_nodes(1, l)
            if (nn > 0) then
               if (kc(nn) == 0) then
                  netNodeReMappedIndex = netNodeReMappedIndex + 1
                  x2dn(netNodeReMappedIndex) = xk(nn)
                  y2dn(netNodeReMappedIndex) = yk(nn)
                  z2dn(netNodeReMappedIndex) = zk(nn)
                  kc(nn) = netNodeReMappedIndex
               end if
            end if
            nn = edge_nodes(2, l)
            if (nn > 0) then
               if (kc(nn) == 0) then
                  netNodeReMappedIndex = netNodeReMappedIndex + 1
                  x2dn(netNodeReMappedIndex) = xk(nn)
                  y2dn(netNodeReMappedIndex) = yk(nn)
                  z2dn(netNodeReMappedIndex) = zk(nn)
                  kc(nn) = netNodeReMappedIndex
               end if
            end if
         end do

         !remapped edge_nodes
         do l = 1, numl2d
            edge_nodes(1, l) = kc(edge_nodes(1, l))
            edge_nodes(2, l) = kc(edge_nodes(2, l))
         end do

         !remapped face_nodes
         do n = 1, ndx2d
            nnSize = size(nd(n)%nod)
            do i = 1, nnSize
               nn = nd(n)%nod(i)
               if (nn > 0) then
                  face_nodes(i, n) = kc(nn)
               end if
            end do
         end do
         ! face_nodes does not need to be re-mapped: 2d cells come first
         ! TODO: AvD: lnx1d+1:lnx includes open bnd links, which may *also* be 1D boundaries (don't want that in mesh2d)
         ! note edge_faces does not need re-indexing, cell number are flow variables and 2d comes first

         if (jafullgridoutput == 0) then
            unc_writeopts = ior(unc_writeopts, UG_WRITE_LYRVAR)
         else
            unc_writeopts = iand(unc_writeopts, not(UG_WRITE_LYRVAR))
         end if

         waterlevelname = 's1'
         bldepthname = 'bldepth'
         if (layer_type == LAYERTYPE_OCEAN_SIGMA_Z .or. layer_type == LAYERTYPE_OCEAN_SIGMA) then
            if (jafou_) then
               waterlevelname = 's1max'
            end if
         end if

         ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids2d, mesh2dname, 2, UG_LOC_EDGE + UG_LOC_FACE, numk2d, numl2d, ndx2d, numNodes, &
                                     edge_nodes, face_nodes, edge_faces, null(), null(), x2dn, y2dn, xue, yue, xz(1:ndx2d), yz(1:ndx2d), &
                                     crs, -999, dmiss, start_index, layer_count, layer_type, &
                                     layer_zs=layer_zs, interface_zs=interface_zs, &
                                     nsigma_opt=numtopsig, &
                                     waterlevelname=trim(waterlevelname), bldepthname=trim(bldepthname), &
                                     writeopts=unc_writeopts)

         ! Add edge type variable (edge-flowlink relation)
         call write_edge_type_variable(ncid, id_tsp%meshids2d, mesh2dname, edge_type)

         ! Write optionally required bldepth and when needed s1max arrays for sigma- and sigma-z layer models
         if (layer_type == LAYERTYPE_OCEAN_SIGMA_Z .or. layer_type == LAYERTYPE_OCEAN_SIGMA) then
            ierr = nf90_def_var(ncid, trim(mesh2dname)//'_'//trim(bldepthname), nf90_double, id_tsp%meshids2d%dimids(mdim_face), id_tsp%id_bldepth(2))
            ierr = nf90_put_att(ncid, id_tsp%id_bldepth(2), 'standard_name', "sea_floor_depth_below_geoid")
            ierr = nf90_put_att(ncid, id_tsp%id_bldepth(2), 'units', "m")
            ierr = nf90_put_att(ncid, id_tsp%id_bldepth(2), 'positive', "down")

            if (jafou_) then
               ierr = nf90_def_var(ncid, trim(mesh2dname)//'_'//trim(waterlevelname), nf90_double, id_tsp%meshids2d%dimids(mdim_face), id_tsp%id_s1max(2))
               ierr = nf90_put_att(ncid, id_tsp%id_s1max(2), 'standard_name', "water level on cell centres")
               ierr = nf90_put_att(ncid, id_tsp%id_s1max(2), 'units', "m")
               ierr = nf90_put_att(ncid, id_tsp%id_s1max(2), 'positive', "down")

               ierr = nf90_enddef(ncid)
               ierr = nf90_put_var(ncid, id_tsp%id_s1max(2), s1max(1:ndx2d))
               ierr = nf90_put_var(ncid, id_tsp%id_bldepth(2), -1 * bl_min(1:ndx2d))
               ierr = nf90_redef(ncid)
            else
               ierr = nf90_enddef(ncid)
               ierr = nf90_put_var(ncid, id_tsp%id_bldepth(2), -1 * bl(1:ndx2d))
               ierr = nf90_redef(ncid)
            end if
         end if

         deallocate (edge_nodes)
         deallocate (face_nodes)
         deallocate (edge_faces)
         deallocate (x2dn)
         deallocate (y2dn)
      end if

      ! NOTE: UNST-1318: backwards compatibility: we write zk values in flowgeom/map file since DELFT3DFM still needs it.
      !       The def_var is inside io_ugrid (needs to be removed), but the put_var is only here.
      ! TODO: below would be better than def_var inside io_ugrid:
      ! TODO: ierr = unc_def_var_map(mapids, mapids%id_netnodez(:),   nf90_double, UNC_LOC_CN, 'node_z', '', 'Bed level at grid nodes', 'm', 0)
      ! ierr = ug_inq_varid(mapids%ncid, mapids%id_tsp%meshids1d, 'node_z', mapids%id_netnodez(1)) ! TODO: AvD: 1D UGRID not entirely yet.
      ierr = ug_inq_varid(ncid, id_tsp%meshids2d, 'node_z', id_tsp%id_netnodez(2))
      ! ierr = ug_inq_varid(mapids%ncid, mapids%id_tsp%meshids3d, 'node_z', mapids%id_netnodez(3)) ! TODO: AvD: 3D UGRID not yet

!   ierr = unc_def_var_map(mapids, mapids%id_flowelemcontourx(:), nf90_double, UNC_LOC_S, 'FlowElemContour_x', '', '', 'm', (/ id_flowelemcontourptsdim, id_seddim, -1 /)).

      ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemba(:), nf90_double, UNC_LOC_S, 'flowelem_ba', 'cell_area', '', 'm2', 0, jabndnd=jabndnd_)
      ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelembl(:), nf90_double, UNC_LOC_S, 'flowelem_bl', 'altitude', 'flow element center bedlevel (bl)', 'm', 0, jabndnd=jabndnd_)
      ! ierr = nf90_put_att(igeomfile, id_flowelembl, 'positive',      'up') ! Not allowed for non-coordinate variables

      !define 1d2dcontacts only after mesh2d is completly defined
      if (n1d2dcontacts > 0 .and. ja2D_) then
         ierr = ug_def_mesh_contact(ncid, id_tsp%meshcontacts, trim(contactname), n1d2dcontacts, id_tsp%meshids1d, id_tsp%meshids2d, UG_LOC_NODE, UG_LOC_FACE, start_index)
      end if

      ! Define domain numbers when it is a parallel run
      if (jampi == 1) then
         ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemdomain(:), nf90_int, UNC_LOC_S, 'flowelem_domain', 'cell_domain_number', 'domain number of flow element', '', 0, jabndnd=jabndnd_, ivalid_max=ndomains)
         ierr = unc_def_var_map(ncid, id_tsp, id_tsp%id_flowelemglobalnr(:), nf90_int, UNC_LOC_S, 'flowelem_globalnr', 'cell_global_number', 'global flow element numbering', '', 0, jabndnd=jabndnd_, ivalid_max=Nglobal_s)
      end if
      ierr = nf90_enddef(ncid)

      ! -- Start data writing (time-independent data) ------------
      ! Flow cell cc coordinates (only 1D + internal 2D)
      if (ndx1d > 0) then
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemba(1), ba(ndx2d + 1:last_1d)) ! TODO: AvD: handle 1D/2D boundaries
         ierr = nf90_put_var(ncid, id_tsp%id_flowelembl(1), bl(ndx2d + 1:last_1d)) ! TODO: AvD: handle 1D/2D boundaries
         ! TODO: AvD: UNST-1318: handle 1d zk as well
      end if
      if (ndx2d > 0 .and. ja2D_) then
         ierr = nf90_put_var(ncid, id_tsp%id_flowelemba(2), ba(1:ndx2d)) ! TODO: AvD: handle 1D/2D boundaries
         ierr = nf90_put_var(ncid, id_tsp%id_flowelembl(2), bl(1:ndx2d)) ! TODO: AvD: handle 1D/2D boundaries
         ierr = nf90_put_var(ncid, id_tsp%id_netnodez(2), z2dn)
      end if

      ! Put the contacts
      if (n1d2dcontacts > 0) then
         ierr = ug_put_mesh_contact(ncid, id_tsp%meshcontacts, contacts(1, :), contacts(2, :), contacttype)
      end if

      if (allocated(edge_type)) then
         deallocate (edge_type)
      end if
      ! TODO: AvD: also edge_type for 1D
      if (associated(layer_zs)) then
         deallocate (layer_zs)
      end if
      if (associated(interface_zs)) then
         deallocate (interface_zs)
      end if
      if (allocated(contacts)) then
         deallocate (contacts)
      end if
      if (allocated(contacttype)) then
         deallocate (contacttype)
      end if
      if (allocated(edge_nodes)) then
         deallocate (edge_nodes)
      end if

      ! TODO: AvD:
      ! * in WAVE: handle the obsolete 'nFlowElemWithBnd'/'nFlowElem' difference
      ! * for WAVE: add FlowElem_zcc back in com file.
      ! * for parallel: add 'FlowElemDomain', 'FlowLinkDomain', 'FlowElemGlobalNr'
      ! domain numbers
      if (jampi == 1) then
         ! FlowElemDomain
         if (ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemdomain(2), idomain(1:ndx2d))
         end if
         ! FlowElemGlobalNr
         if (ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemglobalnr(2), iglobal_s(1:ndx2d))
         end if
         ! FlowElemDomain
         if (ndx1d > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemdomain(1), idomain(ndx2d + 1:last_1d))
         end if
         ! FlowElemGlobalNr
         if (ndx1d > 0) then
            ierr = nf90_put_var(ncid, id_tsp%id_flowelemglobalnr(1), iglobal_s(ndx2d + 1:last_1d))
         end if
      end if

      if (mb_latmin /= dmiss .and. mb_latmax /= dmiss .and. mb_lonmin /= dmiss .and. mb_lonmax /= dmiss) then
         ierr = ionc_add_geospatial_bounds(ncid, mb_latmin, mb_latmax, mb_lonmin, mb_lonmax)
      end if

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 1) then
         ierr = nf90_redef(ncid)
      end if
      if (timon) call timstop(handle_extra(69))

      !call readyy('Writing flow geometry data',-1d0)
      return

888   continue
      ! Possible error.

   end subroutine unc_write_flowgeom_filepointer_ugrid

!> Writes the unstructured 1D flow geometry in UGRID format to an already opened netCDF dataset for use in the dfm volume tool.
   subroutine unc_write_1D_flowgeom_ugrid(id_tsp, ncid, jabndnd, jafou, ja2D, contacts_, contacttype_, numcontacts)

      use m_flowgeom
      use network_data
      use m_sferic
      use m_missing
      use netcdf
      use m_partitioninfo
      use m_alloc
      use dfm_error
      use m_save_ugrid_state !stores the contactname and other saved ugrid names
      use m_CrossSections
      use m_flowtimes, only: handle_extra
      use Timers
      use m_modelbounds
      use io_netcdf_acdd, only: ionc_add_geospatial_bounds
      implicit none

      integer, intent(in) :: ncid !< Handle to open Netcdf file to write the geometry to.
      type(t_unc_timespace_id), intent(inout) :: id_tsp !< Set of time and space related variable id's
      integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.
      logical, optional, intent(in) :: jaFou !< Whether this flowgeom writing is part of a Fourier file or not (affects 3D layer writing)
      logical, optional, intent(in) :: ja2D !< Whether to include the 2D grid (default = .true.)
      integer, optional, intent(out) :: numcontacts !< Output variable that will be filled with the number of contacts
      integer, optional, intent(out), allocatable :: contacts_(:, :) !< output contacts array
      integer, optional, intent(out), allocatable :: contacttype_(:) !< output contact type array

      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last 2/3D node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1D node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.
      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: ndx1d !< Number of internal 1D nodes.

      integer :: nn
      integer, allocatable :: edge_nodes(:, :), face_nodes(:, :), edge_type(:), contacts(:, :)
      integer :: layer_count, layer_type
   !! Geometry options
      integer, parameter :: LAYERTYPE_OCEAN_SIGMA = 1 !< Dimensionless vertical ocean sigma coordinate.
      integer, parameter :: LAYERTYPE_Z = 2 !< Vertical coordinate for fixed z-layers.
      integer, parameter :: LAYERTYPE_OCEAN_SIGMA_Z = 3 !< Combined Z-Sigma layers
      real(kind=dp), dimension(:), pointer :: layer_zs => null(), interface_zs => null()
      logical :: jafou_
      logical :: ja2D_
!   type(t_crs) :: pj

      integer :: ierr
      integer :: i, numContPts, numNodes, n, L, k1, L1
      integer :: Li !< Index of 1D link (can be internal or boundary)
      integer :: id_flowelemcontourptsdim, id_flowelemcontourx, id_flowelemcontoury
      integer :: jaInDefine
      double precision, allocatable :: work2(:, :)
      integer :: n1dedges, n1d2dcontacts, numk2d, start_index
      integer, allocatable :: contacttype(:)

      ! re-mapping of 1d mesh coordinates for UGrid
      double precision, allocatable :: x1dn(:), y1dn(:), x1du(:), y1du(:)
      integer, pointer :: nodebranchidx_remap(:)
      double precision, pointer :: nodeoffsets_remap(:)
      integer, pointer :: edgebranchidx_remap(:)
      double precision, pointer :: edgeoffsets_remap(:)
      character(len=ug_idsLen), allocatable :: nodeids_remap(:)
      character(len=ug_idsLongNamesLen), allocatable :: nodelongnames_remap(:)

      jaInDefine = 0
      n1d2dcontacts = 0
      n1dedges = 0
      start_index = 1
      !if (present(id_tsp_)) then
      !   id_tsp = id_tsp_
      !endif

      if (ndxi <= 0) then
         call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
         return
      end if

      if (timon) call timstrt("unc_write_flowgeom_filepointer_ugrid", handle_extra(69))
      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 1 !boundary nodes are in volume table
      end if

      ! Include boundary cells in output (ndx) or not (ndxi)
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if
      if (present(jafou)) then
         jafou_ = jafou
      else
         jafou_ = .false.
      end if
      if (present(ja2D)) then
         ja2D_ = ja2D
      else
         ja2D_ = .false.
      end if

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      ierr = nf90_redef(ncid)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
         call check_error(ierr)
         return
      end if

      if (jsferic == 1) then
         crs%epsg_code = 4326
      end if

      layer_count = 0
      layer_type = -1

      n1d_write = last_1d - ndx2d
      ndx1d = ndxi - ndx2d

      n1d2dcontacts = 0
      if (ndx1d > 0) then

         ! First store pure 1D nodes (in flow node order), start counting at 1.call realloc(x1dn, ndx1d)
         call realloc(x1dn, n1d_write)
         call realloc(y1dn, n1d_write)
         if (associated(meshgeom1d%ngeopointx)) then ! Indicates that no Deltares-0.10 network topology/branchids have been read.
            call reallocP(nodebranchidx_remap, n1d_write)
            call reallocP(nodeoffsets_remap, n1d_write)
            call realloc(nodeids_remap, n1d_write)
            call realloc(nodelongnames_remap, n1d_write)
         end if

         do n = 1, n1d_write
            x1dn(n) = xz(ndx2d + n)
            y1dn(n) = yz(ndx2d + n)

            if (n <= ndx1d) then ! exclude boundary nodes
               ! Also store the original mesh1d/network variables in the new flowgeom order for ndx1d nodes:
               k1 = nodePermutation(nd(ndx2d + n)%nod(1)) ! This is the node index from *before* setnodadm(),
               ! i.e., as was read from input *_net.nc file.
               if (k1 > 0 .and. associated(meshgeom1d%ngeopointx)) then ! Indicates that no Deltares-0.10 network topology/branchids have been read.
                  nodebranchidx_remap(n) = meshgeom1d%nodebranchidx(k1)
                  nodeoffsets_remap(n) = meshgeom1d%nodeoffsets(k1)
                  nodeids_remap(n) = nodeids(k1)
                  nodelongnames_remap(n) = nodelongnames(k1)
               end if
            end if
         end do

         !count 1d mesh edges and 1d2d contacts
         n1dedges = 0
         n1d2dcontacts = 0
         do L = 1, lnx1d
            if (kcu(L) == 1) then
               n1dedges = n1dedges + 1
            else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
               n1d2dcontacts = n1d2dcontacts + 1
            else
               continue
            end if
         end do
         if (jabndnd_ == 1) then
            ! when writing boundary points, include the boundary links as well
            n1dedges = n1dedges + (lnx1db - lnxi)
         end if

         call realloc(face_nodes, (/0, 0/))
         !allocate mesh edges and 1d2d contacts
         call realloc(edge_nodes, (/2, n1dedges/), fill=-999)
         call realloc(contacts, (/2, n1d2dcontacts/), fill=-999)
         call realloc(id_tsp%edgetoln, n1dedges, keepExisting=.false., fill=0)
         call realloc(x1du, n1dedges)
         call realloc(y1du, n1dedges)
         if (associated(meshgeom1d%ngeopointx)) then ! Indicates that no Deltares-0.10 network topology/branchids have been read.
            call reallocP(edgebranchidx_remap, n1dedges)
            call reallocP(edgeoffsets_remap, n1dedges)
         end if

         call realloc(id_tsp%contactstoln, n1d2dcontacts, keepExisting=.false., fill=0)
         call realloc(contacttype, n1d2dcontacts, keepExisting=.false., fill=0)

         !assign values to mesh edges 1d2d contacts
         n1dedges = 0
         n1d2dcontacts = 0
         do Li = 1, lnx1d + (lnx1db - lnxi) ! optionally include the boundary links?
            if (Li <= lnx1d) then
               L = Li
            elseif (n1d_write == ndx1d) then ! when writing only internal nodes, skip boundary links
               exit
            else
               L = lnxi + (Li - lnx1d)
            end if
            if (abs(kcu(L)) == 1) then ! internal 1D edges and open boundary links
               n1dedges = n1dedges + 1
               edge_nodes(1:2, n1dedges) = ln(1:2, L) - ndx2d !only 1d edge nodes
               !mappings
               id_tsp%edgetoln(n1dedges) = L
               x1du(n1dedges) = xu(L)
               y1du(n1dedges) = yu(L)
               L1 = Lperm(ln2lne(L)) ! This is the edge index from *before* setnodadm(),
               ! i.e., as was read from input *_net.nc file.
               if (L1 > 0 .and. associated(meshgeom1d%ngeopointx)) then ! Indicates that no Deltares-0.10 network topology/branchids have been read.
                  edgebranchidx_remap(n1dedges) = meshgeom1d%edgebranchidx(L1)
                  edgeoffsets_remap(n1dedges) = meshgeom1d%edgeoffsets(L1)
               else
                  continue
               end if
            else if (kcu(L) == 3 .or. kcu(L) == 4 .or. kcu(L) == 5 .or. kcu(L) == 7) then ! 1d2d, lateralLinks, streetinlet, roofgutterpipe
               ! 1D2D link, find the 2D flow node and store its cell center as '1D' node coordinates
               n1d2dcontacts = n1d2dcontacts + 1
               id_tsp%contactstoln(n1d2dcontacts) = L
               contacttype(n1d2dcontacts) = kcu(L)
               if (ln(1, L) > ndx2d) then ! First point of 1D link is 1D cell
                  contacts(1, n1d2dcontacts) = ln(1, L) - ndx2d
                  contacts(2, n1d2dcontacts) = ln(2, L) ! In m_flowgeom: 1D nodenr = ndx2d+n, in UGrid 1D flowgeom: local 1D nodenr = n.
               else ! Second point of 1D link is 1D cell
                  contacts(1, n1d2dcontacts) = ln(2, L) - ndx2d
                  contacts(2, n1d2dcontacts) = ln(1, L) !2d
               end if
            else
               continue
            end if
         end do

         !define 1dmesh
         if (n1dedges > 0) then
            if (associated(meshgeom1d%ngeopointx)) then
               ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, n1d_write, n1dedges, 0, 0, &
                                           edge_nodes, face_nodes, null(), null(), null(), x1dn, y1dn, xu(id_tsp%edgetoln(:)), yu(id_tsp%edgetoln(:)), xz(1:1), yz(1:1), &
                                           crs, -999, dmiss, start_index, layer_count, layer_type, layer_zs, interface_zs, &
                                           id_tsp%network1d, network1dname, meshgeom1d%nnodex, meshgeom1d%nnodey, nnodeids, nnodelongnames, &
                                           meshgeom1d%nedge_nodes(1, :), meshgeom1d%nedge_nodes(2, :), nbranchids, nbranchlongnames, meshgeom1d%nbranchlengths, meshgeom1d%nbranchgeometrynodes, meshgeom1d%nbranches, &
                                           meshgeom1d%ngeopointx, meshgeom1d%ngeopointy, meshgeom1d%ngeometry, &
                                           meshgeom1d%nbranchorder, &
                                           nodeids_remap, nodelongnames_remap, nodebranchidx_remap, nodeoffsets_remap, edgebranchidx_remap, edgeoffsets_remap, &
                                           writeopts=unc_writeopts)
               ! NOTE: UNST-5477: this call is not valid yet for 3D models with ocean_sigma_z combined layering
            else
               ierr = ug_write_mesh_arrays(ncid, id_tsp%meshids1d, mesh1dname, 1, UG_LOC_NODE + UG_LOC_EDGE, n1d_write, n1dedges, 0, 0, &
                                           edge_nodes, face_nodes, null(), null(), null(), x1dn, y1dn, x1du, y1du, xz(1:1), yz(1:1), &
                                           crs, -999, dmiss, start_index, layer_count, layer_type, layer_zs, interface_zs, writeopts=unc_writeopts)
               ! NOTE: UNST-5477: this call is not valid yet for 3D models with ocean_sigma_z combined layering
            end if
         end if

         ! Determine max nr of vertices and contour points
         numNodes = n1d_write
         numContPts = 0
         do i = 1, ndx1d ! exclude boundary nodes
            numNodes = max(numNodes, size(nd(ndx2d + i)%nod))
            numContPts = max(numContPts, size(nd(ndx2d + i)%x))
         end do

         if (allocated(work2)) deallocate (work2)
         allocate (work2(numContPts, n1d_write)); work2 = dmiss

         ierr = nf90_def_dim(ncid, 'n'//trim(mesh1dname)//'_FlowElemContourPts', numContPts, id_flowelemcontourptsdim)

         ! Flow elem contours (plot help)
         ierr = nf90_def_var(ncid, trim(mesh1dname)//'_FlowElemContour_x', nf90_double, (/id_flowelemcontourptsdim, id_tsp%meshids1d%dimids(mdim_node)/), id_flowelemcontourx)
         ierr = nf90_def_var(ncid, trim(mesh1dname)//'_FlowElemContour_y', nf90_double, (/id_flowelemcontourptsdim, id_tsp%meshids1d%dimids(mdim_node)/), id_flowelemcontoury)
         ierr = unc_addcoordatts(ncid, id_flowelemcontourx, id_flowelemcontoury, jsferic)
         ierr = nf90_put_att(ncid, id_flowelemcontourx, 'long_name', 'list of x-coordinates forming flow element')
         ierr = nf90_put_att(ncid, id_flowelemcontoury, 'long_name', 'list of y-coordinates forming flow element')
         ierr = nf90_put_att(ncid, id_flowelemcontourx, '_FillValue', dmiss)
         ierr = nf90_put_att(ncid, id_flowelemcontoury, '_FillValue', dmiss)

         ierr = nf90_put_att(ncid, id_tsp%meshids1d%varids(mid_nodex), 'bounds', trim(mesh1dname)//'_FlowElemContour_x')
         ierr = nf90_put_att(ncid, id_tsp%meshids1d%varids(mid_nodey), 'bounds', trim(mesh1dname)//'_FlowElemContour_y')

         ierr = nf90_enddef(ncid)

         do i = 1, n1d_write
            nn = size(nd(ndx2d + i)%x)
            do n = 1, nn
               work2(n, i) = nd(ndx2d + i)%x(n)
            end do
         end do
         ierr = nf90_put_var(ncid, id_flowelemcontourx, work2(1:numContPts, 1:n1d_write), (/1, 1/), (/numContPts, n1d_write/))

         do i = 1, n1d_write
            nn = size(nd(ndx2d + i)%x)
            do n = 1, nn
               work2(n, i) = nd(ndx2d + i)%y(n)
            end do
         end do
         ierr = nf90_put_var(ncid, id_flowelemcontoury, work2(1:numContPts, 1:n1d_write), (/1, 1/), (/numContPts, n1d_write/))
         ierr = nf90_redef(ncid)

         deallocate (work2)

         if (allocated(x1dn)) then
            deallocate (x1dn, y1dn)
         end if
         if (allocated(x1du)) then
            deallocate (x1du, y1du)
         end if

         deallocate (edge_nodes)
      end if ! 1D flow grid geometry

      numk2d = 0
      ndx1d = ndxi - ndx2d

      ierr = nf90_enddef(ncid)

      ! -- Start data writing (time-independent data) ------------
      ! Flow cell cc coordinates (only 1D + internal 2D)
      !if (ndx1d > 0) then
      !   ierr = nf90_put_var(ncid, id_tsp%id_flowelemba(1), ba(ndx2d+1:last_1d)) ! TODO: AvD: handle 1D/2D boundaries
      !   ierr = nf90_put_var(ncid, id_tsp%id_flowelembl(1), bl(ndx2d+1:last_1d)) ! TODO: AvD: handle 1D/2D boundaries
      !endif

      if (present(contacts_)) then
         call realloc(contacts_, (/2, n1d2dcontacts/), fill=-999)
         if (allocated(contacts)) then
            contacts_ = contacts
         else
            contacts_ = 0
         end if
      end if

      if (present(contacttype_)) then
         call realloc(contacttype_, n1d2dcontacts, keepExisting=.false., fill=0)
         if (allocated(contacttype)) then
            contacttype_ = contacttype
         else
            contacttype_ = 0
         end if
      end if

      if (present(numcontacts)) then
         numcontacts = n1d2dcontacts
      end if

      if (allocated(edge_type)) then
         deallocate (edge_type)
      end if
      ! TODO: AvD: also edge_type for 1D
      if (associated(layer_zs)) then
         deallocate (layer_zs)
      end if
      if (associated(interface_zs)) then
         deallocate (interface_zs)
      end if
      if (allocated(contacts)) then
         deallocate (contacts)
      end if
      if (allocated(contacttype)) then
         deallocate (contacttype)
      end if
      if (allocated(edge_nodes)) then
         deallocate (edge_nodes)
      end if

      if (mb_latmin /= dmiss .and. mb_latmax /= dmiss .and. mb_lonmin /= dmiss .and. mb_lonmax /= dmiss) then
         ierr = ionc_add_geospatial_bounds(ncid, mb_latmin, mb_latmax, mb_lonmin, mb_lonmax)
      end if

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 1) then
         ierr = nf90_redef(ncid)
      end if
      if (timon) call timstop(handle_extra(69))

      ierr = nf90_sync(ncid)
      !call readyy('Writing flow geometry data',-1d0)
      return

888   continue
      ! Possible error.

   end subroutine

!> Writes the unstructured flow geometry to an already opened netCDF dataset.
   subroutine unc_write_flowgeom_filepointer(igeomfile, jabndnd)
      use m_flowgeom
      use network_data
      use m_sferic
      use m_missing
      use netcdf
      use m_partitioninfo
      use m_flowparameters, only: jafullgridoutput
      integer, intent(in) :: igeomfile
      integer, optional, intent(in) :: jabndnd !< Whether to include boundary nodes (1) or not (0). Default: no.

      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: ierr
      integer :: &
         id_netlinkdim, id_netlinkptsdim, &
         id_flowelemdim, id_flowelemmaxnodedim, id_flowelemcontourptsdim, &
         id_flowlinkdim, id_flowlinkptsdim, &
         id_flowelemxcc, id_flowelemycc, id_flowelemzcc, &
         id_flowelemxzw, id_flowelemyzw, &
         id_flowelemcontourx, id_flowelemcontoury, id_flowelemba, &
         id_flowelembl, id_elemlink, &
         id_flowlink, id_flowlinktype, &
         id_flowlinkxu, id_flowlinkyu, &
         id_flowlinklonu, id_flowlinklatu, &
         id_flowelemdomain, id_flowlinkdomain, &
         id_flowelemglobalnr

      integer :: i, numContPts, numNodes, n, nn, L
      integer :: jaInDefine
      integer :: jaghost, idmn
      integer, dimension(:), allocatable :: lne1write
      integer, dimension(:), allocatable :: lne2write

      double precision, dimension(:), allocatable :: zz
      double precision, dimension(:, :), allocatable :: work2

      jaInDefine = 0

      if (ndxi <= 0) then
         call mess(LEVEL_WARN, 'No flow elements in model, will not write flow geometry.')
         return
      end if

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      ! Include boundary cells in output (ndx) or not (ndxi)
      if (jabndnd_ == 1) then
         ndxndxi = ndx
      else
         ndxndxi = ndxi
      end if

      ! Determine max nr of vertices and contour points
      numNodes = 0
      numContPts = 0
      do i = 1, ndxndxi
         numNodes = max(numNodes, size(nd(i)%nod))
         numContPts = max(numContPts, size(nd(i)%x))
      end do

      if (allocated(work2)) deallocate (work2)
      allocate (work2(numContPts, ndxndxi)); work2 = dmiss

      ! Put dataset in define mode (possibly again) to add dimensions and variables.
      ierr = nf90_redef(igeomfile)
      if (ierr == nf90_eindefine) jaInDefine = 1 ! Was still in define mode.
      if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
         call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
         call check_error(ierr)
         return
      end if

      if (jabndnd_ == 1) then
         ierr = nf90_def_dim(igeomfile, 'nFlowElemWithBnd', ndxndxi, id_flowelemdim) ! Different name to easily show boundary nodes are included, rest of code below is generic ndx/ndxi.
      else
         ierr = nf90_def_dim(igeomfile, 'nFlowElem', ndxndxi, id_flowelemdim)
      end if

      ierr = nf90_inq_dimid(igeomfile, 'nNetLinkPts', id_netlinkptsdim)
      if (ierr /= 0) then
         ierr = nf90_def_dim(igeomfile, 'nNetLinkPts', 2, id_netlinkptsdim)
      end if

      ierr = nf90_inq_dimid(igeomfile, 'nNetLink', id_netlinkdim)
      if (ierr /= 0) then
         ierr = nf90_def_dim(igeomfile, 'nNetLink', numl, id_netlinkdim)
      end if

      if (numNodes > 0) then
         ierr = nf90_def_dim(igeomfile, 'nFlowElemMaxNode', numNodes, id_flowelemmaxnodedim)
      end if

      ierr = nf90_def_dim(igeomfile, 'nFlowElemContourPts', numContPts, id_flowelemcontourptsdim)

      if (lnx > 0) then
         ierr = nf90_def_dim(igeomfile, 'nFlowLink', lnx, id_flowlinkdim)
         ierr = nf90_def_dim(igeomfile, 'nFlowLinkPts', 2, id_flowlinkptsdim)
      end if

      ! Flow cells
      ierr = nf90_def_var(igeomfile, 'FlowElem_xcc', nf90_double, id_flowelemdim, id_flowelemxcc)
      ierr = nf90_def_var(igeomfile, 'FlowElem_ycc', nf90_double, id_flowelemdim, id_flowelemycc)
      if (jafullgridoutput == 0) then
         ierr = nf90_def_var(igeomfile, 'FlowElem_zcc', nf90_double, id_flowelemdim, id_flowelemzcc)
      end if
      ierr = nf90_def_var(igeomfile, 'FlowElem_bac', nf90_double, id_flowelemdim, id_flowelemba)

      ierr = unc_addcoordatts(igeomfile, id_flowelemxcc, id_flowelemycc, jsferic)
      ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'long_name', 'x-coordinate of flow element circumcenter')
      ierr = nf90_put_att(igeomfile, id_flowelemycc, 'long_name', 'y-coordinate of flow element circumcenter')
      ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'standard_name', 'bed_level')
      ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'long_name', 'bed level of flow element')
      !ierr = nf90_put_att(igeomfile, id_flowelemzcc, 'positive '    , 'down') ! For WAVE ! ONLY allowed for true coordinate-vars
      ierr = nf90_put_att(igeomfile, id_flowelemxcc, 'bounds', 'FlowElemContour_x')
      ierr = nf90_put_att(igeomfile, id_flowelemycc, 'bounds', 'FlowElemContour_y')

      ierr = nf90_put_att(igeomfile, id_flowelemba, 'long_name', 'flow element area')
      ierr = nf90_put_att(igeomfile, id_flowelemba, 'units', 'm2')
      ierr = nf90_put_att(igeomfile, id_flowelemba, 'standard_name', 'cell_area')

      ! Flow element mass centers
      ierr = nf90_def_var(igeomfile, 'FlowElem_xzw', nf90_double, id_flowelemdim, id_flowelemxzw)
      ierr = nf90_def_var(igeomfile, 'FlowElem_yzw', nf90_double, id_flowelemdim, id_flowelemyzw)
      ierr = unc_addcoordatts(igeomfile, id_flowelemxzw, id_flowelemyzw, jsferic)
      ierr = nf90_put_att(igeomfile, id_flowelemxzw, 'long_name', 'x-coordinate of flow element center of mass')
      ierr = nf90_put_att(igeomfile, id_flowelemyzw, 'long_name', 'y-coordinate of flow element center of mass')
      ierr = nf90_put_att(igeomfile, id_flowelemxzw, 'bounds', 'FlowElemContour_x')
      ierr = nf90_put_att(igeomfile, id_flowelemyzw, 'bounds', 'FlowElemContour_y')

      ! Flow elem contours (plot help)
      ! Todo: generalize x/y's to 2/3-D coords everywhere else [Avd]
      ierr = nf90_def_var(igeomfile, 'FlowElemContour_x', nf90_double, (/id_flowelemcontourptsdim, id_flowelemdim/), id_flowelemcontourx)
      ierr = nf90_def_var(igeomfile, 'FlowElemContour_y', nf90_double, (/id_flowelemcontourptsdim, id_flowelemdim/), id_flowelemcontoury)
      ierr = unc_addcoordatts(igeomfile, id_flowelemcontourx, id_flowelemcontoury, jsferic)
      ierr = nf90_put_att(igeomfile, id_flowelemcontourx, 'long_name', 'list of x-coordinates forming flow element')
      ierr = nf90_put_att(igeomfile, id_flowelemcontoury, 'long_name', 'list of y-coordinates forming flow element')
      ierr = nf90_put_att(igeomfile, id_flowelemcontourx, '_FillValue', dmiss)
      ierr = nf90_put_att(igeomfile, id_flowelemcontoury, '_FillValue', dmiss)

      ! Flow elems bottom levels
      ierr = nf90_def_var(igeomfile, 'FlowElem_bl', nf90_double, id_flowelemdim, id_flowelembl)
      ierr = nf90_put_att(igeomfile, id_flowelembl, 'units', 'm')
      !ierr = nf90_put_att(igeomfile, id_flowelembl, 'positive',      'up') ! Only allowed for true CF vertical coordinate
      !ierr = nf90_put_att(igeomfile, id_flowelembl, 'standard_name', 'sea_floor_depth') ! CF
      ierr = nf90_put_att(igeomfile, id_flowelembl, 'long_name', 'Initial bed level at flow element circumcenter')

      ierr = nf90_def_var(igeomfile, 'ElemLink', nf90_int, (/id_netlinkptsdim, id_netlinkdim/), id_elemlink)
      ierr = nf90_put_att(igeomfile, id_elemlink, 'standard_name', 'elemlink')
      ierr = nf90_put_att(igeomfile, id_elemlink, 'long_name', 'flow nodes between/next to which link between two netnodes lies')
      ierr = nf90_put_att(igeomfile, id_elemlink, 'start_index', 1)

      if (lnx > 0) then
         ierr = nf90_def_var(igeomfile, 'FlowLink', nf90_int, (/id_flowlinkptsdim, id_flowlinkdim/), id_flowlink)
         ierr = nf90_put_att(igeomfile, id_flowlink, 'long_name', 'link/interface between two flow elements')

         ierr = nf90_def_var(igeomfile, 'FlowLinkType', nf90_int, (/id_flowlinkdim/), id_flowlinktype)
         ierr = nf90_put_att(igeomfile, id_flowlinktype, 'long_name', 'type of flowlink')
         ierr = nf90_put_att(igeomfile, id_flowlinktype, 'valid_range', (/1, 7/))
         ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_values', (/1, 2, 3, 4, 5, 7/))
         ierr = nf90_put_att(igeomfile, id_flowlinktype, 'flag_meanings', 'link_between_1D_nodes link_between_2D_nodes embedded_1D2D_link longitudinal_1D2D_link vertically_stacked_1D2D_link roof_gutter_1D2D_link')

         ierr = nf90_def_var(igeomfile, 'FlowLink_xu', nf90_double, (/id_flowlinkdim/), id_flowlinkxu)
         ierr = nf90_def_var(igeomfile, 'FlowLink_yu', nf90_double, (/id_flowlinkdim/), id_flowlinkyu)
         ierr = unc_addcoordatts(igeomfile, id_flowlinkxu, id_flowlinkyu, jsferic)
         ierr = nf90_put_att(igeomfile, id_flowlinkxu, 'long_name', 'x-coordinate of flow link center (velocity point)')
         ierr = nf90_put_att(igeomfile, id_flowlinkyu, 'long_name', 'y-coordinate of flow link center (velocity point)')
      end if

      ! Coordinate/grid mapping
      ierr = unc_addcoordmapping(igeomfile, jsferic)

      ! Add mandatory lon/lat coords too (only if jsferic==0)
      ! BJ: following two lines commented out since QuickPlot will select longitude and latitude based on preference; however, these arrays don't actually contain data yet!
      !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElem',        'cc', (/ id_flowelemdim /),                           id_flowelemloncc,      id_flowelemlatcc,      jsferic)
      !ierr = unc_add_lonlat_vars(igeomfile, 'FlowElemContour', ''  , (/ id_flowelemcontourptsdim, id_flowelemdim /), id_flowelemcontourlon, id_flowelemcontourlat, jsferic)

      ! Add grid_mapping reference to all original coordinate and data variables
      ierr = unc_add_gridmapping_att(igeomfile, &
                                     (/id_flowelembl/), jsferic)
!       (/ id_flowelemxcc, id_flowelemycc, id_flowelemcontourx, id_flowelemcontoury, )&

      if (lnx > 0) then
         ierr = unc_add_lonlat_vars(igeomfile, 'FlowLink', 'u', (/id_flowlinkdim/), id_flowlinklonu, id_flowlinklatu, jsferic)

         ! Add grid_mapping reference to all original coordinate and data variables
         !ierr = unc_add_gridmapping_att(igeomfile, &
         !   (/ id_flowlinkxu, id_flowlinkyu /), jsferic)
      end if

      !   domain numbers and global node/link numbers
      if (jampi == 1) then
         ierr = nf90_def_var(igeomfile, 'FlowElemDomain', nf90_int, id_flowelemdim, id_flowelemdomain)
         ierr = nf90_put_att(igeomfile, id_flowelemdomain, 'long_name', 'domain number of flow element')
         ierr = nf90_def_var(igeomfile, 'FlowLinkDomain', nf90_int, id_flowlinkdim, id_flowlinkdomain)
         ierr = nf90_put_att(igeomfile, id_flowlinkdomain, 'long_name', 'domain number of flow link')
         ierr = nf90_def_var(igeomfile, 'FlowElemGlobalNr', nf90_int, id_flowelemdim, id_flowelemglobalnr)
         ierr = nf90_put_att(igeomfile, id_flowelemglobalnr, 'long_name', 'global flow element numbering')
      end if

      ierr = nf90_enddef(igeomfile)
      ! End of writing time-independent flow net data.
      ! call readyy('Writing flow geometry data',.05d0)

      ! -- Start data writing (time-independent data) ------------
      ! Flow cell cc coordinates (only 1D + internal 2D)
      ierr = nf90_put_var(igeomfile, id_flowelemxcc, xz(1:ndxndxi))
      ierr = nf90_put_var(igeomfile, id_flowelemycc, yz(1:ndxndxi))
      ierr = nf90_put_var(igeomfile, id_flowelemba, ba(1:ndxndxi))
      allocate (zz(ndxndxi), stat=ierr)
      !
      ! DFlowFM: z-positive is upwards
      ! WAVE: z-positive is downwards
      ! Don't change DMISS
      do n = 1, ndxndxi
         if (bl(n) == DMISS) then
            zz(n) = bl(n)
         else
            zz(n) = -bl(n)
         end if
      end do
      ierr = nf90_put_var(igeomfile, id_flowelemzcc, zz(1:ndxndxi))
      if (allocated(zz)) then
         deallocate (zz, stat=ierr)
      end if

      ! Flow cell center of mass coordinates (only 1D + internal 2D)
      ierr = nf90_put_var(igeomfile, id_flowelemxzw, xzw(1:ndxndxi))
      ierr = nf90_put_var(igeomfile, id_flowelemyzw, yzw(1:ndxndxi))

      !call readyy('Writing flow geometry data',.15d0)

      ! Flow cell contours
    !!!do i=1,ndxndxi
    !!!   numContPts = size(nd(i)%x)
    !!!   ierr = nf90_put_var(igeomfile, id_flowelemcontourx, nd(i)%x, (/ 1, i /), (/ numContPts, 1 /) )
    !!!   ierr = nf90_put_var(igeomfile, id_flowelemcontoury, nd(i)%y, (/ 1, i /), (/ numContPts, 1 /) )
    !!!enddo
      !call readyy('Writing flow geometry data',.45d0)

      do i = 1, ndxndxi
         nn = size(nd(i)%x)
         do n = 1, nn
            work2(n, i) = nd(i)%x(n)
         end do
      end do
      ierr = nf90_put_var(igeomfile, id_flowelemcontourx, work2(1:numContPts, 1:ndxndxi), (/1, 1/), (/numContPts, ndxndxi/))

      do i = 1, ndxndxi
         nn = size(nd(i)%x)
         do n = 1, nn
            work2(n, i) = nd(i)%y(n)
         end do
      end do
      ierr = nf90_put_var(igeomfile, id_flowelemcontoury, work2(1:numContPts, 1:ndxndxi), (/1, 1/), (/numContPts, ndxndxi/))

      deallocate (work2)

      ! flowcells bottom levels
      ierr = nf90_put_var(igeomfile, id_flowelembl, bl(1:ndxndxi))
      !call readyy('Writing flow geometry data',.55d0)

      allocate (lne1write(numL))
      allocate (lne2write(numL))
      do L = 1, numL
         lne1write(L) = lne(1, L)
         lne2write(L) = lne(2, L)
      end do

      ierr = nf90_put_var(igeomfile, id_elemlink, lne1write, count=(/1, numl/), start=(/1, 1/))
      ierr = nf90_put_var(igeomfile, id_elemlink, lne2write, count=(/1, numl/), start=(/2, 1/))

      deallocate (lne1write)
      deallocate (lne2write)

      ! Flow links
      ierr = nf90_put_var(igeomfile, id_flowlink, ln(:, 1:lnx))
      do i = 1, lnx1D
         ierr = nf90_put_var(igeomfile, id_flowlinktype, (/1/), start=(/i/))
      end do
      do i = lnx1D + 1, lnx
         ierr = nf90_put_var(igeomfile, id_flowlinktype, (/2/), start=(/i/))
      end do
      !call readyy('Writing flow geometry data',.90d0)

      if (lnx > 0) then
         ! Flow links velocity points
         ierr = nf90_put_var(igeomfile, id_flowlinkxu, xu(1:lnx))
         ierr = nf90_put_var(igeomfile, id_flowlinkyu, yu(1:lnx))
      end if

      ! domain numbers
      if (jampi == 1) then
         ! flow cell domain numbers
         ierr = nf90_put_var(igeomfile, id_flowelemdomain, idomain(1:ndxndxi)) ! TODO: ndxndxi
         ! flow link domain numbers
         do i = 1, Lnx
            ! determine if flow link is a ghost link and get domain number and ghost level of link
            call link_ghostdata(my_rank, idomain(ln(1, i)), idomain(ln(2, i)), jaghost, idmn)
            ierr = nf90_put_var(igeomfile, id_flowlinkdomain, (/idmn/), start=(/i/)) ! corresponds with partition_get_ghosts
         end do
         ierr = nf90_put_var(igeomfile, id_flowelemglobalnr, iglobal_s(1:ndxndxi)) ! TODO: ndxndxi
      end if
      !call readyy('Writing flow geometry data',1d0)

      ! Leave the dataset in the same mode as we got it.
      if (jaInDefine == 1) then
         ierr = nf90_redef(igeomfile)
      end if

      !call readyy('Writing flow geometry data',-1d0)
   end subroutine unc_write_flowgeom_filepointer

! -- PRIVATE ROUTINES ---------------------------
!> Resets current error status and sets informative message for subsequent
!! errors. Generally called at start of any routine that wants to use
!! routine check_error. The informative message is only shown/used when
!! later check_error's indeed detect an error.
   subroutine prepare_error(firstline, level)
      character(len=*), intent(in) :: firstline !< Informative message for screen/log.
      integer, intent(in), optional :: level !< Error level (one from LEVEL_(FATAL|ERROR|WARN|INFO|DEBUG), default: LEVEL_WARN.)

      err_firstline_ = firstline
      err_firsttime_ = .true.
      nerr_ = 0
      if (present(level)) then
         err_level_ = level
      else
         err_level_ = LEVEL_WARN
      end if

   end subroutine prepare_error

!> Check a NetCDF error status and print a message when it is not nf90_noerr.
   subroutine check_error(ierr, info, level)
      integer, intent(in) :: ierr
      character(len=*), intent(in), optional :: info !< Caller's information string. Will be printed as prefix to the NetCDF error string.
      integer, intent(in), optional :: level !< Error level (one from LEVEL_(FATAL|ERROR|WARN|INFO|DEBUG), default: the level set by prepare_error.)

      character(len=255) :: infostring
      integer :: local_level

      if (ierr /= nf90_noerr) then
         nerr_ = nerr_ + 1

         ! Optional informative message (appended to NetCDF error string)
         if (present(info)) then
            infostring = '('//trim(info)//')'
         else
            infostring = ' '
         end if

         ! First error line
         if (err_firsttime_) then
            call mess(LEVEL_WARN, err_firstline_)
            err_firsttime_ = .false.
         end if

         if (present(level)) then
            local_level = level
         else
            local_level = err_level_ ! from prepare_error()
         end if

         ! Actual error line
         call mess(local_level, 'NetCDF error: ', nf90_strerror(ierr), trim(infostring))
      end if
   end subroutine check_error

!function unc_is_netfile(filename)
!    character(len=*), intent(in) :: filename
!    logical :: unc_is_netfile
!
!    unc_is_netfile = .true.
!
!end function unc_is_netfile
!
!    ierr = nf90_def_dim(inetfile, 'nElem', nump, id_elemdim)
!    ierr = nf90_def_dim(inetfile, 'nNode', numk, id_nodedim)
!    ierr = nf90_def_dim(inetfile, 'nConnect', 7, id_connectdim)
!!    ierr = nf90_def_dim(inetfile, 'id_len', 40, id_strlendim)
!!    ierr = nf90_def_dim(inetfile, 'time', nf90_unlimited, id_timedim)
!
!    ierr = nf90_def_var(inetfile, 'grid1', nf90_int, (/ id_connectdim, id_elemdim /), id_grid1topo)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'standard_name', 'net_topology')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'spatial_dimension', 2)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'topological_dimension', 2)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'cell_type', 'nc_mixed')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'index_start', 1)
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'x_nodal_coordinate', 'x')
!    ierr = nf90_put_att(inetfile, id_grid1topo, 'y_nodal_coordinate', 'y')
!
!    ierr = nf90_def_var(inetfile, 'x', nf90_double, id_nodedim, id_nodex)
!    ierr = nf90_put_att(inetfile, id_nodex, 'units', 'm')
!    ierr = nf90_put_att(inetfile, id_nodex, 'long_name', 'nodal x-coordinate')
!    ierr = nf90_def_var(inetfile, 'y', nf90_double, id_nodedim, id_nodey)
!    ierr = nf90_put_att(inetfile, id_nodey, 'units', 'm')
!    ierr = nf90_put_att(inetfile, id_nodey, 'long_name', 'nodal y-coordinate')
!
!    ierr = nf90_enddef(inetfile)
!
!    ierr = nf90_put_var(inetfile, id_nodex, xk(1:numk))
!    ierr = nf90_put_var(inetfile, id_nodey, yk(1:numk))
!    do i=1,nump
!        ierr = nf90_put_var(inetfile, id_grid1topo, (/ netcell(i)%n, ( netcell(i)%nod(k), k=1,netcell(i)%n) /), (/ 1, i /) )
!    enddo
!
!    ierr = nf90_close(inetfile)

!> Reads the flow data from a map file for one single variable, specified by the user.
!! Processing is done elsewhere.
   subroutine read_flowsamples_from_netcdf(fileName, quantityName, ierr)
      use m_samples

      implicit none

      ! I/O variables
      character(len=256), intent(in) :: fileName !< Name of the NetCDF file.
      character(len=256), intent(in) :: quantityName !< Name of the variable (i.e. the QUANTITY in the ext-file, i.e. 'qid').
      integer, intent(out) :: ierr !< Return status (NetCDF operations).

      ! Local variables
      integer :: iNetcdfFile, &
                 id_flowelemdim, &
                 id_flowlinkdim, &
                 id_timedim
      integer :: id_varXcoord, &
                 id_varYcoord, &
                 id_varData
      integer :: ndxi_read, &
                 lnx_read, &
                 nt_read
      logical :: file_exists

      ! Safety check: does the file exist at all?
      inquire (file=trim(fileName), exist=file_exists)
      if (.not. file_exists) then
         call mess(LEVEL_FATAL, 'The specified file for the initial conditions sample set has not been found. Check your .ext file.')
         return
      end if

      ! Reset error status:
      call prepare_error('Could not read flow samples from NetCDF file '''//trim(filename)//'''. Details follow:')

      ierr = unc_open(trim(fileName), nf90_nowrite, iNetcdfFile)
      call check_error(ierr, 'file '''//trim(fileName)//'''')
      if (nerr_ > 0) goto 999

      ! Ask for dimension id of nodes and edges
      ierr = nf90_inq_dimid(iNetcdfFile, 'nFlowElem', id_flowelemdim)
      ierr = nf90_inq_dimid(iNetcdfFile, 'nFlowLink', id_flowlinkdim)
      ierr = nf90_inq_dimid(iNetcdfFile, 'time', id_timedim)

      ! Ask for dimensions of nodes and edges, ergo: the number of netnodes and netlinks
      ierr = nf90_inquire_dimension(iNetcdfFile, id_flowelemdim, len=ndxi_read)
      ierr = nf90_inquire_dimension(iNetcdfFile, id_flowlinkdim, len=lnx_read)
      ierr = nf90_inquire_dimension(iNetcdfFile, id_timedim, len=nt_read)

      ! Read the output data
      if (quantityName == 'initialwaterlevel') then

         ! Allocate the output variable to be read, based on the location of the variable
         ns = ndxi_read

         ! Allocate the output variables to be read
         if (allocated(xs)) then
            deallocate (xs, ys, zs)
         end if
         allocate (xs(ns), ys(ns), zs(ns), stat=ierr)

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))

         ! Read the actual water levels
         ierr = nf90_inq_varid(iNetcdfFile, 's1', id_varData)
         if (ierr < 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No waterlevel data found in the specified NetCDF file.')
            return
         else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start=(/1, nt_read/))
         end if

      elseif (quantityName == 'initialsalinity') then

         ! Allocate the output variable to be read, based on the location of the variable
         ns = ndxi_read

         ! Allocate the output variables to be read
         if (allocated(xs)) then
            deallocate (xs, ys, zs)
         end if
         allocate (xs(ns), ys(ns), zs(ns), stat=ierr)

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))

         ! Read the actual salinity values
         ierr = nf90_inq_varid(iNetcdfFile, 'sa1', id_varData)
         if (ierr < 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No salinity data found in the specified NetCDF file.')
            return
         else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start=(/1, nt_read/))
         end if

      elseif (quantityName == 'initialvelocityx') then

         ! Allocate the output variable to be read, based on the location of the variable
         ns = ndxi_read

         ! Allocate the output variables to be read
         if (allocated(xs)) then
            deallocate (xs, ys, zs)
         end if
         allocate (xs(ns), ys(ns), zs(ns), stat=ierr)

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))

         ! Read the actual velocity x values
         ierr = nf90_inq_varid(iNetcdfFile, 'ucx', id_varData)
         if (ierr < 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No velocityx data found in the specified NetCDF file.')
            return
         else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start=(/1, nt_read/))
         end if

      elseif (quantityName == 'initialvelocityy') then

         ! Allocate the output variable to be read, based on the location of the variable
         ns = ndxi_read

         ! Allocate the output variables to be read
         if (allocated(xs)) then
            deallocate (xs, ys, zs)
         end if
         allocate (xs(ns), ys(ns), zs(ns), stat=ierr)

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_xcc', id_varXcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varXcoord, xs(1:ns))

         ! Read the xcoord data
         ierr = nf90_inq_varid(iNetcdfFile, 'FlowElem_ycc', id_varYcoord)
         ierr = nf90_get_var(iNetcdfFile, id_varYcoord, ys(1:ns))

         ! Read the actual velocity y values
         ierr = nf90_inq_varid(iNetcdfFile, 'ucy', id_varData)
         if (ierr < 0) then
            ierr = unc_close(iNetcdfFile)
            call mess(LEVEL_FATAL, 'No velocity data found in the specified NetCDF file.')
            return
         else
            ierr = nf90_get_var(iNetcdfFile, id_varData, zs(1:ns), start=(/1, nt_read/))
         end if

      else

         call mess(LEVEL_FATAL, 'Initial field specification of this quantity not supported through NetCDF file.')
         return

      end if

      ! Close the netcdf-file
999   continue
      ierr = unc_close(iNetcdfFile)

   end subroutine read_flowsamples_from_netcdf

! Read cell info. in order to bypass findcells
   subroutine readcells(filename, ierr, jaidomain, jaiglobal_s, jareinitialize)

      use network_data
      use m_flowgeom
      use m_alloc
      use m_partitioninfo, only: idomain, ndomains, iglobal_s, Nglobal_s
      use dfm_error
      use gridoperations
      use io_netcdf

      character(len=*), intent(in) :: filename !< Name of NetCDF file.
      integer, intent(out) :: ierr !< Return status (NetCDF operations)
      integer, optional, intent(in) :: jaidomain !<if read idomain
      integer, optional, intent(in) :: jaiglobal_s !< read global cell numbers (1) or not (0)
      integer, optional, intent(in) :: jareinitialize !< if is re-initialize

      integer :: inetfile, &
                 id_netnodedim, id_netlinkdim, id_netelemdim, & !< Dimensions
                 id_netelemmaxnodedim, &
                 id_netlink, id_netlinktype, & !< Link variables
                 id_netelemnode, id_netelemlink, & !< Netcell variables
                 id_idomain, id_iglobal_s

      integer :: L, nv, n, k, s, jaidomain_, jaiglobal_s_, fillvalue, jareinitialize_
      integer :: nerr_store
      integer, allocatable :: netcellnod(:, :), netcelllin(:, :), kn_tmp(:, :), ltype_tmp(:)
      integer :: numl_read, numk_read, numl1d_read, numl2d_read, numk2d_read, numl1d2d_read
      integer, allocatable :: kn12(:, :) !< Placeholder array for the edge_nodes
      integer, allocatable :: idomain1d(:), iglobal_s1d(:)

      integer :: jaugrid !< Whether UGRID file was read or not (1/0)
      integer :: ioncid !< io_netcdf dataset id
      integer :: iconvtype !< io_netcdf conventions type (from NetCDF :Conventions)
      integer :: im1d, im2d !< mesh ids in ioncid dataset, for 1D and 2D
      integer :: numk1d !< Local counter for number of 1d net nodes ("grid points")
      integer :: nump1d !< Local counter for number of 1d nodes ("cells")
      double precision :: convversion !< io_netcdf conventions version number
      integer :: ncontacts, im

      ierr = DFM_NOERR

      if (len_trim(filename) < 1) then
         ierr = DFM_GENERICERROR
         return
      end if

      jaidomain_ = 0
      if (present(jaidomain)) jaidomain_ = jaidomain

      jaiglobal_s_ = 0
      if (present(jaiglobal_s)) then
         jaiglobal_s_ = jaiglobal_s
      end if

      jareinitialize_ = 0
      if (present(jareinitialize)) jareinitialize_ = jareinitialize

      call readyy('Reading net data', 0d0)

      call prepare_error('Could not read net cells from NetCDF file '''//trim(filename)//''' (is not critical). Details follow:', LEVEL_DEBUG)

      nerr_ = 0

      ! First attempt UGRID:
      ierr = ionc_open(filename, NF90_NOWRITE, ioncid, iconvtype, convversion)
      if (ierr == ionc_noerr .and. iconvtype == IONC_CONV_UGRID .and. convversion >= 1.0) then
         jaugrid = 1
         numl_read = 0
         numk_read = 0

         ! 1D mesh
         numk1d = 0
         nump1d = 0
         numl1d_read = 0
         ierr = ionc_get_1d_mesh_id_ugrid(ioncid, im1d)
         if (ierr == ionc_noerr) then
            ierr = ionc_get_node_count(ioncid, im1d, numk1d)
            ierr = ionc_get_edge_count(ioncid, im1d, numl1d_read)
         end if
         numl_read = numl1d_read
         numk_read = numk1d
         nump1d = numk1d ! cells == nodes in 1D

         ! 2D mesh
         nump = 0
         numl2d_read = 0
         numk2d_read = 0
         nv = 0
         ierr = ionc_get_2d_mesh_id_ugrid(ioncid, im2d)
         if (ierr == ionc_noerr) then
            ierr = ionc_get_face_count(ioncid, im2d, nump)
            ierr = ionc_get_edge_count(ioncid, im2d, numl2d_read)
            ierr = ionc_get_node_count(ioncid, im2d, numk2d_read)
            ierr = ionc_get_max_face_nodes(ioncid, im2d, nv)
         end if

         nump1d2d = nump1d + nump
         numl_read = numl_read + numl2d_read
         numk_read = numk_read + numk2d_read
         nv = max(nv, 1)

         ! read contacts
         ncontacts = 0
         numl1d2d_read = 0
         ierr = ionc_get_contact_topo_count(ioncid, ncontacts)
         do im = 1, ncontacts
            ierr = ionc_get_contacts_count_ugrid(ioncid, im, numl1d2d_read)
            numl_read = numl_read + numl1d2d_read
            numk_read = numk_read + numl1d2d_read
         end do
      else
         ! No UGRID, not a problem, code below will fall back to trying old format.
         jaugrid = 0

         ierr = unc_open(filename, nf90_nowrite, inetfile)
         call check_error(ierr, 'file '''//trim(filename)//'''')
         if (nerr_ > 0) goto 888

         ! Get nr of cells
         ierr = nf90_inq_dimid(inetfile, 'nNetElem', id_netelemdim)
         call check_error(ierr, 'nNetElem')
         if (nerr_ > 0) goto 888

         ierr = nf90_inquire_dimension(inetfile, id_netelemdim, len=nump1d2d)
         call check_error(ierr, 'Elem count')
         if (nerr_ > 0) goto 888

         ! check number of netlinks in the network file
         ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
         call check_error(ierr, 'nNetLink')
         ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl_read)
         call check_error(ierr, 'link count')

         ! check number of netnodes in the network file
         ierr = nf90_inq_dimid(inetfile, 'nNetNode', id_netnodedim)
         call check_error(ierr, 'nNetNode')
         ierr = nf90_inquire_dimension(inetfile, id_netnodedim, len=numk_read)
         call check_error(ierr, 'Node count')

         ! check max number of vertices
         ierr = nf90_inq_dimid(inetfile, 'nNetElemMaxNode', id_netelemmaxnodedim)
         !    call check_error(ierr, 'nNetElemMaxNode')
         if (ierr /= NF90_NOERR) goto 888
         ierr = nf90_inquire_dimension(inetfile, id_netelemmaxnodedim, len=nv)
         if (ierr /= NF90_NOERR) goto 888

      end if

      ! check number of netlinks in the network file
      if (numl_read /= numl) then
         goto 888
      end if

      ! check number of netnodes in the network file
      if (numk_read /= numk) then
         goto 888
      end if

      call readyy('Reading net data', .1d0)

!    if (nerr_ > 0) goto 888

      !ierr = nf90_inq_varid(inetfile, 'ElemCenter_x', id_elemcenx)
      !call check_error(ierr, 'x coordinate of cell center')
      !ierr = nf90_inq_varid(inetfile, 'ElemCenter_y', id_elemceny)
      !call check_error(ierr, 'y coordinate of cell center')

      call readyy('Reading net data', .3d0)

      call increasenetcells(nump1d2d, 1.0, .false.)

      allocate (netcellnod(nv, nump1d2d)); netcellnod = 0
      allocate (netcelllin(nv, nump1d2d)); netcelllin = 0
      if (jaugrid == 1) then
         if (im2d > 0) then
            ierr = ionc_get_face_nodes(ioncid, im2d, netcellnod(:, 1:nump), fillvalue, 1)
            ! face-node indices:
            ! read from UGRID file:    1:numk2d
            ! stored in network_data:  numk1d+1:numk1d+numk2d (because unc_read_net_ugrid() has read in order: 1D first, 2D second, 1D2D last.)
            ! face-edge indices:
            ! read from UGRID file:    1:numl2d
            ! stored in network_data:  numl1d+1:numl1d+numl2d (because setnodadm() has placed in order: 1D first (incl 1D2D), 2D second.)
            where (netcellnod(:, 1:nump) /= fillvalue) netcellnod(:, 1:nump) = numk1d + netcellnod(:, 1:nump) ! 1D+2D net nodes are stored together in our arrays, so increment the local UGRID 2D face-node indices.
            ierr = ionc_get_face_edges(ioncid, im2d, netcelllin(:, 1:nump), fillvalue, 1)
            if (ierr == ionc_noerr) then
               where (netcelllin(:, 1:nump) /= fillvalue) netcelllin(:, 1:nump) = numl1d + netcelllin(:, 1:nump) ! 1D+2D net links are stored together in our arrays, so increment the local UGRID 2D face-edge indices.
            else
               ! Face-edge-connectivity is optional in UGRID, so when not found, reconstruct them ourselves.
               call ggeo_construct_netcelllin_from_netcellnod(nump, netcellnod, netcelllin)
            end if
         else
            fillvalue = 0
         end if
         ! Fill 1D netcells with default linear order 1:nump1d
         do n = 1, nump1d
            ! Note: the node ordering construction below may be different when compared to find1dcells().
            ! For pure 1D models, results are identical if also the netlinks are sorted in the input:
            ! by increasing branch index, and within a branch by increasing chainage.
            ! This is because when reading non-UGRID files (or forcing --findcells), 1D order comes from 1d *links*).
            ! When 1D2D links are also present, after the regular 1D links, then the ordering below
            ! may differ from the order if it had been produced with find1dcells(). In the case of orphan
            ! 1D2D links. Results can still be correct, only with different 1D flow node ordering.
            netcellnod(1, nump + n) = n
         end do

      else
         ierr = nf90_inq_varid(inetfile, 'NetElemNode', id_netelemnode)
         !    call check_error(ierr, 'NetElemNode')
         if (ierr /= NF90_NOERR) goto 888
         ierr = nf90_inq_varid(inetfile, 'NetElemLink', id_netelemlink)
         !    call check_error(ierr, 'NetElemLink')
         if (ierr /= NF90_NOERR) goto 888

         ierr = nf90_get_att(inetfile, id_netelemnode, '_FillValue', fillvalue)
         ! Read Netcell connectivity arrays
         ierr = nf90_get_var(inetfile, id_netelemnode, netcellnod)
         call check_error(ierr, 'cell elem.')
         ierr = nf90_get_var(inetfile, id_netelemlink, netcelllin)
         call check_error(ierr, 'cell link')
      end if

      ! Now reconstruct the netcell array for 1D and 2D (in two halves), same for UGRID and non-UGRID.
      nump = 0
      do n = 1, nump1d2d
         if (netcelllin(1, n) == 0) then ! this is a 1d cell
            call realloc(netcell(n)%nod, 1, keepExisting=.false.)
            call realloc(netcell(n)%lin, 1, keepExisting=.false.)
            netcell(n)%nod = netcellnod(1, n)
            netcell(n)%lin = 0
            netcell(n)%n = 1
         else
            nump = nump + 1 ! update Nr of 2D cells
            s = nv ! s will be computed to for Nr of nodes of this cell
            do k = nv, 1, -1
               if (netcellnod(k, n) /= fillvalue) then
                  s = k
                  exit
               end if
            end do
            call realloc(netcell(n)%nod, s, keepExisting=.false.)
            call realloc(netcell(n)%lin, s, keepExisting=.false.)
            netcell(n)%nod = netcellnod(1:s, n)
            netcell(n)%lin = netcelllin(1:s, n)
            netcell(n)%n = s
         end if
      end do

      call readyy('Reading net data', .8d0)

      if (jaugrid == 1) then
         ierr = ionc_get_ncid(ioncid, inetfile)
      end if

      ! read idomain
      if (jaidomain_ == 1) then
         if (jaugrid == 1) then
            id_idomain = 0
            call realloc(idomain, nump1d2d, stat=ierr, keepExisting=.false., fill=-999)
            if (im1d > 0) then ! read 1d idomain
               ierr = ionc_inq_varid(ioncid, im1d, 'netelem_domain', id_idomain)
               call check_error(ierr, '1d netelem_domain')
               if (ierr == nf90_noerr) then
                  call realloc(idomain1d, numk1d, keepExisting=.false., fill=-999)
                  ierr = nf90_get_var(inetfile, id_idomain, idomain1d)
               end if
            end if
            if (im2d > 0) then ! read 2d idomain
               ierr = ionc_inq_varid(ioncid, im2d, 'netelem_domain', id_idomain)
               call check_error(ierr, '2d netelem_domain')
               if (ierr == nf90_noerr) then
                  ierr = nf90_get_var(inetfile, id_idomain, idomain(1:nump))
               end if
            end if
            ! Add idomain1d to idomain
            if (id_idomain > 0 .and. ierr == nf90_noerr) then
               do n = nump + 1, nump1d2d
                  k = netcell(n)%nod(1)
                  idomain(n) = idomain1d(k)
               end do
               ierr = nf90_get_att(inetfile, id_idomain, 'valid_max', ndomains)
               if (ierr /= nf90_noerr) then
                  ndomains = 0
               end if
            else
               ndomains = 0
            end if
         else ! non-UGRID
            ierr = nf90_inq_varid(inetfile, 'idomain', id_idomain)
            call check_error(ierr, 'idomain')
            if (ierr == nf90_noerr) then
               call realloc(idomain, nump1d2d, stat=ierr, keepExisting=.false.)
               ierr = nf90_get_var(inetfile, id_idomain, idomain, count=(/nump1d2d/))
               ierr = nf90_get_att(inetfile, id_idomain, 'valid_max', ndomains)
               if (ierr /= nf90_noerr) then
                  ndomains = 0
               end if
            else
               ndomains = 0
            end if
         end if
         if (ndomains == 0) then ! no subdomain numbers in netfile
            if (allocated(idomain)) deallocate (idomain)
         end if
      end if

      if (jaiglobal_s_ == 1) then
!      store nerr_
         nerr_store = nerr_
         if (jaugrid == 1) then
            id_iglobal_s = 0
            call realloc(iglobal_s, nump1d2d, stat=ierr, keepExisting=.false., fill=-999)
            if (im1d > 0) then ! read 1d globalnr
               ierr = ionc_inq_varid(ioncid, im1d, 'netelem_globalnr', id_iglobal_s)
               call check_error(ierr, '1d netelem_globalnr')
               if (ierr == nf90_noerr) then
                  call realloc(iglobal_s1d, numk1d, keepExisting=.false., fill=-999)
                  ierr = nf90_get_var(inetfile, id_iglobal_s, iglobal_s1d)
               end if
            end if
            if (im2d > 0) then ! read 2d globalnr
               ierr = ionc_inq_varid(ioncid, im2d, 'netelem_globalnr', id_iglobal_s)
               call check_error(ierr, '2d netelem_globalnr')
               if (ierr == nf90_noerr) then
                  ierr = nf90_get_var(inetfile, id_iglobal_s, iglobal_s(1:nump))
               end if
            end if
            ! Add iglobal_s1d to iglobal_s
            if (id_iglobal_s > 0 .and. ierr == nf90_noerr) then
               do n = nump + 1, nump1d2d
                  k = netcell(n)%nod(1)
                  iglobal_s(n) = iglobal_s1d(k)
               end do
               ierr = nf90_get_att(inetfile, id_iglobal_s, 'valid_max', Nglobal_s)
               if (ierr /= nf90_noerr) then
                  Nglobal_s = 0
               end if
            else
               Nglobal_s = 0
            end if
         else ! non-UGRID
            ierr = nf90_inq_varid(inetfile, 'iglobal_s', id_iglobal_s)
            call check_error(ierr, 'iglobal_s')
            if (ierr == nf90_noerr) then
               call realloc(iglobal_s, nump1d2d, stat=ierr, keepExisting=.false.)
               ierr = nf90_get_var(inetfile, id_iglobal_s, iglobal_s, count=(/nump1d2d/))
               ierr = nf90_get_att(inetfile, id_iglobal_s, 'valid_max', Nglobal_s)
               if (ierr /= nf90_noerr) then
                  Nglobal_s = 0
               end if
            else
               Nglobal_s = 0
            end if
         end if
         if (Nglobal_s == 0) then ! no global cell numbers in netfile (not a problem)
            if (allocated(iglobal_s)) deallocate (iglobal_s)
         end if
!      restore nerr_
         nerr_ = nerr_store
      end if

      if (jareinitialize_ /= 0) then ! when re-initialize in GUI, need to read KN, since KN has been changed in renumberflownode
         if (jaugrid == 1) then
            if (im2d > 0) then
               ! Fortunately, only the 2D net links have been permuted by renumberFlowNodes(), so we only re-read the 2D edges here.
               allocate (kn12(2, numl2d_read))
               ierr = ionc_get_edge_nodes(ioncid, im2d, kn12, 1)
               do L = 1, numl2d_read
                  kn(1:2, numl1d + L) = numk1d + kn12(1:2, L)
                  kn(3, numl1d + L) = 2
               end do
            end if
         else
            ierr = nf90_inq_dimid(inetfile, 'nNetLink', id_netlinkdim)
            call check_error(ierr, 'nNetLink')
            ierr = nf90_inquire_dimension(inetfile, id_netlinkdim, len=numl)
            call check_error(ierr, 'link count')
            if (nerr_ > 0) goto 888

            ierr = nf90_inq_varid(inetfile, 'NetLink', id_netlink)
            call check_error(ierr, 'NetLink')
            if (ierr == 0) then
               allocate (kn_tmp(2, numl))
               allocate (ltype_tmp(numl))
               ierr = nf90_get_var(inetfile, id_netlink, kn_tmp)
               call check_error(ierr, 'NetLink')
               kn(1:2, 1:numl) = kn_tmp(1:2, 1:numl)

               ierr = nf90_inq_varid(inetfile, 'NetLinkType', id_netlinktype)
               ierr = nf90_get_var(inetfile, id_netlinktype, ltype_tmp)
               call check_error(ierr, 'NetLinkType')
               kn(3, 1:numl) = ltype_tmp(1:numl)
               deallocate (kn_tmp, ltype_tmp)
               call check_error(ierr, 'NetLink')
            end if
         end if
      end if

      call readyy('Reading net data', 1d0)
      ierr = ionc_close(ioncid)
      if (nerr_ > 0) goto 888

      call readyy('Reading net data', -1d0)
      return ! Return with success

888   continue
      ! Some error occurred
      ierr = 1
      nerr_ = 0
   end subroutine readcells

   subroutine find_flownodesorlinks_merge(n, x, y, n_loc, n_own, iloc_own, iloc_merge, janode, jaerror2sam, inode_merge2loc)
      use kdtree2Factory
      use unstruc_messages
      use m_flowgeom
      use network_data
      use m_missing, only: dmiss
      use m_sferic, only: jsferic
      use m_samples
      use m_alloc
      use m_wall_clock_time

      implicit none
      type(kdtree_instance) :: treeinst
      integer, intent(in) :: n !< number of flownodes in merged map file
      double precision, dimension(n), intent(in) :: x, y !< coordinates of flownode circumcenters or flowlink centers in merged map file
      integer, intent(in) :: n_loc !< number of flownodes or flowlinks of the current subdomain (including ghosts)
      integer, intent(in) :: n_own !< number of flownodes or flowlinks of the current subdomain (excluding ghosts)
      integer, intent(in) :: janode !< if janode==1, find flow nodes, otherwise find flow links
      integer, dimension(n_loc), intent(inout) :: iloc_own !< mapping to the actual index on the current subdomain
      integer, dimension(n_loc), intent(inout) :: iloc_merge !< mapping to the index in the merged map file
      integer, dimension(n), optional, intent(inout) :: inode_merge2loc !< mapping from the index in the merged map file
      integer, intent(in) :: jaerror2sam !< add unfound nodes to samples (1) or not (0)
      integer :: ierror = 1
      integer :: k, nn, i, jj, kk, jamerge2own
      double precision :: R2search = 1d-8 !< Search radius
      double precision :: t0, t1
      character(len=128) :: mesg
      double precision, allocatable :: x_tmp(:), y_tmp(:)

      call wall_clock_time(t0)
      if (present(inode_merge2loc)) then
         jamerge2own = 1
      else
         jamerge2own = 0
      end if

      allocate (x_tmp(n_loc))
      allocate (y_tmp(n_loc))
      if (janode == 1) then
         x_tmp = xzw
         y_tmp = yzw
      else if (janode == 2) then ! boundary waterlevel points
         x_tmp = xz(ndxi + 1:ndx)
         y_tmp = yz(ndxi + 1:ndx)
      else
         x_tmp = xu
         y_tmp = yu
      end if

      !build kdtree
      call build_kdtree(treeinst, n, x, y, ierror, jsferic, dmiss)
      if (ierror /= 0) then
         goto 1234
      end if

      call mess(LEVEL_INFO, 'Restart parallel run: Finding flow nodes/ flow links...')

!  clear samples
      do k = 1, n_own
         !  fill query vector
         kk = iloc_own(k)
         call make_queryvector_kdtree(treeinst, x_tmp(kk), y_tmp(kk), jsferic)
         !  count number of points in search area
         NN = kdtree2_r_count(treeinst%tree, treeinst%qv, R2search)
         if (NN == 0) then
!           call mess(LEVEL_INFO, 'No flownode/flowlink is found')

            if (jaerror2sam == 1) then
               !          add to samples
               if (NS < 5) then
                  call mess(LEVEL_INFO, 'copying unfound flownodes/links to samples')
               end if
               call INCREASESAM(NS + 1)
               NS = NS + 1
               xs(Ns) = x_tmp(kk)
               ys(NS) = y_tmp(kk)
               zs(NS) = dble(NN)
            end if

            cycle ! no points found
         else
            !  reallocate if necessary
            call realloc_results_kdtree(treeinst, NN)

            !  find nearest NN samples
            if (NN > 1) then ! If we found more candidates within small search radius, then we should consider falling back to inside-polygon check of cell contour
!               if (janode == 1) then
!                  write (msgbuf, '(a,i0,a,i0,a)') 'Multiple flow nodes in merged restart file can be matched with current model''s node #', kk, '. Nr of candidates: ', NN, '. Picking last.'
!               else
!                  write (msgbuf, '(a,i0,a,i0,a)') 'Multiple flow links in merged restart file can be matched with current model''s link #', kk, '. Nr of candidates: ', NN, '. Picking last.'
!               endif
!               call err_flush()
!               ! TODO: AvD: return error code from this routine

               if (jaerror2sam == 1) then
                  !          add to samples
!                 call mess(LEVEL_INFO, 'copying double or more found flownodes/links to samples')
                  call INCREASESAM(NS + 1)
                  NS = NS + 1
                  xs(Ns) = x_tmp(kk)
                  ys(NS) = y_tmp(kk)
                  zs(NS) = dble(NN)
               end if

            end if

            call kdtree2_n_nearest(treeinst%tree, treeinst%qv, NN, treeinst%results)
            do i = 1, NN
               jj = treeinst%results(i)%idx
               iloc_merge(k) = jj
               if (jamerge2own > 0) then
                  inode_merge2loc(jj) = k
               end if
            end do
         end if
      end do

      call wall_clock_time(t1)

      write (mesg, "('done in ', F12.5, ' sec.')") t1 - t0
      call mess(LEVEL_INFO, trim(mesg))
      ierror = 0
1234  continue

!    deallocate
      if (treeinst%itreestat /= ITREE_EMPTY) then
         call delete_kdtree2(treeinst)
      end if

      return
   end subroutine find_flownodesorlinks_merge

!! check if the flownodes or flowlinks in the current model have the same numbering with in the rst file
   subroutine check_flownodesorlinks_numbering_rst(n, janode, x_rst, y_rst, ierror)
      use network_data, only: xzw, yzw
      use m_flowgeom, only: xu, yu
      use unstruc_messages
      use m_missing, only: dmiss
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D
      use m_partitioninfo, only: jampi, my_rank
      implicit none

      integer, intent(in) :: n ! Number of flownodes/flowlinks to be checked
      integer, intent(in) :: janode !if janode==1, find flow nodes, otherwise find flow links
      double precision, dimension(:), intent(in) :: x_rst, y_rst ! Coordinates read from rst file
      integer, intent(out) :: ierror

      double precision, allocatable :: x_tmp(:), y_tmp(:)
      integer :: i
      double precision :: dist, dtol = 1d-8
      character(len=128) :: message

      ierror = 0

      allocate (x_tmp(n))
      allocate (y_tmp(n))
      if (janode == 1) then
         call mess(LEVEL_INFO, 'Check flownodes numbering when restart:')
         x_tmp = xzw
         y_tmp = yzw
      else
         call mess(LEVEL_INFO, 'Check flowlinks numbering when restart:')
         x_tmp = xu
         y_tmp = yu
      end if

      do i = 1, n
         dist = dbdistance(x_tmp(i), y_tmp(i), x_rst(i), y_rst(i), jsferic, jasfer3D, dmiss)
         if (dist > dtol) then
            ierror = 1
            if (janode == 1) then
               if (jampi > 0) then
                  write (message, "('my_rank=', I0, ': flownode mismatches: node=', I0,'.')") my_rank, i
                  call mess(LEVEL_ERROR, trim(message))
               else
                  write (message, "('flownode mismatches: node=', I0,'.')") i
                  call mess(LEVEL_ERROR, trim(message))
               end if
            else
               if (jampi > 0) then
                  write (message, "('my_rank=', I0, ': flowlink mismatches: link=', I0,'.')") my_rank, i
                  call mess(LEVEL_ERROR, trim(message))
               else
                  write (message, "('flowlink mismatches: link=', I0,'.')") i
                  call mess(LEVEL_ERROR, trim(message))
               end if
            end if

            exit
         end if
      end do
      return
   end subroutine check_flownodesorlinks_numbering_rst

!> Reads the 1d mesh assuming at least two nodes per branch (old file version)
!! This old reader does not support parallel models.
   integer function read_1d_mesh_convention_one(ioncid, numk_keep, numl_keep, numk_last, numl_last) result(ierr)

      use network_data
      use unstruc_channel_flow
      use m_1d_networkreader
      use m_flow1d_reader
      use m_profiles
      use gridoperations
      use m_save_ugrid_state
      use m_missing

      integer, intent(in) :: ioncid
      integer, intent(inout) :: numk_keep
      integer, intent(inout) :: numl_keep
      integer, intent(inout) :: numk_last
      integer, intent(inout) :: numl_last

      !locals
      type(t_branch), pointer :: pbr
      type(t_node), pointer :: pnod
      integer :: inod, ibr, ngrd, k
      logical :: dflowfm_1d

      !reduce the scope, do deallocation here
      if (allocated(mesh1dNodeIds)) then
         deallocate (mesh1dNodeIds)
      end if
      if (allocated(mesh1dUnmergedToMerged)) then
         deallocate (mesh1dUnmergedToMerged)
      end if
      !if (allocated(mesh1dMergedToUnMerged)) deallocate(mesh1dMergedToUnMerged)

      allocate (mesh1dNodeIds(size(xk)))
      allocate (mesh1dUnmergedToMerged(size(xk)))
      !allocate(mesh1dMergedToUnMerged(size(xk)))

      numMesh1dBeforeMerging = 0
      ierr = 0
      if (.not. network%loaded) then
         dflowfm_1d = .true.
!      call read_1d_ugrid(network, ioncid, dflowfm_1d, nodesOnBranchVertices = 1)
         call read_1d_ugrid(network, ioncid, dflowfm_1d)
         if (network%loaded) then
            call admin_network(network, numl)

         !! TODO: Start temporary fix, to be removed when 1d ugrid file is correct (at this point branches are not connected)
            do inod = 1, network%nds%Count
               pnod => network%nds%node(inod)
               pnod%gridNumber = 0
            end do

            numk = 0
            numl = 0
            do ibr = 1, network%brs%Count
               pbr => network%brs%branch(ibr)
               ! first step add coordinates and bed levels to nodes
               if (pbr%FromNode%gridNumber == 0) then ! TODO: Not safe in parallel models (check gridpointsseq as introduced in UNST-5013)
                  numk = numk + 1
                  pbr%FromNode%gridNumber = numk
                  xk(numk) = pbr%Xs(1)
                  yk(numk) = pbr%Ys(1)
                  zk(numk) = dmiss
               end if
               pbr%grd(1) = pbr%FromNode%gridNumber
               ! id-mesh node mapping
               numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
               mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridPointIDs(1)
               mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(1)
               !mesh1dMergedToUnMerged(pbr%grd(1)) = numMesh1dBeforeMerging
               ngrd = pbr%gridPointsCount

               do k = 2, ngrd - 1
                  ! i do not want to renumber the nodes, otherwise link info gets invalidated
                  numk = numk + 1
                  pbr%grd(k) = numk
                  xk(numk) = pbr%Xs(k)
                  yk(numk) = pbr%Ys(k)
                  zk(numk) = dmiss
                  ! id-mesh node mapping
                  numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
                  mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridpointids(k)
                  mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(k)
                  !mesh1dMergedToUnMerged(pbr%grd(k)) = numMesh1dBeforeMerging
               end do
               if (pbr%toNode%gridNumber == 0) then
                  numk = numk + 1
                  pbr%toNode%gridNumber = numk
                  xk(numk) = pbr%Xs(ngrd)
                  yk(numk) = pbr%Ys(ngrd)
                  zk(numk) = dmiss
               end if
               pbr%grd(ngrd) = pbr%toNode%gridNumber
               ! id-mesh node mapping
               numMesh1dBeforeMerging = numMesh1dBeforeMerging + 1
               mesh1dNodeIds(numMesh1dBeforeMerging) = pbr%gridPointIDs(ngrd)
               mesh1dUnmergedToMerged(numMesh1dBeforeMerging) = pbr%grd(ngrd)
               !mesh1dMergedToUnMerged(pbr%grd(ngrd)) = numMesh1dBeforeMerging

               ! second step create links
               do k = 1, ngrd - 1
                  numl = numl + 1
                  kn(1, numl) = pbr%grd(k)
                  kn(2, numl) = pbr%grd(k + 1)
                  kn(3, numl) = 1
               end do

            end do

            network%numk = numk
            network%numl = numl

            numk_keep = numk
            numl_keep = numl

            numk_last = numk
            numl_last = numl
            ! End temporary fix
            ! TODO: Once dflowfm's own 1D and the flow1d code are aligned, the following switch should probably disappear.

            jainterpolatezk1D = 0
         else
            network%numk = 0
            network%numl = 0
         end if
      end if

      ! re-allocate mesh1dNodeIds and mesh1dUnmergedToMerged
      call realloc(mesh1dNodeIds, numMesh1dBeforeMerging, keepExisting=.true.)
      call realloc(mesh1dUnmergedToMerged, numMesh1dBeforeMerging, keepExisting=.true.)

   end function read_1d_mesh_convention_one

!> Read structure infomation from the rst file
   subroutine read_structures_from_rst(ncid, filename, it_read)
      use unstruc_channel_flow, only: network
      use m_alloc
      use m_GlobalParameters
      use m_flow, only: au
      use m_1d_structures
      use m_General_Structure
      use fm_external_forcings_data
      use m_longculverts
      implicit none
      integer, intent(in) :: ncid !< ID of the rst file
      character(len=*), intent(in) :: filename !< Name of rst file.
      integer, intent(in) :: it_read !< time index for reading

      integer :: nStruDim
      character(len=IdLen), allocatable :: struDimNames(:)
      integer, allocatable :: ids_struDim(:)
      character(len=IdLen) :: struName
      double precision, allocatable :: tmpvar(:), tmpvar3d(:, :, :), tmpvar2d(:, :)
      integer, allocatable :: tmpvar3di(:, :, :), tmpvar2di(:, :)
      integer :: strucDimErr, i, nLinks, nStru, ierr, iStru, nfuru, numlinks, strucVarErr, L, L0, nstages, maxNumStages
      integer :: id_culvert_openh, id_longculvert_valveopen, &
                 id_genstru_crestl, id_genstru_edgel, id_genstru_openw, id_genstru_fu, id_genstru_ru, id_genstru_au, id_genstru_crestw, id_genstru_area, id_genstru_linkw, id_genstru_state, id_genstru_sOnCrest, &
                 id_weirgen_crestl, id_weirgen_crestw, id_weirgen_area, id_weirgen_linkw, id_weirgen_fu, id_weirgen_ru, id_weirgen_au, id_weirgen_state, id_weirgen_sOnCrest, &
                 id_orifgen_crestl, id_orifgen_edgel, id_orifgen_openw, id_orifgen_fu, id_orifgen_ru, id_orifgen_au, id_orifgen_crestw, &
                 id_orifgen_area, id_orifgen_linkw, id_orifgen_state, id_orifgen_sOnCrest, &
                 id_pump_cap, id_pump_ssTrigger, id_pump_dsTrigger
      type(t_structure), pointer :: pstru
      type(t_GeneralStructure), pointer :: genstr

      strucDimErr = 0
      strucVarErr = 0

      if (.not. network%loaded) then
         !call mess(LEVEL_WARN, 'read_structures_from_rst: the network array is not loaded, then skip reading structures. The simulation will continue but the results may not be reliable.')
         return
      end if

      if (nlongculverts > 0) then

         call realloc(tmpvar, nlongculverts, stat=ierr, keepExisting=.false.)
         ierr = nf90_inq_varid(ncid, 'longculvert_valve_relative_opening', id_longculvert_valveopen)
         ierr = nf90_get_var(ncid, id_longculvert_valveopen, tmpvar, start=(/1, it_read/), count=(/nlongculverts, 1/))
         call check_error(ierr, 'longculvert_valve_relative_opening", The simulation will continue but the results may not be reliable.', LEVEL_WARN)

         if (ierr == 0) then
            do i = 1, nlongculverts
               longculverts(i)%valve_relative_opening = tmpvar(i)
            end do
         end if

      end if

      ! Read info. of culverts
      nStru = network%sts%numCulverts
      if (nStru > 0) then
         ! read dimensions
         struName = 'culvert'
         nStruDim = 1
         call realloc(struDimNames, nStruDim)
         call realloc(ids_struDim, nStruDim)
         struDimNames(1) = 'nCulvert'
         call read_structure_dimensions_from_rst(ncid, filename, ST_CULVERT, trim(struName), nStruDim, struDimNames, ids_struDim, network%sts%numCulverts, strucDimErr)

         ! read variables
         if (strucDimErr == 0) then
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'culvert_valve_opening_height', id_culvert_openh)
            ierr = nf90_get_var(ncid, id_culvert_openh, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"culvert_valve_opening_height", The simulation will continue but the results may not be reliable.', LEVEL_WARN)

            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%culvertIndices(i)
                  network%sts%struct(istru)%culvert%valveOpening = tmpvar(i)
               end do
            end if

         end if
      end if

      ! Read info. of general structures
      nStru = network%sts%numGeneralStructures
      if (nStru > 0) then
         ! read dimensions
         struName = 'general structure'
         nStruDim = 3
         call realloc(struDimNames, nStruDim)
         call realloc(ids_struDim, nStruDim)
         struDimNames(1) = 'nGeneral_structures'
         struDimNames(2) = 'nGeneral_structure_furu'
         struDimNames(3) = 'nGeneral_structure_max_numlinks'
         call read_structure_dimensions_from_rst(ncid, filename, ST_GENERAL_ST, trim(struName), nStruDim, struDimNames, ids_struDim, nStru, strucDimErr)

         ! read variables
         if (strucDimErr == 0) then
            ! The following variables are crucial for computation
            ! read general_structure_crest_level
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'general_structure_crest_level', id_genstru_crestl)
            ierr = nf90_get_var(ncid, id_genstru_crestl, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"general_structure_crest_level", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%generalStructureIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%zs_actual = tmpvar(i)
                  genstr%zs = tmpvar(i)
               end do
            end if

            ! read general_structure_crest_width
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'general_structure_crest_width', id_genstru_crestw)
            ierr = nf90_get_var(ncid, id_genstru_crestw, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"general_structure_crest_width", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%generalStructureIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%ws_actual = tmpvar(i)
                  genstr%ws = tmpvar(i)
               end do
            end if

            ! read general_structure_gate_lower_edge_level
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'general_structure_gate_lower_edge_level', id_genstru_edgel)
            ierr = nf90_get_var(ncid, id_genstru_edgel, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"general_structure_gate_lower_edge_level", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%generalStructureIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%gateLowerEdgeLevel_actual = tmpvar(i)
                  genstr%gateLowerEdgeLevel = tmpvar(i)
               end do
            end if

            ! read general_structure_gate_opening_width
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'general_structure_gate_opening_width', id_genstru_openw)
            ierr = nf90_get_var(ncid, id_genstru_openw, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"general_structure_gate_opening_width", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%generalStructureIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%gateopeningwidth_actual = tmpvar(i)
                  genstr%gateopeningwidth = tmpvar(i)
               end do
            end if

            nfuru = 0
            numlinks = 0
            ierr = nf90_inquire_dimension(ncid, ids_struDim(2), len=nfuru)
            ierr = nf90_inquire_dimension(ncid, ids_struDim(3), len=numlinks)
            if (ierr == 0 .and. nfuru > 0 .and. numlinks > 0) then
               ! read general_structure_flow_area
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_flow_area', id_genstru_area)
               ierr = nf90_get_var(ncid, id_genstru_area, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_flow_area", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%au(1:nLinks) = tmpvar2d(1:nLinks, i)
                     do L0 = 1, nLinks
                        L = abs(pstru%linknumbers(L0))
                        au(L) = tmpvar2d(L0, i)
                     end do
                  end do
               end if

               ! read general_structure_link_width_closed_by_gate
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_link_width_closed_by_gate', id_genstru_linkw)
               ierr = nf90_get_var(ncid, id_genstru_linkw, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_link_width_closed_by_gate", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%gateclosedfractiononlink(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if

               ! read general_structure_fu
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_fu', id_genstru_fu)
               ierr = nf90_get_var(ncid, id_genstru_fu, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_fu", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%fu(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read general_structure_ru
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_ru', id_genstru_ru)
               ierr = nf90_get_var(ncid, id_genstru_ru, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_ru", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%ru(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read general_structure_au
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_au', id_genstru_au)
               ierr = nf90_get_var(ncid, id_genstru_au, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_au", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%au(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! The following variables are only for the output at the initial time in history file
               ! read general_structure_state
               call realloc(tmpvar3di, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_state', id_genstru_state)
               ierr = nf90_get_var(ncid, id_genstru_state, tmpvar3di, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_state", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%state(1:nfuru, 1:nLinks) = tmpvar3di(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read general_structure_water_level_on_crest
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'general_structure_water_level_on_crest', id_genstru_sOnCrest)
               ierr = nf90_get_var(ncid, id_genstru_sOnCrest, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"general_structure_water_level_on_crest", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%generalStructureIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%sOnCrest(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if
            else
               call mess(LEVEL_WARN, 'read_structures_from_rst: cannot read variables _fu, _ru, _au, _state and water_level_on_crest of general structure. Skip reading these variables. The simulation will continue but the results may not be reliable.')
            end if
         end if
      end if

      ! Read info. of weirs
      nStru = network%sts%numWeirs
      if (nStru > 0) then
         ! read dimensions
         struName = 'weir'
         nStruDim = 3
         call realloc(struDimNames, nStruDim)
         call realloc(ids_struDim, nStruDim)
         struDimNames(1) = 'nWeirs'
         struDimNames(2) = 'nWeir_furu'
         struDimNames(3) = 'nWeir_max_numlinks'
         call read_structure_dimensions_from_rst(ncid, filename, ST_WEIR, trim(struName), nStruDim, struDimNames, ids_struDim, nStru, strucDimErr)

         ! read variables
         if (strucDimErr == 0) then
            ! The following variables are crucial for computation
            ! read weirgen_crest_level
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'weirgen_crest_level', id_weirgen_crestl)
            ierr = nf90_get_var(ncid, id_weirgen_crestl, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"weirgen_crest_level", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%weirIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%zs_actual = tmpvar(i)
                  genstr%zs = tmpvar(i)
               end do
            end if

            ! read weirgen_crest_width
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'weirgen_crest_width', id_weirgen_crestw)
            ierr = nf90_get_var(ncid, id_weirgen_crestw, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"weirgen_crest_width", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%weirIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%ws_actual = tmpvar(i)
                  genstr%ws = tmpvar(i)
               end do
            end if

            nfuru = 0
            numlinks = 0
            ierr = nf90_inquire_dimension(ncid, ids_struDim(2), len=nfuru)
            ierr = nf90_inquire_dimension(ncid, ids_struDim(3), len=numlinks)
            if (ierr == 0 .and. nfuru > 0 .and. numlinks > 0) then
               ! read weirgen_flow_area
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_flow_area', id_weirgen_area)
               ierr = nf90_get_var(ncid, id_weirgen_area, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_flow_area", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%au(1:nLinks) = tmpvar2d(1:nLinks, i)
                     do L0 = 1, nLinks
                        L = abs(pstru%linknumbers(L0))
                        au(L) = tmpvar2d(L0, i)
                     end do
                  end do
               end if

               ! read weirgen_link_width_closed_by_gate
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_link_width_closed_by_gate', id_weirgen_linkw)
               ierr = nf90_get_var(ncid, id_weirgen_linkw, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_link_width_closed_by_gate", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%gateclosedfractiononlink(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if

               ! read weirgen_fu
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_fu', id_weirgen_fu)
               ierr = nf90_get_var(ncid, id_weirgen_fu, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_fu", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%fu(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read weirgen_ru
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_ru', id_weirgen_ru)
               ierr = nf90_get_var(ncid, id_weirgen_ru, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_ru", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%ru(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read weirgen_au
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_au', id_weirgen_au)
               ierr = nf90_get_var(ncid, id_weirgen_au, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_au", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%au(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! The following variables are only for the output at the initial time in history file
               ! read weirgen_state
               call realloc(tmpvar3di, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_state', id_weirgen_state)
               ierr = nf90_get_var(ncid, id_weirgen_state, tmpvar3di, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_state", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%state(1:nfuru, 1:nLinks) = tmpvar3di(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read weirgen_water_level_on_crest
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'weirgen_water_level_on_crest', id_weirgen_sOnCrest)
               ierr = nf90_get_var(ncid, id_weirgen_sOnCrest, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"weirgen_water_level_on_crest", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%weirIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%sOnCrest(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if
            else
               call mess(LEVEL_WARN, 'read_structures_from_rst: cannot read variables _fu, _ru, _au, _state and water_level_on_crest of weir. Skip reading these variables. The simulation will continue but the results may not be reliable.')
            end if
         end if
      end if

      ! Read info. of orifices
      nStru = network%sts%numOrifices
      if (nStru > 0) then
         ! read dimensions
         struName = 'orifice'
         nStruDim = 3
         call realloc(struDimNames, nStruDim)
         call realloc(ids_struDim, nStruDim)
         struDimNames(1) = 'nOrifices'
         struDimNames(2) = 'nOrifice_furu'
         struDimNames(3) = 'nOrifice_max_numlinks'
         call read_structure_dimensions_from_rst(ncid, filename, ST_ORIFICE, trim(struName), nStruDim, struDimNames, ids_struDim, nStru, strucDimErr)

         ! read variables
         if (strucDimErr == 0) then
            ! The following variables are crucial for computation
            ! read orifice_crest_level
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'orifice_crest_level', id_orifgen_crestl)
            ierr = nf90_get_var(ncid, id_orifgen_crestl, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"orifice_crest_level", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%orificeIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%zs_actual = tmpvar(i)
                  genstr%zs = tmpvar(i)
               end do
            end if

            ! read orifice_crest_width
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'orifice_crest_width', id_orifgen_crestw)
            ierr = nf90_get_var(ncid, id_orifgen_crestw, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"orifice_crest_width", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%orificeIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%ws_actual = tmpvar(i)
                  genstr%ws = tmpvar(i)
               end do
            end if

            ! read orifice_gate_lower_edge_level
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'orifice_gate_lower_edge_level', id_orifgen_edgel)
            ierr = nf90_get_var(ncid, id_orifgen_edgel, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"orifice_gate_lower_edge_level", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%orificeIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%gateLowerEdgeLevel_actual = tmpvar(i)
                  genstr%gateLowerEdgeLevel = tmpvar(i)
               end do
            end if

            ! read orifice_gate_opening_width
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'orifice_gate_opening_width', id_orifgen_openw)
            ierr = nf90_get_var(ncid, id_orifgen_openw, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"orifice_gate_opening_width", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%orificeIndices(i)
                  genstr => network%sts%struct(istru)%generalst
                  genstr%gateopeningwidth_actual = tmpvar(i)
                  genstr%gateopeningwidth = tmpvar(i)
               end do
            end if

            nfuru = 0
            numlinks = 0
            ierr = nf90_inquire_dimension(ncid, ids_struDim(2), len=nfuru)
            ierr = nf90_inquire_dimension(ncid, ids_struDim(3), len=numlinks)
            if (ierr == 0 .and. nfuru > 0 .and. numlinks > 0) then
               ! read orifice_flow_area
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_flow_area', id_orifgen_area)
               ierr = nf90_get_var(ncid, id_orifgen_area, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_flow_area", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%au(1:nLinks) = tmpvar2d(1:nLinks, i)
                     do L0 = 1, nLinks
                        L = abs(pstru%linknumbers(L0))
                        au(L) = tmpvar2d(L0, i)
                     end do
                  end do
               end if

               ! read orifice_link_width_closed_by_gate
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_link_width_closed_by_gate', id_orifgen_linkw)
               ierr = nf90_get_var(ncid, id_orifgen_linkw, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_link_width_closed_by_gate", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%gateclosedfractiononlink(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if
               ! read orifice_fu
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_fu', id_orifgen_fu)
               ierr = nf90_get_var(ncid, id_orifgen_fu, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_fu", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%fu(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read orifice_ru
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_ru', id_orifgen_ru)
               ierr = nf90_get_var(ncid, id_orifgen_ru, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_ru", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%ru(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read orifice_au
               call realloc(tmpvar3d, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_au', id_orifgen_au)
               ierr = nf90_get_var(ncid, id_orifgen_au, tmpvar3d, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_au", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%au(1:nfuru, 1:nLinks) = tmpvar3d(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! The following variables are only for the output at the initial time in history file
               ! read orifice_state
               call realloc(tmpvar3di, (/nfuru, numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_state', id_orifgen_state)
               ierr = nf90_get_var(ncid, id_orifgen_state, tmpvar3di, start=(/1, 1, 1, it_read/), count=(/nfuru, numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_state", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%state(1:nfuru, 1:nLinks) = tmpvar3di(1:nfuru, 1:nLinks, i)
                  end do
               end if

               ! read orifice_water_level_on_crest
               call realloc(tmpvar2d, (/numlinks, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'orifice_water_level_on_crest', id_orifgen_sOnCrest)
               ierr = nf90_get_var(ncid, id_orifgen_sOnCrest, tmpvar2d, start=(/1, it_read/), count=(/numlinks, nStru, 1/))
               call check_error(ierr, '"orifice_water_level_on_crest", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%orificeIndices(i)
                     pstru => network%sts%struct(istru)
                     nLinks = pstru%numlinks
                     pstru%generalst%sOnCrest(1:nLinks) = tmpvar2d(1:nLinks, i)
                  end do
               end if
            else
               call mess(LEVEL_WARN, 'read_structures_from_rst: cannot read variables _fu, _ru, _au, _state and water_level_on_crest of orifice. Skip reading these variables. The simulation will continue but the results may not be reliable.')
            end if
         end if
      end if

      ! Read info. of pumps
      nStru = network%sts%numPumps
      if (nStru > 0) then
         ! read dimensions
         struName = 'pump'
         nStruDim = 2
         call realloc(struDimNames, nStruDim)
         call realloc(ids_struDim, nStruDim)
         struDimNames(1) = 'nPumps'
         struDimNames(2) = 'nPump_max_numstages'
         call read_structure_dimensions_from_rst(ncid, filename, ST_PUMP, trim(struName), nStruDim, struDimNames, ids_struDim, nStru, strucDimErr)

         ! read variables
         if (strucDimErr == 0) then
            ! read pump_capacity
            call realloc(tmpvar, nStru, stat=ierr, keepExisting=.false.)
            ierr = nf90_inq_varid(ncid, 'pump_capacity', id_pump_cap)
            ierr = nf90_get_var(ncid, id_pump_cap, tmpvar, start=(/1, it_read/), count=(/nStru, 1/))
            call check_error(ierr, '"pump_capacity", The simulation will continue but the results may not be reliable.', LEVEL_WARN)

            if (ierr == 0) then
               do i = 1, nStru
                  istru = network%sts%pumpIndices(i)
                  pstru => network%sts%struct(istru)
                  pstru%pump%current_capacity = tmpvar(i)
               end do
            end if

            ierr = nf90_inquire_dimension(ncid, ids_struDim(2), len=maxNumStages)
            if (ierr == 0 .and. maxNumStages > 0) then
               ! read pump_suction_side_trigger
               call realloc(tmpvar2di, (/maxNumStages, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'pump_suction_side_trigger', id_pump_ssTrigger)
               ierr = nf90_get_var(ncid, id_pump_ssTrigger, tmpvar2di, start=(/1, it_read/), count=(/maxNumStages, nStru, 1/))
               call check_error(ierr, '"pump_suction_side_trigger", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%pumpIndices(i)
                     pstru => network%sts%struct(istru)
                     nstages = pstru%pump%nrstages
                     do L0 = 1, nstages
                        if (tmpvar2di(L0, i) == 1) then
                           pstru%pump%ss_trigger(L0) = .true.
                        else
                           pstru%pump%ss_trigger(L0) = .false.
                        end if
                     end do
                  end do
               end if

               ! read pump_delivery_side_trigger
               call realloc(tmpvar2di, (/maxNumStages, nStru/), stat=ierr, keepExisting=.false.)
               ierr = nf90_inq_varid(ncid, 'pump_delivery_side_trigger', id_pump_dsTrigger)
               ierr = nf90_get_var(ncid, id_pump_dsTrigger, tmpvar2di, start=(/1, it_read/), count=(/maxNumStages, nStru, 1/))
               call check_error(ierr, '"pump_delivery_side_trigger", The simulation will continue but the results may not be reliable.', LEVEL_WARN)
               if (ierr == 0) then
                  do i = 1, nStru
                     istru = network%sts%pumpIndices(i)
                     pstru => network%sts%struct(istru)
                     nstages = pstru%pump%nrstages
                     do L0 = 1, nstages
                        if (tmpvar2di(L0, i) == 1) then
                           pstru%pump%ds_trigger(L0) = .true.
                        else
                           pstru%pump%ds_trigger(L0) = .false.
                        end if
                     end do
                  end do
               end if
            else
               call mess(LEVEL_WARN, 'read_structures_from_rst: cannot read variables _sunction_side_trigger of pump. Skip reading these variables. The simulation will continue but the results may not be reliable.')
            end if
         end if
      end if

   end subroutine read_structures_from_rst

!> Read and check all the dimensions of a structure. Return ierr = DFM_NOERR if all the dimensions are correctly read. Otherwise ierr > 0.
   subroutine read_structure_dimensions_from_rst(ncid, filename, istrtypein, struname, nDim, struDimNames, ids_struDim, nstruModel, ierr)
      use dfm_error, only: DFM_GENERICERROR, DFM_NOERR
      use m_alloc
      use m_GlobalParameters
      implicit none
      integer, intent(in) :: ncid !< ID of the rst file
      character(len=*), intent(in) :: filename !< Name of rst file.
      integer, intent(in) :: istrtypein !< The type of the structure. May differ from the struct%type, for example:
      !< an orifice should be called with istrtypein = ST_ORIFICE, whereas its struct(istru)%type = ST_GENERAL_ST.
      character(len=*), intent(in) :: struname !< Name of the structure
      integer, intent(in) :: nDim !< Number of dimensions to be read
      character(len=*), dimension(nDim), intent(in) :: struDimNames !< Names of the structure dimensions
      integer, intent(in) :: nstruModel !< Number of this structure type in the model
      integer, intent(out) :: ierr !< If all dimensions are correclty read, it equals to DFM_NOERR
      integer, dimension(nDim), intent(inout) :: ids_struDim !< Ids of all dimensions
      integer :: nlen

      ierr = DFM_NOERR
      if (nstruModel > 0) then
         ierr = nf90_inq_dimid(ncid, trim(struDimNames(1)), ids_struDim(1))
         if (ierr /= DFM_NOERR) then
            call mess(LEVEL_WARN, 'read_structures_dimensions_from_rst: cannot read dimension '''//trim(struDimNames(1))//''' in restart file '''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
            return
         else
            ierr = nf90_inquire_dimension(ncid, ids_struDim(1), len=nlen)
            call check_error(ierr, trim(struDimNames(1)))
            if (nlen /= nstruModel) then
               call qnerror('Error reading '''//trim(filename)//''': Number of '''//trim(struname)//''' read unequal to number of '''//trim(struname)//''' in model', ' ', ' ')
               ierr = DFM_GENERICERROR
               return
            end if
         end if

         if (any(istrtypein == (/ST_GENERAL_ST, ST_WEIR, ST_ORIFICE/))) then
            ierr = nf90_inq_dimid(ncid, trim(struDimNames(2)), ids_struDim(2))
            if (ierr /= DFM_NOERR) then
               call mess(LEVEL_WARN, 'read_structures_dimensions_from_rst: cannot read dimension '''//trim(struDimNames(2))//''' in restart file '''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
               return
            else
               ierr = nf90_inq_dimid(ncid, trim(struDimNames(3)), ids_struDim(3))
               if (ierr /= DFM_NOERR) then
                  call mess(LEVEL_WARN, 'read_structures_dimensions_from_rst: cannot read dimension '''//trim(struDimNames(3))//''' in restart file '''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
                  return
               end if
            end if
         end if

         if (istrtypein == ST_PUMP) then
            ierr = nf90_inq_dimid(ncid, trim(struDimNames(2)), ids_struDim(2))
            if (ierr /= DFM_NOERR) then
               call mess(LEVEL_WARN, 'read_structures_dimensions_from_rst: cannot read dimension '''//trim(struDimNames(2))//''' in restart file '''//trim(filename)//'''. The simulation will continue but the results may not be reliable.')
               return
            end if
         end if
      end if

   end subroutine read_structure_dimensions_from_rst

!> Defines a new variable in a NetCDF dataset, also setting some frequently used attributes.
   subroutine definencvar(ncid, idq, itype, idims, name, long_name, unit, namecoord, geometry, fillVal, add_gridmapping, extra_attributes)
      use netcdf
      use netcdf_utils
      use m_sferic
      use m_missing, only: dmiss, intmiss
      implicit none

      integer, intent(in) :: ncid !< NetCDF dataset id.
      integer, intent(inout) :: idq !< NetCDF variable id for the newly created variable.
      integer, intent(in) :: itype !< data type, one of the standard nf90_* data types.
      integer, intent(in) :: idims(:) !< NetCDF dimension id(s) for this variable.
      character(len=*), intent(in) :: name !< Variable name in the dataset
      character(len=*), optional, intent(in) :: long_name !< Description of the variable, used in the :long_name attribute.
      character(len=*), optional, intent(in) :: unit !< Units of the variable (udunit-compatible), used in the :units attribute.
      character(len=*), optional, intent(in) :: namecoord !< Text string the with coordinate variable names, used in the :coordinates attribute.
      real(dp), optional, intent(in) :: fillVal !< Fill value that will be stored in the standard :_FillValue attribute
      character(len=*), optional, intent(in) :: geometry !< (optional) Variable name of a geometry variable in the same dataset, used in the :geometry attribute.
      logical, optional, intent(in) :: add_gridmapping !< Whether or not to add a grid mapping attribute. Default: false.. Only use this if your coordinates in namecoord rely on this grid mapping.
      type(ug_nc_attribute), optional, intent(in) :: extra_attributes(:) !< (optional) Set containing additional custom NetCDF attributes for this variable.

      integer :: ierr, int_fill
      real(dp) :: dp_fill
      logical :: add_gridmapping_

      ierr = nf90_noerr

      if (present(add_gridmapping)) then
         add_gridmapping_ = add_gridmapping
      else
         add_gridmapping_ = .false.
      end if

      call check_netcdf_error(nf90_def_var(ncid, name, itype, idims, idq))
      if (present(namecoord)) then
         call check_netcdf_error(nf90_put_att(ncid, idq, 'coordinates', namecoord))
      end if
      if (present(long_name)) then
         call check_netcdf_error(nf90_put_att(ncid, idq, 'long_name', long_name))
      end if
      if (present(unit)) then
         call check_netcdf_error(nf90_put_att(ncid, idq, 'units', unit))
      end if

      if (add_gridmapping_) then
         call check_netcdf_error(unc_add_gridmapping_att(ncid, (/idq/), jsferic))
      end if

      if (present(geometry)) then
         call check_netcdf_error(nf90_put_att(ncid, idq, 'geometry', geometry))
      end if

      if (present(fillVal)) then
         dp_fill = fillVal
         int_fill = int(fillVal)
      else
         dp_fill = dmiss
         int_fill = intmiss
      end if
      ! Add a fill value of the correct type
      select case (itype)
      case (nf90_short, nf90_int)
         call check_netcdf_error(nf90_put_att(ncid, idq, '_FillValue', int_fill))
      case (nf90_float)
         call check_netcdf_error(nf90_put_att(ncid, idq, '_FillValue', real(dp_fill)))
      case (nf90_double)
         call check_netcdf_error(nf90_put_att(ncid, idq, '_FillValue', dp_fill))
      case (nf90_char)
         continue
      case default
         call mess(LEVEL_ERROR, 'unstruc_netcdf/definencvar: invalid netcdf type for fill_value!')
      end select

      if (present(extra_attributes)) then
         if (size(extra_attributes) > 0) then
            call check_netcdf_error(ncu_put_var_attset(ncid, idq, extra_attributes))
         end if
      end if

   end subroutine definencvar

!> Converts between logical values and integers for hysteresis of summer dikes, depending on logic2int.
!! The approach of this conversion is, a 1d link L has two logical values hysteresis(1,L) and hysteresis(2,L).
!! Four integers 0, 1, 2, and 3 represent the following situations:
!! |  value  |  hysteresis(1,L)  |  hysteresis(2,L) |
!! !  3      |  .true.           |  .true.          |
!! |  2      |  .false.          |  .true.          |
!! |  1      |  .true.           |  .false.         |
!! |  0      |  .false.          |  .false.         |
!! Using this approach, the original 2d array, of size (/2, numl1d/), can be stored in a 1d array work1di.
   subroutine convert_hysteresis_summerdike(logic2int, work1di)
      use unstruc_channel_flow, only: network, useVolumeTables
      use m_flowgeom
      use m_VolumeTables

      implicit none
      logical, intent(in) :: logic2int !< true: convert from logic values to integers
      !< false: convert from integers to logic values
      integer, dimension(:), intent(inout) :: work1di !< array storing integers

      integer :: L, irem, k, i

      if (logic2int) then
         if (useVolumeTables) then
            do k = 1, ndx1d
               do i = 1, vltb(k)%numberOfSummerDikes
                  L = vltb(k)%linkNumber(i)
                  if (k == ln(1, L)) then
                     network%adm%hysteresis_for_summerdike(1, L) = vltb(k)%inundationPhase(i)
                  else
                     network%adm%hysteresis_for_summerdike(2, L) = vltb(k)%inundationPhase(i)
                  end if
               end do
            end do
         end if

         do L = 1, network%numl
            work1di(L) = 1 * merge(1, 0, network%adm%hysteresis_for_summerdike(1, L)) &
                         + 2 * merge(1, 0, network%adm%hysteresis_for_summerdike(2, L))
         end do
      else
         do L = 1, network%numl
            irem = modulo(work1di(L), 2)

            network%adm%hysteresis_for_summerdike(1, L) = (irem == 1)
            network%adm%hysteresis_for_summerdike(2, L) = (work1di(L) - irem == 2)
         end do

         if (useVolumeTables) then
            ndx1d = ndx - ndx2d
            do k = 1, ndx1d
               do i = 1, vltb(k)%numberOfSummerDikes
                  L = vltb(k)%linkNumber(i)
                  if (k == ln(1, L)) then
                     vltb(k)%inundationPhase(i) = network%adm%hysteresis_for_summerdike(1, L)
                  else
                     vltb(k)%inundationPhase(i) = network%adm%hysteresis_for_summerdike(2, L)
                  end if
               end do
            end do
         end if

      end if
   end subroutine convert_hysteresis_summerdike

   subroutine linktonode2(u_x, u_y, s_x, s_y, ndxndxi) ! bring 2 scalars on u points to zeta points

      use m_flowgeom
      use m_flow

      implicit none

      double precision :: u_x(:), u_y(:), s_x(:), s_y(:)
      integer :: ndxndxi

      integer :: n, LL, LLL, k1, k2, k3

      s_x = 0.0d0
      s_y = 0.0d0
      do n = 1, ndxndxi
         do LL = 1, nd(n)%lnx
            LLL = abs(nd(n)%ln(LL))
            k1 = ln(1, LLL); k2 = ln(2, LLL)
            k3 = 1; if (nd(n)%ln(LL) > 0) k3 = 2
            s_x(n) = s_x(n) + u_x(LLL) * wcL(k3, LLL)
            s_y(n) = s_y(n) + u_y(LLL) * wcL(k3, LLL)
         end do
      end do
   end subroutine linktonode2

!> write_array_with_dmiss_for_dry_cells_into_netcdf_file
   function write_array_with_dmiss_for_dry_cells_into_netcdf_file(ncid, id_tsp, id_var, data_location, array, jabndnd) result(ierr)
      use m_flowgeom, only: kfs
      use m_alloc, only: aerr
      use m_missing, only: dmiss

      implicit none

      integer, intent(in) :: ncid !< NetCDF dataset id
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Variable ID
      integer, intent(in) :: data_location !< Data location
      double precision, allocatable, intent(in) :: array(:) !< 2D case array to be written
      integer, optional, intent(in) :: jabndnd !< Flag specifying whether boundary nodes are to be written.

      integer :: ierr !< Result status
      integer :: cell
      double precision, allocatable :: temp_array(:)

      if (.not. allocated(kfs)) then
         call mess(LEVEL_INFO, 'Dry cells are not "removed" in a map file due to the current implementation. Please contact DFM developers.')
         ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, array, jabndnd=jabndnd)
         return
      end if

      if (size(array) /= size(kfs)) then
         call mess(LEVEL_INFO, 'Dry cells are not "removed" in a map file due to the current implementation. Please contact DFM developers.')
         ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, array, jabndnd=jabndnd)
         return
      end if

      allocate (temp_array(size(array)), stat=ierr)
      if (ierr /= 0) call aerr('temp_array', ierr, size(array))

      do cell = 1, size(kfs)
         if (kfs(cell) == 0) then
            temp_array(cell) = dmiss
         else
            temp_array(cell) = array(cell)
         end if
      end do
      ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, temp_array, jabndnd=jabndnd)

      deallocate (temp_array)

   end function write_array_with_dmiss_for_dry_cells_into_netcdf_file

!> write_array_with_dmiss_for_dry_faces_into_netcdf_file
   function write_array_with_dmiss_for_dry_faces_into_netcdf_file(ncid, id_tsp, id_var, data_location, array, jabndnd) result(ierr)
      use m_flow, only: hu
      use m_alloc, only: aerr
      use m_missing, only: dmiss

      implicit none

      integer, intent(in) :: ncid !< NetCDF dataset id
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Variable ID
      integer, intent(in) :: data_location !< Data location
      double precision, allocatable, intent(in) :: array(:) !< array to be written
      integer, optional, intent(in) :: jabndnd !< Flag specifying whether boundary nodes are to be written.

      integer :: ierr !< Result status
      integer :: face
      double precision, allocatable :: temp_array(:)

      if (.not. allocated(hu)) then
         call mess(LEVEL_INFO, 'Dry faces are not "removed" in a map file due to the current implementation. Please contact DFM developers.')
         ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, array, jabndnd=jabndnd)
         return
      end if

      if (size(array) /= size(hu)) then
         call mess(LEVEL_INFO, 'Dry faces are not "removed" in a map file due to the current implementation. Please contact DFM developers.')
         ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, array, jabndnd=jabndnd)
         return
      end if

      allocate (temp_array(size(array)), stat=ierr)
      if (ierr /= 0) call aerr('temp_array', ierr, size(array))

      do face = 1, size(hu)
         if (hu(face) == 0) then
            temp_array(face) = dmiss
         else
            temp_array(face) = array(face)
         end if
      end do
      ierr = unc_put_var_map(ncid, id_tsp, id_var, data_location, temp_array, jabndnd=jabndnd)

      deallocate (temp_array)

   end function write_array_with_dmiss_for_dry_faces_into_netcdf_file

!> Put attribute for sediment. It can be used for both internal and
!! boundary cells.
   subroutine add_att_sediment(id_sf1, nf, id_laydim, id_flowelemdim, id_timedim, irstfile, stradd, strcord)

      use m_flow, only: kmx
      use m_transport, only: ISED1, ISEDN, const_names

!input
      integer, allocatable, dimension(:), intent(inout) :: id_sf1
      integer, intent(in) :: nf, irstfile, id_laydim, id_flowelemdim, id_timedim
      character(len=*), intent(in) :: stradd, strcord

!local
      integer, allocatable, dimension(:) :: id1
      character(len=255) :: tmpstr

      integer :: j, i
      integer :: ierr

      if (kmx > 0) then
         id1 = (/id_laydim, id_flowelemdim, id_timedim/)
      else
         id1 = (/id_flowelemdim, id_timedim/)
      end if

      if (.not. allocated(id_sf1)) then
         allocate (id_sf1(nf))
      end if
      do i = ISED1, ISEDN
         j = i - ISED1 + 1
         tmpstr = const_names(i)
         ! Forbidden chars in NetCDF names: space, /, and more.
         call replace_char(tmpstr, 32, 95)
         call replace_char(tmpstr, 47, 95)
         ierr = nf90_def_var(irstfile, trim(tmpstr)//trim(stradd), nf90_double, id1, id_sf1(j))
         ierr = nf90_put_att(irstfile, id_sf1(j), 'coordinates', trim(strcord))
         ierr = nf90_put_att(irstfile, id_sf1(j), 'standard_name', trim(tmpstr)//trim(stradd)//' mass concentration')
         ierr = nf90_put_att(irstfile, id_sf1(j), 'long_name', trim(tmpstr)//trim(stradd)//' mass concentration')
         ierr = nf90_put_att(irstfile, id_sf1(j), 'units', 'kg m-3')
      end do

   end subroutine add_att_sediment

!> Read sediment data to `constituents` (the indexing prevents passing
!  another variable).
   subroutine read_sediment(var, stradd, imapfile, kstart, kcount, it_read, um, target_shift)

      use m_flow, only: kmx, ndkx
      use m_transport, only: ISED1, ISEDN, const_names
      use unstruc_messages, only: mess, LEVEL_WARN
      use m_alloc, only: realloc
      use m_partitioninfo, only: um
      use fm_location_types, only: UNC_LOC_S3D, UNC_LOC_S

!input/output
      double precision, allocatable, dimension(:, :), intent(inout) :: var !< (:,ndkx) Data array into which the sediment data will be read.
      character(len=*), intent(in) :: stradd !< variable name suffix for distinguishing between internal and boundary cells, leave empty for internal cells.
      integer, intent(in) :: imapfile !< file handle for reading in netcdf file
      integer, intent(in) :: kstart !< 2D start position for reading in netcdf file
      integer, intent(in) :: kcount !< 2D length of array for reading in netcdf file
      integer, intent(in) :: it_read !< time index for reading in netcdf file
      type(t_unc_merged), intent(in) :: um !< struct holding all data for ugrid merged map/rst files
      integer, intent(in) :: target_shift !< 2D shift of index for writing

!local
      integer :: i
      integer :: ierr, tmp_loc
      double precision, allocatable :: tmpvar1(:), tmpvar1D(:)
      character(len=255) :: tmpstr

      if (kmx > 0) then
         tmp_loc = UNC_LOC_S3D
      else
         tmp_loc = UNC_LOC_S
      end if

      call realloc(tmpvar1, 0)
      allocate (tmpvar1D(1:ndkx))
      tmpvar1D = 0.0d0
      do i = ISED1, ISEDN
         tmpstr = const_names(i)
         ! Forbidden chars in NetCDF names: space, /, and more.
         call replace_char(tmpstr, 32, 95)
         call replace_char(tmpstr, 47, 95)
         ! concentrations exists in restart file
         ierr = get_var_and_shift(imapfile, trim(tmpstr)//trim(stradd), tmpvar1D, tmpvar1, tmp_loc, kmx, kstart, kcount, it_read, um%jamergedmap, um%inode_own, um%inode_merge, target_shift)
         if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'unc_read_map_or_rst: cannot read variable '''//trim(tmpstr)//trim(stradd)//''' from the specified restart file. Skip reading this variable.')
            call check_error(ierr, const_names(i), LEVEL_WARN)
         else
            call assign_restart_data_to_local_array(tmpvar1D, var, i, kmx, kcount, um%jamergedmap, um%inode_own, .false., target_shift)
         end if
      end do

   end subroutine read_sediment

!> Write 2D/3D array on cell centres and for boundaries
   function unc_put_var_rst_dble(irstfile, id_internal_flow_node_data_var, id_bnd_flow_node_data_var, data_values, itim) result(ierr)

      use m_flowgeom, only: ndxi, ndx
      use m_flow, only: kmx, work1
      use m_flowparameters, only: jarstbnd
      use fm_external_forcings_data, only: ndxbnd_own
      use m_partitioninfo, only: jampi

!input/output
      integer, intent(in) :: irstfile !< file handle for restart file
      integer, intent(in) :: id_internal_flow_node_data_var !< index of internal cells variable on netcdf file
      integer, intent(in) :: id_bnd_flow_node_data_var !< index of boundary cells variable on netcdf file
      integer, intent(in) :: itim !< time index on netcdf file
      double precision, allocatable, intent(in) :: data_values(:) !< array for information at flow nodes {"location": "face", "shape": ["ndkx"]}

!local
      integer :: ierr, ndxbnd

      if (jampi == 0) then
         ndxbnd = ndx - ndxi
      else
         ndxbnd = ndxbnd_own
      end if

      if (kmx > 0) then !3D
         call flow_node_vector_to_matrix(data_values, 1, ndxi, work1)
         ierr = nf90_put_var(irstfile, id_internal_flow_node_data_var, work1(1:kmx, 1:ndxi), (/1, 1, itim/), (/kmx, ndxi, 1/))
      else !2D
         ierr = nf90_put_var(irstfile, id_internal_flow_node_data_var, data_values(1:ndxi), (/1, itim/), (/ndxi, 1/))
      end if !(kmx > 0)
!var at boundaries
      if (jarstbnd > 0 .and. ndxbnd > 0) then
         if (kmx > 0) then !3D
            call flow_node_vector_to_matrix(data_values, ndxi + 1, ndx, work1)
            ierr = nf90_put_var(irstfile, id_bnd_flow_node_data_var, work1(1:kmx, ndxi + 1:ndx), (/1, 1, itim/), (/kmx, ndxbnd, 1/))
         else !2D
            ierr = nf90_put_var(irstfile, id_bnd_flow_node_data_var, data_values(ndxi + 1:ndx), (/1, itim/), (/ndxbnd, 1/))
         end if !(kmx > 0)
      end if

   end function unc_put_var_rst_dble

!> Transfrom vector information to matrix for 3D information on cell centres
   subroutine flow_node_vector_to_matrix(data_values, flow_node_index1, flow_node_index2, data_values_matrix)

      use m_missing, only: dmiss
      use m_get_kbot_ktop

      double precision, allocatable, intent(in) :: data_values(:) !< array for information at flow nodes {"location": "face", "shape": ["ndkx"]}
      integer, intent(in) :: flow_node_index1 !< start index (1:ndx) for transfer of data from vector to matrix format
      integer, intent(in) :: flow_node_index2 !< end index (1:ndx) for transfer of data from vector to matrix format
      double precision, intent(out) :: data_values_matrix(:, :) !< array for information at flow nodes as matrix {"location": "face", "shape": ["num_layers","ndx"]}

      integer :: k, kk, kb, kt, nlayb, nrlay

      data_values_matrix = dmiss
      do kk = flow_node_index1, flow_node_index2
         call getkbotktop(kk, kb, kt)
         call getlayerindices(kk, nlayb, nrlay)
         do k = kb, kt
            data_values_matrix(k - kb + nlayb, kk) = data_values(k)
         end do
      end do

   end subroutine flow_node_vector_to_matrix

end module unstruc_netcdf

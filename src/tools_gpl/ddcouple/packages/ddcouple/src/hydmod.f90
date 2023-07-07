!!  Copyright (C)  Stichting Deltares, 2021-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      module hydmod

      ! module contains everything for the hydrodynamic discription
      ! created June 2004 by Jan van Beek
      !
      ! contains the following derived types:
      !
      !    t_hyd                   ! poperties with respect to a hydrodynamic

      use filmod                   ! module contains everything for the files
      use wstmod                   ! module contains everything for the wastloads discription
      use domain_mod               ! module contains everything for the domain discription
      use dlwqdata_mod
      implicit none

      integer, parameter :: FILE_NAME_SIZE    = 256          ! length all names
      integer, parameter :: NAME_SIZE         =  20          ! size of descriptive names
      integer, parameter :: TEXT_SIZE         =  40          ! descriptive text size

      ! task types

      integer, parameter          :: HYD_TASK_UNKNOWN  =   0          ! unknown
      integer, parameter          :: HYD_TASK_FULL     =   1          ! full-coupling
      integer, parameter          :: HYD_TASK_DDC      =   2          ! dd-coupling

      ! geometry types

      integer, parameter          :: HYD_GEOM_UNKNOWN  =   0          ! unknown
      integer, parameter          :: HYD_GEOM_CURVI    =   1          ! curvilinear-grid
      integer, parameter          :: HYD_GEOM_UNSTRUC  =   2          ! unstructured

      ! layer types

      integer, parameter          :: HYD_LAYERS_UNKNOWN  =   0          ! unknown
      integer, parameter          :: HYD_LAYERS_SIGMA    =   1          ! curvilinear-grid
      integer, parameter          :: HYD_LAYERS_Z        =   2          ! unstructured

      ! attributes types

      integer, parameter          :: ATR_UNKNOWN       =   0          ! unknown
      integer, parameter          :: ATR_OLD           =   1          ! old type only surf-mid-bot
      integer, parameter          :: ATR_COMPLETE      =   2          ! full attribute description
      integer, parameter          :: ATR_FM            =   3          ! fm attribute description

      type t_hyd
         type(t_dlwqfile)                       :: file_hyd               ! name of hydrodynamic description file
         character*80                           :: created_by             ! program and version that created the hyd-file
         character*40                           :: creation_date          ! date and time of hyd-file creation
         integer                                :: task                   !
         integer                                :: geometry               !
         integer                                :: layer_type             !
         real*8                                 :: zbot                   ! Maximum depth in the model (relative to the reference level; unit: metres; positive upwards, z-layer only).
         real*8                                 :: ztop                   ! The ‘imaginary’ maximum water level in the model (relative to the reference level; unit: metres; positive upwards, z-layer only).
                                                                          ! This imaginary level is used only to determine the grid distribution. It does not mark the maximum surface level.
         integer                                :: horizontal_aggregation !
         integer                                :: minimum_vdf_used       !
         integer                                :: vertical_diffusion     !
         character(len=TEXT_SIZE)               :: description(3)         !
         character*14                           :: hyd_ref                ! hydrodynamic reference date
         character*14                           :: hyd_start              ! hydrodynamic start date
         character*14                           :: hyd_stop               ! hydrodynamic stop date
         character*14                           :: hyd_step               ! hydrodynamic time step
         character*14                           :: cnv_ref                ! conversion reference date
         character*14                           :: cnv_start              ! conversion start date
         character*14                           :: cnv_stop               ! conversion stop date
         character*14                           :: cnv_step               ! conversion time step
         integer                                :: cnv_step_sec           ! conversion time step in seconds
         real*8                                 :: time_ref               ! hydrodynamic reference date in julian
         integer                                :: mmax                   ! grid cells m direction
         integer                                :: nmax                   ! grid cells n direction
         integer                                :: kmax                   ! number of layers in hydrodynamics
         integer                                :: nolay                  ! number of layers in conversion
         logical                                :: time_in_seconds        ! time in sources file in seconds or not
         type(t_dlwqfile)                       :: file_com               ! hydrodynamic-file
         type(t_dlwqfile)                       :: file_dwq               ! aggregation-file (horizontal)
         type(t_dlwqfile)                       :: file_vag               ! aggregation-file (vertical)
         type(t_dlwqfile)                       :: file_lga               ! grid-indices-file
         type(t_dlwqfile)                       :: file_cco               ! grid-coordinates-file
         type(t_dlwqfile)                       :: file_vol               ! volumes-file
         type(t_dlwqfile)                       :: file_are               ! areas-file
         type(t_dlwqfile)                       :: file_flo               ! flows-file
         type(t_dlwqfile)                       :: file_poi               ! pointers-file
         type(t_dlwqfile)                       :: file_len               ! lengths-file
         type(t_dlwqfile)                       :: file_sal               ! salinity-file
         type(t_dlwqfile)                       :: file_tem               ! temperature-file
         type(t_dlwqfile)                       :: file_vdf               ! vert-diffusion-file
         type(t_dlwqfile)                       :: file_srf               ! surfaces-file
         type(t_dlwqfile)                       :: file_lgt               ! total-grid-file
         type(t_dlwqfile)                       :: file_src               ! discharges-file
         type(t_dlwqfile)                       :: file_chz               ! chezy-coefficients-file
         type(t_dlwqfile)                       :: file_tau               ! shear-stresses-file
         type(t_dlwqfile)                       :: file_wlk               ! walking-discharges-file
         type(t_dlwqfile)                       :: file_atr               ! attributes-file
         type(t_dlwqfile)                       :: file_dps               ! depths-file
         logical                                :: sal_present            ! indication if salinity is availeble
         logical                                :: tem_present            ! indication if temperature is availeble
         logical                                :: tau_present            ! indication if tau is availeble
         logical                                :: vdf_present            ! indication if vertical diffusion is availeble
         real                                   :: min_vdf_upper          ! minimum-vert-diffusion-upper-layer
         real                                   :: min_vdf_lower          ! minimum-vert-diffusion-lower-layer
         real                                   :: min_vdf_interface      ! minimum-vert-diffusion-interface-depth
         real                                   :: disp_first             ! constant-dispersion-first-direction
         real                                   :: disp_second            ! constant-dispersion-second-direction
         real                                   :: disp_third             ! constant-dispersion-third-direction
         real, pointer                          :: hyd_layers(:)          ! hydrodynamic-layers
         real, pointer                          :: waq_layers(:)          ! water-quality-layers
         type(t_wasteload_coll)                 :: wasteload_coll         ! the wasteloads
         type(t_dlwqdata)                       :: wasteload_data         ! the data of the wasteloads
         type(t_domain_coll)                    :: domain_coll            ! the domains
         type(t_dd_bound_coll)                  :: dd_bound_coll          ! the dd boundaries
         integer                                :: noseg                  ! number of segments
         integer                                :: nosegl                 ! number of segments per layer
         integer                                :: nobnd                  ! number of boundaries
         integer                                :: nobndl                 ! number of boundaries per layer
         integer                                :: noq                    ! number of exchanges
         integer                                :: noq1                   ! number of exchanges in first direction
         integer                                :: noq2                   ! number of exchanges in second direction
         integer                                :: noq3                   ! number of exchanges in third direction
         integer                                :: noq4                   ! number of exchanges in fourth direction
         real, pointer                          :: volume(:)              ! volume
         real, pointer                          :: area(:)                ! area
         real, pointer                          :: flow(:)                ! flow
         real, pointer                          :: displen(:,:)           ! displen
         real, pointer                          :: surf(:)                ! surf
         real, pointer                          :: depth(:)               ! depth
         real, pointer                          :: sal(:)                 ! sal
         real, pointer                          :: tem(:)                 ! tem
         real, pointer                          :: tau(:)                 ! tau
         real, pointer                          :: vdf(:)                 ! vdf
         integer, pointer                       :: lgrid(:,:)             ! active grid table
         integer, pointer                       :: ipoint(:,:)            ! pointer table
         real, pointer                          :: xdepth(:,:)            ! x coordinates depth points
         real, pointer                          :: ydepth(:,:)            ! y coordinates depth points
         integer                                :: atr_type               ! type of attribute information
         integer                                :: no_atr                 ! number of attributes
         integer, pointer                       :: attributes(:)          ! attributes
         real                                   :: min_disp_len           ! minimum-dispersion-length
         logical                                :: l_ascii                ! indication if ascii output is asked
      end type t_hyd

      type t_hyd_coll
         type(t_hyd), pointer                   :: hyd_pnts(:)            ! pointer to the hyd descriptions
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_hyd_coll

      end module hydmod

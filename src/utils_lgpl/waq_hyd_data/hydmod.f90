!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!

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
      use dlwq_hyd_data
      use m_ug_meta
      use m_ug_meshgeom
      use m_ug_crs

      implicit none

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

      ! open boundaries

      type t_openbndlin
         integer                                :: ibnd                   ! boundary number
         integer                                :: ibnd_new               ! renumbered boundary number (0 = inactive)
         real*8                                 :: x1                     ! x1
         real*8                                 :: y1                     ! y1
         real*8                                 :: x2                     ! x2
         real*8                                 :: y2                     ! y2
      end type t_openbndlin

      type t_openbndlin_coll
         type(t_openbndlin), pointer            :: openbndlin_pnts(:)     ! pointer to the openbndlin descriptions
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_openbndlin_coll

      type t_openbndsect
         character(len=256)                     :: name                   ! boundary section name
         type(t_openbndlin_coll)                :: openbndlin_coll        ! the collection of boundaries
      end type t_openbndsect

      type t_openbndsect_coll
         type(t_openbndsect), pointer           :: openbndsect_pnts(:)    ! pointer to the openbndsect descriptions
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_openbndsect_coll

      type t_hyd
         type(t_dlwqfile)                       :: file_hyd               ! name of hydrodynamic description file
         character*80                           :: created_by             ! program and version that created the hyd-file
         character*40                           :: creation_date          ! date and time of hyd-file creation
         integer                                :: task                   !
         integer                                :: geometry               !
         integer                                :: layer_type             !
         real*8                                 :: zbot                   ! Maximum depth in the model (relative to the reference level; unit: metres; positive upwards, z-layer only).
         real*8                                 :: ztop                   ! The �imaginary� maximum water level in the model (relative to the reference level; unit: metres; positive upwards, z-layer only).
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
         type(t_dlwqfile)                       :: file_bnd               ! boundaries-file
         type(t_dlwqfile)                       :: file_geo               ! waqgeom-file
         type(t_dlwqfile)                       :: file_vol               ! volumes-file
         type(t_dlwqfile)                       :: file_are               ! areas-file
         type(t_dlwqfile)                       :: file_flo               ! flows-file
         type(t_dlwqfile)                       :: file_poi               ! pointers-file
         type(t_dlwqfile)                       :: file_len               ! lengths-file
         type(t_dlwqfile)                       :: file_sal               ! salinity-file
         type(t_dlwqfile)                       :: file_tem               ! temperature-file
         type(t_dlwqfile)                       :: file_vdf               ! vert-diffusion-file
         type(t_dlwqfile)                       :: file_srf               ! surfaces-file
         type(t_dlwqfile)                       :: file_hsrf              ! horizontal-surfaces-file
         type(t_dlwqfile)                       :: file_lgt               ! total-grid-file
         type(t_dlwqfile)                       :: file_src               ! discharges-file
         type(t_dlwqfile)                       :: file_chz               ! chezy-coefficients-file
         type(t_dlwqfile)                       :: file_tau               ! shear-stresses-file
         type(t_dlwqfile)                       :: file_wlk               ! walking-discharges-file
         type(t_dlwqfile)                       :: file_atr               ! attributes-file
         type(t_dlwqfile)                       :: file_dps               ! depths-file
         type(t_dlwqfile)                       :: file_ddp               ! ddp-file
         type(t_dlwqfile)                       :: file_rfl               ! river flow file
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
         type(t_openbndsect_coll)               :: openbndsect_coll       ! the (dlflowfm) boundary sections
         integer                                :: noseg                  ! number of segments
         integer                                :: nosegl                 ! number of segments per layer
         integer                                :: nobnd                  ! number of boundaries
         integer                                :: nobndl                 ! number of boundaries per layer
         integer                                :: lnx                    ! number of flow links
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
         real, pointer                          :: gsqs(:,:)              ! hydro grid cell surface
         integer                                :: atr_type               ! type of attribute information
         integer                                :: no_atr                 ! number of attributes
         integer, pointer                       :: attributes(:)          ! attributes
         real                                   :: min_disp_len           ! minimum-dispersion-length
         logical                                :: l_ascii                ! indication if ascii output is asked

         integer, pointer                       :: idomain(:)             ! idomain
         integer, pointer                       :: iglobal(:)             ! global cell numbering
         integer, pointer                       :: iglobal_bnd(:)         ! global boundary numbering
         logical, pointer                       :: ispoint_bnd(:)         ! is boundary a point source
         integer, pointer                       :: ilocal_link(:)         ! local number in owner domain
         integer, pointer                       :: iglobal_link(:)        ! iglobal_link
         integer                                :: numcontpts             ! numcontpts number of contour nodes
         real*8, pointer                        :: flowelemcontourx(:,:)  ! flowelemcontourx
         real*8, pointer                        :: flowelemcontoury(:,:)  ! flowelemcontoury
         integer                                :: lnx1d                  ! number of 1d pointers
         real*8, pointer                        :: xu(:)                  ! xu
         real*8, pointer                        :: yu(:)                  ! yu

         integer                                :: numk                   ! number of nodes
         real*8, pointer                        :: xk(:)                  ! xu
         real*8, pointer                        :: yk(:)                  ! yu
         real*8, pointer                        :: zk(:)                  ! yu
         integer                                :: numl                   !< number of nodes links
         integer, pointer                       :: kn(:,:)                !< node links
         integer                                :: nv                     !< max nr of nodes describing an element
         integer                                :: nump                   !< number of elements
         integer, pointer                       :: netcellnod(:,:)        !< element nodes
         type(t_ug_meta)                        :: meta                   !< meta data
         type(t_ug_meshgeom)                    :: waqgeom                !< geometry
         type(t_crs)                            :: crs                    !< Container for information about coordinate reference system
         integer                                :: conv_type              !< netcdf convention type
         real(8)                                :: conv_version           !< netcdf convension version
         integer, pointer                       :: edge_type(:)           !< edge type
         integer, pointer                       :: global_edge(:)         !< global edge number
         integer, pointer                       :: global_node(:)         !< global node number

      end type t_hyd

      type t_hyd_coll
         type(t_hyd), pointer                   :: hyd_pnts(:)            ! pointer to the hyd descriptions
         integer                                :: maxsize                ! maximum size of the current array
         integer                                :: cursize                ! filled up to this size
      end type t_hyd_coll

      interface coll_find
         module procedure openbndsect_coll_find
      end interface

      interface coll_add
         module procedure openbndsect_coll_add
         module procedure openbndlin_coll_add
      end interface

      contains

      ! function to find a section name in a collection, case sensitive at the moment

      function openbndsect_coll_find( openbndsect_coll, name ) result ( iret )

         type(t_openbndsect_coll)               :: openbndsect_coll       ! collection of openbndsects
         character(LEN=*)                       :: name                   ! name of openbndsect to be found
         integer                                :: iret                   ! result index in collection or 0 if not found

         integer                                :: i                      ! loop counter

         iret = 0
         do i = 1 , openbndsect_coll%cursize
            if ( openbndsect_coll%openbndsect_pnts(i)%name .eq. name ) then
               iret = i
               return
            endif
         end do

      end function openbndsect_coll_find

      ! function to add to a collection of openbndsects

      function openbndsect_coll_add( openbndsect_coll , openbndsect ) result ( cursize )

         type(t_openbndsect_coll)               :: openbndsect_coll       ! collection of openbndsects
         type(t_openbndsect)                    :: openbndsect            ! openbndsect to be added
         integer                                :: cursize                ! return value the new current collection size
                                                                          ! and the index of the added openbndsect

         type(t_openbndsect), pointer           :: openbndsect_pnts(:)    ! pointer for the resize operation
         integer                                :: i                      ! loop counter

         if ( openbndsect_coll%cursize .eq. openbndsect_coll%maxsize ) then

            ! resize, allocate new array

            allocate ( openbndsect_pnts ( openbndsect_coll%maxsize + MAX_NUM ) )

            ! copy the openbndsects into the new array

            do i = 1 , openbndsect_coll%maxsize
               openbndsect_pnts(i) = openbndsect_coll%openbndsect_pnts(i)   ! copies the openbndsects
            enddo

            ! deallocate the old array and attach the new array to the collection

            if ( openbndsect_coll%maxsize .ne. 0 ) deallocate ( openbndsect_coll%openbndsect_pnts )
            openbndsect_coll%openbndsect_pnts => openbndsect_pnts
            openbndsect_coll%maxsize = openbndsect_coll%maxsize + MAX_NUM

         endif

         openbndsect_coll%cursize = openbndsect_coll%cursize + 1
         openbndsect_coll%openbndsect_pnts(openbndsect_coll%cursize ) = openbndsect
         cursize = openbndsect_coll%cursize
         return

      end function openbndsect_coll_add

      ! function to add to a collection of openbndlins

      function openbndlin_coll_add( openbndlin_coll , openbndlin ) result ( cursize )

         type(t_openbndlin_coll)                :: openbndlin_coll        ! collection of openbndlins
         type(t_openbndlin)                     :: openbndlin             ! openbndlin to be added
         integer                                :: cursize                ! return value the new current collection size
                                                                          ! and the index of the added openbndlin

         type(t_openbndlin), pointer            :: openbndlin_pnts(:)     ! pointer for the resize operation
         integer                                :: i                      ! loop counter

         if ( openbndlin_coll%cursize .eq. openbndlin_coll%maxsize ) then

            ! resize, allocate new array

            allocate ( openbndlin_pnts ( openbndlin_coll%maxsize + MAX_NUM ) )

            ! copy the openbndlins into the new array

            do i = 1 , openbndlin_coll%maxsize
               openbndlin_pnts(i) = openbndlin_coll%openbndlin_pnts(i)   ! copies the openbndlins
            enddo

            ! deallocate the old array and attach the new array to the collection

            if ( openbndlin_coll%maxsize .ne. 0 ) deallocate ( openbndlin_coll%openbndlin_pnts )
            openbndlin_coll%openbndlin_pnts => openbndlin_pnts
            openbndlin_coll%maxsize = openbndlin_coll%maxsize + MAX_NUM

         endif

         openbndlin_coll%cursize = openbndlin_coll%cursize + 1
         openbndlin_coll%openbndlin_pnts(openbndlin_coll%cursize ) = openbndlin
         cursize = openbndlin_coll%cursize
         return

      end function openbndlin_coll_add

      end module hydmod

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

module m_hydmod

    !! module contains everything for the hydrodynamic discription
    !!
    !! contains the following derived types:
    !!
    !! t_hydrodynamics
    !!     - poperties with respect to a hydrodynamic

    use m_waq_file                ! module contains everything for the files
    use m_waste_loads             ! module contains everything for the wastloads discription
    use m_domain              ! module contains everything for the domain discription
    use m_waq_data_structure
    use m_ug_meta
    use m_ug_meshgeom
    use m_ug_crs

    implicit none

    integer, parameter :: TEXT_SIZE = 40          ! descriptive text size

    ! task types
    integer, parameter :: HYD_TASK_UNKNOWN = 0          ! unknown
    integer, parameter :: HYD_TASK_FULL = 1          ! full-coupling
    integer, parameter :: HYD_TASK_DDC = 2          ! dd-coupling

    ! geometry types
    integer, parameter :: HYD_GEOM_UNKNOWN = 0          ! unknown
    integer, parameter :: HYD_GEOM_CURVI = 1          ! curvilinear-grid
    integer, parameter :: HYD_GEOM_UNSTRUC = 2          ! unstructured

    ! layer types
    integer, parameter :: HYD_LAYERS_UNKNOWN = 0          ! unknown
    integer, parameter :: HYD_LAYERS_SIGMA = 1          ! curvilinear-grid
    integer, parameter :: HYD_LAYERS_Z = 2          ! unstructured

    ! attributes types
    integer, parameter :: ATR_UNKNOWN = 0          ! unknown
    integer, parameter :: ATR_OLD = 1          ! old type only surf-mid-bot
    integer, parameter :: ATR_COMPLETE = 2          ! full attribute description
    integer, parameter :: ATR_FM = 3          ! fm attribute description

    ! open boundaries
    type t_open_boundary_line
        integer :: ibnd                   ! boundary number
        integer :: ibnd_new               ! renumbered boundary number (0 = inactive)
        real(kind = 8) :: x1                     ! x1
        real(kind = 8) :: y1                     ! y1
        real(kind = 8) :: x2                     ! x2
        real(kind = 8) :: y2                     ! y2
    end type t_open_boundary_line

    type t_open_boundary_line_collection
        type(t_open_boundary_line), pointer :: openbndlin_pnts(:)     ! pointer to the openbndlin descriptions
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size                ! filled up to this size
    contains
        procedure :: add => openbndlin_coll_add
    end type t_open_boundary_line_collection

    type t_openbnd_section
        character(len = 256) :: name                   ! boundary section name
        type(t_open_boundary_line_collection) :: openbndlin_coll        ! the collection of boundaries
    end type t_openbnd_section

    type t_openbndsect_coll
        type(t_openbnd_section), pointer :: openbndsect_pnts(:)    ! pointer to the openbndsect descriptions
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size                ! filled up to this size
    contains
        procedure :: find => openbndsect_coll_find
        procedure :: add => openbndsect_coll_add
    end type t_openbndsect_coll

    type t_hydrodynamics
        type(t_file) :: file_hyd               ! name of hydrodynamic description file
        character(len = 80) :: created_by             ! program and version that created the hyd-file
        character(len = 40) :: creation_date          ! date and time of hyd-file creation
        integer :: task                   !
        integer :: geometry               !
        integer :: layer_type
        ! Maximum depth in the model (relative to the reference level; unit: metres; positive upwards, z-layer only).!
        real(kind = 8) :: zbot
        ! The imaginary maximum water level in the model (relative to the reference level; unit: metres; positive
        ! upwards, z-layer only).
        real(kind = 8) :: ztop
        ! This imaginary level is used only to determine the grid distribution. It does not mark the maximum
        ! surface level.
        integer :: horizontal_aggregation !
        integer :: minimum_vdf_used       !
        integer :: vertical_diffusion     !
        character(len = TEXT_SIZE) :: description(3)         !
        character*14 :: hyd_ref                ! hydrodynamic reference date
        character*14 :: hyd_start              ! hydrodynamic start date
        character*14 :: hyd_stop               ! hydrodynamic stop date
        character*14 :: hyd_step               ! hydrodynamic time step
        character*14 :: cnv_ref                ! conversion reference date
        character*14 :: cnv_start              ! conversion start date
        character*14 :: cnv_stop               ! conversion stop date
        character*14 :: cnv_step               ! conversion time step
        integer :: cnv_step_sec           ! conversion time step in seconds
        real(kind = 8) :: time_ref               ! hydrodynamic reference date in julian
        integer :: mmax                   ! grid cells m direction
        integer :: nmax                   ! grid cells n direction
        integer :: kmax                   ! number of layers in hydrodynamics
        integer :: nolay                  ! number of layers in conversion
        logical :: time_in_seconds        ! time in sources file in seconds or not
        type(t_file) :: file_com               ! hydrodynamic-file
        type(t_file) :: file_dwq               ! aggregation-file (horizontal)
        type(t_file) :: file_vag               ! aggregation-file (vertical)
        type(t_file) :: file_lga               ! grid-indices-file
        type(t_file) :: file_cco               ! grid-coordinates-file
        type(t_file) :: file_bnd               ! boundaries-file
        type(t_file) :: file_geo               ! waqgeom-file
        type(t_file) :: file_vol               ! volumes-file
        type(t_file) :: file_are               ! areas-file
        type(t_file) :: file_flo               ! flows-file
        type(t_file) :: file_poi               ! pointers-file
        type(t_file) :: file_len               ! lengths-file
        type(t_file) :: file_sal               ! salinity-file
        type(t_file) :: file_tem               ! temperature-file
        type(t_file) :: file_vdf               ! vert-diffusion-file
        type(t_file) :: file_srf               ! surfaces-file
        type(t_file) :: file_hsrf              ! horizontal-surfaces-file
        type(t_file) :: file_lgt               ! total-grid-file
        type(t_file) :: file_src               ! discharges-file
        type(t_file) :: file_chz               ! chezy-coefficients-file
        type(t_file) :: file_tau               ! shear-stresses-file
        type(t_file) :: file_wlk               ! walking-discharges-file
        type(t_file) :: file_atr               ! attributes-file
        type(t_file) :: file_dps               ! depths-file
        type(t_file) :: file_ddp               ! ddp-file
        type(t_file) :: file_rfl               ! river flow file
        logical :: sal_present            ! indication if salinity is availeble
        logical :: tem_present            ! indication if temperature is availeble
        logical :: tau_present            ! indication if tau is availeble
        logical :: vdf_present            ! indication if vertical diffusion is availeble
        real :: min_vdf_upper          ! minimum-vert-diffusion-upper-layer
        real :: min_vdf_lower          ! minimum-vert-diffusion-lower-layer
        real :: min_vdf_interface      ! minimum-vert-diffusion-interface-depth
        real :: disp_first             ! constant-dispersion-first-direction
        real :: disp_second            ! constant-dispersion-second-direction
        real :: disp_third             ! constant-dispersion-third-direction
        real, pointer :: hyd_layers(:)          ! hydrodynamic-layers
        real, pointer :: waq_layers(:)          ! water-quality-layers
        type(t_wasteload_coll) :: wasteload_coll         ! the wasteloads
        type(t_data_block) :: wasteload_data         ! the data of the wasteloads
        type(t_domain_collection) :: domain_coll            ! the domains
        type(t_dd_bound_coll) :: dd_bound_coll          ! the dd boundaries
        type(t_openbndsect_coll) :: openbndsect_coll       ! the (dlflowfm) boundary sections
        integer :: noseg                  ! number of segments
        integer :: nosegl                 ! number of segments per layer
        integer :: nobnd                  ! number of boundaries
        integer :: nobndl                 ! number of boundaries per layer
        integer :: lnx                    ! number of flow links
        integer :: noq                    ! number of exchanges
        integer :: noq1                   ! number of exchanges in first direction
        integer :: noq2                   ! number of exchanges in second direction
        integer :: noq3                   ! number of exchanges in third direction
        integer :: noq4                   ! number of exchanges in fourth direction
        real, pointer :: volume(:)              ! volume
        real, pointer :: area(:)                ! area
        real, pointer :: flow(:)                ! flow
        real, pointer :: displen(:, :)           ! displen
        real, pointer :: surf(:)                ! surf
        real, pointer :: depth(:)               ! depth
        real, pointer :: sal(:)                 ! sal
        real, pointer :: tem(:)                 ! tem
        real, pointer :: tau(:)                 ! tau
        real, pointer :: vdf(:)                 ! vdf
        integer, pointer :: lgrid(:, :)             ! active grid table
        integer, pointer :: ipoint(:, :)            ! pointer table
        real, pointer :: xdepth(:, :)            ! x coordinates depth points
        real, pointer :: ydepth(:, :)            ! y coordinates depth points
        real, pointer :: gsqs(:, :)              ! hydro grid cell surface
        integer :: atr_type               ! type of attribute information
        integer :: no_atr                 ! number of attributes
        integer, pointer :: attributes(:)          ! attributes
        real :: min_disp_len           ! minimum-dispersion-length
        logical :: l_ascii                ! indication if ascii output is asked

        integer, pointer :: idomain(:)             ! idomain
        integer, pointer :: iglobal(:)             ! global cell numbering
        integer, pointer :: iglobal_bnd(:)         ! global boundary numbering
        logical, pointer :: ispoint_bnd(:)         ! is boundary a point source
        integer, pointer :: ilocal_link(:)         ! local number in owner domain
        integer, pointer :: iglobal_link(:)        ! iglobal_link
        integer :: numcontpts             ! numcontpts number of contour nodes
        real(kind = 8), pointer :: flowelemcontourx(:, :)  ! flowelemcontourx
        real(kind = 8), pointer :: flowelemcontoury(:, :)  ! flowelemcontoury
        integer :: lnx1d                  ! number of 1d pointers
        real(kind = 8), pointer :: xu(:)                  ! xu
        real(kind = 8), pointer :: yu(:)                  ! yu

        integer :: numk                   ! number of nodes
        real(kind = 8), pointer :: xk(:)                  ! xu
        real(kind = 8), pointer :: yk(:)                  ! yu
        real(kind = 8), pointer :: zk(:)                  ! yu
        integer :: numl                   !< number of nodes links
        integer, pointer :: kn(:, :)                !< node links
        integer :: nv                     !< max nr of nodes describing an element
        integer :: nump                   !< number of elements
        integer, pointer :: netcellnod(:, :)        !< element nodes
        type(t_ug_meta) :: meta                   !< meta data
        type(t_ug_meshgeom) :: waqgeom                !< geometry
        type(t_crs) :: crs                    !< Container for information about coordinate reference system
        integer :: conv_type              !< netcdf convention type
        real(8) :: conv_version           !< netcdf convension version
        integer, pointer :: edge_type(:)           !< edge type
        integer, pointer :: global_edge(:)         !< global edge number
        integer, pointer :: global_node(:)         !< global node number

    end type t_hydrodynamics

    type t_hydrodynamics_collection
        type(t_hydrodynamics), pointer :: hyd_pnts(:)            ! pointer to the hyd descriptions
        integer :: maxsize                ! maximum size of the current array
        integer :: current_size                ! filled up to this size
    end type t_hydrodynamics_collection

    private :: openbndlin_coll_add, openbndsect_coll_find, openbndsect_coll_add

contains

    function openbndlin_coll_add(openbndlin_coll, openbndlin) result (current_size)
        ! function to add to a collection of openbndlins

        class(t_open_boundary_line_collection) :: openbndlin_coll        ! collection of openbndlins
        type(t_open_boundary_line) :: openbndlin             ! openbndlin to be added
        integer :: current_size                ! return value the new current collection size
        ! and the index of the added openbndlin

        type(t_open_boundary_line), pointer :: openbndlin_pnts(:)     ! pointer for the resize operation
        integer :: i                      ! loop counter

        if (openbndlin_coll%current_size == openbndlin_coll%maxsize) then

            ! resize, allocate new array
            allocate (openbndlin_pnts (openbndlin_coll%maxsize + MAX_NUM))

            ! copy the openbndlins into the new array
            do i = 1, openbndlin_coll%maxsize
                openbndlin_pnts(i) = openbndlin_coll%openbndlin_pnts(i)   ! copies the openbndlins
            enddo

            ! deallocate the old array and attach the new array to the collection
            if (openbndlin_coll%maxsize /= 0) deallocate (openbndlin_coll%openbndlin_pnts)
            openbndlin_coll%openbndlin_pnts => openbndlin_pnts
            openbndlin_coll%maxsize = openbndlin_coll%maxsize + MAX_NUM

        endif

        openbndlin_coll%current_size = openbndlin_coll%current_size + 1
        openbndlin_coll%openbndlin_pnts(openbndlin_coll%current_size) = openbndlin
        current_size = openbndlin_coll%current_size

    end function openbndlin_coll_add

    function openbndsect_coll_find(self, name) result (iret)
        ! function to find a section name in a collection, case sensitive at the moment

        class(t_openbndsect_coll) :: self       ! collection of openbndsects
        character(LEN = *) :: name                   ! name of openbndsect to be found
        integer :: iret                   ! result index in collection or 0 if not found
        integer :: i                      ! loop counter

        iret = 0
        do i = 1, self%current_size
            if (self%openbndsect_pnts(i)%name == name) then
                iret = i
                return
            endif
        end do

    end function openbndsect_coll_find

    function openbndsect_coll_add(self, openbndsect) result (current_size)
        ! function to add to a collection of openbndsects
        class(t_openbndsect_coll) :: self       ! collection of openbndsects
        type(t_openbnd_section) :: openbndsect            ! openbndsect to be added
        integer :: current_size                ! return value the new current collection size
        ! and the index of the added openbndsect

        type(t_openbnd_section), pointer :: openbndsect_pnts(:)    ! pointer for the resize operation
        integer :: i                      ! loop counter

        if (self%current_size == self%maxsize) then

            ! resize, allocate new array
            allocate (openbndsect_pnts (self%maxsize + MAX_NUM))

            ! copy the openbndsects into the new array
            do i = 1, self%maxsize
                openbndsect_pnts(i) = self%openbndsect_pnts(i)   ! copies the openbndsects
            enddo

            ! deallocate the old array and attach the new array to the collection
            if (self%maxsize /= 0) deallocate (self%openbndsect_pnts)
            self%openbndsect_pnts => openbndsect_pnts
            self%maxsize = self%maxsize + MAX_NUM

        endif

        self%current_size = self%current_size + 1
        self%openbndsect_pnts(self%current_size) = openbndsect
        current_size = self%current_size

    end function openbndsect_coll_add

end module m_hydmod

!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.!
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

! $Id: nearfield.f90 69136 2021-06-03 12:43:41Z mourits $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20200708_DELFT3D-37467_COSUMO_and_FM/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/nearfield.f90 $

! Processing data obtained from nearfield models like COSUMO inside D-Flow FM
module m_nearfield
    use iso_c_binding
    use precision
    use MessageHandling
    use m_flowexternalforcings
    use m_transport
    !
    implicit none
    !
    ! constants
    !
    integer, parameter :: NEARFIELD_DISABLED = 0   !< If nearfield_mode is NEARFIELD_DISABLED (default), then nearfield/COSUMO is disabled
    integer, parameter :: NEARFIELD_ENABLED  = 1   !< After call addNearfieldData, nearfield_mode is set to NEARFIELD_ENABLED. This ensures:
                                                   !< - addNearfieldData does not need to be called again, until the data is updated and
                                                   !<   passes the pointers again.
                                                   !< - that subroutine setNFEntrainmentMomentum can be called if flag NearFieldEntrainmentMomentum
                                                   !<   is switched on
    integer, parameter :: NEARFIELD_UPDATED  = 2   !< nearfield_mode is set to NEARFIELD_UPDATED, everytime DIMR passes a data pointer
                                                   !< from cosumo_bmi to D-Flow FM. This is the trigger to call addNearfieldData in
                                                   !< set_external_forcings.
    integer, parameter :: NF_IX     = 1            !< Column 1 in COSUMO data          : x-coordinate
    integer, parameter :: NF_IY     = 2            !< Column 2 in COSUMO data          : y-coordinate
    integer, parameter :: NF_IZ     = 3            !< Column 3 in COSUMO data          : z-coordinate
    integer, parameter :: NF_IS     = 4            !< Column 4 in COSUMO data          : S-factor: if S changes along the sink traject, then entrainment appears
    integer, parameter :: NF_IH     = 5            !< Column 5 in COSUMO data          : half the plume height
    integer, parameter :: NF_IW     = 6            !< Column 6 in COSUMO data          : half the plume width
    integer, parameter :: NF_IUMAG  = 7            !< Column 7 in COSUMO data, OPTIONAL: momentum magnitude
    integer, parameter :: NF_IUDIR  = 8            !< Column 8 in COSUMO data, OPTIONAL: momentum direction: 0: to North, 90: to East
    integer, parameter :: NUM_TRACK = 1000         !< If there is only one source point, a track is created perpendicular to "last sink -> source",
                                                   !< using NF_IW. NUM_TRACK points are defined on this track to detect all cells containing source points.
    integer, parameter :: CONST_OPERATOR_UNDEFINED = 0
    integer, parameter :: CONST_OPERATOR_EXCESS    = 1
    integer, parameter :: CONST_OPERATOR_ABSOLUTE  = 2
    !
    ! integers
    !
    integer                                        :: nearfield_mode    !< Switch to enable/disable COSUMO
    integer                                        :: nf_num_dif        !< Number of diffusers     as obtained from COSUMO_BMI
    integer                                        :: nf_numconst       !< Number of constituents  as obtained from COSUMO_BMI
    integer                                        :: nf_numintake      !< Number of intake points as obtained from COSUMO_BMI
    integer                                        :: nf_numsour        !< Number of source points as obtained from COSUMO_BMI
    integer                                        :: nf_numsink        !< Number of sink points   as obtained from COSUMO_BMI
    integer                                        :: nf_namlen         !< Length of character strings in nf_const_operator as obtained from COSUMO_BMI
    integer                                        :: nf_sour_track_max !< Maximum (over all diffusers) of all source track flow cells
    integer                                        :: nf_intake_cnt_max !< Maximum (over all diffusers) of all number of intake points
    integer                                        :: nf_entr_max       !< Maximum (over all diffusers) of all entrainment points (coupled sink source points)
    !
    ! Pointers to data inside COSUMO_BMI, allocated in COSUMO_BMI
    !
    real(fp)            , dimension(:)       , pointer  :: nf_q_source        !< Qsource
    real(fp)            , dimension(:)       , pointer  :: nf_q_intake        !< Qintake
    real(fp)            , dimension(:,:)     , pointer  :: nf_const           !< Constituent values
                                                                              !< DIM 1: diffuser
                                                                              !< DIM 2: constituent
    real(fp)            , dimension(:,:,:)   , pointer  :: nf_intake          !< Intake
                                                                              !< DIM 1: diffuser
                                                                              !< DIM 2: intake point id
                                                                              !< DIM 3: X, Y, Z
    real(fp)            , dimension(:,:,:)   , pointer  :: nf_sink            !< Sinks
                                                                              !< DIM 1: diffuser
                                                                              !< DIM 2: sink point id
                                                                              !< DIM 3: X, Y, Z, S, H, B
    real(fp)            , dimension(:,:,:)   , pointer  :: nf_sour            !< Sources
                                                                              !< DIM 1: diffuser
                                                                              !< DIM 2: source point id
                                                                              !< DIM 3: X, Y, Z, S, H, B, Umag, Udir
    character(:)        , dimension(:)       , pointer  :: nf_const_operator  !< Constituent operator
    logical(kind=c_bool), dimension(:)       , pointer  :: nf_src_mom         !< true: Umag and Udir in nf_sour are filled
    !
    ! NearField arrays on FM domain, allocated in FM
    !
    integer , dimension(:,:)    , allocatable :: nf_sink_n         !<    Flow cell index of sink   points of all diffusers
    integer , dimension(:,:)    , allocatable :: nf_sour_n         !<    Flow cell index of source points of all diffusers
    integer , dimension(:,:)    , allocatable :: nf_intake_n       !<    Flow cell index of intake points of all diffusers
    integer , dimension(:,:)    , allocatable :: nf_intake_nk      !< 3D index of nf_intake_n
    integer , dimension(:)      , allocatable :: nf_numintake_idif !< nf_numintake per diffuser
    integer , dimension(:)      , allocatable :: nf_entr_start     !< Start index in src arrays of entrainment points (coupled sink source)        of all diffusers
    integer , dimension(:)      , allocatable :: nf_entr_end       !< End   index in src arrays of entrainment points (coupled sink source)        of all diffusers
    integer , dimension(:,:)    , allocatable :: nf_sinkid         !< 3D index of nf_sink_n for each src point (from nf_entr_start to nf_entr_end) of all diffusers
    real(fp), dimension(:,:)    , allocatable :: nf_sour_wght      !< Fraction * 1000.0       of each source point of all diffusers
    real(fp), dimension(:)      , allocatable :: nf_sour_wght_sum  !< Sum of all source weights for each diffuser
    real(fp), dimension(:,:)    , allocatable :: nf_intake_wght    !< Fraction * nf_numintake of each intake point of all diffusers
    real(fp), dimension(:,:)    , allocatable :: nf_intake_z       !< Z coordinate            of each intake point of all diffusers

    contains
!
!
!==============================================================================
!> Main entry point from within D-Flow FM (set_external_forcings),
!! called after cosumo_bmi has updated data.
!! By calling desa and nearfieldToFM, the new data is processed and added to the
!! arrays used during the D-Flow FM computation
subroutine addNearfieldData()
    call desa()
    call nearfieldToFM() !ksrc(1,:),ksrc(4,:),qstss, srcnames, zsrc, zsrc2, later also area
    nearfield_mode = NEARFIELD_ENABLED
end subroutine addNearfieldData
!
!
!==============================================================================
!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to a computation, only call reset_nearfieldData() instead.
subroutine default_nearfieldData()
    nearfield_mode = NEARFIELD_DISABLED
end subroutine default_nearfieldData
!
!
!==============================================================================
!> Resets only variables in this module intended for a restart of a simulation.
!! Upon loading of new model, call default_nearfieldData() instead.
subroutine reset_nearfieldData()
    nf_num_dif     = 0
    nf_numconst    = 0
    nf_numintake   = 0
    nf_numsour     = 0
    nf_numsink     = 0
    numsrc_nf      = 0
    nf_entr_start  = 0
    nf_entr_end    = 0
    !
    ! Pointers to data inside COSUMO_BMI
    !
    nf_q_source       => null()
    nf_q_intake       => null()
    nf_const          => null()
    nf_intake         => null()
    nf_sink           => null()
    nf_sour           => null()
    nf_const_operator => null()
    nf_src_mom        => null()
    !
    ! NearField arrays in FM
    !
    call dealloc_nfarrays()
end subroutine reset_nearfieldData
!
!
!==============================================================================
subroutine dealloc_nfarrays()
    !
    ! Locals
    integer :: istat
    !
    ! Body
    if (allocated(nf_sink_n)        ) deallocate(nf_sink_n        , stat=istat)
    if (allocated(nf_sour_n)        ) deallocate(nf_sour_n        , stat=istat)
    if (allocated(nf_intake_n)      ) deallocate(nf_intake_n      , stat=istat)
    if (allocated(nf_intake_nk)     ) deallocate(nf_intake_nk     , stat=istat)
    if (allocated(nf_numintake_idif)) deallocate(nf_numintake_idif, stat=istat)
    if (allocated(nf_sinkid)        ) deallocate(nf_entr_start    , stat=istat)
    if (allocated(nf_sinkid)        ) deallocate(nf_entr_end      , stat=istat)
    if (allocated(nf_sinkid)        ) deallocate(nf_sinkid        , stat=istat)
    if (allocated(nf_sour_wght)     ) deallocate(nf_sour_wght     , stat=istat)
    if (allocated(nf_sour_wght_sum) ) deallocate(nf_sour_wght_sum , stat=istat)
    if (allocated(nf_intake_wght)   ) deallocate(nf_intake_wght   , stat=istat)
    if (allocated(nf_intake_z)      ) deallocate(nf_intake_z      , stat=istat)
end subroutine dealloc_nfarrays
!
!
!==============================================================================
!> Converts Cosumo output to D-Flow FM sources following the DESA methodology of Joseph Lee
!> Input:  "Pointers to data inside COSUMO_BMI" (nf_q_source, nf_q_intake, ..., nf_src_mom)
!> Result: "NearField arrays on FM domain" are filled (nf_sink_n, nf_sour_n, ..., nf_intake_z)
subroutine desa()
    use m_alloc
    use MessageHandling, only: IdLen
    use m_GlobalParameters, only: INDTP_1D, INDTP_2D, INDTP_ALL
    !
    ! Locals
    integer                                     :: i
    integer                                     :: j
    integer                                     :: idif
    integer                                     :: isour
    integer                                     :: itrack
    integer                                     :: istat
    integer                                     :: kbot
    integer                                     :: ktop
    integer                                     :: jakdtree  = 1     !< use kdtree (1) or not (other)
    integer                                     :: jaoutside = 0     !< allow outside cells (for 1D) (1) or not (0)
    !
    ! Body
    !
    ! Initialization
    !
    ! During debugging, sometimes arrays contain strange values. Clean the most important once.
    if (allocated(nf_sink_n)  ) deallocate(nf_sink_n  , stat=istat)
    if (allocated(nf_sour_n)  ) deallocate(nf_sour_n  , stat=istat)
    if (allocated(nf_intake_n)) deallocate(nf_intake_n, stat=istat)
    !
    ! Sink: dimension is read from NearField and is fixed: allocate
    call realloc(nf_sink_n       , (/ nf_num_dif, nf_numsink /), keepExisting=.false., fill = 0)
    call realloc(nf_sour_wght_sum,    nf_num_dif               , keepExisting=.false., fill = 0.0_hp)
    !
    ! Source: number of source points is going to be determined. start with 1.
    nf_sour_track_max = 1
    nf_sour_wght      = 0.0d0
    nf_sour_wght_sum  = 0.0d0
    !
    ! Intake: May vary per diffuser. Start with 0 or 1
    call realloc(nf_numintake_idif, nf_num_dif, keepExisting=.false., fill = nf_numintake)
    if (nf_numintake == 0) then
        nf_intake_cnt_max = 0
    else
        nf_intake_cnt_max = 1
    endif
    nf_intake_wght = 0.0d0
    !
    ! For each diffuser
    do idif = 1, nf_num_dif
        !
        ! Sinks
        call getSinkLocations(idif, jakdtree, jaoutside, INDTP_2D)
        !
        ! Intakes
        call getIntakeLocations(idif, jakdtree, jaoutside, INDTP_2D)
        !
        ! Sources
        call getSourceLocations(idif, jakdtree, jaoutside, INDTP_2D)
    enddo !idiffuser
end subroutine desa
!
!
!==============================================================================
!> Convert Nearfield data into arrays actually used during the D-Flow FM computation
!> Input:  All "nf_" arrays
!> Result: Filled:
!>            ksrc  : (1,i) horizontal cell index of sink
!>                    (4,i) horizontal cell index of source
!>            zsrc  : (1,i) bottom z-coordinate of sink
!>                    (2,i) bottom z-coordinate of source
!>            zsrc2 : (1,i) top    z-coordinate of sink
!>                    (2,i) top    z-coordinate of source
!>            qstss : discharge volumes and constituents
!>         For momentum:
!>            arsrc : area of discharge. Velocity = qstss_volume / arsrc
!>            cssrc : cosine of angle of discharge
!>            snsrc : sinus  of angle of discharge
subroutine nearfieldToFM()
    use m_alloc
    use m_physcoef, only: NFEntrainmentMomentum
    !
    ! Locals
    integer                             :: i
    integer                             :: idif
    integer                             :: isink
    integer                             :: isour
    integer                             :: istat
    integer                             :: iconst
    integer                             :: iconst_operator
    integer                             :: iintake
    integer                             :: kbot
    integer                             :: ktop
    integer                             :: nk
    real(fp)                            :: sum_weight_intakes
    !
    ! Body
    !
    ! Reset the FM dimensions:
    !     numsrc   : without numsrc_nf
    !     numsrc_nf: 0
    ! They will be redefined in this subroutine
    do i = numsrc+1, numsrc_nf
        arsrc(i) = 0.0_hp
    enddo
    numsrc      = numsrc - numsrc_nf
    numsrc_nf   = 0
    nf_entr_max = 0
    if (NFEntrainmentMomentum > 0) then
        call realloc(nf_entr_start, nf_num_dif, keepExisting=.false., fill = 0)
        call realloc(nf_entr_end  , nf_num_dif, keepExisting=.false., fill = 0)
    endif
    if (allocated(nf_sinkid)) nf_sinkid = 0
    !
    ! For each diffuser
    do idif = 1, nf_num_dif
        !
        ! Intake preparations:
        ! sum_weight_intakes is needed to compute the discharge in each intake point
        sum_weight_intakes = 0.0_fp
        do iintake = 1, nf_intake_cnt_max
            if (nf_intake_n(idif,iintake) == 0) exit
            sum_weight_intakes = sum_weight_intakes + nf_intake_wght(idif,iintake)
        enddo
        !
        ! ENTRAINMENT:
        call entrainmentToSrc(idif)
        !
        ! DISCHARGE ITSELF:
        call dischargeToSrc(idif, sum_weight_intakes)
        !
        ! INTAKES:
        call intakesToSrc(idif, sum_weight_intakes)
    enddo
end subroutine nearfieldToFM
!
!
!==============================================================================
!> Use find_flownode to convert x,y-coordinates of each sink location into nf_sink_n index
!> Keep all sinks separated, even if the n-index is the same: height varying is allowed
subroutine getSinkLocations(idif, jakdtree, jaoutside, iLocTp)
    use m_alloc
    !
    ! Arguments
    integer, intent(in)    :: idif      !< Diffuser id
    integer, intent(inout) :: jakdtree
    integer, intent(in)    :: jaoutside
    integer, intent(in)    :: iLocTp
    !
    ! Locals
    integer                                     :: i
    integer                                     :: istat
    real(hp)        , dimension(:), allocatable :: find_x            !< array containing x-coordinates of locations for which the cell index n is searched for by calling find_flownode
    real(hp)        , dimension(:), allocatable :: find_y            !< array containing y-coordinates of locations for which the cell index n is searched for by calling find_flownode
    character(IdLen), dimension(:), allocatable :: find_name         !< array containing names         of locations for which the cell index n is searched for by calling find_flownode
    integer         , dimension(:), allocatable :: find_n            !< array containing the result of a call to find_flownode
    !
    ! Body
    call realloc(find_x, nf_numsink, keepExisting=.false., fill = 0.0_hp)
    call realloc(find_y, nf_numsink, keepExisting=.false., fill = 0.0_hp)
    call realloc(find_n, nf_numsink, keepExisting=.false., fill = 0)
    if (allocated(find_name)) deallocate(find_name, stat=istat)
    allocate(character(IdLen)::find_name(nf_numsink), stat=istat)
    find_name = ' '
    do i = 1, nf_numsink
        find_x(i) = nf_sink(idif,i,NF_IX)
        find_y(i) = nf_sink(idif,i,NF_IY)
        write(find_name(i),'(i0.4,a,i0.4)') idif, "sink", i
    enddo
    call find_flownode(nf_numsink, find_x, find_y, find_name, find_n, jakdtree, jaoutside, iLocTp)
    do i = 1, nf_numsink
        if (find_n(i) == 0) then
            call mess(LEVEL_ERROR, "Sink point '", trim(find_name(i)),"' not found")
        endif
        nf_sink_n(idif,i) = find_n(i)
    enddo
    !
    if (allocated(find_x)    ) deallocate(find_x    , stat=istat)
    if (allocated(find_y)    ) deallocate(find_y    , stat=istat)
    if (allocated(find_name) ) deallocate(find_name , stat=istat)
    if (allocated(find_n)    ) deallocate(find_n    , stat=istat)
end subroutine getSinkLocations
!
!
!==============================================================================
!> Use find_flownode to convert x,y-coordinates of each intake location into nf_intake_n index
!> Also get nk-index, sum for each nk, define weights
subroutine getIntakeLocations(idif, jakdtree, jaoutside, iLocTp)
    use m_alloc
    use m_flow, only: zws
    !
    ! Arguments
    integer, intent(in)    :: idif      !< Diffuser id
    integer, intent(inout) :: jakdtree
    integer, intent(in)    :: jaoutside
    integer, intent(in)    :: iLocTp
    !
    ! Locals
    integer                                     :: i
    integer                                     :: j
    integer                                     :: istat
    integer                                     :: nf_intake_cnt
    integer                                     :: nk
    integer                                     :: kbot
    integer                                     :: ktop
    real(hp)        , dimension(:), allocatable :: find_x            !< array containing x-coordinates of locations for which the cell index n is searched for by calling find_flownode
    real(hp)        , dimension(:), allocatable :: find_y            !< array containing y-coordinates of locations for which the cell index n is searched for by calling find_flownode
    character(IdLen), dimension(:), allocatable :: find_name         !< array containing names         of locations for which the cell index n is searched for by calling find_flownode
    integer         , dimension(:), allocatable :: find_n            !< array containing the result of a call to find_flownode
    !
    ! Body
    call realloc(find_x, nf_numintake, keepExisting=.false., fill = 0.0_hp)
    call realloc(find_y, nf_numintake, keepExisting=.false., fill = 0.0_hp)
    call realloc(find_n, nf_numintake, keepExisting=.false., fill = 0)
    !call realloc(nf_numintake_idif, nf_num_dif, keepExisting=.false., fill = 0)
    if (allocated(find_name)) deallocate(find_name, stat=istat)
    allocate(character(IdLen)::find_name(nf_numintake), stat=istat)
    find_name = ' '
    do i = 1, nf_numintake
        if (comparereal(nf_intake(idif,i,NF_IX),0.0_hp)==0 .and. &
            comparereal(nf_intake(idif,i,NF_IY),0.0_hp)==0) then
            nf_numintake_idif(idif) = i-1
            exit
        endif
        find_x(i) = nf_intake(idif,i,NF_IX)
        find_y(i) = nf_intake(idif,i,NF_IY)
        write(find_name(i),'(i0.4,a,i0.4)') idif, "intake", i
    enddo
    call find_flownode(nf_numintake_idif(idif), find_x, find_y, find_name, find_n, jakdtree, jaoutside, iLocTp)
    !
    if (nf_numintake_idif(idif) /= 0) then
        !
        ! First handle the first intake point of this diffuser: it will always result in an additional intake point
        ! Copy nf_intake(:,:,NF_IZ) to nf_intake_z: administration index has changed
        nf_intake_cnt = 1
        call realloc(nf_intake_n   , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0)
        call realloc(nf_intake_nk  , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0)
        call realloc(nf_intake_z   , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0.0_hp)
        call realloc(nf_intake_wght, (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0.0_hp)
        if (find_n(1) == 0) then
            call mess(LEVEL_ERROR, "Intake point '", trim(find_name(1)),"' not found")
        endif
        call getkbotktop(find_n(1),kbot,ktop)
        do nk = kbot, ktop
            if ( zws(nk) > - nf_intake(idif,1,NF_IZ) .or. nk == ktop ) then
                exit
            endif
        enddo
        nf_intake_n   (idif,1) = find_n(1)
        nf_intake_nk  (idif,1) = nk
        nf_intake_z   (idif,1) = - nf_intake(idif,1,NF_IZ)
        nf_intake_wght(idif,1) = nf_intake_wght(idif,1) + 1.0_fp
        !
        ! Now handle the rest of the intake points of this diffuser
        do i = 2, nf_numintake_idif(idif)
            if (comparereal(nf_intake(idif,i,NF_IX),0.0_hp)==0 .and. &
                comparereal(nf_intake(idif,i,NF_IY),0.0_hp)==0 .and. &
                comparereal(nf_intake(idif,i,NF_IZ),0.0_hp)==0) then
                exit
            endif
            if (find_n(i) == 0) then
                call mess(LEVEL_ERROR, "Intake point '", trim(find_name(i)),"' not found")
            endif
            call getkbotktop(find_n(i),kbot,ktop)
            do nk = kbot, ktop
                if ( zws(nk) > - nf_intake(idif,i,NF_IZ) .or. nk == ktop ) then
                    exit
                endif
            enddo
            !
            ! Check whether this nk-point is already in array nf_intake_nk
            ! If yes: increase wght, set nk=0
            do j = 1, nf_intake_cnt
                if (nf_intake_nk(idif,j) == nk) then
                    nf_intake_wght(idif,j) = nf_intake_wght(idif,j) + 1.0_fp  ! weight/wght_tot: relative withdrawal from this cell
                    nk = 0
                    exit
                endif
            enddo
            !
            ! nk /= 0: This nk-point is not yet in array nf-intake_nk, so this is a new flow node:
            ! Increase arrays and add the new point
            if (nk /= 0) then
                nf_intake_cnt          = nf_intake_cnt + 1  ! For this diffuser
                nf_intake_cnt_max      = max(nf_intake_cnt_max,nf_intake_cnt) ! Of all diffusers
                call realloc(nf_intake_n   , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0)
                call realloc(nf_intake_nk  , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0)
                call realloc(nf_intake_z   , (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0.0_hp)
                call realloc(nf_intake_wght, (/ nf_num_dif, nf_intake_cnt_max /), keepExisting=.true., fill = 0.0_hp)
                nf_intake_n   (idif,nf_intake_cnt) = find_n(i)
                nf_intake_nk  (idif,nf_intake_cnt) = nk
                nf_intake_z   (idif,nf_intake_cnt) = - nf_intake(idif,i,NF_IZ)
                nf_intake_wght(idif,nf_intake_cnt) = 1.0_hp
            endif
        enddo
    endif
    !
    if (allocated(find_x)    ) deallocate(find_x     , stat=istat)
    if (allocated(find_y)    ) deallocate(find_y     , stat=istat)
    if (allocated(find_name) ) deallocate(find_name  , stat=istat)
    if (allocated(find_n)    ) deallocate(find_n     , stat=istat)
end subroutine getIntakeLocations
!
!
!==============================================================================
!> Use find_flownode to convert x,y-coordinates of each sink location into nf_sink_n index
!> Keep all sinks separated, even if the n-index is the same: height varying is allowed
subroutine getSourceLocations(idif, jakdtree, jaoutside, iLocTp)
    use m_alloc
    use mathconsts, only: pi
    !
    ! Arguments
    integer, intent(in)    :: idif      !< Diffuser id
    integer, intent(inout) :: jakdtree
    integer, intent(in)    :: jaoutside
    integer, intent(in)    :: iLocTp
    !
    ! Locals
    integer                                     :: i
    integer                                     :: istat
    integer                                     :: isour
    integer                                     :: itrack
    real(hp)        , dimension(:), allocatable :: find_x            !< array containing x-coordinates of locations for which the cell index n is searched for by calling find_flownode
    real(hp)        , dimension(:), allocatable :: find_y            !< array containing y-coordinates of locations for which the cell index n is searched for by calling find_flownode
    character(IdLen), dimension(:), allocatable :: find_name         !< array containing names         of locations for which the cell index n is searched for by calling find_flownode
    integer         , dimension(:), allocatable :: find_n            !< array containing the result of a call to find_flownode
    integer                                     :: nf_sour_track     !< number of source cells for a diffuser
                                                                     !< if source points are in the same cell, nf_sour_track is not increased
    integer                                     :: nk
    real(hp)                                    :: ang_end           !< nf_numsour==1: angle of line connecting the last sink point with the only source point
    real(hp)                                    :: dx                !< nf_numsour==1: used to construct the source track
    real(hp)                                    :: dy                !< nf_numsour==1: used to construct the source track
    real(hp)                                    :: xstart            !< nf_numsour==1: used to construct the source track
    real(hp)                                    :: ystart            !< nf_numsour==1: used to construct the source track
    real(hp)                                    :: xend              !< nf_numsour==1: used to construct the source track
    real(hp)                                    :: yend              !< nf_numsour==1: used to construct the source track
    !
    ! Body
    if (nf_numsour > 0) then
        if (nf_numsour==1 .and. nf_numsink>0) then
            ! Centre point and width specified at input
            ! (Single) source point:
            ! Determine the flow nodes over which to distribute the diluted discharge:
            ! Both sink and source point needed for direction connection line.
            ! Without Sink, single source is handled in Loop for multiple sources!
            ! Define the line through the source point, perpendicular to the line connecting the
            ! last sink point with the source point. Define the line piece on this line, using
            ! the specified source-width. Walk with 1000 steps over this line piece and check in what
            ! flow node n you are. With this information, you can define the n-points to distribute the
            ! sources over and their relative weights.
            ! Vertically: the height specification of the source is used; nothing to do here
            !
            nf_sour_wght_sum(idif) = 1000.0_fp
            !
            ! Begin and end of horizontal distribution line piece
            !
            ang_end = atan2( (nf_sour(idif,1,NF_IY)-nf_sink(idif,nf_numsink,NF_IY)) , (nf_sour(idif,1,NF_IX)-nf_sink(idif,nf_numsink,NF_IX)) )
            dx      = -1.0_hp*nf_sour(idif,1,NF_IW)*cos(pi/2.0_hp - ang_end)
            dy      =  1.0_hp*nf_sour(idif,1,NF_IW)*sin(pi/2.0_hp - ang_end)
            !
            xstart   = nf_sour(idif,1,NF_IX) + dx
            ystart   = nf_sour(idif,1,NF_IY) + dy
            xend     = nf_sour(idif,1,NF_IX) - dx
            yend     = nf_sour(idif,1,NF_IY) - dy
            !
            dx = (xend - xstart)/999.0_fp
            dy = (yend - ystart)/999.0_fp
            !
            call realloc(find_x, NUM_TRACK, keepExisting=.false., fill = 0.0_hp)
            call realloc(find_y, NUM_TRACK, keepExisting=.false., fill = 0.0_hp)
            call realloc(find_n, NUM_TRACK, keepExisting=.false., fill = 0)
            if (allocated(find_name)) deallocate(find_name, stat=istat)
            allocate(character(IdLen)::find_name(NUM_TRACK), stat=istat)
            find_name = ' '
            do itrack = 1, NUM_TRACK
                find_x(itrack) = xstart + (itrack-1)*dx
                find_y(itrack) = ystart + (itrack-1)*dy
                write(find_name(itrack),'(i0.4,a,i0.4)') idif, "sour track", itrack
            enddo
            call find_flownode(NUM_TRACK, find_x, find_y, find_name, find_n, jakdtree, jaoutside, iLocTp)
            !
            ! First handle the first source_track point of this diffuser: it will always result in an additional source point
            nf_sour_track = 1
            call realloc(nf_sour_n   , (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0)
            call realloc(nf_sour_wght, (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0.0_hp)
            if (find_n(1) == 0) then
                call mess(LEVEL_ERROR, "Source point '", trim(find_name(1)),"' not found")
            endif
            nf_sour_n   (idif,1) = find_n(1)
            nf_sour_wght(idif,1) = nf_sour_wght(idif,1) + 1.0_fp
            !
            ! Now handle the rest of the source_track points of this diffuser
            do itrack = 2, NUM_TRACK
                if (find_n(itrack) == 0) then
                    call mess(LEVEL_ERROR, "Source point '", trim(find_name(itrack)),"' not found")
                endif
                !
                ! If find_n is different from the last found n: This is a new flow node:
                ! Increase arrays and add the new point
                if (find_n(itrack) /= nf_sour_n(idif,nf_sour_track)) then
                    nf_sour_track          = nf_sour_track + 1  ! For this diffuser
                    nf_sour_track_max      = max(nf_sour_track_max,nf_sour_track) ! Of all diffusers
                    call realloc(nf_sour_n   , (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0)
                    call realloc(nf_sour_wght, (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0.0_hp)
                    nf_sour_n(idif,nf_sour_track) = find_n(itrack)
                endif
                nf_sour_wght(idif,nf_sour_track) = nf_sour_wght(idif,nf_sour_track) + 1.0_fp  ! weight/wght_tot: relative discharge in this cell
            enddo
        else
            ! nf_numsour > 1
            !
            call realloc(find_x, nf_numsour, keepExisting=.false., fill = 0.0_hp)
            call realloc(find_y, nf_numsour, keepExisting=.false., fill = 0.0_hp)
            call realloc(find_n, nf_numsour, keepExisting=.false., fill = 0)
            if (allocated(find_name)) deallocate(find_name, stat=istat)
            allocate(character(IdLen)::find_name(nf_numsour), stat=istat)
            find_name = ' '
            do isour = 1, nf_numsour
                find_x(isour) = nf_sour(idif,isour,NF_IX)
                find_y(isour) = nf_sour(idif,isour,NF_IY)
                write(find_name(isour),'(i0.4,a,i0.4)') idif, "sour", isour
            enddo
            call find_flownode(nf_numsour, find_x, find_y, find_name, find_n, jakdtree, jaoutside, iLocTp)
            !
            ! Keep the sources separated, even if they are in the same cell: momentum specification might differ
            !
            nf_sour_track_max = max(nf_sour_track_max,nf_numsour) ! Of all diffusers
            call realloc(nf_sour_n   , (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0)
            call realloc(nf_sour_wght, (/ nf_num_dif, nf_sour_track_max /), keepExisting=.true., fill = 0.0_hp)
            nf_sour_wght_sum(idif) = real(nf_numsour,fp)
            do isour = 1, nf_numsour
                if (find_n(isour) == 0) then
                    call mess(LEVEL_ERROR, "Source point '", trim(find_name(isour)),"' not found")
                endif
                nf_sour_n   (idif,isour) = find_n(isour)
                nf_sour_wght(idif,isour) = 1.0_fp  ! nf_numsour>1: each source line has a weight of 1.0
            enddo
        endif
    endif
    !
    if (allocated(find_x)    ) deallocate(find_x    , stat=istat)
    if (allocated(find_y)    ) deallocate(find_y    , stat=istat)
    if (allocated(find_name) ) deallocate(find_name , stat=istat)
    if (allocated(find_n)    ) deallocate(find_n    , stat=istat)
end subroutine getSourceLocations
!
!
!==============================================================================
!> Convert entrainment data into src arrays of D-Flow FM
!> From each sink point to each source (track) point
subroutine entrainmentToSrc(idif)
    use m_alloc
    use m_physcoef, only: NFEntrainmentMomentum
    use m_flow, only: zws
    !
    ! Arguments
    integer, intent(in) :: idif !< Diffuser id
    !
    ! Locals
    integer :: iconst
    integer :: isink
    integer :: isour
    integer :: ktop
    integer :: kbot
    integer :: nk
    !
    ! Body
    if (NFEntrainmentMomentum > 0) then
        nf_entr_start(idif) = numsrc    + 1
        nf_entr_end(idif)   = nf_entr_start(idif) - 1
    endif
    ! Start with isink=2: For isink=1, q is zero, because nf_sink(i,isink-1,IS) is undefined
    do isink = 2, nf_numsink
        do isour = 1, nf_sour_track_max
            !
            ! Create a new entry in the src arrays for each combination of a sink_flow_node and source_flow_node
            if (nf_sour_n(idif,isour) == 0) exit ! This might happen if the number of sources is not the same for each diffuser
            numsrc_nf         = numsrc_nf         + 1
            numsrc            = numsrc            + 1
            if (NFEntrainmentMomentum > 0) then
                nf_entr_end(idif) = nf_entr_end(idif) + 1
                nf_entr_max       = max(nf_entr_max, nf_entr_end(idif)-nf_entr_start(idif)+1)
            endif
            call reallocsrc(numsrc)
            !
            ! Name
            write(srcname(numsrc),'(3(a,i0.4))') "diffuser ", idif, " , sink ", isink, " , source_track ", isour
            !
            ! Sink
            ksrc (1,numsrc) = nf_sink_n(idif,isink)
            zsrc (1,numsrc) = - nf_sink(idif,isink,NF_IZ) - nf_sink(idif,isink,NF_IH)
            zsrc2(1,numsrc) = - nf_sink(idif,isink,NF_IZ) + nf_sink(idif,isink,NF_IH)
            !
            ! Source
            ksrc (4,numsrc) = nf_sour_n(idif,isour)
            if (nf_numsour == 1) then
                zsrc (2,numsrc) = - nf_sour(idif,nf_numsour,NF_IZ) - nf_sour(idif,nf_numsour,NF_IH)
                zsrc2(2,numsrc) = - nf_sour(idif,nf_numsour,NF_IZ) + nf_sour(idif,nf_numsour,NF_IH)
            else
                !
                ! Do not use NF_IH, but just NF_IZ
                zsrc (2,numsrc) = - nf_sour(idif,isour,NF_IZ)
                zsrc2(2,numsrc) = - nf_sour(idif,isour,NF_IZ)
            endif
            call check_mixed_source_sink(numsrc)
            !
            ! q = delta_IS * Q_TOT * this_cell_fraction
            qstss((1+numconst)*(numsrc-1) + 1) = (nf_sink(idif,isink,NF_IS)-nf_sink(idif,isink-1,NF_IS)) * nf_q_source(idif) &
                                               & * nf_sour_wght(idif,isour) / nf_sour_wght_sum(idif)
            !
            ! Constituents: Entrainment does not cause addition
            do iconst = 1, numconst
                qstss((1+numconst)*(numsrc-1) + 1 + iconst) = 0.0_hp
            enddo
            !
            if (NFEntrainmentMomentum > 0) then
                !
                ! Store the nk index of the flow nodes containing the sink locations.
                ! They are used in subroutine setNFEntrainmentMomentum
                !
                call realloc(nf_sinkid, (/ nf_num_dif, nf_entr_max /), keepExisting=.true., fill = 0)
                call getkbotktop(nf_sink_n(idif,isink),kbot,ktop)
                do nk = kbot, ktop
                    if ( zws(nk) > - nf_sink(idif,isink,NF_IZ) .or. nk == ktop ) then
                        exit
                    endif
                enddo
                nf_sinkid(idif, nf_entr_end(idif)-nf_entr_start(idif)+1) = nk
            endif
        enddo
    enddo
end subroutine entrainmentToSrc
!
!
!==============================================================================
!> Convert entrainment data into src arrays of D-Flow FM
!> For each source (track) point, no related sink point
subroutine dischargeToSrc(idif, sum_weight_intakes)
    use m_alloc
    use m_physcoef, only: NFEntrainmentMomentum
    use m_flow, only: zws
    use mathconsts, only: degrad, eps_fp
    !
    ! Arguments
    integer , intent(in) :: idif               !< Diffuser id
    real(fp), intent(in) :: sum_weight_intakes
    !
    ! Locals
    integer                             :: iconst
    integer                             :: iintake
    integer                             :: isour
    integer                             :: iconst_operator
    integer                             :: istat
    integer                             :: sourId
    real(fp)                            :: area
    real(fp), dimension(:), allocatable :: intake_avg_consts   !< If CONST_OPERATOR = EXCESS: Constituent values, averaged over all intake points
    !
    ! Body
    !
    ! Constituent operator may differ per diffuser
    iconst_operator = constOperatorStringToInt(nf_const_operator(idif))
    if (iconst_operator==CONST_OPERATOR_EXCESS) then
        if (nf_intake_n(idif,1) == 0) then
            call mess(LEVEL_ERROR, "ConstituentsOperator is 'excess' but there are no intake points specified")
        endif
        call realloc(intake_avg_consts, numconst, keepExisting=.false., fill = 0.0_hp)
        do iintake = 1, nf_intake_cnt_max
            if (nf_intake_n(idif,iintake) == 0) exit
            do iconst = 1, numconst
                intake_avg_consts(iconst) = intake_avg_consts(iconst) + constituents(iconst,nf_intake_nk(idif,iintake))*nf_intake_wght(idif,iintake)
            enddo
        enddo
        if (sum_weight_intakes > eps_fp) then
            do iconst = 1, numconst
                intake_avg_consts(iconst) = intake_avg_consts(iconst) / sum_weight_intakes
            enddo
        endif
    endif
    !
    do isour = 1, nf_sour_track_max
        if (nf_sour_n(idif,isour) == 0) exit
        numsrc_nf = numsrc_nf + 1
        numsrc    = numsrc    + 1
        call reallocsrc(numsrc)
        if (nf_numsour == 1) then
            sourId = nf_numsour
        else
            sourId = isour
        endif
        !
        ! Name
        write(srcname(numsrc),'(3(a,i0.4))') "diffuser ", idif, " , discharge at source_track ", isour
        !
        ! Sink
        ksrc (1,numsrc) = 0
        zsrc (1,numsrc) = 0.0_hp
        zsrc2(1,numsrc) = 0.0_hp
        !
        ! Source
        ksrc (4,numsrc) = nf_sour_n(idif,isour)
        if (nf_numsour == 1) then
            zsrc (2,numsrc) = - nf_sour(idif,nf_numsour,NF_IZ) - nf_sour(idif,nf_numsour,NF_IH)
            zsrc2(2,numsrc) = - nf_sour(idif,nf_numsour,NF_IZ) + nf_sour(idif,nf_numsour,NF_IH)
        else
            !
            ! Do not use NF_IH, but just NF_IZ
            zsrc (2,numsrc) = - nf_sour(idif,isour,NF_IZ)
            zsrc2(2,numsrc) = - nf_sour(idif,isour,NF_IZ)
        endif
        call check_mixed_source_sink(numsrc)
        !
        ! q = Q_TOT * this_cell_fraction
        qstss((1+numconst)*(numsrc-1) + 1) = nf_q_source(idif) * nf_sour_wght(idif,isour) / nf_sour_wght_sum(idif)
        !
        ! Constituents: Additions as specified by NearField
        do iconst = 1, numconst
            if (iconst_operator == CONST_OPERATOR_ABSOLUTE) then
                qstss((1+numconst)*(numsrc-1) + 1 + iconst) = nf_const(idif,iconst)
            else
                ! Excess
                qstss((1+numconst)*(numsrc-1) + 1 + iconst) = nf_const(idif,iconst) + intake_avg_consts(iconst)
            endif
        enddo
        !
        ! Momentum:
        ! Do not use "D0" nor "SIGMA0" from Cosumo input file.
        ! Use columns 7 (UMAG) and 8 (UDIR) from each source line
        !
        ! Area: Atot = Q_TOT / UMAG
        if (nf_sour(idif,sourId,NF_IUMAG) < eps_fp) then
            area = 0.0_hp
        else
            area = nf_q_source(idif) / nf_sour(idif,sourId,NF_IUMAG)
        endif
        !
        ! Area of this fraction is total area divided by the weight factor:
        ! Qtot**2/Atot must be conserved when dividing it over multiple cells (where we can choose a1, a2, ...):
        !            Qtot**2/Atot                         = (Qtot*w1)**2/a1 + (Qtot*w2)**2/a2 + ...
        ! Since w1+w2+...=1.0, we can write this as:
        !         w1*Qtot**2/Atot + w2*Qtot**2/Atot + ... = (Qtot*w1)**2/a1 + (Qtot*w2)**2/a2 + ...
        ! =>
        !         wi*Qtot**2/Atot  = (Qtot*wi)**2/ai
        ! =>
        !         ai = Atot / wi
        !
        arsrc  (numsrc) = area * nf_sour_wght_sum(idif) / nf_sour_wght(idif,isour)
        ! Direction:
        ! nf_sour(:,:,NF_IUDIR)                           : 0=east , 90=north
        ! To be consistent with Delft3D4, change this into: 0=north, 90=east
        cssrc(2,numsrc) = cos(degrad*(90.0_hp-nf_sour(idif,sourId,NF_IUDIR)))
        snsrc(2,numsrc) = sin(degrad*(90.0_hp-nf_sour(idif,sourId,NF_IUDIR)))
    enddo
    if (allocated(intake_avg_consts)) deallocate(intake_avg_consts, stat=istat)
end subroutine dischargeToSrc
!
!
!==============================================================================
!> Convert intake data into src arrays of D-Flow FM
!> Sinks, not related to source points
subroutine intakesToSrc(idif, sum_weight_intakes)
    !
    ! Arguments
    integer , intent(in) :: idif               !< Diffuser id
    real(fp), intent(in) :: sum_weight_intakes
    !
    ! Locals
    integer   :: iconst
    integer   :: iintake
    !
    ! Body
    !
    do iintake = 1, nf_intake_cnt_max
        if (nf_intake_n(idif,iintake) == 0) exit
        numsrc_nf = numsrc_nf + 1
        numsrc    = numsrc    + 1
        call reallocsrc(numsrc)
        !
        ! Name
        write(srcname(numsrc),'(3(a,i0.4))') "diffuser ", idif, " , intake ", iintake
        !
        ! Sink
        ksrc (1,numsrc) = nf_intake_n(idif,iintake)
        zsrc (1,numsrc) = nf_intake_z(idif,iintake)
        zsrc2(1,numsrc) = nf_intake_z(idif,iintake)
        !
        ! Source
        ksrc (4,numsrc) = 0
        zsrc (2,numsrc) = 0.0_hp
        zsrc2(2,numsrc) = 0.0_hp
        !
        call check_mixed_source_sink(numsrc)
        !
        ! q = Q_TOT * this_cell_fraction
        qstss((1+numconst)*(numsrc-1) + 1) = nf_q_intake(idif) * nf_intake_wght(idif,iintake) / sum_weight_intakes
        !
        ! Constituents, not relevant for pure sinks
        do iconst = 1, numconst
            qstss((1+numconst)*(numsrc-1) + 1 + iconst) = 0.0_hp
        enddo
    enddo
end subroutine intakesToSrc
!
!
!==============================================================================
!> Convert constituent operator string, as specified by COSUMO, into an integer
function constOperatorStringToInt(operator_string) result(i_operator)
    !
    ! Arguments
    character(*), intent(in)  :: operator_string
    integer                   :: i_operator
    !
    ! Body
    select case (trim(operator_string))
    case ('excess')
        i_operator = CONST_OPERATOR_EXCESS
    case ('absolute')
        i_operator = CONST_OPERATOR_ABSOLUTE
    case default
        i_operator = CONST_OPERATOR_UNDEFINED
    end select
    if (i_operator == CONST_OPERATOR_UNDEFINED) then
        call mess(LEVEL_ERROR, "Expecting ConstituentsOperator 'excess' or 'absolute' but found '", trim(operator_string), "'")
    endif
end function constOperatorStringToInt
!
!
!==============================================================================
!> If "NearFieldEntrainmentMomentum" is switched on:
!> Every timestep:
!> Update arsrc, cssrc, snsrc based on ucx/ucy in the sink point
!> The nk index of the sink point is stored in nf_sinkid
!> Keep in mind that the number of entrainment (coupled sink/source) points may vary per diffuser. As a result,
!> the values in nf_sinkid are shifted: instead of using "i", use "i-nf_entr_start(idif)+1"
!> nf_entr_start(idif): first index in src arrays belonging to an entrainment point of diffuser idif
!> nf_entr_end  (idif): last  index in src arrays belonging to an entrainment point of diffuser idif
subroutine setNFEntrainmentMomentum()
    use mathconsts, only: eps_fp
    use m_flow, only: ucx, ucy
    !
    ! Locals
    integer  :: i
    integer  :: idif
    integer  :: nk
    real(fp) :: umag
    !
    ! Body
    !
    ! For each diffuser
    do idif = 1, nf_num_dif
        do i = nf_entr_start(idif), nf_entr_end(idif)
            nk   = nf_sinkid(idif,i-nf_entr_start(idif)+1)
            umag = sqrt(ucx(nk)**2 + ucy(nk)**2)
            if (umag < eps_fp) then
                arsrc(i) = 0.0_hp
            else
                arsrc(  i) = qstss((1+numconst)*(i-1) + 1) / umag
                cssrc(2,i) = ucx(nk) / umag
                snsrc(2,i) = ucy(nk) / umag
            endif
        enddo
    enddo
end subroutine setNFEntrainmentMomentum
!
!
!==============================================================================
!> Every time an src point is added: 
!> - Check whether the new sink   location coincides with an existing source location
!> - Check whether the new source location coincides with an existing sink   location
!> Generate a warning if this happens
subroutine check_mixed_source_sink(numsrc)
    integer, intent(in) :: numsrc
    !
    ! Locals
    integer        :: i
    character(300) :: message
    !
    ! Body
    do i=1, numsrc-1
        !
        ! Check if new sink coincides with an already existing source
        ! Horizontally:
        if (ksrc(1,numsrc)==ksrc(4,i) .and. ksrc(1,numsrc)/=0) then
            ! Vertically:
            ! If ktop1>kbot2 and ktop2>kbot1 then they coincide
            if (zsrc2(1,numsrc) > zsrc(2,i) .and. zsrc2(2,i)>zsrc(1,numsrc)) then
                write(message,'(5a,i0)') "The sink location of '", trim(srcname(numsrc)), &
                                       & "' coincides with the source location of '", trim(srcname(i)), &
                                       & "'. Horizontal cell index: ", ksrc(1,numsrc)
                call mess(LEVEL_WARN, trim(message))
            endif
        endif
        !
        ! Check if new source coincides with an already existing sink
        ! Horizontally:
        if (ksrc(4,numsrc) == ksrc(1,i) .and. ksrc(4,numsrc)/=0) then
            ! Vertically:
            ! If ktop1>kbot2 and ktop2>kbot1 then they coincide
            if (zsrc2(2,numsrc) > zsrc(1,i) .and. zsrc2(1,i)>zsrc(2,numsrc)) then
                write(message,'(5a,i0)') "The source location of '", trim(srcname(numsrc)), &
                                       & "' coincides with the sink location of '", trim(srcname(i)), &
                                       & "'. Horizontal cell index: ", ksrc(4,numsrc)
                call mess(LEVEL_WARN, trim(message))
            endif
        endif
    enddo
end subroutine check_mixed_source_sink


end module m_nearfield

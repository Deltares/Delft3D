!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module dryfld_mod
    use m_waq_precision
    implicit none
    real(kind = real_wp), dimension(:), allocatable, save :: sumvol
end module dryfld_mod

!>      Sets feature of dry cells to zero
!!
!!      Determines which cells were dry at start of time step.
!!      This is an explicit setting of the feature in the
!!      sense that it gives the state of the start of the time step.
!!      The DRY_THRESH variable is used as a thickness (default 1.0 mm) together with
!!      the SURF parameter of segment function. If SURF is absent, 1.0 m2 is
!!      assumed and the DRY_THRESH directly compares vomes in m3.
!!      A dry cell may have transport, because it may be wet at the
!!      end of the time step. It has however no processes yet. The wetting within the
!!      time step is tested by the dryfle routine later in this file.
subroutine dryfld(nosegw, noseg, nolay, volume, noq12, &
        area, nocons, coname, cons, surface, &
        iknmrk, iknmkv)

    use timers
    use dryfld_mod
    use waq_attribute_utils, only : set_feature
    use m_string_utils, only : index_in_array
    implicit none

    integer(kind = int_wp), intent(in)  :: nosegw               !< number of computational volumes water
    integer(kind = int_wp), intent(in)  :: noseg                !< number of computational volumes total
    integer(kind = int_wp), intent(in)  :: nolay                !< number of layers
    real(kind = real_wp), intent(inout) :: volume (noseg)       !< volumes at start of time step
    integer(kind = int_wp), intent(in)  :: noq12                !< number of horizontal exchanges
    real(kind = real_wp), intent(inout) :: area   (noq12)       !< areas at start of time step
    integer(kind = int_wp), intent(in)  :: nocons               !< number of constants
    character(20), intent(in)           :: coname (nocons)      !< names of the constants
    real(kind = real_wp), intent(in)    :: cons   (nocons)      !< values of the constants
    real(kind = real_wp), intent(in)    :: surface(noseg)       !< horizontal surface area
    integer(kind = int_wp), intent(in)  :: iknmrk (noseg)       !< constant feature array
    integer(kind = int_wp), intent(out) :: iknmkv (noseg)       !< time varying feature array

    !     Local variables
    integer(kind = int_wp) :: idryfld         ! help variable to find dry_tresh constant
    real(kind = real_wp)   :: threshold       ! drying and flooding value
    real(kind = real_wp)   :: minvolume       ! minimum volume in a cell
    real(kind = real_wp)   :: minarea         ! minimum exhange area of a horizontal exchange
    integer(kind = int_wp) :: nosegl          ! number of computational volumes per layer
    integer(kind = int_wp) :: isegl           ! loop variable volumes
    integer(kind = int_wp) :: ivol            ! index for this computational volumes
    integer(kind = int_wp) :: ilay            ! loop variable layers
    integer(kind = int_wp) :: ikm             ! feature
    real(kind = real_wp)   :: sum             ! help variable

    integer(kind = int_wp) :: ithandl = 0
    if (timon) call timstrt ("dryfld", ithandl)

    ! Initialisations

    nosegl = nosegw / nolay

    ! Allocate the work array - reallocate if
    ! for some reason the model size has changed
    if (.not. allocated(sumvol)) then
        allocate(sumvol(nosegl))
    endif
    if (size(sumvol) /= nosegl) then
        deallocate(sumvol)
        allocate(sumvol(nosegl))
    endif

    threshold = 0.001                                        ! default value of 1 mm
    idryfld = index_in_array('DRY_THRESH', coname)
    if (idryfld > 0) threshold = cons(idryfld)          ! or the given value

    minvolume = 0.001                                        ! default value of 0.001 m3 = 1 L
    idryfld = index_in_array('MIN_VOLUME', coname)
    if (idryfld > 0) minvolume = cons(idryfld)          ! or the given value

    ivol = 0
    sumvol = 0.0
    do ilay = 1, nolay
        ! Use OpenMP? ikm, ivol private
        ! Is ikm important?
        !$omp parallel do private(ikm,ivol)
        do isegl = 1, nosegl
            ivol = isegl + (ilay - 1) * nosegl
            sumvol(isegl) = sumvol(isegl) + volume(ivol)
        enddo
    enddo

    ivol = 0
    do ilay = 1, nolay
        ! Use OpenMP? ivol private
        !$omp parallel do private(ivol)
        do isegl = 1, nosegl
            ivol = isegl + (ilay - 1) * nosegl
            if (sumvol(isegl) < surface(isegl) * threshold) then
                call set_feature(1, iknmkv(ivol), 0)               ! zero the last bit
                call set_feature(2, iknmkv(ivol), 0)               ! and the second feature
                volume(ivol) = max(volume(ivol), minvolume)
            else
                iknmkv(ivol) = iknmrk(ivol)                    ! become wet again
                volume(ivol) = max(volume(ivol), minvolume)
            endif
        enddo
    enddo

    minarea = 1.00E-04                                      ! default value of 1.00E-04 m2 = 1 cm2
    idryfld = index_in_array('MIN_AREA', coname)
    if (idryfld > 0) minarea = cons(idryfld)           ! or the given value
    area = max(area, minarea)                             ! set minimum area

    if (timon) call timstop (ithandl)

    return
end

!>               Wettens cells that became wet during the time step
!!
!!               Determines which cells have become wet during the time step.
!!               A dry cell may have transport, because it may be wet at the
!!               end of the time step. It has however no processes yet in this step.\n
!!               NB. This routine does NOT set cells dry, it only wettens any dry cells.
subroutine dryfle(nosegw, noseg, volume, nolay, nocons, &
        coname, cons, surface, iknmrk, iknmkv)

    use timers
    use dryfld_mod
    use m_string_utils, only : index_in_array
    implicit none

    integer(kind = int_wp), intent(in)    :: nosegw               !< number of computational volumes water
    integer(kind = int_wp), intent(in)    :: noseg                !< number of computational volumes
    real(kind = real_wp), intent(inout)   :: volume (noseg)       !< volumes at end of time step
    integer(kind = int_wp), intent(in)    :: nolay                !< number of layers
    integer(kind = int_wp), intent(in)    :: nocons               !< number of constants
    character(20), intent(in)             :: coname (nocons)      !< names of the constants
    real(kind = real_wp), intent(in)      :: cons   (nocons)      !< values of the constants
    real(kind = real_wp), intent(in)      :: surface(noseg)       !< horizontal surface area
    integer(kind = int_wp), intent(in)    :: iknmrk (noseg)       !< constant feature array
    integer(kind = int_wp), intent(inout) :: iknmkv (noseg)       !< time varying feature array

    ! local variables
    integer(kind = int_wp) :: idryfld         ! help variable to find dry_tresh constant
    real(kind = real_wp)   :: threshold       ! drying and flooding value
    real(kind = real_wp)   :: minvolume       ! minimum volume in a cell
    integer(kind = int_wp) :: nosegl          ! number of computational volumes per layer
    integer(kind = int_wp) :: isegl           ! loop variable
    integer(kind = int_wp) :: ivol            ! this computational volume
    integer(kind = int_wp) :: ilay            ! loop variable layers
    integer(kind = int_wp) :: ikm             ! feature
    real(kind = real_wp)   :: sum             ! help variable

    integer(kind = int_wp) :: ithandl = 0
    if (timon) call timstrt ("dryfle", ithandl)

    nosegl = nosegw / nolay

    ! Allocate the work array - reallocate if
    ! for some reason the model size has changed
    if (.not. allocated(sumvol)) then
        allocate(sumvol(nosegl))
    endif
    if (size(sumvol) /= nosegl) then
        deallocate(sumvol)
        allocate(sumvol(nosegl))
    endif

    threshold = 0.001                                         ! default value of 1 mm
    idryfld = index_in_array('DRY_THRESH', coname)
    if (idryfld > 0) threshold = cons(idryfld)           ! or the given value

    minvolume = 0.001                                        ! default value of 0.001 m3 = 1 L
    idryfld = index_in_array('MIN_VOLUME', coname)
    if (idryfld > 0) minvolume = cons(idryfld)          ! or the given value

    sumvol = 0.0
    do ilay = 1, nolay
        ! Use OpenMP? ikm, ivol private
        !$omp parallel do private(ikm,ivol)
        do isegl = 1, nosegl
            ivol = isegl + (ilay - 1) * nosegl
            sumvol(isegl) = sumvol(isegl) + volume(ivol)
        enddo
    enddo

    do ilay = 1, nolay
        ! Use OpenMP? ikm, ivol private
        !$omp parallel do private(ivol)
        do isegl = 1, nosegl
            ivol = isegl + (ilay - 1) * nosegl
            if (sumvol(isegl) > surface(isegl) * threshold) then
                iknmkv(ivol) = iknmrk(ivol)
            endif
            volume(ivol) = max(volume(ivol), minvolume)
        enddo
    enddo
    if (timon) call timstop (ithandl)
    return
end subroutine dryfle

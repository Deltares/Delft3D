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
    use timers
    use m_string_utils, only : index_in_array

    implicit none
    real(kind = real_wp), dimension(:), allocatable, save :: sumvol

    contains

    !> Sets feature of dry cells to zero
    !! Determines which cells were dry at start of time step.
    !! This is an explicit setting of the feature in the
    !! sense that it gives the state of the start of the time step.
    !! The DRY_THRESH variable is used as a thickness (default 1.0 mm) together with
    !! the SURF parameter of segment function. If SURF is absent, 1.0 m2 is
    !! assumed and the DRY_THRESH directly compares vomes in m3.
    !! A dry cell may have transport, because it may be wet at the
    !! end of the time step. It has however no processes yet. The wetting within the
    !! time step is tested by the dryfle routine later in this file.
    subroutine dryfld(nosegw, noseg, nolay, volume, noq12, &
            area, nocons, coname, cons, surface, &
            iknmrk, iknmkv)

        use waq_attribute_utils, only : set_feature

        integer(kind = int_wp), intent(in)  :: nosegw               !< Number of computational volumes water
        integer(kind = int_wp), intent(in)  :: noseg                !< Number of computational volumes total
        integer(kind = int_wp), intent(in)  :: nolay                !< Number of layers
        real(kind = real_wp), intent(inout) :: volume (noseg)       !< Volumes at start of time step
        integer(kind = int_wp), intent(in)  :: noq12                !< Number of horizontal exchanges
        real(kind = real_wp), intent(inout) :: area   (noq12)       !< Areas at start of time step
        integer(kind = int_wp), intent(in)  :: nocons               !< Number of constants
        character(20), intent(in)           :: coname (nocons)      !< Names of the constants
        real(kind = real_wp), intent(in)    :: cons   (nocons)      !< Values of the constants
        real(kind = real_wp), intent(in)    :: surface(noseg)       !< Horizontal surface area
        integer(kind = int_wp), intent(in)  :: iknmrk (noseg)       !< Constant feature array
        integer(kind = int_wp), intent(out) :: iknmkv (noseg)       !< Time varying feature array

        ! Local variables
        integer(kind = int_wp) :: idryfld         !< Help variable to find dry_tresh constant
        real(kind = real_wp)   :: threshold       !< Drying and flooding value
        real(kind = real_wp)   :: minvolume       !< Minimum volume in a cell
        real(kind = real_wp)   :: minarea         !< Minimum exhange area of a horizontal exchange
        integer(kind = int_wp) :: nosegl          !< Number of computational volumes per layer
        integer(kind = int_wp) :: isegl           !< Loop variable volumes
        integer(kind = int_wp) :: ivol            !< Index for this computational volumes
        integer(kind = int_wp) :: ilay            !< Loop variable layers
        integer(kind = int_wp) :: ikm             !< Feature
        real(kind = real_wp)   :: sum             !< Help variable

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
    end subroutine dryfld

    !> Wettens cells that became wet during the time step
    !! Determines which cells have become wet during the time step.
    !! A dry cell may have transport, because it may be wet at the
    !! end of the time step. It has however no processes yet in this step.\n
    !! NB. This routine does NOT set cells dry, it only wettens any dry cells.
    subroutine dryfle(nosegw, noseg, volume, nolay, nocons, &
                      coname, cons, surface, iknmrk, iknmkv)

        integer(kind = int_wp), intent(in)    :: nosegw               !< Number of computational volumes water
        integer(kind = int_wp), intent(in)    :: noseg                !< Number of computational volumes
        real(kind = real_wp), intent(inout)   :: volume (noseg)       !< Volumes at end of time step
        integer(kind = int_wp), intent(in)    :: nolay                !< Number of layers
        integer(kind = int_wp), intent(in)    :: nocons               !< Number of constants
        character(20), intent(in)             :: coname (nocons)      !< Names of the constants
        real(kind = real_wp), intent(in)      :: cons   (nocons)      !< Values of the constants
        real(kind = real_wp), intent(in)      :: surface(noseg)       !< Horizontal surface area
        integer(kind = int_wp), intent(in)    :: iknmrk (noseg)       !< Constant feature array
        integer(kind = int_wp), intent(inout) :: iknmkv (noseg)       !< Time varying feature array

        ! Local variables
        integer(kind = int_wp) :: idryfld         !< Help variable to find dry_tresh constant
        real(kind = real_wp)   :: threshold       !< Drying and flooding value
        real(kind = real_wp)   :: minvolume       !< Minimum volume in a cell
        integer(kind = int_wp) :: nosegl          !< Number of computational volumes per layer
        integer(kind = int_wp) :: isegl           !< Loop variable
        integer(kind = int_wp) :: ivol            !< This computational volume
        integer(kind = int_wp) :: ilay            !< Loop variable layers
        integer(kind = int_wp) :: ikm             !< Feature
        real(kind = real_wp)   :: sum             !< Help variable

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
    end subroutine dryfle
end module dryfld_mod

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
module m_setset
    use m_waq_precision

    implicit none

contains

    !> Initialisation of Variables structure
    subroutine setset(lurep, nocons, nopa, nofun, nosfun, &
            nosys, notot, nodisp, novelo, nodef, &
            noloc, ndspx, nvelx, nlocx, nflux, &
            nopred, novar, nogrid, vgrset)

        use timers
        implicit none

        integer(kind = int_wp), intent(in   ) :: lurep                 !< Unit number monitoring file (not used)
        integer(kind = int_wp), intent(in   ) :: nocons                !< Number of constants
        integer(kind = int_wp), intent(in   ) :: nopa                  !< Number of parameters
        integer(kind = int_wp), intent(in   ) :: nofun                 !< Number of functions
        integer(kind = int_wp), intent(in   ) :: nosfun                !< Number of segment functions
        integer(kind = int_wp), intent(in   ) :: nosys                 !< Number of transported substances
        integer(kind = int_wp), intent(in   ) :: notot                 !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: nodisp                !< Number of user-dispersions
        integer(kind = int_wp), intent(in   ) :: novelo                !< Number of user-flows
        integer(kind = int_wp), intent(in   ) :: nodef                 !< Number of default values
        integer(kind = int_wp), intent(in   ) :: noloc                 !< Number of local values
        integer(kind = int_wp), intent(in   ) :: ndspx                 !< Number of dspx
        integer(kind = int_wp), intent(in   ) :: nvelx                 !< Number of velx
        integer(kind = int_wp), intent(in   ) :: nlocx                 !< Number of locx
        integer(kind = int_wp), intent(in   ) :: nflux                 !< Number of flux
        integer(kind = int_wp), intent(in   ) :: nopred                !< Not used
        integer(kind = int_wp), intent(in   ) :: novar                 !< Number of variables on the grids
        integer(kind = int_wp), intent(in   ) :: nogrid                !< Number of grids
        integer(kind = int_wp), intent(inout) :: vgrset(novar, nogrid) !< Number of grids

        ! Local declarations
        integer(kind = int_wp) :: i, ivar, igrid      !< Auxiliary variables for loop and index counting
        integer(kind = int_wp) :: iset                !< Auxiliary variable 1 for igrid = 1, 0 for igrid > 1

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("setset", ithandl)

        do igrid = 1, nogrid
            iset = 0
            if (igrid == 1) iset = 1
            ivar = 0
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! volume
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! area
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! flow
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! length 1
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! length 2
            ivar = ivar + nocons                              ! constants
            ivar = ivar + nopa                                ! parameters
            do i = 1, nofun                                   ! functions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, nosfun                                  ! segment functions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, notot                                   ! concentrations
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, notot                                   ! masses
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, notot                                   ! derivatives
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, nodisp                                  ! dispersions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, novelo                                  ! velocities
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, nodef                                   ! default values
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, noloc                                   ! local values
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            ivar = ivar + ndspx                               ! dspx
            ivar = ivar + nvelx                               ! velx
            ivar = ivar + nlocx                               ! locx
            ivar = ivar + nflux                               ! flux
        enddo
        if (timon) call timstop (ithandl)
    end subroutine setset
end module m_setset

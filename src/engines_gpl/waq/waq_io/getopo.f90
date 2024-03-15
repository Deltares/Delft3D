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
module m_getopo
    use m_waq_precision
    use m_varpoi

    implicit none

contains


    subroutine getopo (noutp, nrvar, nrvarm, dlwnam, iopoin, &
            nmis, notot, syname, nocons, coname, &
            nopa, paname, nofun, funame, nosfun, &
            sfname, lurep)
        !>\file
        !>                 Sets the pointers for all extra vars

        !     Deltares Software Centre

        !     CREATED:    November  1992 by Jan van Beek

        !     LOGICAL UNITNUMBERS : LUREP   - report file

        !     SUBROUTINES CALLED  : VARPOI, Sets pointer for one variable

        use timers       !   performance timers

        implicit none

        !     kind           function         name                    Descriptipon

        integer(kind = int_wp), intent(in) :: noutp                  !< Number of output files
        integer(kind = int_wp), intent(in) :: nrvar (noutp)         !< No of output vars per file
        integer(kind = int_wp), intent(in) :: nrvarm                 !< Maximum of output variables p.p.
        integer(kind = int_wp), intent(out) :: nmis                   !< Number of missing input vars
        character*(*), intent(in) :: dlwnam(nrvarm, noutp)  !< Name of input variables
        integer(kind = int_wp), intent(out) :: iopoin(nrvarm, noutp)   !< Number of missing input vars
        integer(kind = int_wp), intent(in) :: notot                  !< Total number of substances
        integer(kind = int_wp), intent(in) :: nopa                   !< Number of parameters
        integer(kind = int_wp), intent(in) :: nosfun                 !< Number of segment functions
        character(20), intent(in) :: syname(notot)         !< Names of systems
        integer(kind = int_wp), intent(in) :: nocons                 !< Number of constants used
        integer(kind = int_wp), intent(in) :: nofun                  !< Number of functions ( user )
        character(20), intent(in) :: coname(nocons)        !< Constant names
        character(20), intent(in) :: paname(nopa)        !< Parameter names
        character(20), intent(in) :: funame(nofun)        !< Function names
        character(20), intent(in) :: sfname(nosfun)        !< Segment function names
        integer(kind = int_wp), intent(in) :: lurep                  !< Unit nr. report file

        !     Local

        character(20) varnam            ! Name of variable to be identified
        integer(kind = int_wp) :: ivarip             ! Pointer in the SSA
        integer(kind = int_wp) :: iout               ! loop variable of outputs
        integer(kind = int_wp) :: inrv               ! loop variable output number
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("getopo", ithndl)

        write(lurep, *)
        write(lurep, *) ' Determining the place of the output variables'
        write(lurep, *)

        nmis = 0

        do iout = 1, noutp
            do inrv = 1, nrvar(iout)
                varnam = dlwnam(inrv, iout)
                if (varnam == ' ') then
                    ivarip = 0
                else
                    call varpoi (notot, nopa, nosfun, syname, nocons, &
                            nofun, coname, paname, funame, sfname, &
                            varnam, ivarip, lurep)
                    if (ivarip == -1) then
                        nmis = nmis + 1
                        write(lurep, '(3a)') '   INFO:', varnam, &
                                '; NOT FOUND, delwaq will detect variables from process library'
                    endif
                endif
                iopoin(inrv, iout) = ivarip
            end do
        end do

        if (timon) call timstop(ithndl)
        return
    end

end module m_getopo

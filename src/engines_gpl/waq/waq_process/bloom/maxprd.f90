!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_maxprd
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *    SUBROUTINE TO CALCULATE MAXIMAL PRIMARY PRODUCTION AND         *
    !  *                  RESPIRATION RATES                                *
    !  *********************************************************************

    subroutine maxprd(t)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_io
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: t
        integer(kind = int_wp) :: i, k

        !  Calculate respiration rate constants.
        !  Calculate the maximum gross growth rate per day as a linear or
        !  exponential function of the temperature T.
        !  It is assumed that the input function should be incremented with
        !  the respiration rate constant. As an option the mortality rate
        !  constant can be added as well (LPMORT=1).
        !  As another option, Pmax can be set to some small number (BASMOR) if
        !  the temperature is below the value stored in TEMLIM.
        !  If, however, NREP is 1, the temperature limitation option is ignored
        !  because otherwise the model does not have an intial solution for the
        !  first time step.
        if (t >= temlim) go to 20
        if (nrep == 1) go to 20
        do i = 1, nuspec
            resp(i) = res1(i) * res2(i) ** t
            pmax(i) = basmor
        end do
        go to 40
        20 continue
        do i = 1, nuspec
            resp(i) = res1(i) * res2(i) ** t
            if (lpmax(i) == 0) then
                pmax(i) = (pmax1(i) * pmax2(i) ** t) + resp(i) + lpmort * rmort(i)
            else
                if (t <= pmax2(i)) then
                    pmax(i) = 0.01
                else
                    pmax(i) = pmax1(i) * (t - pmax2(i)) + resp(i) + lpmort * rmort(i)
                end if
            end if
        end do
        40 continue

        !  If option "dump" was turned on, print pmax, resp and rmort.
        if (idump /= 0) then
            write (outdbg, 50) (pmax(k), k = 1, nuspec)
            50    format ('  Pmax(T,j): ', 30(F5.2, 1X))
            write (outdbg, 60) (resp(k), k = 1, nuspec)
            60    format ('  Resp(T,j): ', 30(F5.2, 1X))
            write (outdbg, 70) (rmort (k), k = 1, nuspec)
            70    format ('  Rmort(T,j):', 30(F5.2, 1X))
            write (outdbg, 80) (sdmix (k), k = 1, nuspec)
            80    format ('  Sdmix(j):  ', 30(F5.2, 1X))
        end if
        return
    end

end module m_maxprd

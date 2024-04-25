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
module m_natmor
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *     SUBROUTINE TO SET,CALCULATE OR CALIBRATE NATURAL MORTALITY    *
    !  *                       RATE CONSTANT                               *
    !  *********************************************************************

    subroutine natmor(death, temp)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_io
        use bloom_data_phyt

        implicit none

        integer(kind = int_wp) :: i
        real(kind = dp) :: death, temp, temp2, tmpcor
        !
        temp2 = temp
        if (temp < temlim) then
            do i = 1, nuspec
                rmort(i) = basmor
            end do
            death = basmor
        else
            death = 0.0
            do i = 1, nuspec
                if (rmort2(i)>=0) then
                    tmpcor = rmort2(i)
                else
                    tmpcor = 1.0
                endif
                rmort(i) = rmort1(i) * tmpcor ** temp2
                if ((rmort2(i)<0.).and.(temp>-1. * rmort2(i))) then
                    rmort(i) = max(rmort(i), (temp + rmort2(i)) * rmort3(i))
                end if
                if (rmort(i) > death) death = rmort(i)
            end do
        end if
        return
    end

end module m_natmor

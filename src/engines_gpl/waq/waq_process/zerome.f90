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
module m_zerome
    use m_waq_precision

    implicit none

contains


    SUBROUTINE ZEROME (NAME)
        use m_srstop
        use m_monsys

        character(len=*) NAME
        INTEGER(kind = int_wp) :: LUNREP

        CALL GETMLU(LUNREP)
        WRITE (LUNREP, *) ' Coefficient ', NAME, ' = 0'
        WRITE (LUNREP, *) ' Please supply value not equal to zero'
        WRITE (*, *) ' Coefficient ', NAME, ' = 0'
        WRITE (*, *) ' Please supply value not equal to zero'
        CALL SRSTOP(1)
        RETURN
    END

end module m_zerome

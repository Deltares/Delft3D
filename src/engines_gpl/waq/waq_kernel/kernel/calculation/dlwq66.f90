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
module m_dlwq66
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQ66 (AMASS, VOLUME, CONC, NOTOT, NOSEG)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED             : june 1988 by L.Postma
        !
        !     FUNCTION            : makes masses from conc and volumes
        !
        !     LOGICAL UNITNUMBERS : none
        !
        !     SUBROUTINES CALLED  : none
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     AMASS   REAL   NOTOT*NOSEG  OUTPUT  closure error correction
        !     VOLUME  REAL      NOSEG     INPUT   volume
        !     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
        !     NOTOT   INTEGER     1       INPUT   number of systems
        !     NOSEG   INTEGER     1       INPUT   number of segments
        !
        use timers

        real(kind = real_wp) :: AMASS(NOTOT, *), VOLUME(*), CONC(NOTOT, *)
        real(kind = real_wp) :: v1

        integer(kind = int_wp) :: notot, noseg
        integer(kind = int_wp) :: isys, iseg

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq66", ithandl)
        !
        !         loop over the number of segments and systems
        !
        DO ISEG = 1, NOSEG
            V1 = VOLUME(ISEG)
            DO ISYS = 1, NOTOT
                AMASS(ISYS, ISEG) = CONC(ISYS, ISEG) * V1
            end do
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_dlwq66

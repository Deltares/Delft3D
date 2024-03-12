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
module m_dhnolay
    use m_waq_precision

    implicit none

contains


    subroutine dhnolay(pnolay)
        !
        !     Deltares
        !
        !     created             : nov 07 by jan van beek
        !
        !     function            : get nolay from /sysn/ common , system characteristics
        !
        !     logical unitnumbers : -
        !
        !     subroutines called  : -
        !
        !     parameters          : -
        !
        !     name    kind     length     funct.  description
        !     ----    -----    ------     ------- -----------
        !     pnolay  integer       1     output  copy of the nolay from sysn
        !
        !     declarations
        !
        use m_sysn          ! System characteristics
        integer(kind = int_wp) :: pnolay
        !
        !     common  /  sysn   /   system characteristics
        !

        pnolay = nolay

        return
    end
end module m_dhnolay

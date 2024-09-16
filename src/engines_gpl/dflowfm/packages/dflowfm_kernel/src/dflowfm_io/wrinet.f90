!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
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

!
!
module m_wrinet
   implicit none
contains
   subroutine WRINET(MNET)
      use m_netw

      integer :: MNET

      integer :: k
      integer :: l
      integer :: lcdum

      write (mNET, '(A,I12)') 'NR of NETNODES  = ', numk ! nump = ndx
      write (mNET, '(A,I12)') 'NR of NETLINKS  = ', numL ! nump = ndx
      write (MNET, '(A)') 'NODE LIST, X, Y COORDINATES'

      do K = 1, NUMK
         write (MNET, '(3F26.15)') XK(K), YK(K), ZK(K)
      end do

      write (MNET, '(A)') 'LINK LIST, LEFT AND RIGHT NODE NRS'

      LCDUM = 1
      do L = 1, NUML
         write (MNET, '(3I16)') KN(1, L), KN(2, L), KN(3, L)
      end do

      call DOCLOSE(MNET)
      return
   end subroutine WRINET
end module m_wrinet

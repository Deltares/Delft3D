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

module m_typevalue

   implicit none

contains

   subroutine TYPEVALUE(RD, KEY)
      use precision, only: dp
      use M_DEVICES
      implicit none
      real(kind=dp) :: rdin
      real(kind=dp) :: RD
      integer :: KEY
      integer :: infoinput

      RDIN = RD
      call INPOPUP('ON')
      call INHIGHLIGHT('WHITE', 'RED')
      call INDOUBLEXYDEF(IWS / 2 - 10, IHS - 3, 'VALUE : ', 1, RD, 11, '(F11.4)')
      KEY = InfoInput(55)
      if (KEY == 23) then
         RD = RDIN
      end if
      call INPOPUP('OFF')
      return
   end subroutine TYPEVALUE

end module m_typevalue

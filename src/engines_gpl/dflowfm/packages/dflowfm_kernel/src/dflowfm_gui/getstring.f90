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

   !> Get a string
   subroutine getstring(text, string)
      use m_devices
      implicit none
      character(len=*), intent(in) :: text
      character(len=*), intent(out) :: string

      integer :: infoattribute
      integer :: infoinput
      integer :: ixp
      integer :: iyp
      integer :: key
      integer :: nbckgr
      integer :: nforgr
      integer :: nlevel
      integer :: lstring
      character string_tmp * 40
      character wrdkey * 40
      common / helpnow / wrdkey, nlevel

      ixp = iws / 2
      iyp = ihs / 2
      nforgr = InfoAttribute(13)
      nbckgr = InfoAttribute(14)

      call inpopup('on')
20    continue
      call itextcolour('bwhite', 'red')
      call inhighlight('blue', 'bwhite')
      call timlin()
      call InStringXYDef(ixp, iyp, text, 1, string_tmp, lstring)
      call timlin()
      key = InfoInput(55)
      if (key >= 24 .and. key <= 26) then
         nlevel = 3
         wrdkey = text
         call fkeys(key)
         if (key == 3) then
            call inpopup('off')
            call itextcolourn(nforgr, nbckgr)
            return
         end if
         goto 20
      else if (key == 21 .or. key == 22) then
         string = string_tmp(1:lstring)
      else
         string = ''
      end if
      call inpopup('off')
      call itextcolourn(nforgr, nbckgr)
      return
   end subroutine getstring

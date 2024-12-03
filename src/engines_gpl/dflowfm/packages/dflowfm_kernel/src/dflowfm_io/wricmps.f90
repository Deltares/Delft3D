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

module m_wricmps

   implicit none

   private

   public :: wricmps

contains

   !> Writes a set of template component files (_xxxx.cmp) associated with the current polyline.
 !! Should only be called directly after savepol has been called.
 !! If the current polyline was then saved to polname.pli, then
 !! polname_0001.cmp up to polname_xxxn.cmp will be saved (with n the nr 'npl' of polyline points).
   subroutine wricmps(fnam)
      use m_polygon
      use m_missing
      implicit none

      character(len=*), intent(in) :: fnam !< Filename from .pli file (should have been saved just before)
      integer :: mou2, L, n, jacmps = 0
      logical :: jawel
      character(len=len(fnam)) :: fnamc
      character(len=256) :: qid
      L = index(fnam, '.pli')
      fnamc = fnam

      if (L > 0) then

         L = L - 1
         if (jacmps == 1) then

            write (fnamc(l + 1:l + 1), '(a)') '_'
            write (fnamc(l + 2:l + 5), '(i4.4)') n
            inquire (file=fnamc(1:L + 5)//'.cmp', exist=jawel)
            if (.not. jawel) then
               call newfil(mou2, fnamc(1:L + 5)//'.cmp')
               write (mou2, '(a)') '* COLUMNN=3'
               write (mou2, '(a)') '* COLUMN1=Period (min) or Astronomical Componentname'
               write (mou2, '(a)') '* COLUMN2=Amplitude (ISO)'
               write (mou2, '(a)') '* COLUMN3=Phase (deg)'
               write (mou2, '(a)') '0.0  1.0  0.0 '
               call doclose(mou2)
            end if

         else if (index(fnam, 'crit') == 0) then

            do n = 1, 1 ! min(2,npl)
               write (fnamc(l + 1:l + 1), '(a)') '_'
               write (fnamc(l + 2:l + 5), '(i4.4)') n
               inquire (file=fnamc(1:L + 5)//'.tim', exist=jawel)
               if (.not. jawel) then
                  call newfil(mou2, fnamc(1:L + 5)//'.tim')
                  write (mou2, '(a)') '0.0         0.0 '
                  write (mou2, '(a)') '60.0        1.0 '
                  write (mou2, '(a)') '65.0        0.0 '
                  write (mou2, '(a)') '99999999.0  0.0 '
                  call doclose(mou2)
               end if
               if (xpl(n) == dmiss) exit ! only for 1st polygon
            end do

         end if

         inquire (file=fnamc(1:L)//'.ext', exist=jawel)
         if (.not. jawel) then
            if (index(fnam, 'lev') > 0) then
               qid = 'QUANTITY=waterlevelbnd'
            else if (index(fnam, 'dis') > 0) then
               qid = 'QUANTITY=dischargebnd'
            else if (index(fnam, 'crit') > 0) then
               qid = 'QUANTITY=criticaloutflowbnd'
            else if (index(fnam, 'out') > 0) then
               qid = 'QUANTITY=outflowbnd'
            else
               qid = ' '
            end if
            if (qid(1:1) /= ' ') then
               call newfil(mou2, fnamc(1:L)//'.ext')
               write (mou2, '(a)') qid
               write (mou2, '(a)') 'FILENAME='//trim(fnam)
               write (mou2, '(a)') 'FILETYPE=9'
               write (mou2, '(a)') 'METHOD=3'
               write (mou2, '(a)') 'OPERAND=O'
               write (mou2, '(a)') ' '
            end if
         end if

      end if

      if (index(fnam, '.pol') > 0) then

         L = index(fnam, '.pol')
         L = L - 1
         inquire (file=fnamc(1:L)//'.ext', exist=jawel)
         if (.not. jawel) then
            if (index(fnam, 'fric') > 0) then
               qid = 'QUANTITY=frictioncoefficient'
            else if (index(fnam, 'visc') > 0) then
               qid = 'QUANTITY=horizontaleddyviscositycoefficient'
            else if (index(fnam, 'inilev') > 0) then
               qid = 'QUANTITY=initialwaterlevel'
            else
               qid = ' '
            end if
            if (qid(1:1) /= ' ') then
               call newfil(mou2, fnamc(1:L)//'.ext')
               write (mou2, '(a)') trim(qid)
               write (mou2, '(a)') 'FILENAME='//trim(fnam)
               write (mou2, '(a)') 'FILETYPE=10'
               write (mou2, '(a)') 'METHOD=4'
               write (mou2, '(a)') 'OPERAND=O'
               write (mou2, '(a)') 'VALUE=specify here'
               write (mou2, '(a)') ' '
            end if
         end if

      end if

   end subroutine wricmps

end module m_wricmps

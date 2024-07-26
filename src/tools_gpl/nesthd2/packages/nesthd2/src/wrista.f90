subroutine wrista(lun   , mcnes , ncnes , maxbnd)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : wrista
! version            : v1.0
! date               : June 1997
! specs last update  :
! programmer         : Theo van der Kaaij
!
! function           : writes nest stations to file
!
!***********************************************************************

   integer      mcnes(maxbnd*2*4), ncnes (maxbnd*2*4)

   character*20 namsta
!               12345678901234567890
   namsta = '(M,N)=(?????,?????) '
!
!-----first remove double stations and sort
!
   do 10 ista = 2, maxbnd*2*4
      if (mcnes (ista) .ne. 0) then
         do 20 i = 1, ista - 1
            if ((mcnes (ista) .eq. mcnes(i)) .and.&
            &(ncnes (ista) .eq. ncnes(i)) ) then
               mcnes (ista) = 0
               ncnes (ista) = 0
            endif
20       continue
      endif
10 continue

   do 21 ista = 1, maxbnd*2*4 - 1
      do 21 i = ista + 1, maxbnd*2*4
         if (mcnes(i) .lt. mcnes (ista) ) then
            m = mcnes(i)
            n = ncnes(i)
            mcnes(i) = mcnes(ista)
            ncnes(i) = ncnes(ista)
            mcnes(ista) = m
            ncnes(ista) = n
         endif
         if (mcnes(i) .eq. mcnes (ista) ) then
            if (ncnes(i) .lt. ncnes (ista) ) then
               m = mcnes(i)
               n = ncnes(i)
               mcnes(i) = mcnes(ista)
               ncnes(i) = ncnes(ista)
               mcnes(ista) = m
               ncnes(ista) = n
            endif
         endif
21 continue
!
!-----then remove stations already writen to file
!     let op: eng!!!!!
!
   rewind (lun   )

25 read (lun   ,'(21x,i5,1x,i5)',end=40) m,n

   do 30 ista = 1, maxbnd*2*4

      if (mcnes(ista) .eq. m .and. ncnes(ista) .eq. n) then
         mcnes(ista) = 0
         ncnes(ista) = 0
      endif
30 continue

   goto 25

40 continue
!
!-----finally: write resulting stations to file
!
   backspace (lun   )

   do 50 ista = 1, maxbnd*2*4
      if (mcnes (ista) .ne. 0) then
         write (namsta( 8:12),'(a5)') '   '
         write (namsta(14:18),'(a5)') '   '
         write (namsta( 8:12),'(i5)') mcnes (ista)
         write (namsta(14:18),'(i5)') ncnes (ista)

         write (lun   ,'(a20,2(1x,i5))')&
         &namsta, mcnes (ista), ncnes(ista)
      endif
50 continue

   return

end

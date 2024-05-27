subroutine wricon(lundia,lun   ,bndval,itypc ,nobnd ,notims,&
&kmax  ,lstci ,nocon ,tstart,dtmin )
   implicit none
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
! subroutine         : wricon
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Writes transport bc. to TRISULA attribute file
!                      (old format)
! limitations        :
! subroutines called :
!***********************************************************************

   integer lundia, lun, nobnd, notims, kmax, lstci, nocon
   double precision bndval (nobnd ,notims,kmax  ,lstci ,2)
   double precision tstart
   double precision dtmin

   character*19 format
   integer itypc, itim, ibnd, k, icon

   write (*     ,&
   &'('' >>> Writing transport boundary conditions <<<'')')
   write (lundia,&
   &'('' >>> Writing transport boundary conditions <<<'')')

   format = '(a10,   (1x,ES15.7))'
   write (format( 6: 9),'(i3)') 2*kmax
!----------------------------------------------------------------------
!     Uniform and linear boundary conditions
!     (uniform also a linear profile with identical surface and bed
!      values)
!----------------------------------------------------------------------
   if (itypc .eq. 1 .or. itypc .eq. 2) then
      do 10 itim =1, notims
         write (lun,'(f12.3)') tstart + (itim - 1)*dtmin
         do 20 ibnd = 1, nobnd
            do 20 icon = 1, nocon
               write (lun,'(a10,5(1x,f12.3))') 'Linear    ',0.0,&
               &bndval(ibnd,itim,1   ,icon,1),&
               &bndval(ibnd,itim,kmax,icon,1),&
               &bndval(ibnd,itim,1   ,icon,2),&
               &bndval(ibnd,itim,kmax,icon,2)
20       continue
10    continue
!----------------------------------------------------------------------
!     3d-profile data
!----------------------------------------------------------------------
   elseif (itypc .eq. 3) then
      do 30 itim =1, notims
         write (lun,'(f12.3)') tstart + (itim - 1)*dtmin
         do 40 ibnd = 1, nobnd
            do 40 icon = 1, nocon
               write (lun,'(a10,20(1x,f12.3))') '3d-profile',&
               &(bndval(ibnd,itim,k,icon,1),k=1,kmax),&
               &(bndval(ibnd,itim,k,icon,2),k=1,kmax)
40       continue
30    continue

   endif

   return

end

subroutine addinp(a0    , dav   , cgen  , cadd  , cmax  ,&
&cmin  , itypc , typbnd, nobnd , namcon,&
&lstci , kmax                          )
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
!**********************************************************************
! subroutine         : addinp
! version            : v1.3
! date               : October 1997
! programmer         : Theo van der Kaaij
!
! function           : Get additional input for nesting so waiting
!                      is no longer required (specially for HK)
!***********************************************************************

   integer      cgen  (  6   )
   integer itypc
   integer ibnd
   integer kmax
   integer icon
   integer nobnd
   integer lstci

   double precision cadd  (  6   ), cmax  (   6   ), cmin  (  6   )
   double precision a0

   logical      dav   , atlone

   character*1  char
   character*1  typbnd(nobnd )

   character*20 namcon(lstci + 2)

   itypc  = 0
   atlone = .false.
!-----------------------------------------------------------------------
!     A0 correction water levels
!-----------------------------------------------------------------------

   do 10 ibnd = 1, nobnd
      if (typbnd (ibnd) .eq. 'z') then
         write (*,'('' A0 correction water'',&
         &                 '' level boundaries : '',$)')
         read  (*,*) a0
         goto 20
      endif
10 continue

20 continue

!-----------------------------------------------------------------------
!     Depth averaged velocities from 3D computation ????
!-----------------------------------------------------------------------
   if (kmax .eq. 1) goto 40

   do 30 ibnd = 1, nobnd
      if (typbnd (ibnd) .eq. 'c') then
         write (*,'('' Depth averaged velocities (y/n) ? '',$)')
         read (*,'(a1)') char
         call small (char,1)
         if (char .eq. 'y') then
            dav = .true.
         else
            dav = .false.
         endif
         goto 40
      endif
30 continue

40 continue

!-----------------------------------------------------------------------
!     Adjustment transport bc.
!-----------------------------------------------------------------------

   if (lstci .ge. 1) then
      do 50 icon = 1, lstci
         write (*,'(/,'' Generate boundary conditions for '',&
         &                   a20,'' (y/n) ? '',$)') namcon (icon)
         read (*,'(a1)') char
         call small (char,1)
         if (char .eq. 'y') then
            atlone = .true.
            cgen (icon) = 1
            write (*,'(  '' Add to  '',a20,'' : '',$)') namcon(icon)
            read  (*,*) cadd (icon)
            write (*,'(  '' Maximum '',a20,'' : '',$)') namcon(icon)
            read  (*,*) cmax (icon)
            write (*,'(  '' Minimum '',a20,'' : '',$)') namcon(icon)
            read  (*,*) cmin (icon)
         else
            cgen (icon) = 0
         endif
50    continue
   endif

   if (atlone) then
      write (*,'(/,'' Type of profile:'')')
      write (*,'(  ''  1) Uniform (2DH) '')')
      write (*,'(  ''  2) Linear        '')')
      write (*,'(  ''  3) 3D          : '',$)')
      read  (*,*) itypc
   endif

   return

end

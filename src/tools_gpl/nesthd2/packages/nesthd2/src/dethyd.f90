subroutine dethyd(lundia,fout  ,lunadm,bndval,mcbsp ,ncbsp ,&
&typbnd,mnstat,wl    ,uu    ,vv    ,iwet  ,&
&nobnd ,notims,nostat,kmax  ,a0           )
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
! subroutine         : dethyd
! version            : v1.0
! date               : July 1997
! programmer         : Theo van der Kaaij
!
! function           : Determines hydrodynamic boundary conditions
!                      (time-series)
! limitations        :
! subroutines called : getwgh, check
!***********************************************************************

   integer lundia
   integer notims
   integer nostat
   integer iwet  (nostat), mnes  (  4   )  , nnes (  4   )
   integer mnstat(2,nostat),&
   &mcbsp (nobnd ,2), ncbsp(nobnd ,2)

   double precision weight(  4   )

   double precision a0
   double precision wl    (nostat,notims)

   double precision uu    (nostat,kmax  ,notims),&
   &vv    (nostat,kmax  ,notims)

   double precision bndval(nobnd ,notims,kmax  ,1     ,2)

   character*  1 type
   character*  1 typbnd(nobnd )

   double precision angle

   integer ibnd
   integer isize
   integer m
   integer n
   integer lunadm
   integer iwght
   integer istat
   integer itim
   integer k
   integer nobnd
   integer kmax

   logical fout

   write (*     ,'('' >>> Generating hydrodynamic boundary '',&
   &                ''conditions <<<'')')
   write (lundia,'('' >>> Generating hydrodynamic boundary '',&
   &                ''conditions <<<'')')
!
!-----cycle over all boundary support points
!
   do 10 ibnd = 1, nobnd
      do 10 isize = 1, 2
!
!-----------first get nesting stations, weights and orientation
!           of support point
!
         m    = mcbsp(ibnd,isize)
         n    = ncbsp(ibnd,isize)
         type = typbnd(ibnd)

         call getwgh (lundia,fout  ,lunadm,m     ,n     ,type  ,&
         &mnes  ,nnes  ,weight,angle               )
         if (fout) goto 999
!
!-----------Check if available on history file
!           if not: adjust weights
!
         call check  (lundia,fout  ,mnstat,mnes  ,nnes  ,weight ,&
         &m     ,n     ,iwet  ,nostat               )
         if (fout) goto 999
!
!-----------Finally create time series at boundary support points
!
         do 20 iwght = 1, 4
            if (mnes(iwght) .ne. 0) then
               do 30 istat = 1, nostat
                  if (mnstat(1,istat) .eq. mnes(iwght) .and.&
                  &mnstat(2,istat) .eq. nnes(iwght) ) then
                     goto 40
                  endif
30             continue

40             continue
               do 50 itim = 1, notims
!
!--------------------for water level boundaries
!
                  if (type .eq. 'z') then
                     bndval (ibnd,itim,1,1,isize) =&
                     &bndval (ibnd,itim,1,1,isize) +&
                     &weight (iwght) * (wl (istat,itim) +&
                     &a0              )
!
!--------------------or velocity boundaries
!
                  else
                     do 60 k = 1, kmax
                        bndval (ibnd,itim,k,1,isize) =&
                        &bndval (ibnd,itim,k,1,isize)   +&
                        &(uu (istat,k,itim) * sin(angle) -&
                        &vv (istat,k,itim) * cos(angle) )*&
                        &weight (iwght)
60                   continue
                  endif
50             continue
            endif
20       continue
10 continue

999 continue

   return

end

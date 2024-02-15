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

   subroutine wave_makeplotvars
   use m_waves, only: ustokes, vstokes, ust_mag, fwav_mag, ustx_cc, usty_cc
   use m_flowparameters, only: jawave
   use m_flow
   use m_flowgeom

   implicit none

   integer           :: ierror
   integer           :: L, LL, Lb, Lt, k1, k2
   double precision :: ust_mag_u

   ust_mag=0d0
   fwav_mag=0d0
   if (jawave.ne.4) then
      ustx_cc=0d0
      usty_cc=0d0
   endif

   do L = 1, lnx   ! safe for 3D
      k1 = ln(1,L); k2 = ln(2,L)
      call getLbotLtop(L, Lb, Lt)
      do LL = Lb,Lt
         k1=ln(1,LL);k2=ln(2,LL)
         ustx_cc(k1) = ustx_cc(k1)+wcx1(L)*ustokes(LL)
         usty_cc(k1) = usty_cc(k1)+wcy1(L)*ustokes(LL)
         ustx_cc(k2) = ustx_cc(k2)+wcx2(L)*ustokes(LL)
         usty_cc(k2) = usty_cc(k2)+wcy2(L)*ustokes(LL)
      end do

   end do
   ust_mag = hypot(ustx_cc,usty_cc)

   if (jawave==3 .or. jawave==4 .or. jawave==6) then
      do L=1,lnx
         call getLbotLtop(L, Lb, Lt)
         do LL = Lb,Lt
            k1=ln(1,LL);k2=ln(2,LL)
            fwav_mag(k1) = fwav_mag(k1)+wcl(1,L)*hypot(wavfu(LL),wavfv(LL))*rhomean*hu(L)
            fwav_mag(k2) = fwav_mag(k2)+wcl(2,L)*hypot(wavfu(LL),wavfv(LL))*rhomean*hu(L)
         enddo
      enddo
   else
      fwav_mag = 0d0
   end if
   !
   ! bed shear stress
   workx = 0d0
   worky = 0d0
   do L=1,lnx
      k1=ln(1,L)
      k2=ln(2,L)
      if (hu(L) > epshu) then
         workx(k1) = workx(k1) + taubu(L)*wcx1(L)
         workx(k2) = workx(k2) + taubu(L)*wcx2(L)
         worky(k1) = worky(k1) + taubu(L)*wcy1(L)
         worky(k2) = worky(k2) + taubu(L)*wcy2(L)
      end if
   enddo
   taus = hypot(workx(1:ndx),worky(1:ndx))

   ierror = 0
1234 continue
   return

   end subroutine wave_makeplotvars

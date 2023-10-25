!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

   !> subroutine to compute wave forces
   subroutine setwavfu()
   use unstruc_messages
   use MessageHandling
   use m_flowparameters
   use m_flowgeom
   use m_flow, only: hu, huvli, wavfu, wavfv, rhomean,kmx,rho
   use m_waves, m_waves_hminlw=>hminlw
   use m_xbeach_data, xb_hminlw=>hminlw
   use m_physcoef, only: sag
   implicit none

   integer          :: mout
   integer          :: L,LL,Lb,Lt, i
   double precision :: wavfx, wavfy, wavfbx, wavfby
   double precision :: wavfu_loc, wavfbu_loc, twavL, hwavL, wavfuL, wavfvL, cc
   double precision :: wavfv_loc, wavfbv_loc, wavfmag, wavfbmag,wavfang, wavfbang
   double precision :: fmax, ac1, ac2, hminlwi, rhoL, zw, hminlw, gammaloc, halfwav

   integer          :: k1, k2

   if (jawaveforces==0) then
      wavfu = 0d0
      wavfv = 0d0
      return
   endif

   ! Set correct limiting depth
   if (jawave==3) then
      hminlw = m_waves_hminlw
      hminlwi = 1d0/m_waves_hminlw
      gammaloc = gammax
   endif

   if (jawave==4) then
      hminlw = xb_hminlw
      hminlwi = 1d0/xb_hminlw
      gammaloc = gammaxxb
   endif

   facmax     = 0.25d0*sag*rhomean*gammaloc**2

   wavfu = 0d0
   wavfv = 0d0

   if (kmx==0) then
      do i = 1, wetLink2D-1
         wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
         wavfv(L) = wavfv(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
      enddo
      
      do i = wetLink2D, wetLinkCount
         L = onlyWetLinks(i)
         k1 = ln(1,L) ; k2 = ln(2,L)
         ac1 = acl(L)
         ac2 = 1d0-ac1

         wavfx = ac1*sxwav(k1) + ac2*sxwav(k2)
         wavfy = ac1*sywav(k1) + ac2*sywav(k2)

         wavfbx = ac1*sbxwav(k1) + ac2*sbxwav(k2)
         wavfby = ac1*sbywav(k1) + ac2*sbywav(k2)

         twavL = ac1*twav(k1)   + ac2*twav(k2)

         ! limit forces
         fmax       = facmax*hu(L)**1.5 / max(0.1d0, twavL)

         ! projection in face-normal direction
         wavfu_loc  = wavfx*csu(L)  + wavfy*snu(L)
         wavfv_loc  = -wavfx*snu(L)  + wavfy*csu(L)
         wavfbu_loc = wavfbx*csu(L) + wavfby*snu(L)
         wavfbv_loc = -wavfbx*snu(L) + wavfby*csu(L)

         ! Should be done on the vector norm, nt separate comps
         wavfmag = min(hypot(wavfu_loc,wavfv_loc),fmax)
         wavfbmag = min(hypot(wavfbu_loc,wavfbv_loc),fmax)
         wavfang  = atan2(wavfv_loc,wavfu_loc)
         wavfbang  = atan2(wavfbv_loc,wavfbu_loc)    ! necessary?
         wavfu_loc = wavfmag*cos(wavfang)
         wavfv_loc = wavfmag*sin(wavfang)
         wavfbu_loc = wavfbmag*cos(wavfbang)
         wavfbv_loc = wavfbmag*sin(wavfbang)

         wavfu(L) = wavfu_loc + wavfbu_loc
         wavfv(L) = wavfv_loc + wavfbv_loc
         wavfu(L) = wavfu(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
         wavfv(L) = wavfv(L) * min(huvli(L), hminlwi) / rhomean       ! Dimensions [m/s^2]
      enddo
   else  ! kmx>0
      ! A. Conservative radiation stress forcing
      ! Get breaking-induced forcing + conservative part of vertical radiation stress gradient
      

      ! Get bottom frictional forcing


      ! B. 
      
   endif
1234 continue
   return
   end subroutine setwavfu

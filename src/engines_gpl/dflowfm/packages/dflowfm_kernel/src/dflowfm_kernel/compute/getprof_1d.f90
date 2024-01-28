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

! =================================================================================================
! =================================================================================================
subroutine getprof_1D(L, hprL, area, width, japerim, calcConv, perim)
use m_profiles
use m_flow
use m_flowgeom
use m_flowtimes
use m_missing
use unstruc_channel_flow
use m_crosssections
use m_cross_helper
use unstruc_model, only: md_restartfile
use precision_basics

implicit none
integer          :: L, japerim, calcConv
double precision :: hprL                 !< hoogte in profiel
double precision :: area                 !< wet cross sectional area
double precision :: width                !< width at water surface
double precision :: perim                !< wet perimeter

double precision :: profw                !< width  of profile
double precision :: profh                !< height of profile
double precision :: hydrad               !< hydraulic radius

double precision :: area2, width2, perim2, cf2, alfa ! second prof i.c. interpolation
double precision :: area_sbk, width_sbk ! second prof i.c. interpolation
double precision :: perimgr, perimgr2, alfg, czg, hpr

double precision :: frcn, cz, cf, conv, af_sub(3), perim_sub(3), cz_sub(3)
double precision :: q_sub(3)             ! discharge per segment
integer          :: LL, ka, kb, itp, itpa, ifrctyp, ibndsect
integer          :: k1, k2
integer          :: jacustombnd1d
double precision :: u1L, q1L, s1L, dpt, factor, maxflowwidth
type(t_CrossSection), pointer :: cross1, cross2

LL = L
if (L > lnxi) then                      ! for 1D boundary links, refer to attached link
   LL = LBND1D(L)
endif

hpr = hprL

jacustombnd1d = 0
if (kcu(L) == -1 .and. allocated(bndWidth1D)) then
   ibndsect = lnxbnd(L-lnxi)
   if (ibndsect > 0) then
      if (bndWidth1D(ibndsect) /= dmiss) then
         jacustombnd1d = 1
      end if
   end if
end if

if (jacustombnd1d == 1) then ! This link is a 1D bnd *and* has a custom width.
   width = bndwidth1D(ibndsect)
   area = hpr*width
   perim = width+2*hpr

   ! Use the water level at the inner point of the boundary link
   k2 = ln(2,L)

   if ((japerim == 1) .and. (calcConv==1)) then
      hydrad = area / perim
      perim_sub = (/perim, 0d0, 0d0/)
      af_sub = (/area, 0d0, 0d0/)
      !
      ! Calculate the conveyance and Chezy value, using the friction parameters on the internal link, using the
      ! local water depth on this boundary link.
      ! NOTE: In case of a YZ-type cross section the conveyance is computed, using the given water depth, but
      !       using the cross sectional profile of this YZ-cross section. In that case we need the Chezy value
      !       for computing the conveyance based on the rectangular.
      if (network%rgs%timeseries_defined) then
         factor = max(0d0,(time1 - times_update_roughness(1))/(times_update_roughness(2)-times_update_roughness(1)))
      else
         factor = 1d0
      end if
      call getconveyance(network, hpr, u1(L), q1(L), s1(k2), LL, perim_sub, af_sub, conv, cz_sub, cz, area, perim, factor)
      frcu(L)        = cz
      frcu_mor(L)    = cz
      u_to_umain(L)  = 1d0
      q1_main(L)     = q1(L)
      wu(L)          = width

      if (hydrad > 0d0 .and. cz > 0d0) then
         cfuhi(L)       = ag/(hydrad*cz*cz)
      else
         cfuhi(L) = 0d0
      end if
   end if

   return

else if (abs(kcu(ll))==1 .and. network%loaded) then !flow1d used only for 1d channels and not for 1d2d roofs and gullies
   cz = 0d0

   if (japerim == 0) then ! calculate total area and volume
      call GetCSParsTotal(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, width, CSCalculationOption, network%adm%hysteresis_for_summerdike(:,LL))
   else ! japerim = 1: calculate flow area, conveyance and perimeter.
      cz = 0d0
      call GetCSParsFlow(network%adm%line2cross(LL, 2), network%crs%cross, hpr, area, perim, width, maxflowwidth = maxflowwidth, af_sub = af_sub, perim_sub = perim_sub)

      if (calcConv ==1) then
         u1L = u1(LL)
         q1L = q1(LL)
         k1 = ln(1,LL)
         k2 = ln(2,LL)
         s1L = acl(L)*s1(k1) + (1d0-acl(L))*s1(k2)
         dpt = hu(L)
         cz = 0d0
         if (network%rgs%timeseries_defined) then
            factor = max(0d0,(time1 - times_update_roughness(1))/(times_update_roughness(2)-times_update_roughness(1)))
         else
            factor = 1d0
         end if
         call getconveyance(network, dpt, u1L, q1L, s1L, LL, perim_sub, af_sub, conv, cz_sub, cz, area, perim, factor)

         ! For sediment transport the discharge in the main channel is required:
         ! Qmain/ QT = Kmain/KT -> u_main = Kmain/KT * (AT/Amain)
         if (conv > 0d0) then
            u_to_umain(L)  = area*cz_sub(1) * sqrt(af_sub(1)/perim_sub(1)) /  conv
            cfuhi(L)       = ag/(conv/area)**2
            frcu(L)        = cz
            frcu_mor(L)    = cz_sub(1)
            call getCrossDischarge(perim_sub, af_sub, cz_sub, q1(L), q_sub)
            q1_main(L) = q_sub(1)
         else
            u_to_umain(L) = 1d0
            cfuhi(L) = 0d0
         endif
      endif

      wu(L) = max(0.01d0, maxflowwidth)

   endif
   ! finished for 1d network from flow1d
   return
endif


! No flow1d cross input, OR a 1d2d link. Proceed with conventional prof1D approach.
if (prof1D(1,LL) >= 0 ) then            ! direct profile based upon link value
    ka    = 0; kb = 0                   ! do not use profiles
    profw = prof1D(1,LL)
    profh = prof1D(2,LL)
    itp   = prof1D(3,LL)
    if (japerim == 1) then
       frcn    = frcu(LL)
       ifrctyp = ifrcutp(LL)
    endif
else
    ka    = -prof1D(1,LL); kb = -prof1D(2,LL)
    profw = profiles1D(ka)%width
    profh = profiles1D(ka)%height
    itp   = profiles1D(ka)%ityp
    alfa  = prof1d(3,LL)
    itpa  = itp

    if (japerim == 1) then
       if (profiles1D(ka)%frccf .ne. dmiss .and. profiles1D(kb)%frccf .ne. dmiss .and.  &
           profiles1D(ka)%frctp == profiles1D(kb)%frctp) then
           frcn    =  (1d0-alfa)*profiles1D(ka)%frccf  + alfa*profiles1D(kb)%frccf
           ifrctyp = profiles1D(ka)%frctp
       else
           frcn    = frcu(LL)
           ifrctyp = ifrcutp(LL)
       endif
    endif

endif

if (jagrounlay > 0) then
   hpr = hpr + grounlay(LL)
endif

! base nrs == open, negative = closed
if (abs(itp) == 1) then   ! pipe
    call pipe(hpr, profw, area, width, japerim, perim)
else if (abs(itp) == 2) then   ! rectan, peri=wu + 2*hpr
    call rectan  (hpr, profw, profh, area, width, japerim, perim, itp < 0)
else if (abs(itp) == 3) then  ! rectan, peri=wu
    call rectan2D(hpr, profw, profh, area, width, japerim, perim)
else if (abs(itp) == 100 .or. abs(itp) == 101) then  !                          itp >= 100, yzprof
   call yzprofile(hpr,ka,itp, area, width, japerim, frcn, ifrctyp, perim, cf )
endif


if (ka .ne. 0 .and. kb .ne. ka) then     ! interpolate in profiles
    profw = profiles1D(kb)%width
    profh = profiles1D(kb)%height
    itp   = profiles1D(kb)%ityp

   if (abs(itpa)<100 .and. abs(itp)>99) then
      ! doe hier backup cf
      if (frcn > 0) then
            hydrad  = area / perim                   ! hydraulic radius
            call getcz(hydrad, frcn, ifrctyp, cz,L)
            cf = ag/(hydrad*cz*cz)             ! see note on 2D conveyance in sysdoc5
         else
            cf = 0d0
         endif
   endif

    if (abs(itp) == 1) then                   ! pipe
       call pipe(hpr, profw, area2, width2, japerim, perim2)
    else if (abs(itp) == 2) then              ! rectan, peri=wu + 2*hpr
       call rectan  (hpr, profw, profh, area2, width2, japerim, perim2, itp < 0)
    else if (abs(itp) == 3) then              ! rectan, peri=wu
       call rectan2D(hpr, profw, profh, area2, width2, japerim, perim2)
    else if (abs(itp) == 100 .or. abs(itp) == 101) then ! >= 10, conveyance approach
      call yzprofile(hpr, kb,itp,area2, width2, japerim, frcn, ifrctyp, perim2, cf2 )
    endif
    area  = (1d0-alfa)*area  + alfa*area2
    width = (1d0-alfa)*width + alfa*width2

    if (japerim == 1) then
       if  (abs(itp) == 101) then                 ! 1D conveyance
          cf     = (1d0-alfa)*cf     + alfa*cf2
       else
          perim  = (1d0-alfa)*perim  + alfa*perim2
       endif
    endif
endif


if (jagrounlay > 0) then
   if (grounlay(LL) > 0) then
       area = area - argr(LL)
       if (japerim == 1) then
          perim = perim - pergr(LL) + wigr(LL)
       endif
   endif
endif

if (japerim == 1 .and. calcconv==1) then

   if (abs(itp) == 101) then                      ! 1D conveyance

      cfuhi(L) = cf

   else

      if (frcn > 0) then
         hydrad  = area / perim                   ! hydraulic radius
         call getcz(hydrad, frcn, ifrctyp, cz,L)
         cfuhi(L) = ag/(hydrad*cz*cz)             ! see note on 2D conveyance in sysdoc5
      else
         cfuhi(L) = 0d0
      endif

      if (jagrounlay > 0) then
         if (grounlay(LL) > 0) then
            call getcz(hydrad, frcuni1Dgrounlay, ifrctyp, czg, L)
            alfg     = wigr(LL)/perim
            cfuhi(L) = (ag/hydrad)*(alfg/czg**2 + (1d0-alfg)/cz**2)
         endif
      endif

   endif

endif

end subroutine getprof_1D

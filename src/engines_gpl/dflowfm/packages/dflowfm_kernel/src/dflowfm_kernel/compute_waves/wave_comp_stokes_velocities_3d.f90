!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2022.                                
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

! $Id: getustwav.f90 142075 2022-11-24 08:58:19Z reyns $
! $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20221101_SO_3dwaves/src/engines_gpl/dflowfm/packages/dflowfm_kernel/src/dflowfm_kernel/compute/getustwav.f90 $

subroutine wave_comp_stokes_velocities_3d()
   use m_flow
   use m_flowgeom
   use m_waves
   use m_sferic
   use m_physcoef
   use m_xbeach_data, only: R, cwav, gammaxxb,roller

   implicit none

   double precision, external      :: sinhsafei
   integer                         :: k1, k2 , LL, Lb, Lt, L,Lmin
   double precision                :: Tsig, Hrms, asg, rk, shs, phi1, phi2,  omeg, f1u, f2u, f3u,  sintu, costu
   double precision                :: p1, p2, h, z, uusto
   double precision                :: rolthk,rmax,erol,crol,mass, huL, csw, snw

   do LL = 1, lnx
      call getLbotLtop(LL,Lb,Lt)
      k1   = ln(1,LL) ; k2 = ln(2,LL)
      tsig = 0.5d0*( twav(k1)   + twav(k2) )
      if (tsig > 0.05d0) then
         omeg  = twopi/tsig
      else
         if (jawaveStokes > 0) then
            ustokes(Lb:Lt) = 0d0 ; vstokes(Lb:Lt) = 0d0
         endif
         return
      endif
      
      phi1   = phiwav(k1) ; phi2  = phiwav(k2)
      csw = 0.5d0*(cos(phi1*dg2rd)+cos(phi2*dg2rd))
      snw = 0.5d0*(sin(phi1*dg2rd)+sin(phi2*dg2rd))
      
      huL    = hu(LL) 
      rk     = kwL(L)
      hrms   = 0.5d0*(hwav(k1) + hwav(k2))
      hrms   = min(hrms,gammax*huL)
      asg    = 0.5d0*hrms                              ! Wave amplitude = 0.5*Hrms
      shs    = sinhsafei(rk*huL)
      costu  =  csw*csu(LL) + snw*snu(LL)              ! and compute stokes drift
      sintu  = -csw*snu(LL) + snw*csu(LL)
      
      if (jawaveStokes == 1) then
         uusto          =  0.5d0*omeg*asg*asg/huL
         ustokes(Lb:Lt) =  costu*uusto
         vstokes(Lb:Lt) =  sintu*uusto
         ustokes(LL)    =  costu*uusto      ! for convenience
         vstokes(LL)    =  sintu*uusto
      else if (jawaveStokes >= 2) then      ! timing in Matlab: using linear theory is 10% faster
         f1u    = omeg*rk*asg**2
         f3u    = (1d0 - exp(-2d0*rk*huL ) )**2
      
         do L   = Lb, Lt
            z   = 0.5d0*( hu(L) + hu(L-1) )         ! here, z is vertical coordinate upward, bed = 0, (not z = 0 at average wl)
            p1  = max(-25d0,  2d0*rk*(z - huL))
            p2  = max(-25d0, -4d0*rk*(z    ))       ! maximisation not necessary
            f2u = exp(p1) * ( 1d0 + exp(p2) )
            uusto      = f1u   * f2u / f3u
            ustokes(L) = costu * uusto
            vstokes(L) = sintu * uusto
         enddo
         ! depth averaged
         ustokes(LL) = costu*ag*asg*asg*rk/omeg/2d0/huL    ! these are needed, also for 3D models (see u bnd furu)
         vstokes(LL) = sintu*ag*asg*asg*rk/omeg/2d0/huL
      
         ! add 3D roller contribution to stokes drift
         if (alfa_roller > 0d0) then
            if (jawave==4 .and. roller==1) then
               ! roller mass flux from surfbeat
               rmax = 0.125d0*rhomean*ag*(gammaxxb*huL)**2
               erol = min(0.5d0*(R(k1)+R(k2)),rmax)
               crol = max(0.5d0*(cwav(k1)+cwav(k2)),1d-1)
            else
               rmax = 0.125d0*rhomean*ag*(gammax*huL)**2
               crol = rk/omeg
               erol = 0.5d0*rhomean*crol*.9d0*hrms*hrms/tsig   ! Svendsen 1984, Guerin 2018
            endif
            mass = 2d0*erol/crol/rhomean
            !
            if (Lt>Lb) then
               !
               ! determine roller thickness
               lmin=Lt
               rolthk = 0d0
               do L=Lt-1,Lb,-1
                  lmin=L
                  rolthk = hu(Lt)-hu(L)
                  if (rolthk>=0.5d0*hrms) exit
               enddo
               !
               ! depth dependent contribution
               ustokes(Lmin:Lt) = ustokes(Lmin:Lt) + mass/rolthk*costu
               vstokes(Lmin:Lt) = vstokes(Lmin:Lt) + mass/rolthk*sintu
            endif
            !
            ! depth averaged contribution
            ustokes(LL) = ustokes(LL) + mass/h*costu
            vstokes(LL) = ustokes(LL) + mass/h*sintu
         endif
      
      endif
   end do

end subroutine wave_comp_stokes_velocities_3d

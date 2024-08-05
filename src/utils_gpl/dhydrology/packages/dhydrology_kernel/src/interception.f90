!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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
!-------------------------------------------------------------------------------
module interception
   implicit none
   contains
   
   function rainfall_interception_modrut(Precipitation, PotEvap, CanopyStorage, CanopyGapFraction, Cmax, n, NetInterception,&
                                         ThroughFall, StemFlow,LeftOver, Interception) result(ierr)
      use dhydrology_error
      
      integer,          intent(in  )   :: n
      double precision, intent(in  )   :: Precipitation(n), PotEvap(n), CanopyGapFraction(n), Cmax(n) 
      double precision, intent(inout)  :: CanopyStorage(n)
      double precision, intent(  out)  :: NetInterception(n), ThroughFall(n), StemFlow(n), LeftOver(n), Interception(n)
   
      ! local
      double precision                 :: pt(n), Pfrac(n), DD(n), dC(n), D(n) 
      integer                          :: ierr              !< Result status, DHYD_NOERR if successful.

      ierr = DHYD_NOERR
   
      !f2py intent(in) :: n, Precipitation, PotEvap, CanopyGapFraction, Cmax
      !f2py intent(out) :: NetInterception, ThroughFall, StemFlow,LeftOver, Interception
      !f2py intent(in,out) :: CanopyStorage
   
      pt = 0.1 * CanopyGapFraction
      ! Amount of P that falls on the canopy
      Pfrac = (1 - CanopyGapFraction - pt * Precipitation)
      ! S cannot be larger than Cmax, no gravity drainage below that
      where(CanopyStorage > Cmax)
         DD = CanopyStorage - Cmax
      elsewhere
         DD = 0.
      end where
       
      CanopyStorage = CanopyStorage - DD

      ! Add the precipitation that falls on the canopy to the store
      CanopyStorage = CanopyStorage + Pfrac

      ! Now do the Evap, make sure the store does not get negative
      where(CanopyStorage > PotEvap)
         dC = -1. * PotEvap
      elsewhere
         dC = -1. * CanopyStorage
      end where
       
      CanopyStorage = CanopyStorage + dC

      LeftOver = PotEvap + dC ! Amount of evap not used

      ! Now drain the canopy storage again if needed...
      where(CanopyStorage > Cmax)
         D = CanopyStorage - Cmax
      elsewhere
         D = 0.0
      end where
           
      CanopyStorage = CanopyStorage - D

      ! Calculate throughfall
      ThroughFall = DD + D + CanopyGapFraction * Precipitation
      StemFlow = Precipitation * pt

      ! Calculate interception, this is NET Interception
      NetInterception = Precipitation - ThroughFall - StemFlow
      Interception = -dC
   
   
   end function rainfall_interception_modrut


   function rainfall_interception_gash(Cmax, EoverR, CanopyGapFraction, Precipitation, CanopyStorage, maxevap, n, &
                                         ThroughFall, Interception, StemFlow) result(ierr)
      
      use dhydrology_error
      
      integer,                 intent(in   ) :: n
      double precision,        intent(in   ) :: Cmax(n), EoverR(n), CanopyGapFraction(n), Precipitation(n), maxevap(n) 
      double precision,        intent(inout) :: CanopyStorage(n)
      double precision,        intent(  out) :: ThroughFall(n), Interception(n), StemFlow(n)
      integer                                :: ierr
      
      ! local
      double precision                       :: pt(n), P_sat(n), Iwet(n), Isat(n), Idry(n), Itrunc(n), OverEstimate(n)
   
      !f2py intent(in) :: n, Cmax, EoverR, CanopyGapFraction, Precipitation,maxevap
      !f2py intent(out) :: ThroughFall, Interception, StemFlow
      !f2py intent(in,out) :: CanopyStorage
      
      ierr = DHYD_NOERR
      pt = 0.1 * CanopyGapFraction
      P_sat = MAX(0.0d0,(-Cmax / EoverR) * LOG (1.0 - (EoverR/(1.0 - CanopyGapFraction - pt))))

      where(Precipitation > P_sat)
         Iwet = ((1 - CanopyGapFraction - pt) * P_sat) - Cmax
         Isat = EoverR * (Precipitation - P_sat)
         Idry = Cmax
      elsewhere
         Iwet = Precipitation * (1 - CanopyGapFraction - pt)
         Isat = 0.0
         IDry = 0.0
      end where

      Itrunc = 0

      StemFlow = pt * Precipitation
   
      ThroughFall = Precipitation- Iwet - Idry- Isat - Itrunc - StemFlow
      Interception = Iwet + Idry + Isat + Itrunc
   
      ! Now corect for area without any Interception (say open water Cmax -- zero)
      where(Cmax <= 0.0)
         ThroughFall = Precipitation
         Interception = 0.0
         StemFlow = 0.0
      end where

      ! Now correct for maximum potential evap
      where(Interception > maxevap)
         OverEstimate = Interception - maxevap
      elsewhere
         OverEstimate = 0.0
      end where

      Interception = MIN(Interception, maxevap)

      ! Add surpluss to the thoughdfall
      ThroughFall = ThroughFall + OverEstimate

   end function rainfall_interception_gash
end module interception
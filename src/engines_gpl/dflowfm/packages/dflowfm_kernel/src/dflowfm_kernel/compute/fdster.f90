!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_fdster

   implicit none

contains

   subroutine fdster(dster, taucr, thetcr, pclay, g, d50, rhos, rhow, FCR)
      use precision, only: dp
      implicit none
      real(kind=dp) :: dster, taucr, thetcr, pclay, g, d50, rhos, rhow, FCR
      real(kind=dp) :: dsand, dsilt, cmaxs, fch1, cmax, fpack, fclay
      if (DSTER <= 1.0_dp) then
         THETCR = 0.24_dp ! this line added by hk and svdp: critical shields parameter
      end if
      if (1.0_dp < DSTER .and. DSTER <= 4.0_dp) then
         THETCR = 0.115_dp / (DSTER)**0.5_dp
      end if
      if (4.0_dp < DSTER .and. DSTER <= 10.0_dp) then
         THETCR = 0.14_dp * DSTER**(-0.64_dp)
      end if
      if (10.0_dp < DSTER .and. DSTER <= 20.0_dp) then
         THETCR = 0.04_dp * DSTER**(-0.1_dp)
      end if
      if (20.0_dp < DSTER .and. DSTER <= 150.0_dp) then
         THETCR = 0.013_dp * DSTER**(0.29_dp)
      end if
      if (DSTER > 150.0_dp) then
         THETCR = 0.055_dp
      end if
!Soulsby gives one single formula
!THETCR=(0.24/DSTER)+0.055*(1.0-exp(-0.02*DSTER))
      dsand = 0.000062
      dsilt = 0.000032
      cmaxs = 0.65
      fch1 = (dsand / d50)**1.5
      cmax = (d50 / dsand) * cmaxs
!cmaxs=maximum bed concentration in case of sandy bottom (=0.65)
      if (cmax < 0.05) then
         cmax = 0.05
      end if
      if (cmax > cmaxs) then
         cmax = cmaxs
      end if
      fpack = cmax / cmaxs
      if (fch1 < 1.) then
         fch1 = 1.
      end if
      if (fpack > 1.) then
         fpack = 1.
      end if
      fclay = 1.
      if (pclay >= 0.) then
         fclay = (1.+Pclay)**3.
      end if
!  if(pclay.ge.0..and.d50.ge.dsand)fclay=(1.+Pclay)**3.
      if (fclay >= 2.) then
         fclay = 2.
      end if

      thetcr = FCR * fpack * fch1 * fclay * THETCR

      TAUCR = (RHOS - RHOW) * G * D50 * THETCR
   end

end module m_fdster

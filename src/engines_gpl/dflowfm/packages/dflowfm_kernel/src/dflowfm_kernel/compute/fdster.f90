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

 subroutine fdster(dster, taucr, thetcr, pclay, g, d50, rhos, rhow, FCR)
    implicit none
    double precision :: dster, taucr, thetcr, pclay, g, d50, rhos, rhow, FCR
    double precision :: dsand, dsilt, cmaxs, fch1, cmax, fpack, fclay
    if (DSTER <= 1.) THETCR = .24 ! this line added by hk and svdp: critical shields parameter
    if (DSTER <= 4.) THETCR = 0.115 / (DSTER)**0.5
    if (4. < DSTER .and. DSTER <= 10.) THETCR = .14 * DSTER**(-.64)
    if (10. < DSTER .and. DSTER <= 20.) THETCR = .04 * DSTER**(-.1)
    if (20. < DSTER .and. DSTER <= 150.) THETCR = .013 * DSTER**(.29)
    if (DSTER > 150.) THETCR = .055
!Soulsby gives one single formula
!THETCR=(0.24/DSTER)+0.055*(1.0-exp(-0.02*DSTER))
    dsand = 0.000062
    dsilt = 0.000032
    cmaxs = 0.65
    fch1 = (dsand / d50)**1.5
    cmax = (d50 / dsand) * cmaxs
!cmaxs=maximum bed concentration in case of sandy bottom (=0.65)
    if (cmax < 0.05) cmax = 0.05
    if (cmax > cmaxs) cmax = cmaxs
    fpack = cmax / cmaxs
    if (fch1 < 1.) fch1 = 1.
    if (fpack > 1.) fpack = 1.
    fclay = 1.
    if (pclay >= 0.) fclay = (1.+Pclay)**3.
!  if(pclay.ge.0..and.d50.ge.dsand)fclay=(1.+Pclay)**3.
    if (fclay >= 2.) fclay = 2.

    thetcr = FCR * fpack * fch1 * fclay * THETCR

    TAUCR = (RHOS - RHOW) * G * D50 * THETCR
 end

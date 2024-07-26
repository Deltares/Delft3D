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

module m_vegetation
   integer :: javeg = 0 ! 0,1,2,3 , jabaptist is javeg
   ! only for kmx == 0:
   integer :: jabaptist = 0 ! 1 = use standard baptist, only cfuhi      unfortunately, taubed/ro is not computed correctly
   ! 2 = use DFM formulation for cfuhi and alfaveg, such that taubed/ro=cfu*umod**2
   ! 3 = use cfuveg and alfaveg provided by python, such that taubed/ro=cfu*umod**2
   double precision :: densvegminbap = 0d0 ! minimum vegetation density for baptist formulation (1/m2)
   integer :: jaCdvegsp = 0 ! 1 = use bmi for Cdvegsp
   double precision, allocatable, target :: rnveg(:) !< [1/m2] 3D plant density , 2D part is basis input (1/m2) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: diaveg(:) !< [m] 3D plant diameter, 2D part is basis input (m) {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable, target :: cfuveg(:) !< [ ]   2D only, g/C2 in 2D such that bedstress is computed correctly {"location": "face", "shape": ["lnx"]}
   double precision, allocatable, target :: alfaveg(:) !< [1/m] 2D only, stem contribution {"location": "face", "shape": ["lnx"]}
   double precision, allocatable, target :: stemdens(:) !< [1/m2] TEMP 2D plant density (1/m2) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: stemdiam(:) !< [m] TEMP 2D plant diameters (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: stemheight(:) !< [m] 2D plant heights (m) {"location": "face", "shape": ["ndx"]}
   double precision, allocatable, target :: Cdvegsp(:) !< [m] spatial plant Cdveg () {"location": "face", "shape": ["ndkx"]}
   double precision, allocatable :: alfav(:) !< [1/m] used in DFM, computed onboard for jabaptist==2, or pyton if jabaptist==3
   double precision, allocatable :: phiv(:) ! 2D plant stem angle ()
   double precision, allocatable :: phivt(:) ! 2D plant angle velocity (1/s)
   double precision :: Clveg = 0.8d0 ! factor on average stem distance ( ) (eps. eq.)
   double precision :: Cdveg = 0.7d0 ! Cd drag coefficient  ( )
   double precision :: Cbveg = 0.0d0 ! Bend stiffness coefficient (kg.m2/s2) Moment=Cbveg.phiv
   double precision :: Rhoveg = 0d0 ! if > 0d0 then floatmodel
   double precision :: Stemheightstd = 0d0 ! stemheight standard deviation
   double precision :: r3 = .333333d0 !
   double precision :: growthunidicouv ! uniform values in veg growth model diffusion coef
   double precision :: growthunidiam ! uniform values in veg growth model diam
   double precision :: growthuniheight ! uniform values in veg growth model height

   double precision :: expchistem = 0d0
   double precision :: uchistem = 0d0
   double precision :: expchileaf = 0d0
   double precision :: uchileaf = 0d0
   double precision :: arealeaf = 0d0
   double precision :: Cdleaf = 1d0

end module m_vegetation

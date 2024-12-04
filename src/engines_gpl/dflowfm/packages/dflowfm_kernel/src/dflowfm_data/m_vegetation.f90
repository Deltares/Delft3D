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
   use precision, only: dp

   integer :: javeg = 0 ! 0,1,2,3 , jabaptist is javeg
   ! only for kmx == 0:
   integer :: jabaptist = 0 ! 1 = use standard baptist, only cfuhi      unfortunately, taubed/ro is not computed correctly
   ! 2 = use DFM formulation for cfuhi and alfaveg, such that taubed/ro=cfu*umod**2
   ! 3 = use cfuveg and alfaveg provided by python, such that taubed/ro=cfu*umod**2
   real(kind=dp) :: densvegminbap = 0d0 ! minimum vegetation density for baptist formulation (1/m2)
   integer :: jaCdvegsp = 0 ! 1 = use bmi for Cdvegsp
   real(kind=dp), allocatable, target :: rnveg(:) !< [1/m2] 3D plant density , 2D part is basis input (1/m2) {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target :: diaveg(:) !< [m] 3D plant diameter, 2D part is basis input (m) {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable, target :: cfuveg(:) !< [ ]   2D only, g/C2 in 2D such that bedstress is computed correctly {"location": "face", "shape": ["lnx"]}
   real(kind=dp), allocatable, target :: alfaveg(:) !< [1/m] 2D only, stem contribution {"location": "face", "shape": ["lnx"]}
   real(kind=dp), allocatable, target :: stemdens(:) !< [1/m2] TEMP 2D plant density (1/m2) {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: stemdiam(:) !< [m] TEMP 2D plant diameters (m) {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: stemheight(:) !< [m] 2D plant heights (m) {"location": "face", "shape": ["ndx"]}
   real(kind=dp), allocatable, target :: Cdvegsp(:) !< [m] spatial plant Cdveg () {"location": "face", "shape": ["ndkx"]}
   real(kind=dp), allocatable :: alfav(:) !< [1/m] used in DFM, computed onboard for jabaptist==2, or pyton if jabaptist==3
   real(kind=dp), allocatable :: phiv(:) ! 2D plant stem angle ()
   real(kind=dp), allocatable :: phivt(:) ! 2D plant angle velocity (1/s)
   real(kind=dp) :: Clveg = 0.8d0 ! factor on average stem distance ( ) (eps. eq.)
   real(kind=dp) :: Cdveg = 0.7d0 ! Cd drag coefficient  ( )
   real(kind=dp) :: Cbveg = 0.0d0 ! Bend stiffness coefficient (kg.m2/s2) Moment=Cbveg.phiv
   real(kind=dp) :: Rhoveg = 0d0 ! if > 0d0 then floatmodel
   real(kind=dp) :: Stemheightstd = 0d0 ! stemheight standard deviation
   character(len=128) :: stemheightConvention = 'upward_from_bed' ! User/MDU-setting. Stem height convention: 'upward_from_bed' or 'downward_from_surface'
   integer, parameter :: UPWARD_FROM_BED = 1
   integer, parameter :: DOWNWARD_FROM_SURFACE = 2
   integer :: stemheight_convention = UPWARD_FROM_BED
   real(kind=dp) :: r3 = .333333d0 !
   real(kind=dp) :: growthunidicouv ! uniform values in veg growth model diffusion coef
   real(kind=dp) :: growthunidiam ! uniform values in veg growth model diam
   real(kind=dp) :: growthuniheight ! uniform values in veg growth model height

   real(kind=dp) :: expchistem = 0d0
   real(kind=dp) :: uchistem = 0d0
   real(kind=dp) :: expchileaf = 0d0
   real(kind=dp) :: uchileaf = 0d0
   real(kind=dp) :: arealeaf = 0d0
   real(kind=dp) :: Cdleaf = 1d0

end module m_vegetation

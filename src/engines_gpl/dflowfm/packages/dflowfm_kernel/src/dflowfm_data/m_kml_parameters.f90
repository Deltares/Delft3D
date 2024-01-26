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

module m_kml_parameters
implicit none

    integer          :: kml_janet      !< Whether or not (1/0) to export flat view of 2D+1D grid (faster)
    integer          :: kml_jadepth    !< Whether or not (1/0) to export bathymetry view of grid cells (nicer).
    integer          :: kml_jadepth3d  !< Whether or not (1/0) to export bathymetry view in 3D.
    double precision :: kml_altfact    !< Altitude exaggeration factor: altitude differences are multiplied by this.
    integer          :: kml_jaoffsetzk !< Whether or not (1/0) to offset all altitudes with deepest zk-value.
    double precision :: kml_useroffset !< Additional user offset for altitude values.
    double precision :: kml_dmiss      !< Dummy altitude to replace missing zk values.
    double precision :: kml_zmin, kml_zmax !< Min/max values used for color scaling.

contains

!> This subroutine should be called during program initialization.
subroutine default_kml_parameters()
    kml_janet       = 1    !< Whether or not (1/0) to export flat view of 2D+1D grid (faster)
    kml_jadepth     = 0    !< Whether or not (1/0) to export bathymetry view of grid cells (nicer).
    kml_jadepth3d   = 0    !< Whether or not (1/0) to export bathymetry view in 3D.
    kml_altfact     = 5    !< Altitude exaggeration factor: altitude differences are multiplied by this.
    kml_jaoffsetzk  = 1    !< Whether or not (1/0) to offset all altitudes with deepest zk-value.
    kml_useroffset  = 0d0  !< Additional user offset for altitude values.
    kml_dmiss       = 99d0 !< Dummy altitude to replace missing zk values.
end subroutine default_kml_parameters

end module m_kml_parameters

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

module unstruc_channel_flow
use m_network
implicit none
type(t_network), target              :: network
integer                       :: CSCalculationOption  !< Calculation option for total area computation in 1d

logical,                public :: useVolumeTables                    !< Indicates whether 1d volume tables are useds
double precision,       public :: tableIncrement                     !< Increment for volume tables
logical,                public :: useVolumeTableFile                 !< Write the volume tables to file (or not)
character(len=IdLen),   public :: volumeTableFile                    !< Name of the table input file
contains


!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_*() instead.
subroutine default_channel_flow()
    CSCalculationOption = CS_TYPE_PREISMAN     !< calculation option for total area computation in 1d
    useVolumeTables     = .false.
    useVolumeTableFile  = .false.
    tableIncrement      = 0.1d0
!call dealloc(network)
end subroutine default_channel_flow

end module unstruc_channel_flow

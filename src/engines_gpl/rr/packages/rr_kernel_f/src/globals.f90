!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

 module globals

   logical                             :: dll_mode  = .false.  ! Can be set back to false if old boundaries are used.
   logical                             :: dimr_mode = .false.  ! Will be set to true if running under DIMR
   
   double precision                    :: julStart
   
   integer                             :: maxFileUnitNumber = -214748300
   integer                             :: minFileUnitNumber = 214748300
   
   logical, public                     :: in_f90_runner = .false.   !< Set to true when running as rr.exe or from flow1d_runner
                                                                    !  (f90, can not handle exceptions)
   
   logical                             :: isMessLevelSet = .false.
   integer                             :: messLevelSet   = 0
   
   character(len=256)                  :: rr_version_string

end module globals

!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
module gdp
   ! adri.mourits@deltares.nl
   use precision
   use m_ec_module
   !
   implicit none
   !
   ! Grid
   !
   integer                          , save :: kmax
   integer                          , save :: mmax
   integer                          , save :: nmax
   integer , dimension(:),   pointer, save :: kcs
   real(hp), dimension(:),   pointer, save :: x
   real(hp), dimension(:),   pointer, save :: y
   real(fp), dimension(:),   pointer, save :: uwind
   real(fp), dimension(:),   pointer, save :: vwind
   real(fp), dimension(:),   pointer, save :: patm
   real(fp), dimension(:,:), pointer, save :: uwind2d
   real(fp), dimension(:,:), pointer, save :: vwind2d
   real(fp), dimension(:,:), pointer, save :: patm2d
   logical                          , save :: sferic
   !
   ! Time
   !
   real(hp) :: reference_time !< reference_date as a Julian Day Number
   real(hp), save :: dt     !< time loop step size, as a Julian Day Number
   real(hp), save :: curtim !< current time, as a Julian Day Number
   real(hp) :: tstop  !< Julian Day Number of model stop
   !
   ! EC stuff
   !
   type(tEcInstance), pointer, save :: ecInstancePtr !< The one and only EC access handle ensuring thread safety
   logical        , save :: ECPrivate     ! FALSE: Use the (only) EC-module, usable by all kernels
end module gdp


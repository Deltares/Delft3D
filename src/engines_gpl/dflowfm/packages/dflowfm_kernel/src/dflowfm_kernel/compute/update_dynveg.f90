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
   
subroutine update_dynveg()
   use m_physcoef
   use m_sediment
   use m_flow
   
   implicit none
   
   if (dynroughveg > 0)  then
      where ((dynveg) .and. (cumes .gt. 0d0))                 ! linear function do to deposition ( sedero > 0 )
         frcu    = frcumin + min( max( (dstem-cumes)/dstem , 0.d0), 1.0d0) * (frcu0 - frcumin)
      elsewhere   ((dynveg) .and. (cumes < (-1d0*droot) ) )   ! step function do to erosion larger than root than always minimum ( sedero < -droot )
         frcu    = frcumin
         dynveg  = .false.
      elsewhere (dynveg)                                               ! linear function do to deposition ( -droot < sedero < 0 )
         frcu    = frcumin + min( max( (droot+cumes)/droot , 0.d0), 1.0d0) * (frcu0 - frcumin)
      elsewhere
         ! do nothing
         frcu = frcu0
      endwhere
   endif

end subroutine update_dynveg

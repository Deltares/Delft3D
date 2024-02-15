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

subroutine flow_dredgeinit()
   use m_dad
   use dredge_data_module,   only: initdredge
   use m_fm_dredge,   only: fm_rddredge
   use unstruc_model, only: md_dredgefile
   use m_sediment, only: stm_included, jased
   use MessageHandling, only: mess, LEVEL_FATAL
   
   implicit none

   logical                   :: error

   if (.not.stm_included) return
   dad_included = len_trim(md_dredgefile) /= 0
   if (.not. dad_included) return
   
   call initdredge(dadpar)
   call fm_rddredge(dadpar, md_dredgefile, error)
   if (error) then
      call mess(LEVEL_FATAL, 'unstruc::flow_dredgeinit - Error in initialisation of dredging module.')
   end if

end subroutine flow_dredgeinit

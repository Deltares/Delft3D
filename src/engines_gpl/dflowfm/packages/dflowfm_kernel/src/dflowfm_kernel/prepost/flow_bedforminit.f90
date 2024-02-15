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

!
! Bedform prediction routines
!
subroutine flow_bedforminit(stage)
   use m_bedform
   use m_bedform_io, only: fm_rdbedformpar, fm_initbedformpar
   use unstruc_model, only: md_bedformfile
   use m_flowparameters, only: jawave, modind
   use MessageHandling, only: mess, LEVEL_FATAL

   implicit none

   logical                      :: error
   integer, intent(in)          :: stage

   if (stage==1) then

      call fm_initbedformpar(bfmpar, error)              ! need to initialize the data structure for
                                                         ! eg dredge, tauwave and bed roughness, even if no bedformfile there..
                                                         ! this resets bfmpar%lfbedfrmrou = .true. to .false., so need two stages:
                                                         ! one before sedmorinit, and one after
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in initialisation of bedform module.')
         return
      end if

   else if (stage==2) then

      if (.not. bfm_included) return
      !
      call fm_rdbedformpar(bfmpar, md_bedformfile, error)
      if (error) then
         call mess(LEVEL_FATAL, 'unstruc::flow_bedforminit - Error in reading of bedform file.')
         return
      end if
      !
      ! safety: running waves with rouwav=vr04 can happen without sediment, or trachytopes for that matter
      if (jawave>0 .and. modind==9) then
         bfmpar%lfbedfrmrou = .true.
      endif      
   end if

end subroutine flow_bedforminit

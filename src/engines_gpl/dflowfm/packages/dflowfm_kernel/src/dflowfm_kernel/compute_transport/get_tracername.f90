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

!> Convert qid (from .ext file) to tracer name (split in generic qidname and specific tracer name).
!! If the input qid is not tracer, then the same qid is returned (and no tracer name)
subroutine get_tracername(qid, trname, qidname)
   use m_transport, only: DEFTRACER
   implicit none

   character(len=*), intent(in)  :: qid     !< Original quantityid, e.g., 'tracerbndfluor'.
   character(len=*), intent(out) :: trname  !< The trimmed tracer name, e.g., 'fluor'.
   character(len=*), intent(out) :: qidname !< The base quantity name for further use in external forcing, e.g., 'tracerbnd'.

   trname = ''
   qidname = qid

   if ( qid(1:9).eq.'tracerbnd' ) then
      qidname = qid(1:9)
      if ( len_trim(qid).gt.9 ) then
         trname = trim(qid(10:))
      else
         trname = trim(DEFTRACER)
      end if
   else if (qid(1:13).eq.'initialtracer' ) then
      qidname = qid(1:13)
      if ( len_trim(qid).gt.13 ) then
         trname = trim(qid(14:))
      else
         trname = trim(DEFTRACER)
      end if
   end if

   return
end subroutine get_tracername

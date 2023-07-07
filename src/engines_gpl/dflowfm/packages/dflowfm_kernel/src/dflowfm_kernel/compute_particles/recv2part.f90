!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> add particles from received data
subroutine recv2part(numrecv,work)
   use m_particles
   use m_partparallel
   use m_partitioninfo, only: ndomains
   use unstruc_messages
   use m_missing
   implicit none

   integer,                                   intent(in)  :: numrecv  !< number of received particles
   double precision, dimension(NDIM,numrecv), intent(in)  :: work     !< received data

   integer,          dimension(:),            allocatable :: iperm

   integer                                                :: i, ipoint, j
   integer                                                :: Nreplace

   integer                                                :: ierror

   if ( numrecv.gt.0 ) then
!     get number of existing particles that may be replaced
      Nreplace = 0
      do i=1,Npart
         if ( kpart(i).eq.0 ) then
            Nreplace = Nreplace+1
         end if
      end do

      call realloc_particles(Npart+max(numrecv-Nreplace,0), .true., ierror)

      ipoint = 1
      do j=1,numrecv
         if ( ipoint.le.Npart ) then
            do while ( kpart(ipoint).ne.0 )
               ipoint = ipoint+1
            end do
         end if

         xpart(ipoint) = work(INDX_XPART,j)
         ypart(ipoint) = work(INDX_YPART,j)
         dtremaining(ipoint) = work(INDX_DTREM,j)
         iglob(ipoint) = int(work(INDX_IGLOB,j))
         kpart(ipoint) = int(work(INDX_KPART,j))
         if ( INDX_ZPART.ne.0 ) then
            zpart(ipoint) = work(INDX_ZPART,j)
         end if

         Npart = max(Npart,ipoint)
      end do
   end if

   return
end subroutine recv2part

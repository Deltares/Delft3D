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

module m_partparallel
   double precision, dimension(:,:), allocatable :: worksnd, workrecv   ! work arrays for sending/receiving data

   integer,          dimension(:),   allocatable :: icellother   ! cell number in other domain
   integer,          dimension(:),   allocatable :: isend, jsend ! send list in CRS-format
   integer,          dimension(:),   allocatable :: jrecv        ! start pointers in receive list
   integer,          dimension(:),   allocatable :: jpoint       ! work array
   integer,          dimension(:),   allocatable :: irequest

   integer,          dimension(:), allocatable   :: numsendarr
   integer,          dimension(:), allocatable   :: numrecvarr

   integer                                       :: NDIM         ! data dimension per particle in send/receive arrays
   integer                                       :: INDX_XPART=1
   integer                                       :: INDX_YPART=2
   integer                                       :: INDX_DTREM=3
   integer                                       :: INDX_IGLOB=4
   integer                                       :: INDX_KPART=5
   integer                                       :: INDX_ZPART=0

   integer                                       :: japartsaved   ! particles saved to work array (1) or not (safety)
end module m_partparallel

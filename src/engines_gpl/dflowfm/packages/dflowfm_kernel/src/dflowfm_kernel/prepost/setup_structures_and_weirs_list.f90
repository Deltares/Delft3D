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
!> adjust bobs and iadvec for dams and structs
subroutine setup_structures_and_weirs_list()
   use m_alloc
   use m_flowgeom
   use m_flowparameters
   use m_flow
   use m_netw
   use m_fixedweirs
   use unstruc_channel_flow
   use m_1d_structures
   use m_compound
   use stdlib_sorting, only: radix_sort
   implicit none
   type(t_structure), pointer :: pstru
   integer :: L0
   integer          :: ng, L, n, istru, i
   integer          :: count, Llast, k
   integer, allocatable :: listtmp(:)
   if (.not. ChangeVelocityAtStructures) then
      return
   endif
   call realloc(structuresAndWeirsList, lnx, keepExisting=.false.)
   numberOfStructuresAndWeirs = 0

   ! Generate a list for all possible flow links, where bob0 /= bob, resulting in a difference between au_nostrucs and au.
   ! In general this will be the locations of fixed weirs. Because this check is not completely water tight., all structures
   ! a fixed weir with crest level == bed level will be skipped.
   ! All other structures are added to the list seperately.
   do L = 1, lnx
      if (bob(1,L) /= bob0(1,L) .or. bob(2,L) /= bob0(2,L)) then
         numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
         structuresAndWeirsList(numberOfStructuresAndWeirs) = L
      endif
   enddo

   do ng = 1,ncdamsg                          
      do n   = L1cdamsg(ng), L2cdamsg(ng)
         L        = kcdam(3,n)
         numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
         structuresAndWeirsList(numberOfStructuresAndWeirs) = L
      enddo
   enddo

   do ng = 1,ncgensg                          
      do n   = L1cgensg(ng), L2cgensg(ng)
         L        = kcgen(3,n)
         numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
         structuresAndWeirsList(numberOfStructuresAndWeirs) = L
      enddo
   enddo
   
   do istru = 1, network%sts%count
      pstru => network%sts%struct(istru)
      if (pstru%type ==ST_PUMP) then
         ! skip pump structures
         cycle
      endif
      
      do L0 = 1, pstru%numlinks
         L  = iabs(pstru%linknumbers(L0))
         numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
         structuresAndWeirsList(numberOfStructuresAndWeirs) = L
      enddo
   enddo

   if (ndambreak > 0) then ! needed, because ndambreaksg may be > 0, but ndambreak==0, and then arrays are not available.
      do n = 1, ndambreaksg
         istru = dambreaks(n)
         if (istru.ne.0) then
            do k = L1dambreaksg(n), L2dambreaksg(n)
               L = abs(kdambreak(3,k))
               numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
               structuresAndWeirsList(numberOfStructuresAndWeirs) = L
            enddo
         endif
      enddo
   end if

! Sort the flow links and remove double occurrences
   count = numberOfStructuresAndWeirs
   if (count > 0) then 
      numberOfStructuresAndWeirs = 0
      allocate(listtmp(count))
      listtmp = structuresAndWeirsList(1:count)
      call radix_sort(listtmp)
      Llast = 0
      do i = 1, count
         L = listtmp(i)
         if (L /= Llast) then
            numberOfStructuresAndWeirs = numberOfStructuresAndWeirs + 1
            structuresAndWeirsList(numberOfStructuresAndWeirs) = L
            Llast = L
         endif
      enddo
      
      deallocate(listtmp)
   endif
   return
  end subroutine setup_structures_and_weirs_list

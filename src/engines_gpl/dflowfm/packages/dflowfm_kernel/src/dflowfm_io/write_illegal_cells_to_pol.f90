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

!> output illegal cells to polygon file
subroutine write_illegal_cells_to_pol(jausekc)
   use m_netw
   use m_polygon
   use m_missing
   use gridoperations

   implicit none

   integer, intent(in) :: jausekc   !<use existing kc (1), or not (0), kc=0 is inactive node, kc=-1 is active node on original netboundary, kc=1 is internal active node

   integer, dimension(:), allocatable :: kc_sav

   integer :: i, ic, ii, k, k1, k2, L
   integer :: isillegalcell
   integer :: ifil


   allocate(kc_sav(numk))

!  mark nodes to find the illegal cells: unmasked nodes
   if ( jausekc.eq.1 ) then
      do k=1,numk
         kc_sav(k) = kc(k)
      end do
   else
!     mark nodes on original net boundary with _valid_ lnn
      kc_sav = 1
      do L=1,numL
         if ( lnn(L).eq.1 .and. kn(3,L).ne.0 ) then
            k1 = kn(1,L)
            k2 = kn(2,L)
            kc_sav(k1) = -abs(kc_sav(k1))
            kc_sav(k2) = -abs(kc_sav(k2))
         end if
      end do
   end if

   call savepol()
   call savecells()
   NPL = 0

!  find cells without node mask, no setnodadm
   kc = 1
   call findcells(11000)

   NPL = 0

!  compare with stored node masked and detect illegal cells
   do ic=1,nump
      isillegalcell = 1
      do ii=1,netcell(ic)%N
         L = netcell(ic)%lin(ii)
         if ( L.gt.0 ) then
            k1 = kn(1,L)
            k2 = kn(2,L)

            if ( kc_sav(k1).ne.0 .or. kc_sav(k2).ne.0 ) then  ! link is active
               if (  lnn(L).eq.1 .or. (kc_sav(k1).ne.-1 .or. kc_sav(k2).ne.-1) ) then   ! not an original boundary link, or new boundary link
                  isillegalcell = 0
                  exit
               end if
            end if
         end if
      end do

      if ( isillegalcell.eq.1 ) then
         i = NPL
         NPL = NPL+netcell(ic)%N + 1
         call increasepol(NPL,1)
         do ii=1,netcell(ic)%N
            i = i+1
            k = netcell(ic)%nod(ii)
            xpl(i) = xk(k)
            ypl(i) = yk(k)
         end do
         i=i+1
         xpl(i) = DMISS
         ypl(i) = DMISS
      end if
   end do

   if ( NPL.gt.0 ) then
      call newfil(ifil,'illegalcells.pol')
      call wripol(ifil)
   end if

   call restorepol()
   call restorecells()

   if ( allocated(kc_sav) ) deallocate(kc_sav)

   return
end subroutine write_illegal_cells_to_pol

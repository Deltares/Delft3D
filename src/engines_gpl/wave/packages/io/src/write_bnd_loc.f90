subroutine write_bnd_loc (inest,sg)
!
! Head routine for calling write_bnd
!
use swan_flow_grid_maps
use read_grids
implicit none
type (grid)                 :: sg       ! actual swan grid
integer                     :: inest    ! swan nested grid no.
   if (sg%unstructured) then
      call write_unstructured_bnd(sg, inest)
      sg%tmp_name = sg%grid_name(1:min(37, len_trim(sg%grid_name)))
   else
      call write_bnd(sg%x        ,sg%y       ,sg%mmax   ,sg%nmax   , &
                   & inest      )
      call write_swan_grid (sg%x,sg%y,sg%mmax,sg%nmax,inest,sg%tmp_name)
   endif
end subroutine write_bnd_loc


subroutine write_bnd(xc        ,yc        ,mc        ,nc        , &
                & inest      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  
!  
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    !
    implicit none
!
! Global variables
!
    integer                   , intent(in)  :: inest
    integer                   , intent(in)  :: mc
    integer                   , intent(in)  :: nc
    real(kind=hp)   , dimension(mc, nc)     :: xc
    real(kind=hp)   , dimension(mc, nc)     :: yc
!
! Local variables
!
    integer           :: i
    integer           :: j
    integer           :: lunbot
    character(37)     :: fname
!
!! executable statements -------------------------------------------------------
!
    if (inest>1) then
       fname      = ' '
       fname(1:12) = 'SWANIN_NGRID'
       write (fname(13:15),'(I3.3)') inest
       open (newunit=lunbot, file=fname(1:15))
       do i=1,mc
          if (xc(i,1)/=0.) write(lunbot,'(2(E25.17,3X))')  xc(i,1) ,yc(i,1)
       enddo
       do j=2,nc
          if (xc(mc,j)/=0.) write(lunbot,'(2(E25.17,3X))')  xc(mc,j),yc(mc,j)
       enddo
       do i=mc-1,1,-1
          if (xc(i,nc)/=0.) write(lunbot,'(2(E25.17,3X))')  xc(i,nc),yc(i,nc)
       enddo
       do j=nc-1,2,-1
          if (xc(1,j)/=0.) write(lunbot,'(2(E25.17,3X))')  xc(1,j) ,yc(1,j)
       enddo
       close(lunbot)
    endif
end subroutine write_bnd


subroutine write_unstructured_bnd(sg, inest)
use swan_flow_grid_maps
implicit none
type(grid) :: sg
integer, intent(in) :: inest
integer :: i
integer :: ierr
integer :: lunbot
integer :: nedge
integer :: nbedge
integer :: start
integer :: cur
integer :: prev
integer :: next
integer :: v1
integer :: v2
integer :: fname_len
integer, dimension(:), allocatable :: ea
integer, dimension(:), allocatable :: eb
integer, dimension(:,:), allocatable :: adj
integer, dimension(:), allocatable :: degree
logical, dimension(:), allocatable :: visited
character(37) :: fname
!
   if (inest <= 1) return
   nedge = 3*sg%ncell
   allocate(ea(nedge), eb(nedge), adj(2,sg%mmax), degree(sg%mmax), visited(sg%mmax), stat=ierr)
   if (ierr /= 0) then
      call wavestop(1, 'Allocation problem while writing unSWAN nest boundary.')
   end if
   do i = 1, sg%ncell
      call set_edge(3*i-2, sg%kvertc(1,i), sg%kvertc(2,i))
      call set_edge(3*i-1, sg%kvertc(2,i), sg%kvertc(3,i))
      call set_edge(3*i  , sg%kvertc(3,i), sg%kvertc(1,i))
   enddo
   call sort_edges(ea, eb, nedge)
   adj = 0
   degree = 0
   nbedge = 0
   do i = 1, nedge
      if (is_boundary_edge(i)) then
         nbedge = nbedge + 1
         call add_adj(ea(i), eb(i))
         call add_adj(eb(i), ea(i))
      endif
   enddo
   fname      = ' '
   fname(1:12) = 'SWANIN_NGRID'
   write (fname(13:15),'(I3.3)') inest
   fname_len = 15
   open (newunit=lunbot, file=fname(1:fname_len))
   visited = .false.
   do
      start = 0
      do i = 1, sg%mmax
         if (degree(i) == 1 .and. .not.visited(i)) then
            start = i
            exit
         endif
      enddo
      if (start == 0) then
         do i = 1, sg%mmax
            if (degree(i) > 0 .and. .not.visited(i)) then
               start = i
               exit
            endif
         enddo
      endif
      if (start == 0) exit
      prev = 0
      cur = start
      do
         write(lunbot,'(2(E25.17,3X))') sg%x(cur,1), sg%y(cur,1)
         visited(cur) = .true.
         next = 0
         do i = 1, degree(cur)
            if (adj(i,cur) /= prev .and. .not.visited(adj(i,cur))) then
               next = adj(i,cur)
               exit
            endif
         enddo
         if (next == 0) then
            do i = 1, degree(cur)
               if (adj(i,cur) /= prev) then
                  next = adj(i,cur)
                  exit
               endif
            enddo
         endif
         if (next == 0 .or. next == start) exit
         prev = cur
         cur = next
      enddo
   enddo
   if (nbedge == 0 .and. associated(sg%vmark)) then
      do i = 1, sg%mmax
         if (sg%vmark(i) /= 0) write(lunbot,'(2(E25.17,3X))') sg%x(i,1), sg%y(i,1)
      enddo
   endif
   close(lunbot)
   deallocate(ea, eb, adj, degree, visited, stat=ierr)
contains
   subroutine set_edge(idx, ja, jb)
      integer, intent(in) :: idx
      integer, intent(in) :: ja
      integer, intent(in) :: jb
      ea(idx) = min(ja,jb)
      eb(idx) = max(ja,jb)
   end subroutine set_edge

   logical function is_boundary_edge(idx)
      integer, intent(in) :: idx
      is_boundary_edge = .true.
      if (idx > 1) then
         if (ea(idx) == ea(idx-1) .and. eb(idx) == eb(idx-1)) is_boundary_edge = .false.
      endif
      if (idx < nedge) then
         if (ea(idx) == ea(idx+1) .and. eb(idx) == eb(idx+1)) is_boundary_edge = .false.
      endif
   end function is_boundary_edge

   subroutine add_adj(ja, jb)
      integer, intent(in) :: ja
      integer, intent(in) :: jb
      if (degree(ja) < 2) then
         degree(ja) = degree(ja) + 1
         adj(degree(ja),ja) = jb
      endif
   end subroutine add_adj

   subroutine sort_edges(la, lb, n)
      integer, intent(in) :: n
      integer, dimension(n), intent(inout) :: la
      integer, dimension(n), intent(inout) :: lb
      integer :: left
      integer :: right
      integer :: tmpa
      integer :: tmpb
      if (n <= 1) return
      do left = n/2, 1, -1
         call sift_down(la, lb, left, n)
      enddo
      do right = n, 2, -1
         tmpa = la(1); tmpb = lb(1)
         la(1) = la(right); lb(1) = lb(right)
         la(right) = tmpa; lb(right) = tmpb
         call sift_down(la, lb, 1, right-1)
      enddo
   end subroutine sort_edges

   subroutine sift_down(la, lb, root, last)
      integer, dimension(:), intent(inout) :: la
      integer, dimension(:), intent(inout) :: lb
      integer, intent(in) :: root
      integer, intent(in) :: last
      integer :: child
      integer :: swap
      integer :: r
      integer :: tmpa
      integer :: tmpb
      r = root
      do while (2*r <= last)
         child = 2*r
         swap = r
         if (edge_less(la(swap), lb(swap), la(child), lb(child))) swap = child
         if (child+1 <= last) then
            if (edge_less(la(swap), lb(swap), la(child+1), lb(child+1))) swap = child+1
         endif
         if (swap == r) return
         tmpa = la(r); tmpb = lb(r)
         la(r) = la(swap); lb(r) = lb(swap)
         la(swap) = tmpa; lb(swap) = tmpb
         r = swap
      enddo
   end subroutine sift_down

   logical function edge_less(a1, b1, a2, b2)
      integer, intent(in) :: a1
      integer, intent(in) :: b1
      integer, intent(in) :: a2
      integer, intent(in) :: b2
      edge_less = (a1 < a2) .or. (a1 == a2 .and. b1 < b2)
   end function edge_less
end subroutine write_unstructured_bnd

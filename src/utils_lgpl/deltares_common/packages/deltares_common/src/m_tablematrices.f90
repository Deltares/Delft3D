module m_tablematrices
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!!--description-----------------------------------------------------------------
!
!    Function: - Various table manipulation routines.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

   use MessageHandling
   use m_alloc

   implicit none

   private

   !
   ! functions and subroutines
   !
   public setTableMatrix
   public deallocTableMatrix
   public interpolate
   public dealloc

   interface dealloc
      module procedure deallocTableMatrix
   end interface
   
   interface interpolate
      module procedure interpolateTableMatrix
   end interface

   type, public :: t_tablematrix                                                    !< table definition
      integer                                      :: lengths(2)   = 0
      double precision, dimension(:), pointer  :: x => null()                       !< x-values of table
      double precision, dimension(:), pointer  :: y => null()                       !< y-values of table
      double precision, dimension(:,:), pointer  :: fun => null()                   !< function-values of table (x,y)
   end type t_tablematrix

contains

   subroutine setTableMatrix(table, x, y, lengths, matrix, linear)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_tablematrix)                          :: table
      integer, dimension(2)                        :: lengths
      double precision, dimension(:)               :: x
      double precision, dimension(:)               :: y
      double precision, dimension(:,:), optional   :: matrix
      double precision, dimension(:)  , optional   :: linear

      ! local parameters
      integer :: i

      ! Program code
      ! Table will be replaced
      if (table%lengths(1) > 0) then
         deallocate(table%x)
         deallocate(table%y)
         deallocate(table%fun)
      endif
      
      table%lengths      = lengths
      if ( (lengths(1) > 0) .and. (lengths(1) > 0) ) then
         allocate(table%x(lengths(1)))
         allocate(table%y(lengths(2)))
         allocate(table%fun(lengths(1), lengths(2)))
         table%x     = x(1:lengths(1))
         table%y     = y(1:lengths(2))
         if (present(matrix)) then
            table%fun   = matrix(1:lengths(1), 1:lengths(2))
         else
            do i = 1, lengths(2)
               table%fun(1:lengths(1),i)   = linear((i-1)*lengths(1)+1:i*lengths(1))
            enddo
         endif
      endif
      
   end subroutine setTableMatrix

   subroutine deallocTableMatrix(table)
      ! Modules

      implicit none

      ! Input/output parameters
      type(t_tablematrix), intent(inout),pointer          :: table

      ! Local variables

      ! Program code
      if (associated(table%x)) deallocate(table%x)
      if (associated(table%y)) deallocate(table%y)
      if (associated(table%fun)) deallocate(table%fun)
      table%lengths = 0
      deallocate(table)
      
      table => null()

   end subroutine deallocTableMatrix

   double precision function InterpolateTableMatrix(table, xs, ys)
       implicit none
   !
   ! Global variables
   !
       type(t_tableMatrix)            :: table
       double precision, intent(in)   :: xs
       double precision, intent(in)   :: ys
   !
   !
   ! Local variables
   !
       integer             :: len1, len2
       integer             :: i1, i2
       integer             :: j1, j2
       logical             :: found
       double precision    :: fac_x
       double precision    :: fac_y
       double precision    :: val1
       double precision    :: val2
   !
   !
   !! executable statements -------------------------------------------------------
   !
      len1 = table%lengths(1) 
      len2 = table%lengths(2) 
      found = .false. 
      
      call findInterpolationInterval(xs, table%x, len1, i1, i2, fac_x)
      call findInterpolationInterval(ys, table%y, len2, j1, j2, fac_y)
      ! interpolate to x
      val1 = fac_x*table%fun(i1,j1) + (1d0-fac_x)*table%fun(i2,j1)
      val2 = fac_x*table%fun(i1,j2) + (1d0-fac_x)*table%fun(i2,j2)
      InterpolateTableMatrix = fac_y*val1 + (1d0-fac_y)*val2
      
   end function InterpolateTableMatrix
   
   subroutine findInterpolationInterval(xs, xarr, lenarr, i1, i2, fac)
      double precision              , intent(in   )   :: xs
      double precision, dimension(:), intent(in   )   :: xarr
      integer                       , intent(in   )   :: lenarr
      integer                       , intent(  out)   :: i1
      integer                       , intent(  out)   :: i2
      double precision              , intent(  out)   :: fac
      
      logical found
      
      found = .false.
      do i2 = 1, lenarr
         if (xs < xarr(i2)) then
            found = .true.
            exit
         endif
      enddo
      if (.not.found) then
         i1 = lenarr
         i2 = lenarr
         fac = 0d0
      elseif (i2 <= 1) then
         i1 = 1
         i2 = 1
         fac = 0d0
      else
         i1  = i2 - 1
         fac = (xarr(i2) - xs)/(xarr(i2) - xarr(i1))
      endif
   end subroutine findInterpolationInterval

end module m_tablematrices

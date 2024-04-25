module test_running_mean
   !----- LGPL --------------------------------------------------------------------
   !                                                                               
   !  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
   !    Function: - Tests for the modules runsum and runsum_weighted
   !
   !!--pseudo code and references--------------------------------------------------
   ! NONE
   !!--declarations----------------------------------------------------------------
   use precision_basics, only : hp, sp
   use ftnunit
   use runsum
   use AR1MA
   implicit none

   private

   public :: tests_running_mean

contains

   subroutine tests_running_mean
      call test( testRunningMean1,  'Test 1 for running mean, nd=5' )
      call test( testRunningMean2,  'Test 2 for running mean, nd=13' )
      call test( testRunningMean3,  'Test 3 for running mean, weighted' )
   end subroutine tests_running_mean

   subroutine testRunningMean1
      implicit none

      type (TRunSum) :: running
      integer        :: nx, nd, nmax
      integer        :: i, j
      real(kind=hp)  :: f
      real(kind=hp), dimension(:), pointer  :: dataptr
      real(kind=sp), parameter :: ref_values(3) = [4.9, 4.803, 4.7089]

      nd = 5
      nx = 3
      nmax = 100

      allocate(dataptr(nx))
      call running%init(nx,nd,dataptr)
      do i = 1, nmax
         f = i * 1.d0/nmax
         do j = 1, nx
            dataptr(j) = f**j
         end do
         call running%update()
      end do
      call assert_comparable(running%state, ref_values, 1e-6, 'difference in last state')
      deallocate(dataptr)
   end subroutine testRunningMean1

   subroutine testRunningMean2
      implicit none

      type (TRunSum) :: running
      integer        :: nx, nd, nmax
      integer        :: i, j
      real(kind=hp)  :: f
      real(kind=hp), dimension(:), pointer  :: dataptr
      real(kind=sp), parameter :: ref_values(3) = [12.22, 11.505, 10.84892]

      nd = 13
      nx = 3
      nmax = 100

      allocate(dataptr(nx))
      call running%init(nx,nd,dataptr)
      do i = 1, nmax
         f = i * 1.d0/nmax
         do j = 1, nx
            dataptr(j) = f**j
         end do
         call running%update()
      end do
      call assert_comparable(running%state, ref_values, 1e-6, 'difference in last state')
      deallocate(dataptr)
   end subroutine testRunningMean2

   subroutine testRunningMean3
      implicit none

      type (TAR1MA)  :: running_wt
      integer        :: nx, nd, nmax
      integer        :: i, j
      real(kind=hp)  :: f
      real(kind=hp), dimension(:), pointer  :: dataptr
      real(kind=hp), dimension(:), pointer  :: weightptr
      real(kind=sp), parameter :: ref_values(3) = [2.97, 2.9405, 2.911491]

      nd = 5
      nx = 3
      nmax = 100

      allocate(dataptr(nx))
      allocate(weightptr(0:nd-1))
      call running_wt%init(nx,nd,dataptr)
      call running_wt%setpar(weightptr)
      weightptr(0:nd-1) = 1.d0
      weightptr(3:4) = 0.d0
      do i = 1, nmax
         f = i * 1.d0/nmax
         do j = 1, nx
            dataptr(j) = f**j
         end do
         call running_wt%update()
      end do
      call assert_comparable(running_wt%state, ref_values, 1e-6, 'difference in last state')
      deallocate(dataptr, weightptr)
   end subroutine testRunningMean3

end module test_running_mean

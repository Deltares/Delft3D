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

! unstruc.f90
!---------------------------------------------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------------------------------------------
! solve_guus.f90
 module m_reduce

 implicit none
    !
    !
    ! Derived Type definitions
    !
 type list
    integer l
    integer, allocatable :: j(:)
 end type list
 type listb
    integer l
    integer, allocatable :: j(:)
    logical, allocatable :: b(:)
 end type listb
 type listc
    integer l
    integer, allocatable :: j(:)
    integer, allocatable :: a(:)
 end type listc
 type listd
    integer l
    integer, allocatable :: j(:)
    integer, allocatable :: a(:)
    integer, allocatable :: f(:)
 end type listd
 !
 ! Local variables
 !
 integer                       :: maxdge   = 6 ! 500
 integer                       :: noactive = 0
 integer                       :: nodtot   = 0 ! (nodtot=ndx)
 integer                       :: lintot   = 0 ! (lintot=lnx)
 integer                       :: nocg     = 0
 integer                       :: nocg0    = 0
 integer                       :: nogauss  = 0
 integer                       :: nogauss0 = 0
 integer                       :: noexpl   = 0
 integer                       :: nowet    = 0
 integer                       :: ijstck   = 0
 integer                       :: npmax    = 0  ! ccc arrays size in gauss_elimination
 integer, allocatable          :: noel(:)
 integer, allocatable          :: noel0(:)
 integer, allocatable          :: nbrstk(:)
 integer, allocatable          :: nodstk(:)
 integer, allocatable          :: nodbr2(:)

 integer, allocatable          :: lv2(:) ! old linev(2,L), linev(5,L) and (6,L) are now ln(1,L) and ln(2,L)

 integer, allocatable          :: jagauss(:)


 type (listb), allocatable     :: ij(:)
 type (listc), allocatable     :: ia(:)
 type (listd), allocatable     :: row(:)

 integer, allocatable          :: intbuf(:) !< Work array in pointonstack>ijtrue
 logical, allocatable          :: logbuf(:) !< Work array in pointonstack>ijtrue

 double precision              :: epscg   = 1d-14    ! epsilon waterlevels cg method (maximum)
 double precision              :: epsdiff = 1d-3     ! tolerance in (outer) Schwarz iterations (for Schwarz solver)
 integer                       :: maxmatvecs  = 100000 ! maximum number of matrix-vector multiplications in Saad solver

 double precision, allocatable :: bbr(:), bbl(:)     ! not left !
 double precision, allocatable :: ccr(:), ccrsav(:)
 double precision, allocatable :: ddr(:)

 double precision, allocatable :: d0 (:)
 double precision, allocatable :: zkr(:)
 double precision, allocatable :: pk (:)
 double precision, allocatable :: apk(:)
 double precision, allocatable :: rk (:)

 double precision, allocatable :: ccc(:)  !< work array in gauss_elimination

 integer, allocatable          :: L1row(:), L2row(:), iarow(:) , jrow(:), ifrow(:), ift(:)  ! for jipjan

 integer                       :: nbr
 integer                       :: nodl
 integer                       :: nodr
 integer                       :: ndn
 integer                       :: mindgr
 integer                       :: nocgiter

 double precision, allocatable, dimension(:) :: s1_ghost ! for testsolver

 end module m_reduce

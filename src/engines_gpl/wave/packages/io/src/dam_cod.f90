subroutine dam_cod(xz      ,yz      ,kcs      ,mmax      ,nmax )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
    integer                        , intent(in)  :: mmax
    integer                        , intent(in)  :: nmax
    integer , dimension(mmax, nmax)              :: kcs
    real(hp), dimension(mmax, nmax)              :: xz
    real(hp), dimension(mmax, nmax)              :: yz
!
! Local variables
!
    integer                             :: ierr
    integer                             :: inout
    integer                             :: ip
    integer                             :: m
    integer                             :: n
    integer                             :: ncol
    integer                             :: np
    integer                             :: umsk
    real(hp), dimension(:), allocatable :: xdam      ! hp, because it is a parameter for subroutine ipon
    real(hp), dimension(:), allocatable :: ydam      ! idem
    character(4)                        :: blname
    logical                             :: exists
!
!! executable statements -------------------------------------------------------
!
    ! Read ldb file dams
    !
    inquire(file='wavemask.ldb',exist=exists)
    if (.not.exists) goto 1000
    open (newunit = umsk, file = 'wavemask.ldb')
  100 continue
    read (umsk, '(A)', err = 999) blname
    if (blname(1:1)=='*') goto 100
    read (umsk, *, err = 999) np, ncol
    allocate(xdam(np))
    allocate(ydam(np))
    do ip = 1, np
       read (umsk, *, err = 999) xdam(ip), ydam(ip)
    enddo
    do n = 1, nmax
       do m = 1, mmax
          call ipon(xdam, ydam, np, xz(m, n), yz(m, n), inout)
          if (inout >= 0) then
             kcs(m,n) = 0
          endif
       enddo
    enddo
    goto 100
    !
  999 continue
    close (umsk)
    deallocate(xdam, stat=ierr)
    deallocate(ydam, stat=ierr)
 1000 continue
end subroutine dam_cod

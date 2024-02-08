subroutine getfield(itim, comfil, grpnam, funam, ierr, fcom, nmaxus, mmax, kmax, lmax, l)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    implicit none
    !
    integer                                                             , intent(in)    :: nmaxus ! First dimension length
    integer                                                             , intent(in)    :: mmax   ! Second dimension length
    integer                                                             , intent(in)    :: kmax   ! Third dimension length
    integer                                                             , intent(in)    :: lmax   ! Fourth dimension length
    integer                                                             , intent(in)    :: l      ! Fourth dimension index
    integer                                                             , intent(in)    :: itim   ! requested index
    real(fp), dimension(nmaxus, mmax, kmax, lmax)                       , intent(inout) :: fcom   ! data array
    character(*)                                                        , intent(in)    :: comfil ! file name
    character(16)                                                       , intent(in)    :: funam  ! element name
    character(16)                                                       , intent(in)    :: grpnam ! group name
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i
    integer                                       :: ierr   ! Flag for error when writing to Communication file 
    integer                                       :: k
    integer                                       :: m
    integer                                       :: n
    integer      , dimension(3,5)                 :: uindex
    integer                        , external     :: getelt
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
!
!! executable statements -------------------------------------------------------
!
    uindex (1,1) = itim ! start index
    uindex (2,1) = itim ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ierr = open_datdef(comfil, fds, .true.)
    if (ierr /= 0) return
    !
    call sbuff_checksize(nmaxus*mmax*kmax)
    ierr = getelt(fds, grpnam, funam, uindex, 1, 4*nmaxus*mmax*kmax, sbuff)
    if (ierr/=0) return
    !
    ierr = clsnef(fds)
    if (ierr/=0) return
    !
    i = 0
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             i = i+1
             fcom(n, m, k, l) = sbuff(i)
          enddo
       enddo
    enddo
end subroutine getfield
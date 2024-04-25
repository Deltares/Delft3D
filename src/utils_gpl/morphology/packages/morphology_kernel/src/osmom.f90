subroutine osmom(hrms      ,depth     ,tp        ,g         ,cr        , &
               & qbb       ,ev1b      ,ev2b      ,ev3b      ,ev5b      , &
               & od2b      ,od3b      ,od4b      )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
    !
    ! COMMON variables
    !
    real(fp), save :: dh
    real(fp), save :: tstep
    integer , save :: iih
    integer , save :: iit
    integer , save :: itable
    real(fp), dimension(0:40, 0:40, 12), save :: table               ! Table containing the moments
!
! Arguments
!
    real(fp), intent(in) :: cr
    real(fp), intent(in) :: depth
    real(fp)             :: ev1b
    real(fp)             :: ev2b
    real(fp)             :: ev3b
    real(fp)             :: ev5b
    real(fp), intent(in) :: g
    real(fp), intent(in) :: hrms
    real(fp)             :: od2b
    real(fp)             :: od3b
    real(fp)             :: od4b
    real(fp), intent(in) :: qbb
    real(fp), intent(in) :: tp
!
!
! Local variables
!
    integer                        :: ierr
    integer                        :: if
    integer                        :: ih
    integer                        :: ih0
    integer                        :: ih1
    integer                        :: it
    integer                        :: it0
    integer                        :: it1
    integer                        :: utab
    logical                        :: error
    real(fp)                       :: f0
    real(fp)                       :: f1
    real(fp)                       :: f2
    real(fp)                       :: f3
    real(fp)                       :: h
    real(fp)                       :: p
    real(fp)                       :: q
    real(fp)                       :: su2bi
    real(fp)                       :: su2ubi
    real(fp)                       :: su2unl
    real(fp)                       :: su3bi
    real(fp)                       :: su3ubi
    real(fp)                       :: su3unl
    real(fp)                       :: su5bi
    real(fp)                       :: subi
    real(fp)                       :: suubi
    real(fp)                       :: suunl
    real(fp)                       :: t0
    real(fp)                       :: sqrtgh
    real(fp)                       :: sqrtgh2
    real(fp)                       :: sqrtgh3
    real(fp), dimension(12)        :: z
    character(256)                 :: pathd
!
    data itable/0/
!
!! executable statements -------------------------------------------------------
!
    !
    if (itable==0) then
       !
       ! This part is only performed the first time
       ! The central moments are read from file momtab and put in 3d-array 'table'.
       !
       error = .false.
       call getmp(error, pathd)
       if (error) then
          write (*, '(a)') "ERROR: Directory ""default"" not found"
          call throwexception()
       endif
       open (newunit = utab, file = trim(pathd) // 'tabmom', status='old', action='read', iostat=ierr)
       if (ierr /= 0) then
          write (*, '(3a)') "ERROR: File """,trim(pathd) // 'tabmom', """ not found"
          call throwexception()
       endif
       read (utab, *) iih, iit, dh, tstep
       do it = 1, iit
          do ih = 1, iih
             read (utab, *) (table(ih, it, if), if = 1, 12)
          enddo
       enddo
       itable = 1
       close (utab)
    endif
    !
    ! Make everything dimensionless
    !
    h = hrms/depth
    if (h>=0.8) h = 0.799
    if (h<dh) h = dh + 1.E-5
    t0 = tp*sqrt(g/depth)
    if (t0<=1.) t0 = 1.00001
    if (t0>=40.) t0 = 39.999
    sqrtgh = sqrt(g*depth)
    !
    ! Interpolate central moments of bed velocity from 'table'
    !
    ih0 = int(h/dh)
    it0 = int(t0/tstep)
    ih1 = ih0 + 1
    it1 = it0 + 1
    p   = (h - ih0*dh)/dh
    q   = (t0 - it0*tstep)/tstep
    f0  = (1. - p)*(1. - q)
    f1  = p*(1. - q)
    f2  = q*(1. - p)
    f3  = p*q
    do if = 1, 12
       z(if) = f0*table(ih0, it0, if) + f1*table(ih1, it0, if) +      &
             & f2*table(ih0, it1, if) + f3*table(ih1, it1, if)
    enddo
    !
    ! Compute moments
    !
    subi   = z(3)
    su2bi  = z(4)
    su3bi  = z(5)
    su5bi  = z(6)
    suubi  = z(7)
    su2ubi = z(8)
    su3ubi = z(9)
    suunl  = z(10)
    su2unl = z(11)
    su3unl = z(12)
    ev1b   = subi
    ev2b   = su2bi
    ev3b   = su3bi
    ev5b   = su5bi
    od2b   = (1. - qbb)*suunl + suubi*cr
    od3b   = (1. - qbb)*su2unl + su2ubi*cr
    od4b   = (1. - qbb)*su3unl + su3ubi*cr
    !
    ! Add dimension to moments
    !
    !hrms   = h*depth
    sqrtgh2 = sqrtgh*sqrtgh
    sqrtgh3 = sqrtgh*sqrtgh2
    ev1b    = ev1b*sqrtgh
    ev2b    = ev2b*sqrtgh2
    ev3b    = ev3b*sqrtgh3
    ev5b    = ev5b*sqrtgh3*sqrtgh2
    od2b    = od2b*sqrtgh2
    od3b    = od3b*sqrtgh3
    od4b    = od4b*sqrtgh3*sqrtgh
end subroutine osmom

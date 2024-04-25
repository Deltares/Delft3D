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
program testfill
    use precision     ! pntrsize, used in fsm.i
    implicit none
    include 'fsm.i'

    integer                :: len_lvl
    integer                :: len_u
    integer(kind=pntrsize) :: ierror
    integer(kind=pntrsize) :: ilvl
    integer(kind=pntrsize) :: iu
    integer                :: status
    real                   :: level
    real                   :: u_vel

    len_lvl = 5
    len_u   = 7

    status = fsmini (0, 0)

    ierror = makptr ('level', rtyp, len_lvl)
    ierror = makptr ('u_vel', rtyp, len_u)

    ilvl = getptr ('level')
    iu   = getptr ('u_vel')
    call fill (rbuf(ilvl), len_lvl, 2.0)
    call fill (rbuf(iu), len_u, 3.0)

    status = prtkey ()

    level = rbuf(getptr ('level'))
    u_vel = rbuf(getptr ('u_vel'))

    write(*,*) '-------------'
    call prtarr (rbuf(getptr('level')), len_lvl)
    write(*,*) '-------------'
    call prtarr (rbuf(getptr('u_vel')), len_u)
    write(*,*) '-------------'
    call prtarr (rbuf(getptr('level')), len_lvl)
    write(*,*) '-------------'
end

!-------------------------------------------------------------------------------

subroutine fill(a, length, fac)
    integer length
    real a(length)
    real fac
    integer i
    do i=1,length
        a(i)=fac*i
    enddo
endsubroutine

subroutine prtarr(a, length)
    integer length
    real a(length)
    integer i
    do i=1,length
        write(*,*) a(i)
    enddo
endsubroutine

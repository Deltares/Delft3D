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

 !> Adds administration for an open boundary segment, intended
 !! for postprocessing.
 !!
 !! An open boundary section is associated with one polyline
 !! and consists of one or more netlink numbers.
 subroutine addopenbndsection(nbnd, netlinknrs, plifilename, ibndtype)
 use m_alloc
 use m_flowexternalforcings
 implicit none
    integer,          intent(in) :: nbnd             !< Nr. of net links in this open bnd section.
    integer,          intent(in) :: netlinknrs(nbnd) !< Net link nrs in this open bnd section (in any order)
    character(len=*), intent(in) :: plifilename      !< File name of the original boundary condition definition polyline.
    integer,          intent(in) :: ibndtype         !< Type of this boundary section (one of IBNDTP_ZETA, etc...)
    integer :: maxopenbnd, istart, i, n1, n2

    ! Start index (-1) of net link numbers for this net boundary section:
    if (nopenbndsect >= 1) then
        istart = nopenbndlin(nopenbndsect)
    else
        istart = 0
    end if

    nopenbndsect = nopenbndsect + 1
    maxopenbnd = max(size(nopenbndlin), int(1.2*nopenbndsect)+1, 5)
    call realloc(openbndname, maxopenbnd, fill = ' ')
    call realloc(openbndfile, maxopenbnd, fill = ' ')
    call realloc(openbndtype, maxopenbnd, fill = IBNDTP_UNKNOWN)
    call realloc(nopenbndlin, maxopenbnd)
    call realloc(openbndlin,  istart+nbnd)

    ! Strip off trailing file extension .pli
    n2  = index(plifilename,'.', .true.) - 1
    if (n2 < 0) then
       n2 = len_trim(plifilename)
    end if

    ! Strip off leading path /dir/name/bnd/
    n1  = index(plifilename(1:n2),'\', .true.) ! Win
    if (n1 == 0) then
       n1  = index(plifilename(1:n2),'/', .true.) ! Or try UX
    end if

    openbndfile(nopenbndsect) = trim(plifilename)
    openbndname(nopenbndsect) = plifilename(N1+1:N2)
    openbndtype(nopenbndsect) = ibndtype

    do i = 1,nbnd
        openbndlin(istart+i) = netlinknrs(i)
    end do

    nopenbndlin(nopenbndsect) = istart + nbnd

 end subroutine addopenbndsection

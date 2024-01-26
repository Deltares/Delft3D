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

 !> set wall to flowlinks and wall to flownode oientations
 subroutine setwallorientations()
    use m_flowgeom
    use network_data, only: xk, yk
    use m_sferic
    use m_alloc
    use m_missing, only : dmiss
    use geometry_module, only :half, spher2locvec

    implicit none

    double precision :: xh, yh

    integer          :: k1, k3, k4
    integer          :: L1, L2
    integer          :: nw

    integer          :: ierr

    if ( allocated (csbw) ) deallocate(csbw)
    if ( allocated (snbw) ) deallocate(snbw)

    if ( allocated (csbwn) ) deallocate(csbwn)
    if ( allocated (snbwn) ) deallocate(snbwn)

    if ( jsferic.eq.0 .or. jasfer3D.eq.0 ) return

    allocate ( csbw(2,mxwalls) , stat  = ierr) ; csbw = 1d0
    call aerr('csbw(2,mxwalls)', ierr, 2*Lnx)
    allocate ( snbw(2,mxwalls) , stat  = ierr) ; snbw = 0d0
    call aerr('snbw(2,mxwalls)', ierr, 2*Lnx)


    allocate ( csbwn(mxwalls) , stat  = ierr) ; csbwn = 1d0
    call aerr('csbwn(mxwalls)', ierr, 2*Lnx)
    allocate ( snbwn(mxwalls) , stat  = ierr) ; snbwn = 0d0
    call aerr('snbwn(mxwalls)', ierr, 2*Lnx)

    do nw=1,mxwalls
       k1 = walls(1,nw) ! inner flownode
       k3 = walls(2,nw) ! first corner (netnode)
       k4 = walls(3,nw) ! second corner (netnode)
       L1 = walls(4,nw) ! first flowlink
       L2 = walls(5,nw) ! second flowlink

!      compute wall midpoint coordinates
       call half(xk(k3),yk(k3),xk(k4),yk(k4),xh,yh, jsferic, jasfer3D)

!      compute orientation of flowlinks w.r.t. wall mid point
       if ( L1.gt.0 ) then
          call spher2locvec(xu(L1),yu(L1),1,(/xh/),(/yh/),(/1d0/),(/0d0/),csbw(1,nw),snbw(1,nw),jsferic, jasfer3D, dmiss)
       end if
       if ( L2.gt.0 ) then
          call spher2locvec(xu(L2),yu(L2),1,(/xh/),(/yh/),(/1d0/),(/0d0/),csbw(2,nw),snbw(2,nw),jsferic, jasfer3D, dmiss)
       end if

!      compute orientation of flownode w.r.t. wall mid point
       call spher2locvec(xz(k1),yz(k1),1,(/xh/),(/yh/),(/1d0/),(/0d0/),csbwn(nw),snbwn(nw),jsferic, jasfer3D, dmiss)
    end do

    return
 end subroutine setwallorientations

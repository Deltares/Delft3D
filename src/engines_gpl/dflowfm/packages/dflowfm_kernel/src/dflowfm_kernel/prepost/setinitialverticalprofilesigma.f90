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

subroutine setinitialverticalprofilesigma(yy,ny,filename) ! polyfil
 use m_flowgeom
 use m_flow
 use m_polygon
 implicit none
 integer                   :: ny
 double precision          :: xx(kmxx),xxx(kmxx)
 double precision          :: yy(ny)
 character(*),  intent(in) :: filename              ! file name for polygonfile

 integer                   :: minp0, n, k, kb, kt, ktx

 call oldfil(minp0, filename)
 call savepol()
 call reapol(minp0, 0)

 do n=1,ndxi
    call getkbotktop(n,kb,kt)
    do k = kb, kt
       xx(k-kb+1) = zws(k)-zws(k-1)
    enddo
    xx(1) = xx(1)/(s1(n)-bl(n))  ! upper layer face values
    do k = kb+1,kt
       xx(k-kb+1) = xx(k-kb+1)/(s1(n)-bl(n)) + xx(k-kb)
    enddo
    xxx(1) = 0.5d0*xx(1)   ! cell centre coordinate values
    do k = kb+1,kt
       xxx(k-kb+1) = 0.5d0*(xx(k-kb+1)+xx(k-kb))
    enddo

    ktx = kt-kb + 1
    call lineinterp(xxx, yy(kb:), ktx, xpl, ypl, npl)
 enddo

 call restorepol()

 end subroutine setinitialverticalprofilesigma

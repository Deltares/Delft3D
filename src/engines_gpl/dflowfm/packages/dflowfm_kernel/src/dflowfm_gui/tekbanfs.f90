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

subroutine tekbanfs()
use m_netw
use m_flowgeom
use m_flow, only: kbot
use m_sediment
implicit none
double precision :: x, y, z, v, hsk
integer          :: kk, n, k, kj, ncol, ndraw
COMMON /DRAWTHIS/ ndraw(50)

double precision               :: flx  (mxgr)           !< sed erosion flux (kg/s)                 , dimension = mxgr
double precision               :: seq  (mxgr)           !< sed equilibrium transport rate (kg/m/s) , dimension = mxgr
double precision               :: wse  (mxgr)           !< effective fall velocity (m/s)           , dimension = mxgr, ws*crefa=wse*seq

if (ndraw(34) <= 1 .or. jaceneqtr == 1 .or. jased == 0 ) return
call setcol(3)
do kk = 1,mxban

    call getequilibriumtransportrates(kk, seq, wse, mxgr, hsk)    ! get per netnode and store in small array seq

    n = nban(1,kk)  ! net node
    k = nban(2,kk)  ! flow node

    x = 0.5d0*(xk(n) + xz(k))
    y = 0.5d0*(yk(n) + yz(k))
    v = seq(jgrtek)
    call isocol(v,ncol)

    if (ndraw(34) == 2) then
       CALL dHTEXT(seq(jgrtek),X,Y,Z)
    else if (ndraw(34) == 3) then
       !CALL dHTEXT(seq(jgrtek)-sed(jgrtek,kbot(k)),X,Y,Z)
       CALL dHTEXT(seq(jgrtek)-sed(jgrtek,kbot(k)),X,Y,Z)
    else if (ndraw(34) == 4) then
       z = n
       CALL dHTEXT(z,X,Y,Z)
    else if (ndraw(34) == 5) then
       z = k
       CALL dHTEXT(z,X,Y,Z)
    else if (ndraw(34) == 6) then
       z = kk
       CALL dHTEXT(z,X,Y,Z)
    endif

 enddo

 end subroutine tekbanfs

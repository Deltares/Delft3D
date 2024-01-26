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

subroutine copycellstosam()
use m_samples
use m_netw
use m_missing
use m_polygon, only: NPL, xpl, ypl, zpl
use geometry_module, only: dbpinpol


implicit none
integer                       :: in, k, c
double precision, external    :: znetcell
in = -1
k  = ns

!check if the netcells are included in the polygon
LC = 0
do c = 1,nump
    if (rlin(c) .ne. dmiss) then
        CALL DBPINPOL(xzw(c), yzw(c), IN,  dmiss, JINS, NPL, xpl, ypl, zpl)
        IF (IN == 1) THEN
            LC(c) = 1
            K     = K + 1
        ENDIF
    ENDIF
ENDDO

CALL INCREASESAM(k)

!assign the calculated value in rlin
K = NS
do c = 1,nump
    IF (LC(c) == 1) THEN
        k = k + 1
        xs(k) = xzw(c) ; ys(k) = yzw(c) ; zs(k) = znetcell(k)
    endif
enddo
ns = k

end subroutine copycellstosam

!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

 double precision FUNCTION BESSJ0(X)
 implicit none
 double precision :: X
 double precision :: Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,S1,S2,S3,S4,S5,S6
 DATA P1,P2,P3,P4,P5/1.D0,-.1098628627D-2,.2734510407D-4,                &
     -.2073370639D-5,.2093887211D-6/, Q1,Q2,Q3,Q4,Q5/-.1562499995D-1,    &
     .1430488765D-3,-.6911147651D-5,.7621095161D-6,-.934945152D-7/
 DATA R1,R2,R3,R4,R5,R6/57568490574.D0,-13362590354.D0,651619640.7D0,    &
     -11214424.18D0,77392.33017D0,-184.9052456D0/,                       &
     S1,S2,S3,S4,S5,S6/57568490411.D0,1029532985.D0,                     &
     9494680.718D0,59272.64853D0,267.8532712D0,1.D0/
 double precision :: ax, z, xx

 IF(ABS(X).LT.8d0)THEN
   Y=X**2
   BESSJ0=(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
 ELSE
   AX=ABS(X)
   Z=8./AX
   Y=Z**2
   XX=AX-.785398164
   BESSJ0=SQRT(.636619772d0/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
 ENDIF
 RETURN
 END FUNCTION BESSJ0

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

  SUBROUTINE TEKLAN(NCOL)
  USE M_LANDBOUNDARY
  use m_wearelt
  USE unstruc_colors
  use unstruc_display

  implicit none
  integer :: NCOL
  integer :: NDRAW
  COMMON /DRAWTHIS/ ndraw(50)

  integer :: j1
  integer :: k
  integer :: ncl
  integer :: ncold
  double precision :: rh
  logical inview


  IF (NDRAW(3) .EQ. 0) return

  IF (NDRAW(3) .EQ. 4 .or. NDRAW(3) .EQ. 8) then
     call linewidth(3)
  endif

  CALL DISP3C(XLAN, YLAN, ZLAN, NCLAN, MXLAN, 0d0, NCOL)

  NCOLD = 0
  DO K = 1,MXLAN
     NCL = NCLAN(K)
     IF (NCL .LT. 0) THEN
        IF (NCOLD .EQ. 0) THEN
           NCOLD = ABS(NCL)
           J1    = K
        ELSE IF (ABS(NCL) .NE. NCOLD) THEN
           CALL PFILLER(XLAN(J1),YLAN(J1),K-J1,NCOLD,NCOLD)
           NCOLD = 0
        ENDIF
     ELSE IF (NCOLD .NE. 0) THEN
        CALL PFILLER(XLAN(J1),YLAN(J1),K-J1,NCOLD,NCOLD)
        NCOLD = 0
     ENDIF
  ENDDO
  if (ndraw(3) == 2 .or. ndraw(3) == 6) then
     CALL SETCOL(NCOLDG)
     rh = 0.2*rcir
     DO K = 1,MXLAN
        if ( inview(xlan(k), ylan(k) ) )  then
            !CALL PTABS( XLAN(K), YLAN(K) )
            call fbox(xlan(k)-rh, ylan(k)-rh, xlan(k)+rh, ylan(k)+rh)
        endif
     enddo
  endif

  if (ndraw(3) == 3 .or. ndraw(3) == 7) then
     CALL SETCOL(NCOLDG)
     DO K = 1,MXLAN
        if ( inview ( xlan(k), ylan(k) ) )  then
            RH = 0
            CALL DHITEXT(K,XLAN(K),YLAN(K),RH)
        endif
     enddo
  endif

  call linewidth(1)

  RETURN
  END SUBROUTINE TEKLAN

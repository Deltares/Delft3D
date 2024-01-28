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

 subroutine GETSHIPCONTROL()
    use m_ship
    implicit none

    integer :: ndraw
    COMMON /DRAWTHIS/ ndraw(50)

    integer          :: key, n

    CALL InKeyEventIMM(KEY)

    n = 0
                                 !        pijltjesbeweging
    IF (KEY .EQ. 128) THEN
       fstuw(1) = min( 1d0, fstuw(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 129) THEN
       fstuw(1) = max(-1d0, fstuw(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 130) THEN
       fROER(1) = MIN( 1D0, fROER(1) + 0.02)
       n = 1
    ELSE IF (KEY .EQ. 131) THEN
       fROER(1) = MAX(-1D0, fROER(1) - 0.02)
       n = 1
    ELSE IF (KEY .EQ. 53) THEN
       FSTUW(1) = 0D0
       FROER(1) = 0D0
       n = 1
    ENDIF

    IF (KEY .EQ. 87 .OR. KEY .EQ. 87+32) THEN ! W
       fstuw(2) = min( 1d0, fstuw(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 83 .OR. KEY .EQ.  83+32) THEN ! S
       fstuw(2) = max(-1d0, fstuw(2) - 0.02)
       n = 2
    ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
       fROER(2) = MIN( 1D0, fROER(2) + 0.02)
       n = 2
    ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
       fROER(2) = MAX(-1D0, fROER(2) - 0.02)
       n = 2
    else if (KEY .EQ. 81 .OR. KEY .EQ. 81+32) then
       FSTUW(2) = 0D0
       FROER(2) = 0D0
       n = 2
    ENDIF

    if (n > 0) THEN
        ndraw(1) = 0        ! no CLS
        call tekship()
    endif

 end subroutine getshipcontrol

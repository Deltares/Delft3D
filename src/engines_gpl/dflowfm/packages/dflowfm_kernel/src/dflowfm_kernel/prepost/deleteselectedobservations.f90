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

   SUBROUTINE deleteSelectedObservations()
   use m_observations
      USE M_SAMPLES
      use M_POLYGON
      use m_missing
      use geometry_module, only: pinpok
      implicit none

      integer :: i
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi

      IF (Npl .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO DELETE all Observation Points? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         call deleteObservations()
         RETURN
      ENDIF

      DO 10 I = 1,numobs
            CALL PINPOK(xobs(I), yobs(I), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) then
                call deleteObservation(I)
            end if
   10 CONTINUE
    call purgeObservations()

      RETURN

   END SUBROUTINE deleteSelectedObservations

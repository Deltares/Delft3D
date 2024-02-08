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

   SUBROUTINE deleteSelectedCrossSections()
      USE m_monitoring_crosssections
      use M_POLYGON
      USE M_MISSING
      implicit none

      integer :: i, j
      integer :: inhul
      integer :: ja
      integer :: k
      integer :: key
      integer :: nsol
      double precision :: rd
      double precision :: xi
      double precision :: yi
      logical :: jaAllPoints

      !IF (Npl .LE. 2) THEN
         CALL CONFRM('NO POLYGON will be used, SO DELETE all cross sections? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
         call delCrossSections()
         RETURN
!      ENDIF

      I = 1
      DO
        if (I > ncrs) exit
        jaAllPoints = .true.
        DO j=1,crs(I)%path%np
            CALL PINPOK(crs(i)%path%xp(j), crs(i)%path%yp(j), Npl, Xpl, Ypl, INHUL, jins, dmiss)
            jaAllPoints = jaAllPoints .and. (INHUL==1)
        enddo
        if (jaAllPoints) then
 !AvD: Disabled           call delCrossSection(I)
            ! cross sections are shifted to the left, so don't increment I.
        else
            I = I+1
        end if
      end do

      RETURN

   END SUBROUTINE deleteSelectedCrossSections

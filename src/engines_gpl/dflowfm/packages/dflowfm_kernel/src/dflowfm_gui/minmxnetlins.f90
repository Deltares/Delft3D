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

  SUBROUTINE MINMXNETLINS()

  use m_netw
  use m_missing

  implicit none
  double precision :: dv
  integer :: i
  integer :: jaauto
  integer :: k1
  integer :: k2
  integer :: l
  integer :: ncols
  integer :: nie
  integer :: nis
  integer :: nv
  double precision :: rd
  double precision :: rmax
  double precision :: rmin
  double precision :: val
  double precision :: vmax
  double precision :: vmin
  double precision :: xp1
  double precision :: xp2
  double precision :: yp1
  double precision :: yp2
  double precision :: zp1
  double precision :: zp2
  logical inview
  ! BEPAAL MINIMUM EN MAXIMUM VAN WAARDES BINNEN VIEWING AREA
  COMMON /DEPMAX2/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  IF (JAAUTO > 0) THEN
     RMIN =  1.0D30
     linmin = 0
     RMAX = -1.0d30
     linmax = 0
     DO L = 1,NUML
        K1   = KN(1,L)
        K2   = KN(2,L)
        IF (RLIN(L) .NE. DMISS .AND. K1 .NE. 0 .AND. K2 .NE. 0) THEN
           XP1  = XK(K1)
           YP1  = YK(K1)
           ZP1  = ZK(K1)
           XP2  = XK(K2)
           YP2  = YK(K2)
           ZP2  = ZK(K2)
           IF (INVIEW(XK(K1),YK(K1)) .OR. INVIEW(XK(K2),YK(K2)) ) THEN
               RD = RLIN(L)
               IF (RD < RMIN) THEN
                   RMIN = RD
                   LINMIN = L
               ENDIF
               IF (RD > RMAX) THEN
                   RMAX = RD
                   LINMAX = L
               ENDIF
           ENDIF
        ENDIF
     ENDDO

     VMAX = RMAX
     VMIN = RMIN
  ENDIF

  DV   = VMAX - VMIN
  DO I = 1,NV
     VAL(I) = VMIN + (I-1)*DV/(NV-1)
  ENDDO

  RETURN
  END SUBROUTINE MINMXNETLINS

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

SUBROUTINE MINMXNETCELLS()

  use m_netw
  use m_flowgeom
  use m_missing

  implicit none

  double precision :: dv
  integer :: i
  integer :: jaauto
  integer :: k
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

  double precision, external :: znetcell
  logical inview

  ! BEPAAL MINIMUM EN MAXIMUM VAN DIEPTES BINNEN VIEWING AREA
  COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO

  IF (JAAUTO > 0) THEN
     RMIN =  1.0D30
     NODMIN = 0
     RMAX = -1.0D30
     NODMAX = 0
     DO K = 1,max(NUMP,nump1d2d)
        IF ( INVIEW(XZ(K),YZ(K)) ) THEN
           RD = RLIN(K)
           IF (rd .ne. dmiss) then
              IF (RD < RMIN ) THEN
                 RMIN = RD
                 netcelMIN = K
              ENDIF
              IF (RD > RMAX) THEN
                 RMAX = RD
                 netcelMAX = K
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
  END SUBROUTINE MINMXNETCELLS

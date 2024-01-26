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

      SUBROUTINE ONSAMELINE(IPT,MP,NP,JA)
      implicit none
      integer :: mp, np, ja, ipt
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: md, nd
      JA = 1
      IF (ITYPE .EQ. 1) THEN
         IF (IPT .EQ. 1 .AND. MB(2) .NE. 0) THEN
            MD = MP - MB(2)
            ND = NP - NB(2)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ELSE IF (IPT .EQ. 2) THEN
            MD = MP - MB(1)
            ND = NP - NB(1)
            IF (MD .NE. 0 .AND. ND .NE. 0) JA = 0
         ENDIF
      ENDIF
      RETURN
      END subroutine onsameline

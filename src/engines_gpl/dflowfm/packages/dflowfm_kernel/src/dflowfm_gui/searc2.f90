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

      SUBROUTINE SEARC2(NAHEAD,NLEVEL,HLPTXT,NUMTXT,LOOKUP,NUMCHC,JOFND)
      implicit none
      integer :: jofnd
      integer :: k,len
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Search everywhere
      CHARACTER HLPTXT(NUMTXT)*(*),LOOKUP*20

      LEN   = len_trim(LOOKUP)
      IF (LEN .EQ. 0) RETURN

      JOFND = 0
      K     = NUMCHC - NAHEAD

   10 CONTINUE
      K = K + NAHEAD
      IF (K .GT. NUMTXT .OR. K .LT. 1) THEN
         IF (JOFND .EQ. 0) CALL OKAY(0)
         RETURN
      ELSE
         IF (INDEX(HLPTXT(K),LOOKUP(1:LEN)) .EQ. 0) GOTO 10
      ENDIF

      JOFND  = 1
      NUMCHC = MIN(NUMTXT,K + 1)
      RETURN
      END

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

   !> Reads raw restart data from a formatted restart file by wrirst.
   !! Water levels and velocities are directly stored into the flow arrays.
   SUBROUTINE REARST(Mrst,JA)
   use unstruc_model
   USE UNSTRUC_MESSAGES
   USE M_FLOWTIMES
   USE M_FLOW
   USE M_FLOWGEOM
   implicit none
   INTEGER, intent(inout)  :: Mrst  !< Input file pointer (should already be open)
   integer, intent(out)    :: ja    !< Return status (0 = success)

   integer   :: k
   integer   :: l
   INTEGER   :: NDXR, LNXR       ! alleen binnen deze subroutine
   LOGICAL   :: JAWEL
   DOUBLE PRECISION    :: DUM

   ja = 0
 ! READ(Mrst,*)  REFDATLOC, TSTART_USERLOC, NDXR, LNXR
   READ(Mrst,*)  DUM , DUM,                 NDXR, LNXR

   IF (NDXR .NE. NDX .OR. LNXR .NE. LNX) THEN
       WRITE(MSGBUF, '(A)' ) 'DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS'  ; CALL MSG_FLUSH()
       CALL QNERROR        ( 'DIMENSIONS ON RESTART FILE NOT EQUAL TO CURRENT MODEL DIMENSIONS' , ' ', ' ')
       ja = 1
   ENDIF

   READ(Mrst,*)
   DO K = 1,NDX
      READ(Mrst,*, END = 999, ERR = 888) S0(K)
   ENDDO
   S0 = MAX(BL, S0)
   s1 = s0

   READ(Mrst,*)

   DO L = 1,LNX
      READ(Mrst,*, END = 999) U0(L)
   ENDDO
   call doclose(mrst)
   u1 = u0

   RETURN

   888 ja = 1
   return
   999 CALL QNEOFERROR(MRST)
   call doclose(mrst)
   ja = 1

   END SUBROUTINE reaRST

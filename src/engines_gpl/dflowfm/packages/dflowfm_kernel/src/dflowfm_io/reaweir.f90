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

      SUBROUTINE REAweir(   MMDD, JA)
      use m_missing
      use m_fixedweirs
      USE M_GRID
      implicit none


      integer :: mmdd, ja, m1, n1, m2, n2, L1, L2, L3, L4, L5
      integer :: m, n, MOUT
      double precision :: af, hu, hv, Du1, Du2, Dv1, Dv2

      CHARACTER REC*132

      JA = 0

      CALL NEWFIL(MOUT, 'WEIRS.POL')

    5 CONTINUE

      READ(MMDD,'(A)',END = 777) REC

      IF ( index(rec,'#') ==0) THEN

          READ (REC(2:), *, ERR=999)   M, N, HU, Du1, Du2, HV, Dv1, Dv2

          IF (HU > 0) THEN
             WRITE(MOUT,*) XC(M,N  ) , YC(M,N  ), HU, DU1, DU2
             WRITE(MOUT,*) XC(M,N-1) , YC(M,N-1), HU, DU1, DU2
             WRITE(MOUT,*) DMISS, DMISS, DMISS
          ENDIF

          IF (HV > 0) THEN
             WRITE(MOUT,*) XC(M  ,N) , YC(M  ,N), HV, DV1, DV2
             WRITE(MOUT,*) XC(M-1,N) , YC(M-1,N), HV, DV1, DV2
             WRITE(MOUT,*) DMISS, DMISS, DMISS
          ENDIF

      ENDIF

      GOTO 5


  777 CALL DOCLOSE (MMDD)
      CALL DOCLOSE (MOUT)
      JA = 1
      RETURN

  999 CONTINUE
      CALL QNEOFERROR(MMDD)
      CALL READYY('Reading SIMONA *.bottom File',-1d0)
      CALL DOCLOSE (MMDD)
      JA = 0
      RETURN

  888 CALL QNREADERROR('Reading ERROR SIMONA WEIR File', REC, MMDD)
      CALL DOCLOSE (MMDD)
      JA = 0
      END SUBROUTINE REAWEIR

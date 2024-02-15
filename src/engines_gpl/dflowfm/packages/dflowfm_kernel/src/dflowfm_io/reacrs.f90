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

      SUBROUTINE REAcrs(   MMDD, JA)
      USE M_GRID
      use m_missing

      implicit none


      integer :: mmdd, ja, m1, n1, m2, n2, MH, NH , NR2

      integer :: m, n, MOUT
      double precision :: af, hu, hv, d

      CHARACTER REC*132

      JA  = 0
      NR2 = 2

      CALL NEWFIL(MOUT, 'sections_crs.pli')

    5 CONTINUE

      READ(MMDD,'(A)',END = 777) REC

      IF ( index(rec,'#') ==0) THEN

          READ (REC(21:), *, ERR=999)   M1, N1, M2, N2

          IF (M1 > M2) THEN
             MH = M2; M2 = M1; M1 = MH
          ENDIF
          IF (N1 > N2) THEN
             NH = N2; N2 = N1; N1 = NH
          ENDIF

     !     WRITE(MOUT,'(A  )') REC(1:20)
          IF (M1 == M2) THEN
             WRITE(MOUT,'(A  )') REC(1:20)
             WRITE(MOUT, '(2I8)') N2-N1+2, NR2
             DO N = N1-1,N2
                WRITE (MOUT,*) XC(M1,N), YC(M1,N)
             ENDDO
          ENDIF

          IF (N1 == N2) THEN
             if (m1 == m2) then
                WRITE(MOUT,'(A  )') REC(1:20)//'b'
             else
                WRITE(MOUT,'(A  )') REC(1:20)
             endif
             WRITE(MOUT, '(2I8)') M2-M1+2, NR2
             DO M = M1-1,M2
                WRITE (MOUT,*) XC(M,N1), YC(M,N1)
             ENDDO
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
      END SUBROUTINE REAcrs

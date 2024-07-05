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

    SUBROUTINE REALAN( MLAN, ANTOT)
      use m_polygon
      use M_landboundary
      USE M_MISSING
      implicit none
      integer, intent(inout)                ::  mlan
      integer, intent(inout), optional      ::  antot

      integer :: i
      integer :: ncl
      integer :: newlin
      integer :: nkol
      integer :: nrow
      integer :: ntot, n, k, kd, ku
      double precision :: xlr

      CHARACTER CHARMC*5, MATR*4, REC*132
      DOUBLE PRECISION :: XL, YL, ZL

      if (present(antot)) then
         NTOT   = antot
      else
         NTOT   = 0
      endif

      if (ntot == 0) then
         call increaselan(10000)
      endif

      CALL READYY('READING land boundary',0d0)
   10 CONTINUE
      READ(MLAN,'(A)',END=777,ERR=887) MATR
      IF (MATR(1:1) .EQ. '*') GOTO 10

      READ(MLAN,'(A)',END = 777) REC
      READ(REC,*,ERR = 666) NROW, NKOL

      NEWLIN = 0
      do I = 1,NROW
         IF (NTOT .GE. MAXLAN-1) THEN
            call increaselan(NTOT+1)
         ENDIF
         READ(MLAN,'(A)',END = 999) REC
         NCL = 0
         ZL  = 0
         if (nkol == 2) then
            READ (REC,*,ERR=881) XL,YL
         else if (nkol == 3) then
            READ (REC,*,ERR=881) XL,YL,NCL
         else if (nkol == 4) then
            READ (REC,*,ERR=881) XL,YL,ZL,NCL
         endif

         XLR = XL

   881   IF (XL .EQ. 999.999d0 .OR. XLR == 999.999d0) THEN
            XL  = dmiss
            YL  = dmiss
            ZL  = dmiss
            NCL = 0
         ENDIF
         IF (NTOT == 0) THEN
            NTOT  = NTOT + 1
            MXLAN = NTOT
            XLAN(NTOT)    = XL
            YLAN(NTOT)    = YL
            ZLAN(NTOT)    = ZL
            NCLAN(NTOT)   = NCL
         ELSE IF (XL .ne. XLAN(NTOT) .or. YL .ne. YLAN(NTOT) )  THEN
            NTOT  = NTOT + 1
            MXLAN = NTOT
            XLAN(NTOT)    = XL
            YLAN(NTOT)    = YL
            ZLAN(NTOT)    = ZL
            NCLAN(NTOT)   = NCL
         ENDIF
         IF (MOD(I,1000) .EQ. 0) THEN
            CALL READYY(' ',MIN( 1d0,dble(I)/MAXLAN ) )
         ENDIF
      end do
      NTOT  = NTOT + 1
      MXLAN = NTOT
      XLAN(NTOT)  = dmiss
      YLAN(NTOT)  = dmiss
      ZLAN(NTOT)  = dmiss

      GOTO 10

  777 CONTINUE
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)

      if (present(antot)) then
         antot = NTOT
      endif

      return

      n = 1                                    ! remove double points in lineseg oriented files
      xpl(n) = xlan(1) ; ypl(n) = ylan(1)
      do k  = 2,mxlan-1
         kd = k - 1; ku = k + 1
         if (xlan(k) == dmiss .and. xlan(kd) == xlan(ku) .and. ylan(kd) == ylan(ku) ) then

         else
            n = n + 1
            xpl(n) = xlan(k) ; ypl(n) = ylan(k)
         endif
      enddo
      n = n + 1
      xpl(n) = xlan(mxlan) ; ypl(n) = ylan(mxlan)

      npl = n

      RETURN

  666 CALL QNREADERROR('SEARCHING NROWS,NCOLS, BUT GETTING', REC, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  888 CALL QNREADERROR('SEARCHING COORDINATES, BUT GETTING', REC, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  887 CALL QNREADERROR('EXPECTING 4 CHAR, BUT GETTING', MATR, MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

  999 CALL QNEOFERROR(MLAN)
      MXLAN = NTOT
      CALL READYY(' ', 1d0)
      CALL READYY(' ',-1d0)
      call doclose (MLAN)
      RETURN

      END

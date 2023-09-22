!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dlwq5h

      implicit none
      
      contains


      SUBROUTINE compact_usefor_list ( LUNUT  , IAR    , ITMNR  , NOITM  , IDMNR  ,
     *                    NODIM  , IORDER , CNAMES , IOFFI  , IOFFC  ,
     *                             IODS   , IOFFD, I , ICNT, IERR, IWAR)
!
!
!     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED            : October '00  by L. Postma
!
!     MODIFIED           :
!
!     FUNCTION           : Compacts USEFOR lists if unresolved externals
!
!     SUBROUTINES CALLED : none
!
!     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
!                          LUN(29) = unit formatted output file
!
!     PARAMETERS    :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ---------------------------------------------------------
!     LUNUT   INTEGER    1         INPUT   unit number for ASCII output
!     IAR     INTEGER  IIMAX       IN/OUT  integer   workspace
!     ITMNR   INTEGER    1         IN/OUT  nr of items for assignment
!     NOITM   INTEGER    1         IN      nr of items in computational rule
!     IDMNR   INTEGER    1         IN/OUT  nr of subst for assignment
!     NODIM   INTEGER    1         IN      nr of subst in computational rule
!     IORDER  INTEGER    1         IN      1 = items first, 2 is subst first
!     CNAMES  CHAR*(*)  NITM       INPUT   Items to check for presence
!     IOFFI   INTEGER    1         IN/OUT  Offset in input array
!     IOFFC   INTEGER    1         IN/OUT  Offset in character array
!     IOFFD   INTEGER    1         IN/OUT  Base offset in both arrays
!     IODS    INTEGER    1         INPUT   Shift counter ODS files
!     I       INTEGER    1         INPUT   loop counter
!     ICNT    INTEGER    1         IN/OUT  counter
!
!
      use timers       !   performance timers

      CHARACTER*(*) CNAMES(*)
      DIMENSION     IAR(*)
      CHARACTER*20  CHULP,  MESSAGE_TYPE
      integer(4) :: ithndl = 0
      integer    :: I1, I3, I4, I5
      integer    :: lunut, I, icnt, ioffc, iorder, ntt, idmnr, nitm, nodim
      integer    :: itmnr, noitm, I2, iar, ioffd, ishft, ioffi, iods
      
      INTEGER :: KKVACA, IERR, IWAR
      
      
      IERR = -1
      MESSAGE_TYPE = "WARNING"
      if (timon) call timstrt( "compact_usefor_list", ithndl )
!
!       Write message
!
      WRITE ( LUNUT ,   *  )
      !WRITE ( LUNUT , 1010 ) I+ICNT, CNAMES(I+IOFFC)
      IF ( IORDER == 1 ) THEN ! items first
          NTT  = IDMNR
          NITM = NODIM
      ELSE ! subst first
          NTT  = ITMNR
          NITM = NOITM
      ENDIF
!
!       Look backwards
!
      I4 = 0
      DO I1 = I,1,-1
         I2 = IAR(I1+IOFFC)
         IF ( I2 > -100000 ) EXIT
      END DO
!
!       Additional messages for this sequence
      IF ( I2 <= 0 .AND. I2 > -100000 ) THEN
!       Try to find the reference
         DO I3 = 1 , I
            I5 = IAR(I3+IOFFC)
            IF ( I5 > 0 )   I4 = IAR(I3+IOFFC)
            IF ( I5 <= 0 .AND. I5 > -100000 )   I4 = I4 + 1
         END DO
         CHULP = CNAMES(I4+IOFFD)
         IF ( CNAMES(I+IOFFC) /= CHULP ) THEN
            IF ( IORDER == 2 ) THEN
               WRITE (LUNUT,1030) I4,CHULP
            ELSE
               WRITE (LUNUT,1040) I4,CHULP
            END IF
         END IF
      ELSE IF ( I2 > 0 .AND. I2 <  100000 ) THEN
         I4 = I2
         CHULP = CNAMES( I2+IOFFD)
!         WRITE(*,*)
!         DO KKVACA = 1, LEN(CNAMES)
!             WRITE(*,*) "H->", IOFFC, KKVACA, CNAMES(KKVACA)
!         END DO
         
         IF ( CNAMES(I+IOFFC) == CHULP ) THEN
             IWAR = IWAR + 1
             MESSAGE_TYPE = "WARNING"
             WRITE ( LUNUT , 1010 ) MESSAGE_TYPE, I+ICNT, CNAMES(I+IOFFC)
         ELSE
             MESSAGE_TYPE = "ERROR"
             WRITE ( LUNUT , 1010 ) MESSAGE_TYPE, I+ICNT, CNAMES(I+IOFFC)
             IERR = 1
             IF ( IORDER == 2 ) THEN
               WRITE (LUNUT,1030)  MESSAGE_TYPE, I2, CHULP
            ELSE
               WRITE (LUNUT,1040)  MESSAGE_TYPE, I2, CHULP
            END IF
         END IF
      ENDIF
      I2 = I4
!
!     Determine the shift in locations
      ISHFT = 1
      DO I4 = I1+1,NITM
         I3 = IAR(I4+IOFFC)
         IF ( I3 > -1000000 ) EXIT
         ISHFT = ISHFT + 1
      END DO
!
!     Shift the third array heap
      DO I4 = I1, NITM
         IAR   (I4+IOFFI) = IAR(I4+IOFFI+ISHFT)
      END DO
!
!     Shift the second array heap
      DO I4 = I1, NITM*2+IODS
         IAR   (I4+IOFFC) = IAR   (I4+IOFFC+ISHFT)
         CNAMES(I4+IOFFC) = CNAMES(I4+IOFFC+ISHFT)
      END DO
      NITM  = NITM  - ISHFT
      IOFFI = IOFFI - ISHFT
      IOFFC = IOFFC - 1
      IOFFI = IOFFI - 1
      ICNT  = ICNT  + ISHFT
!
!     Shift the base array heap
      DO I5 = I2+IOFFD , NTT+IOFFD+NITM*2+IODS
         IAR   (I5) = IAR   (I5+1)
         CNAMES(I5) = CNAMES(I5+1)
      END DO
!
!      Renumber the second array heap
      DO I4 = I1 , NITM
         IF ( IAR(I4+IOFFC) > I2 ) THEN
             IAR(I4+IOFFC) = IAR(I4+IOFFC) -1
         END IF
      END DO
!
!      Update totals
      IF ( IORDER == 1 .OR.  IODS > 0 ) THEN
         IDMNR = IDMNR-1
         NODIM = NODIM-ISHFT
      ELSE IF ( IORDER == 2 .AND. IODS == 0 ) THEN
         ITMNR = ITMNR-1
         NOITM = NOITM-ISHFT
      ENDIF
!
      if (timon) call timstop( ithndl )
      RETURN
!
 1010 FORMAT ( ' ', A, ': Input item : ',I3,' not resolved: ',A)
 1030 FORMAT ( ' ', A, ': Item number: ',I3,' also not resolved: ',A)
 1040 FORMAT ( ' ', A, ': Substance  : ',I3,' also not resolved: ',A)
!
      END subroutine compact_usefor_list

      end module m_dlwq5h

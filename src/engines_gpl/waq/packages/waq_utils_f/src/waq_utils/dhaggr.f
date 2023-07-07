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
      module m_dhaggr

      use m_srstop
      use m_monsys
      use m_getcom

      implicit none

      contains


      SUBROUTINE DHAGGR ( NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +                    NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +                    IPGRID, IAGTYP, ARRINP, WEIGHT, ARRHLP,
     +                    ARROUT)
!
!     Deltares
!
!     Function            : Aggregates value to coarser grid
!
!     Subroutines called  : GETMLU, Get unit number report file
!                           SRSTOP, Stops execution
!                           ZERO  , Zero's a real array
!
!     Arguments           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOSEG1  INTEGER  1          INPUT   Number of segments on finer grid
!     NOSEG2  INTEGER  1          INPUT   Number of segments on coarser grid
!     IPGRID  INTEGER  NOSEG1     INPUT   Grid pointers to coarser grid
!     IAGTYP  INTEGER  1          INPUT   Aggregation type
!                                         IAGTYP_ACCUM = Accumulation
!                                         IAGTYP_AVG   = Average
!                                         IAGTYP_WAVG  = Average weighted with WEIGHT
!                                         IAGTYP_MIN   = Using minimum value
!                                         IAGTYP_ACSGN = Using signed accumulation (for combining flows in opposite directions)
!     ARRINP  REAL     NOSEG1     INPUT   Array to be aggregated
!     WEIGHT  REAL     NOSEG1     INPUT   Weigth in averaging
!     ARRHLP  REAL     NOSEG2     LOCAL   Local help array
!     ARROUT  REAL     NOSEG2     OUTPUT  Aggregated array
      
      use m_aggregation_types
!
!     Declaration of arguments
!
      INTEGER        NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +               NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +               IAGTYP
      INTEGER        IPGRID(NOSEG1)
      REAL           ARRINP(NOTOTI,NOSEG1) , WEIGHT(NOTOTW,NOSEG1) ,
     +               ARRHLP(NOTOTH,NOSEG2) , ARROUT(NOTOTO,NOSEG2)
!
!     Local declaration
!
!     ISEG1   INTEGER  1          LOCAL   Segment index finer grid
!     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
!     LUREP   INTEGER  1          LOCAL   Unit number report file
!
      INTEGER        ISEG1 , ISEG2 , LUREP
      real                     :: vmin          ! minimum in weight variable
      logical                  :: lfound        ! command line option found
      integer                  :: idummy        !
      character                :: cdummy        !
      integer                  :: ierr2         !
      integer                  :: lunrep        ! report file
      logical                  :: lfirst = .true.
      real, parameter          :: rmiss = -999.

      save           lfirst, vmin


      if ( lfirst ) then
         lfirst = .false.
         call getcom('-vmin',2,lfound,idummy,vmin,cdummy,ierr2)
         if ( lfound ) then
            call getmlu(lunrep)
            if ( ierr2 .ne. 0 ) then
               write(*,*) 'error commandline option -vmin value could not be interpreted'
               write(lunrep,*) 'error commandline option -vmin value could not be interpreted'
               call srstop(1)
            endif
            write(*,*) ' commandline option -vmin ',vmin
            write(lunrep,*) ' commandline option -vmin ',vmin
         else
            vmin = 0.0
         endif
      endif
!
!     Zero accumulation arrays
!
      IF ( IAGTYP .EQ. IAGTYP_ACCUM  .OR. IAGTYP .EQ. IAGTYP_ACSGN ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
         ENDDO
      ELSEIF ( IAGTYP .EQ. IAGTYP_AVG    .OR. IAGTYP .EQ. IAGTYP_WAVG ) THEN
         DO ISEG2 = 1 , NOSEG2
            ARROUT(ISYSO,ISEG2) = 0.0
            ARRHLP(ISYSH,ISEG2) = 0.0
         ENDDO
      ENDIF
!
!     Accumulate
!
      IF ( IAGTYP .EQ. IAGTYP_ACCUM ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = ABS(IPGRID(ISEG1))
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. IAGTYP_AVG ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
               ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) + 1.0
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. IAGTYP_WAVG ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1) *
     +                               WEIGHT(ISYSW,ISEG1)
               ARRHLP(ISYSH,ISEG2) = ARRHLP(ISYSH,ISEG2) +
     +                               WEIGHT(ISYSW,ISEG1)
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. IAGTYP_MIN ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               IF ( WEIGHT(ISYSW,ISEG1) .GT. VMIN ) THEN
                  IF ( ARRINP(ISYSI,ISEG1) .NE. RMISS ) THEN
                     IF ( ARROUT(ISYSO,ISEG2) .NE. RMISS ) THEN
                        ARROUT(ISYSO,ISEG2) = MIN(ARROUT(ISYSO,ISEG2),ARRINP(ISYSI,ISEG1))
                     ELSE
                        ARROUT(ISYSO,ISEG2) = ARRINP(ISYSI,ISEG1)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSEIF ( IAGTYP .EQ. IAGTYP_ACSGN ) THEN
         ! modified version of IAGTYP == IAGTYP_ACCUM : deduct values when pointer is negative
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = ABS(IPGRID(ISEG1))
            IF ( IPGRID(ISEG1) .GT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) +
     +                               ARRINP(ISYSI,ISEG1)
            ELSEIF ( IPGRID(ISEG1) .LT. 0 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) -
     +                               ARRINP(ISYSI,ISEG1)
            ENDIF
         ENDDO
      ELSE
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IAGTYP
         CALL SRSTOP(1)
      ENDIF
!
!     Average
!
      IF ( IAGTYP .EQ. IAGTYP_AVG .OR. IAGTYP .EQ. IAGTYP_WAVG ) THEN
         DO ISEG2 = 1 , NOSEG2
            IF ( ABS(ARRHLP(ISYSH,ISEG2)) .GT. 1.E-20 ) THEN
               ARROUT(ISYSO,ISEG2) = ARROUT(ISYSO,ISEG2) /
     +                               ARRHLP(ISYSH,ISEG2)
            ELSE
               ARROUT(ISYSO,ISEG2) = 0.0
            ENDIF
         ENDDO
      ENDIF
!
      RETURN
 2000 FORMAT ( ' ERROR: undefind aggregation type in DHAGGR :',I8 )
      END
      end module m_dhaggr

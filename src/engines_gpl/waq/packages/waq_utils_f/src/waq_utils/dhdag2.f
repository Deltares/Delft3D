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
      module m_dhdag2
      use m_srstop
      use m_monsys


      implicit none

      contains


      SUBROUTINE DHDAG2 ( NOSEG1, NOSEG2, NOTOTI, NOTOTW, NOTOTH,
     +                    NOTOTO, ISYSI , ISYSW , ISYSH , ISYSO ,
     +                    NSYS  , IPGRID, IDATYP, ARRINP, WEIGHT,
     +                    ISWCUM, ARRHLP, ARROUT)
!
!     Deltares
!
!     Created             : June 1998 by Jan van Beek
!
!     Function            : Dis-aggregates value to finer grid
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
!     IDATYP  INTEGER  1          INPUT   Dis-aggregation type
!                                         1 = expansion of value
!                                         2 = distribute by WEIGHT
!                                         3 = distribute
!     ARRINP  REAL     NOSEG2     INPUT   Array to be dis-aggregated
!     WEIGHT  REAL     NOSEG1     INPUT   Weigth in dis-aggregation
!     ISWCUM  INTEGER  1          INPUT   Accummulaton in ARROUT switch (0=no/1=yes)
!     ARRHLP  REAL     NOSEG2     LOCAL   Local help array
!     ARROUT  REAL     NOSEG1     OUTPUT  Dis-aggregated array
!
!     Declaration of arguments
!
      INTEGER        NOSEG1, NOSEG2, IDATYP, ISWCUM, NOTOTI, NOTOTW, NOTOTH, NOTOTO
      INTEGER        ISYS, ISYSO, ISYSH, ISYSW, ISYSI, NSYS
      INTEGER        IPGRID(NOSEG1)
      REAL           ARRINP(NOTOTI,NOSEG2) , WEIGHT(NOTOTW,NOSEG1) ,
     +               ARRHLP(NOTOTH,NOSEG2) , ARROUT(NOTOTO,NOSEG1)
!
!     Local declaration
!
!     ISEG1   INTEGER  1          LOCAL   Segment index finer grid
!     ISEG2   INTEGER  1          LOCAL   Segment index coarser grid
!     LUREP   INTEGER  1          LOCAL   Unit number report file
!
      INTEGER        ISEG1 , ISEG2 , LUREP
!
!     Zero arrays
!
      IF ( ISWCUM .EQ. 0 ) THEN
         DO ISEG1 = 1 , NOSEG1
            DO ISYS = 0 , NSYS - 1
               ARROUT(ISYSO+ISYS,ISEG1) = 0.0
            ENDDO
         ENDDO
      ENDIF
      IF ( IDATYP .EQ. 2 .OR. IDATYP .EQ. 2 ) THEN
         DO ISEG2 = 1 , NOSEG2
            DO ISYS = 0 , NSYS - 1
               ARRHLP(ISYSH+ISYS,ISEG2) = 0.0
            ENDDO
         ENDDO
      ENDIF
!
!     Accumulate WEIGHT in ARRHLP
!
      IF ( IDATYP .EQ. 3 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  ARRHLP(ISYSH+ISYS,ISEG2) = ARRHLP(ISYSH+ISYS,ISEG2) +
     +                                       1.0
               ENDDO
            ENDIF
         ENDDO
      ENDIF
      IF ( IDATYP .EQ. 2 ) THEN
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  ARRHLP(ISYSH+ISYS,ISEG2) = ARRHLP(ISYSH+ISYS,ISEG2) +
     +                                       WEIGHT(ISYSW+ISYS,ISEG1)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
!
!     Expand or distribute
!
      IF ( IDATYP .EQ. 1 ) THEN
!
!        Expand
!
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               IF ( ISWCUM .EQ. 0 ) THEN
                  DO ISYS = 0 , NSYS - 1
                     ARROUT(ISYSO+ISYS,ISEG1)=ARRINP(ISYSI+ISYS,ISEG2)
                  ENDDO
               ELSE
                  DO ISYS = 0 , NSYS - 1
                     ARROUT(ISYSO+ISYS,ISEG1)=ARROUT(ISYSO+ISYS,ISEG1)+
     +                                        ARRINP(ISYSI+ISYS,ISEG2)
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ELSEIF ( IDATYP .EQ. 3 ) THEN
!
!        Distribute by weight
!
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  IF ( ABS(ARRHLP(ISYSH+ISYS,ISEG2)) .GT. 1.E-20 ) THEN
                     IF ( ISWCUM .EQ. 0 ) THEN
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARRINP(ISYSI+ISYS,ISEG2) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ELSE
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARROUT(ISYSO+ISYS,ISEG1) +
     +                                       ARRINP(ISYSI+ISYS,ISEG2) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSEIF ( IDATYP .EQ. 2 ) THEN
!
!        Distribute
!
         DO ISEG1 = 1 , NOSEG1
            ISEG2 = IPGRID(ISEG1)
            IF ( ISEG2 .GT. 0 ) THEN
               DO ISYS = 0 , NSYS - 1
                  IF ( ABS(ARRHLP(ISYSH+ISYS,ISEG2)) .GT. 1.E-20 ) THEN
                     IF ( ISWCUM .EQ. 0 ) THEN
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARRINP(ISYSI+ISYS,ISEG2) *
     +                                       WEIGHT(ISYSW+ISYS,ISEG1) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ELSE
                        ARROUT(ISYSO+ISYS,ISEG1) =
     +                                       ARROUT(ISYSO+ISYS,ISEG1) +
     +                                       ARRINP(ISYSI+ISYS,ISEG2) *
     +                                       WEIGHT(ISYSW+ISYS,ISEG1) /
     +                                       ARRHLP(ISYSH+ISYS,ISEG2)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ELSE
!
!        ERROR , undefined dis-aggregation type
!
         CALL GETMLU(LUREP)
         WRITE(LUREP,2000) IDATYP
         CALL SRSTOP(1)
      ENDIF
!
      RETURN
 2000 FORMAT ( ' ERROR: undefined dis-aggregation type in DHDAGG :',I8 )
      END
      end module m_dhdag2

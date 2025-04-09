!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_rd_tabr4
   use m_waq_precision

   implicit none

contains

   subroutine RD_TABR4(DEFFDS, &
                       NO_OUTP_MAX, NO_OUTP, &
                       R4_PID, R4_IID, &
                       R4_NUMB, R4_DOC, &
                       R4_SEX, LUNREP, &
                       IERROR)
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_R4 group from NEFIS file
!
!     FILES              :  NEFIS file assumed opened
!
!     SUBROUTINES CALLED :
!
!     ARGUMENTS
!
!     NAME    TYPE     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DEFFDS       INT    2993    I/O     Definition file descriptor
!     DATFDS       INT    999     I/O     Data file descriptor
!     NO_OUTP_MAX  INT            I       maximum number of rows in table r4
!     NO_OUTP      INT            O       number of rows in table r4
!     R4_PID       CHA*10 NO_OUTP O       process identification
!     R4_IID       CHA*10 NO_OUTP O       item identification
!     R4_NUMB      INT    NO_OUTP O       serial number
!     R4_DOC       CHA*1  NO_OUTP O       documneted y/n
!     R4_SEX       INT    NO_OUTP O       segment or exchange
!     LUNREP       INT    1       I       Unit number report file
!     IERROR       INT    1       O       Error
!
!     IMPLICIT NONE for extra compiler checks
!     SAVE to keep the group definition intact
!
      implicit none
      save
!
!     declaration of arguments
!
      integer(kind=int_wp) :: NO_OUTP_MAX, NO_OUTP, &
                              LUNREP, IERROR
      integer(kind=int_wp) :: DEFFDS
      character(len=10) R4_PID(NO_OUTP_MAX)
      character(len=10) R4_IID(NO_OUTP_MAX)
      integer(kind=int_wp) :: R4_NUMB(NO_OUTP_MAX)
      character(len=1) R4_DOC(NO_OUTP_MAX)
      integer(kind=int_wp) :: R4_SEX(NO_OUTP_MAX)
!
!     Local variables
!
!     GRPNAM  CHAR*16     1       LOCAL   group name (table)
!     NELEMS  INTEGER     1       LOCAL   number of elements in group (=cell)
!     ELMNMS  CHAR*16  NELEMS     LOCAL   name of elements on file
!     ELMTPS  CHAR*16  NELEMS     LOCAL   type of elements
!     ELMDMS  INTEGER  6,NELEMS   LOCAL   dimension of elements
!     NBYTSG  INTEGER  NELEMS     LOCAL   length of elements (bytes)
!
      integer(kind=int_wp) :: NELEMS
      parameter(NELEMS=6)
!
      integer(kind=int_wp) :: I, IELM, &
                              BUFLEN
      integer(kind=int_wp) :: ELMDMS(2, NELEMS), NBYTSG(NELEMS), &
                              UINDEX(3)
      character(len=16) GRPNAM
      character(len=16) ELMNMS(NELEMS), ELMTPS(NELEMS)
      character(len=64) ELMDES(NELEMS)
!
!     External NEFIS Functions
!
      integer(kind=int_wp) :: GETELS &
                              , GETELT
      external GETELS &
         , GETELT
!
!     element names
!
      data GRPNAM/'TABLE_R4'/
      data &
         (ELMNMS(I), ELMTPS(I), NBYTSG(I), ELMDMS(1, I), ELMDMS(2, I), ELMDES(I), &
          I=1, NELEMS) &
         /'NO_OUTP', 'INTEGER', 4, 1, 1, 'number of rows  in table R4', &
         'R4_PID', 'CHARACTER', 10, 1, 0, 'reference to process', &
         'R4_IID', 'CHARACTER', 10, 1, 0, 'reference to item (output)', &
         'R4_NUMB', 'INTEGER', 4, 1, 0, 'serial number in process', &
         'R4_DOC', 'CHARACTER', 1, 1, 0, 'documented yes/no', &
         'R4_SEX', 'INTEGER', 4, 1, 0, 'segment/exchange indication'/
!
!     Read group
!
      UINDEX(1) = 1
      UINDEX(2) = 1
      UINDEX(3) = 1
      BUFLEN = NBYTSG(1) * ELMDMS(2, 1)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(1), &
                      UINDEX, 1, &
                      BUFLEN, NO_OUTP)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(1)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      if (NO_OUTP > NO_OUTP_MAX) then
         write (LUNREP, *) 'ERROR reading group', GRPNAM
         write (LUNREP, *) 'Actual number of input items:', NO_OUTP
         write (LUNREP, *) 'greater than maximum:', NO_OUTP_MAX
         IERROR = 1
         goto 900
      end if
!
!     Set dimension of table
!
      do IELM = 2, NELEMS
         ELMDMS(2, IELM) = NO_OUTP
      end do

      BUFLEN = NBYTSG(2) * ELMDMS(2, 2)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(2), &
                      UINDEX, 1, &
                      BUFLEN, R4_PID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(2)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(3) * ELMDMS(2, 3)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(3), &
                      UINDEX, 1, &
                      BUFLEN, R4_IID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(3)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(4) * ELMDMS(2, 4)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(4), &
                      UINDEX, 1, &
                      BUFLEN, R4_NUMB)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(4)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(5) * ELMDMS(2, 5)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(5), &
                      UINDEX, 1, &
                      BUFLEN, R4_DOC)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(5)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(6) * ELMDMS(2, 6)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(6), &
                      UINDEX, 1, &
                      BUFLEN, R4_SEX)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(6)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
!
900   continue
      return
!
   end

end module m_rd_tabr4

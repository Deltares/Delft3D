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
module m_rd_tabr5
   use m_waq_precision

   implicit none

contains

   subroutine RD_TABR5(DEFFDS, &
                       NO_OUTF_MAX, NO_OUTF, &
                       R5_PID, R5_IID, &
                       R5_NUMB, R5_DOC, &
                       LUNREP, IERROR)
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_R5 group from NEFIS file
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
!     NO_OUTF_MAX  INT            I       maximum number of rows in table r3
!     NO_OUTF      INT            O       number of rows in table r3
!     R5_PID       CHA*10 NO_OUTF O       process identification
!     R5_IID       CHA*10 NO_OUTF O       item identification
!     R5_NUMB      INT    NO_OUTF O       serial number
!     R5_DOC       CHA*1  NO_OUTF O       documneted y/n
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
      integer(kind=int_wp) :: NO_OUTF_MAX, NO_OUTF, &
                              LUNREP, IERROR
      integer(kind=int_wp) :: DEFFDS
      character(len=10) R5_PID(NO_OUTF_MAX)
      character(len=10) R5_IID(NO_OUTF_MAX)
      integer(kind=int_wp) :: R5_NUMB(NO_OUTF_MAX)
      character(len=1) R5_DOC(NO_OUTF_MAX)
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
      parameter(NELEMS=5)
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
      data GRPNAM/'TABLE_R5'/
      data &
         (ELMNMS(I), ELMTPS(I), NBYTSG(I), ELMDMS(1, I), ELMDMS(2, I), ELMDES(I), &
          I=1, NELEMS) &
         /'NO_OUTF', 'INTEGER', 4, 1, 1, 'number of rows  in table R5', &
         'R5_PID', 'CHARACTER', 10, 1, 0, 'reference to prcocess', &
         'R5_IID', 'CHARACTER', 10, 1, 0, 'reference to item (flux)', &
         'R5_NUMB', 'INTEGER', 4, 1, 0, 'serial number in process', &
         'R5_DOC', 'CHARACTER', 1, 1, 0, 'documneted yes/no'/
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
                      BUFLEN, NO_OUTF)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(1)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      if (NO_OUTF > NO_OUTF_MAX) then
         write (LUNREP, *) 'ERROR reading group', GRPNAM
         write (LUNREP, *) 'Actual number of rows in table R5:', NO_OUTF
         write (LUNREP, *) 'greater than maximum:', NO_OUTF_MAX
         IERROR = 1
         goto 900
      end if
!
!     Set dimension of table
!
      do IELM = 2, NELEMS
         ELMDMS(2, IELM) = NO_OUTF
      end do
      BUFLEN = NBYTSG(2) * ELMDMS(2, 2)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(2), &
                      UINDEX, 1, &
                      BUFLEN, R5_PID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(2)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      BUFLEN = NBYTSG(3) * ELMDMS(2, 3)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(3), &
                      UINDEX, 1, &
                      BUFLEN, R5_IID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(3)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      BUFLEN = NBYTSG(4) * ELMDMS(2, 4)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(4), &
                      UINDEX, 1, &
                      BUFLEN, R5_NUMB)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(4)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      BUFLEN = NBYTSG(5) * ELMDMS(2, 5)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(5), &
                      UINDEX, 1, &
                      BUFLEN, R5_DOC)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(5)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
!
900   continue
      return
!
   end

end module m_rd_tabr5

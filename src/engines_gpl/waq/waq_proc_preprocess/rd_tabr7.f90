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
module m_rd_tabr7
   use m_waq_precision

   implicit none

contains

   subroutine RD_TABR7(DEFFDS, &
                       NO_VSTO_MAX, NO_VSTO, &
                       R7_VID, R7_SID, &
                       R7_SCAL, LUNREP, &
                       IERROR)
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_R7 group from NEFIS file
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
!     NO_VSTO_MAX  INT            I       maximum number of rows in table r7
!     NO_VSTO      INT            O       number of rows in table r7
!     R7_VID       CHA*10 NO_VSTO O       velocity identification
!     R7_SID       CHA*10 NO_VSTO O       substance identification
!     R7_SCAL      REAL   NO_VSTO O       scale factor
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
      integer(kind=int_wp) :: NO_VSTO_MAX, NO_VSTO, &
                              LUNREP, IERROR
      integer(kind=int_wp) :: DEFFDS
      character(len=10) R7_VID(NO_VSTO_MAX)
      character(len=10) R7_SID(NO_VSTO_MAX)
      real(kind=real_wp) :: R7_SCAL(NO_VSTO_MAX)
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
      parameter(NELEMS=4)
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
      data GRPNAM/'TABLE_R7'/
      data &
         (ELMNMS(I), ELMTPS(I), NBYTSG(I), ELMDMS(1, I), ELMDMS(2, I), ELMDES(I), &
          I=1, NELEMS) &
         /'NO_VSTO', 'INTEGER', 4, 1, 1, 'number of rows in table R7', &
         'R7_VID', 'CHARACTER', 10, 1, 0, 'velocity identification', &
         'R7_SID', 'CHARACTER', 10, 1, 0, 'substance identification', &
         'R7_SCAL', 'REAL', 4, 1, 0, 'scale factor'/
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
                      BUFLEN, NO_VSTO)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(1)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      if (NO_VSTO > NO_VSTO_MAX) then
         write (LUNREP, *) 'ERROR reading group', GRPNAM
         write (LUNREP, *) 'Actual number of rows in table R7:', NO_VSTO
         write (LUNREP, *) 'greater than maximum:', NO_VSTO_MAX
         IERROR = 1
         goto 900
      end if
!
!     Set dimension of table
!
      do IELM = 2, NELEMS
         ELMDMS(2, IELM) = NO_VSTO
      end do

      BUFLEN = NBYTSG(2) * ELMDMS(2, 2)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(2), &
                      UINDEX, 1, &
                      BUFLEN, R7_VID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(2)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(3) * ELMDMS(2, 3)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(3), &
                      UINDEX, 1, &
                      BUFLEN, R7_SID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(3)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(4) * ELMDMS(2, 4)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(4), &
                      UINDEX, 1, &
                      BUFLEN, R7_SCAL)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(4)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
!
900   continue
      return
!
   end

end module m_rd_tabr7

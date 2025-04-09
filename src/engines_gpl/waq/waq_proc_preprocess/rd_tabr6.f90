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
module m_rd_tabr6
   use m_waq_precision

   implicit none

contains

   subroutine RD_TABR6(DEFFDS, &
                       NO_STOC_MAX, NO_STOC, &
                       R6_FID, R6_SID, &
                       R6_SCAL, LUNREP, &
                       IERROR)
!
!     Deltares
!
!     CREATED            :  june 1999 by Jan van Beek
!
!     FUNCTION           :  Read TABLE_R6 group from NEFIS file
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
!     NO_STOC_MAX  INT            I       maximum number of rows in table r3
!     NO_STOC      INT            0       number of rows in table r3
!     R6_FID       CHA*10 NO_STOC 0       flux identification
!     R6_SID       CHA*10 NO_STOC 0       substance identification
!     R6_SCAL      REAL   NO_STOC 0       scale factor
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
      integer(kind=int_wp) :: NO_STOC_MAX, NO_STOC, &
                              LUNREP, IERROR
      integer(kind=int_wp) :: DEFFDS
      character(len=10) R6_FID(NO_STOC_MAX)
      character(len=10) R6_SID(NO_STOC_MAX)
      real(kind=real_wp) :: R6_SCAL(NO_STOC_MAX)
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
      data GRPNAM/'TABLE_R6'/
      data &
         (ELMNMS(I), ELMTPS(I), NBYTSG(I), ELMDMS(1, I), ELMDMS(2, I), ELMDES(I), &
          I=1, NELEMS) &
         /'NO_STOC', 'INTEGER', 4, 1, 1, 'number of rows in table R6', &
         'R6_FID', 'CHARACTER', 10, 1, 0, 'flux identification', &
         'R6_SID', 'CHARACTER', 10, 1, 0, 'substance identification', &
         'R6_SCAL', 'REAL', 4, 1, 0, 'scale factor'/
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
                      BUFLEN, NO_STOC)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(1)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if
      if (NO_STOC > NO_STOC_MAX) then
         write (LUNREP, *) 'ERROR reading group', GRPNAM
         write (LUNREP, *) 'Actual number of rows in table R6:', NO_STOC
         write (LUNREP, *) 'greater than maximum:', NO_STOC_MAX
         IERROR = 1
         goto 900
      end if
!
!     Set dimension of table
!
      do IELM = 2, NELEMS
         ELMDMS(2, IELM) = NO_STOC
      end do

      BUFLEN = NBYTSG(2) * ELMDMS(2, 2)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(2), &
                      UINDEX, 1, &
                      BUFLEN, R6_FID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(2)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(3) * ELMDMS(2, 3)
      IERROR = GETELS(DEFFDS, &
                      GRPNAM, ELMNMS(3), &
                      UINDEX, 1, &
                      BUFLEN, R6_SID)
      if (IERROR /= 0) then
         write (LUNREP, *) 'ERROR reading element', ELMNMS(3)
         write (LUNREP, *) 'ERROR number:', IERROR
         goto 900
      end if

      BUFLEN = NBYTSG(4) * ELMDMS(2, 4)
      IERROR = GETELT(DEFFDS, &
                      GRPNAM, ELMNMS(4), &
                      UINDEX, 1, &
                      BUFLEN, R6_SCAL)
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

end module m_rd_tabr6

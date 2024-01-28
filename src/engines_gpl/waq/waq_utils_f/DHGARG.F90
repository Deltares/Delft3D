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
module m_dhgarg
   use m_waq_precision

   implicit none

   contains

   SUBROUTINE DHGARG(IARG, LINE)

      ! Returns the IARG'th argument from the command line

      USE DHCOMMAND

      !  Arguments
      INTEGER(kind=int_wp) ::NARG
      INTEGER(kind=int_wp) ::IARG
      CHARACTER*(*)        :: LINE

      ! Local
      INTEGER(kind=INT16)  :: N, STATUS
      CHARACTER*256        :: BUFFER
      INTEGER(kind=int_wp) :: COUNT
      INTEGER(kind=int_wp) :: STORED
      LOGICAL              :: OPENED, EXISTS
      INTEGER(kind=int_wp) :: IERR
      INTEGER(kind=int_wp) :: LUN
      INTEGER(kind=int_wp) :: I

      !  Any stored arguments?
      STORED = DHSTORED_NUMBER_ARGS()
      IF (STORED > 0) THEN
         LINE = DHSTORED_ARG(IARG)
      ELSE
         !  Call system routine
         NARG = IARGC() + 1
         N = IARG
         CALL GETARG(N, BUFFER)
         ! Store result
         LINE = BUFFER

         ! Read from file?
         INQUIRE (FILE='delwaq.options', EXIST=EXISTS)
         IF (EXISTS) THEN
            IF (NARG .GT. 0) THEN
               COUNT = IARG - NARG
            ELSE
               COUNT = IARG - 1
            END IF

            OPEN (NEWUNIT=LUN, FILE='delwaq.options')
            DO
               READ (LUN, '(A)', IOSTAT=IERR) LINE
               IF (IERR .NE. 0) THEN
                  EXIT
               END IF
               IF (LINE .NE. ' ') THEN
                  IF (COUNT .EQ. 0) THEN
                     EXIT
                  END IF
                  COUNT = COUNT - 1
               END IF
            END DO
            CLOSE (LUN)
         END IF
      END IF

      RETURN
   END SUBROUTINE DHGARG
end module m_dhgarg

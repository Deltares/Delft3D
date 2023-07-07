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
      module m_dhcarg

      implicit none

      contains


      INTEGER FUNCTION DHCARG ( )
!
!     Delwaq Hulp Count ARGuments
!
!     Arguments
!
!
!     Local
!
      USE DHCOMMAND

      INTEGER           NARGS
      LOGICAL           EXISTS, OPENED
      INTEGER           I     , LUN   , IERR
      CHARACTER(LEN=20) LINE
!
!     Any stored arguments?
!
      DHCARG = DHSTORED_NUMBER_ARGS()

      IF ( DHCARG .EQ. 0 ) THEN
!
!         Call system routine
!
          DHCARG = IARGC() + 1
!
!         Take care of virtual zeroth argument
!
          IF ( DHCARG .EQ. 0 ) THEN
              DHCARG = 1
          ENDIF

          INQUIRE( FILE = 'delwaq.options', EXIST = EXISTS )
          IF ( EXISTS ) THEN
              OPEN( NEWUNIT = LUN, FILE = 'delwaq.options' )
              DO
                  READ( LUN, '(A)', IOSTAT = IERR ) LINE
                  IF ( IERR .NE. 0 ) THEN
                      EXIT
                  ENDIF
                  IF ( LINE .NE. ' ' ) THEN
                      DHCARG = DHCARG + 1
                  ENDIF
              ENDDO
              CLOSE( LUN )
          ENDIF
      ENDIF
!
      RETURN
      END
      end module m_dhcarg

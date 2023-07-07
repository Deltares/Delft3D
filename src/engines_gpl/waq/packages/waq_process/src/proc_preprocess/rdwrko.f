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

      SUBROUTINE RDWRKO ( LUNWRO, LCH   , LUREP , NOUTP , NRVART,
     +                    NBUFMX, IOUTPS, IOPOIN, OUNAM , VERSIO,
     +                    NOWARN, IERR  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : november 1994 by Jan van Beek
!
!     FUNCTION            : Reads output work file.
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : LUNWRO, Proces work file
!                           LUREP , Monitoring file
!
!     PARAMETERS          : 11
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUNWRO  INTEGER       1     INPUT   Output work file
!     LCH     CHA*(*)       1     INPUT   Name output work file
!     LUREP   INTEGER       1     INPUT   Monitoring file
!     NOUTP   INTEGER       1     INPUT   Number of output files
!     NRVART  INTEGER       1     INPUT   Number of extra output vars
!     NBUFMX  INTEGER       1     INPUT   length of output buffer
!     IOUTPS  INTEGER 7*NOUTP    OUTPUT   Output structure
!                                            index 1 = start time
!                                            index 2 = stop time
!                                            index 3 = time step
!                                            index 4 = number of vars
!                                            index 5 = kind of output
!                                            index 6 = format of output
!                                            index 7 = initialize flag
!     IOPOIN  INTEGER  NRVART    OUTPUT   Pointer to DELWAQ array's
!     OUNAM   CHAR*(*) NRVART    OUTPUT   name of output variable
!     NOWARN  INTEGER       1    IN/OUT   Cummulative warning count
!     IERR    INTEGER       1    IN/OUT   cummulative error count
!
!     Declaration of arguments
!
      use m_srstop
      use timers       !   performance timers

      INTEGER       LUNWRO, LUREP , NOUTP , NRVART, NBUFMX,
     +              NOWARN, IERR
      INTEGER       IOUTPS(7,*)   , IOPOIN(*)
      REAL          VERSIO
      CHARACTER*(*) LCH
      CHARACTER*(*) OUNAM(*)
!
!     Local declarations
!
      PARAMETER   ( VERSI1 = 0.0 , VERSI2 = 0.1 )
      INTEGER       NOUTPD, NRVARD, NBUFMD
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "rdwrko", ithndl )
!
!     read and check version number
!
      READ (LUNWRO, ERR=900, END=900) VERSIO
!
!     less than lowest supported version, ERROR
!
      IF ( VERSIO .LT. VERSI1 ) THEN
         WRITE ( LUREP, 2000 ) VERSIO , VERSI1
         CALL SRSTOP(1)
      ENDIF
!
!     greater than this version, WARNING
!
      IF ( VERSIO .GT. VERSI2 ) THEN
         NOWARN = NOWARN + 1
         WRITE ( LUREP, 2010 ) VERSIO , VERSI2
      ENDIF
!
!     read and check dimensions
!
      READ (LUNWRO, ERR=900, END=900) NOUTPD, NRVARD, NBUFMD
      IF ( NOUTPD .NE. NOUTP  ) THEN
         WRITE ( LUREP, 2020 ) NOUTPD, NOUTP
         IERR = IERR + 1
      ENDIF
      IF ( NRVARD .NE. NRVART ) THEN
         WRITE ( LUREP, 2030 ) NRVARD, NRVART
         IERR = IERR + 1
      ENDIF
      IF ( NBUFMD .NE. NBUFMX ) THEN
         WRITE ( LUREP, 2040 ) NBUFMD, NBUFMX
         IERR = IERR + 1
      ENDIF
      IF ( IERR .GT. 0 ) GOTO 910
!
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(1,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(2,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(3,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(4,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(5,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOUTPS(6,K) , K = 1 , NOUTP )
      READ (LUNWRO, ERR=900, END=900) ( IOPOIN(K)   , K = 1 , NRVART)
      READ (LUNWRO, ERR=900, END=900) ( OUNAM (K)   , K = 1 , NRVART)
!
      if (timon) call timstop( ithndl )
      RETURN
!
!     unsuccessful read
!
  900 CONTINUE
      WRITE ( LUREP   , 2050 ) LCH, LUNWRO
      IDUM = IDUM + 1
!
  910 CONTINUE
      if (timon) call timstop( ithndl )
      RETURN
!
!     output formats
!
 2000 FORMAT ( ' ERROR  : version output intput ',F5.2,' NOT supported'
     +        /'          by OUTPUT sytem version,',F5.2)
 2010 FORMAT ( ' WARNING: version output intput ',F5.2,' greater than'
     +        /'          OUTPUT sytem version,',F5.2)
 2020 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NOUTP',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2030 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NRVART',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2040 FORMAT ( ' ERROR  : Output work file doesn''t match dimensions in'
     +        /'          DELWAQ boot file for NBUFMX',
     +        /'          ',I6,' in output,',I6,' in boot file.')
 2050 FORMAT ( ' ERROR  : Reading output work file;',A,
     +        /'          on unit number ',I3)
!
      END

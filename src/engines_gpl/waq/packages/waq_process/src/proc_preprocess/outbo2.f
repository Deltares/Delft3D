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

      SUBROUTINE OUTBO2 ( NOUTP , IOUTPS, NOSEG , NODUMP, NX    ,
     +                    NY    , NRVART, NBUFMX, NDMPAR, NOTOT ,
     +                    NCBUFM, NORAAI)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:           by Jan van Beek
!
!     FUNCTION            : Sets the boot variables for OUTPUT system
!
!     LOGICAL UNITNUMBERS : -
!
!     SUBROUTINES CALLED  : -
!
!     PARAMETERS          : 10
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOUTP   INTEGER       1     INPUT   Number of processes in def file
!     IOUTPS  INTEGER   7,NOUTP   INPUT   output structure
!     NOSEG   INTEGER       1     INPUT   Number of segments
!     NODUMP  INTEGER       1     INPUT   Number of monitoring points
!     NX      INTEGER       1     INPUT   Length of dump grid
!     NY      INTEGER       1     INPUT   Width of dump grid
!     NRVART  INTEGER       1     OUTPUT  Total number of output variables
!     NBUFMX  INTEGER       1     OUTPUT  Length of output buffer needed
!     NDMPAR  INTEGER       1     INPUT   number of dump areas
!     NOTOT   INTEGER       1     INPUT   Number of substances
!     NCBUFM  INTEGER       1     IN/OUT  Length of character buffer
!     NORAAI  INTEGER       1     INPUT   number of raaien
!
!     Declaration of arguments
!
      use timers       !   performance timers
      use output

      INTEGER     NOUTP , NOSEG , NODUMP, NX    , NY    ,
     +            NRVART, NBUFMX, NDMPAR, NOTOT , NCBUFM,
     +            NORAAI
      INTEGER     IOUTPS(7,NOUTP)
!
!     Local
!
      PARAMETER ( IGSEG = 1 , IGMON = 2 , IGGRD = 3 , IGSUB = 4 )
      INTEGER     IGRID , NOCEL , NBUFOU, ISRTO
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "outbo2", ithndl )
!
!     Loop over the output files
!
      NRVART = 0
      NBUFMX = 0
      DO 100 IOUT = 1 , NOUTP
         NRVAR  = IOUTPS(4,IOUT)
         NRVART = NRVART + NRVAR
!
!        Grid
!
         IGRID = IOUTPS(6,IOUT)
         IF ( IGRID .EQ. IGSEG ) THEN
            NOCEL = NOSEG
         ELSEIF ( IGRID .EQ. IGMON ) THEN
            NOCEL = NODUMP
         ELSEIF ( IGRID .EQ. IGGRD ) THEN
            NOCEL = NX*NY
         ELSEIF ( IGRID .EQ. IGSUB ) THEN
            NOCEL = NDMPAR
         ENDIF
!
!        Calculate outputbuffer size for this file, for some (NEFIS,SUB)
!        also a character buffer size
!
         NCBUFO = 0
         ISRTO  = IOUTPS(5,IOUT)
         IF ( ISRTO .EQ. IHNF .OR. ISRTO .EQ. IMNF     ) THEN
!
!           NEFIS file, extra array with length NOCEL needed
!           substance names and output names in char buffer.
!
            NBUFOU = NOCEL * ( NRVAR + 1 )
            NCBUFO = NOTOT + NRVAR
         ELSEIF ( ISRTO .EQ. IHN2 .OR. ISRTO .EQ. IMN2 ) THEN
!
!           NEFIS file, extra array with length NOCEL needed
!
            NBUFOU = NOCEL * ( NRVAR + 1 )
         ELSEIF ( ISRTO .EQ. IMO3 ) THEN
!
!           On subarea's substances also in buffer, only the
!           first half of the nrvar are real output vars.
!           substance names and output names in char buffer.
!
            NBUFOU = NOCEL * ( NOTOT + NRVAR/2 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IHI3 ) THEN
!
!           On subarea's substances also in buffer, only the
!           first half of the nrvar are real output vars.
!           substance names and output names in char buffer.
!           also output for raaien
!
            NBUFOU = (NOCEL+NORAAI) * ( NOTOT + NRVAR/2 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IHN3 ) THEN
!
!           NEFIS file, extra array with length NOCEL needed
!           On subarea's substances also in buffer, only the
!           first half of the nrvar are real output vars.
!           substance names and output names in char buffer.
!           also output for raaien
!
            NBUFOU = (NOCEL+NORAAI) * ( NOTOT + NRVAR/2 + 1 )
            NCBUFO = NOTOT + NRVAR/2
         ELSEIF ( ISRTO .EQ. IMO4 .OR. ISRTO .EQ. IHI4 ) THEN
!
!           On subarea's only the first half of the nrvar are
!           real output vars.
!
            NBUFOU = NOCEL * ( NRVAR/2 )
         ELSEIF ( ISRTO .EQ. IHN4 ) THEN
!
!           NEFIS file, extra array with length NOCEL needed
!           On subarea's only the first half of the nrvar are
!           real output vars.
!
            NBUFOU = NOCEL * ( NRVAR/2 + 1 )
         ELSE
!
!           Rest, normal
!
            NBUFOU = NOCEL * NRVAR
         ENDIF
!
!        Buffer is as big as the largest needed
!
         NBUFMX = MAX ( NBUFMX, NBUFOU )
         NCBUFM = MAX ( NCBUFM, NCBUFO )
!
  100 CONTINUE
!
      if (timon) call timstop( ithndl )
      RETURN
      END

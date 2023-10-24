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
      module m_fioutv
      use m_waq_type_definitions


      implicit none

      contains


      SUBROUTINE FIOUTV (OUTVAL, IOPOIN, NRVAR , NOCONS, NOPA  ,
     +                   NOFUN , NOSFUN, NOTOT , CONC  , SEGFUN,
     +                   FUNC  , PARAM , CONS  , IDT   , ITIME ,
     +                   VOLUME, NOSEG , NOSYS , NODUMP, IDUMP ,
     +                   NX    , NY    , LGRID , IGRID , BOUND ,
     +                   NOLOC , PROLOC, NODEF , DEFAUL)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : may 1993 by Jan van Beek
!
!     FUNCTION            : Fills output buffer OUTVAL.
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : -
!
!     PARAMETERS          : 29
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     OUTVAL  REAL(kind=sp) ::NRVAR,*     OUTPUT  Values for vars on output grid
!     IOPOIN  INTEGER(kind=int_32) ::*     INPUT   Pointers to arrays for vars
!     NRVAR   INTEGER(kind=int_32) ::1     INPUT   Number of output vars
!     NOCONS  INTEGER(kind=int_32) ::1     INPUT   Number of constants used
!     NOPA    INTEGER(kind=int_32) ::1     INPUT   Number of parameters
!     NOFUN   INTEGER(kind=int_32) ::1     INPUT   Number of functions ( user )
!     NOSFUN  INTEGER(kind=int_32) ::1     INPUT   Number of segment functions
!     NOTOT   INTEGER(kind=int_32) ::1     INPUT   Total number of substances
!     CONC    REAL(kind=sp) ::NOTOT,NOSEG  INPUT   Model concentrations
!     SEGFUN  REAL(kind=sp) ::NOSEG,NOSFUN IN/OUT  Segment functions at ITIME
!     FUNC    REAL(kind=sp) ::*     IN/OUT  Model functions at ITIME
!     PARAM   REAL(kind=sp) ::NOPA,NOSEG  IN/OUT  Model parameters
!     CONS    REAL(kind=sp) ::*     IN/OUT  Model constants
!     IDT     INTEGER(kind=int_32) ::1     INPUT   Simulation timestep
!     ITIME   INTEGER(kind=int_32) ::1     INPUT   Time in system clock units
!     VOLUME  REAL(kind=sp) ::NOSEG     INPUT   Segment volumes
!     NOSEG   INTEGER(kind=int_32) ::1     INPUT   Nr. of computational elements
!     NOSYS   INTEGER(kind=int_32) ::1     INPUT   Number of active substances
!     NODUMP  INTEGER(kind=int_32) ::1     INPUT   number of dump locations
!     IDUMP   INTEGER(kind=int_32) ::NODUMP   INPUT   dump segment numbers
!     NX      INTEGER(kind=int_32) ::1     INPUT   Width of output grid
!     NY      INTEGER(kind=int_32) ::1     INPUT   Depth of output grid
!     LGRID   INTEGER(kind=int_32) ::NX*NY   INPUT   grid-layout
!     IGRID   INTEGER(kind=int_32) ::1     INPUT   Output grid indication
!     BOUND   REAL(kind=sp) ::NOTOT*?    INPUT   boundary      values
!     NOLOC   INTEGER(kind=int_32) ::1     INPUT   Number of variables in PROLOC
!     PARAM   REAL(kind=sp) ::NOLOC,NOSEG  INPUT   Parameters local in PROCES system
!     NODEF   INTEGER(kind=int_32) ::1     INPUT   Number of used defaults
!     DEFAUL  REAL(kind=sp) ::*     INPUT   Default proces parameters
!
!     Declaration of arguments
!
      use timers

      INTEGER(kind=int_32) ::NRVAR , NOCONS, NOPA  , NOFUN , NOSFUN,
     +           NOTOT , IDT   , ITIME , NOSEG , NOSYS ,
     +           NODUMP, NX    , NY    , IGRID , NOLOC ,
     +           NODEF
      INTEGER(kind=int_32) ::IOPOIN(*)     , IDUMP(*)      ,
     +           LGRID(*)
      REAL(kind=sp) ::OUTVAL(*)      , CONC(NOTOT,*),
     +           SEGFUN(NOSEG,*), FUNC(*)      ,
     +           PARAM(*)       , CONS(*)      ,
     +           VOLUME(*)      , BOUND(*)     ,
     +           PROLOC(*)      , DEFAUL(*)
!
!     Local
!
      integer(kind=int_32), PARAMETER  ::IGSEG = 1 , IGMON = 2 , IGGRD = 3 , IGSUB = 4
      real(kind=sp),    PARAMETER  ::RMISS = -999.
      integer(kind=int_32), PARAMETER  ::NOPRED= 6
      INTEGER(kind=int_32) ::IOPA  , IOFUNC, IOSFUN, IOCONC, IOLOC ,
     +            IODEF , IP, icel, iseg, iocons, nocel, i, iicel, iip
      integer(kind=int_32) ::ithandl = 0
      if ( timon ) call timstrt ( "fioutv", ithandl )
!
!     Pointer offsets
!
      IOCONS = NOPRED + 1
      IOPA   = IOCONS + NOCONS
      IOFUNC = IOPA   + NOPA
      IOSFUN = IOFUNC + NOFUN
      IOCONC = IOSFUN + NOSFUN
      IOLOC  = IOCONC + NOTOT
      IODEF  = IOLOC  + NOLOC
!
!     GRID
!
      IF ( IGRID .EQ. IGSEG ) THEN
         NOCEL = NOSEG
      ELSEIF ( IGRID .EQ. IGMON ) THEN
         NOCEL = NODUMP
      ELSEIF ( IGRID .EQ. IGGRD ) THEN
         NOCEL = NX*NY
      ENDIF
!
!     FILL OUTVAL
!
      DO 200 ICEL = 1 , NOCEL
!
!        What segment ?
!
         IF ( IGRID .EQ. IGSEG ) THEN
            ISEG = ICEL
         ELSEIF ( IGRID .EQ. IGMON ) THEN
            ISEG = IDUMP(ICEL)
         ELSEIF ( IGRID .EQ. IGGRD ) THEN
            ISEG = LGRID(ICEL)
         ENDIF

         DO 100 I = 1 , NRVAR
            IICEL = (ICEL-1)*NRVAR+I
            IP = IOPOIN(I)
!
!           What value
!
            IF ( ISEG .LT. 0 ) THEN
               IF ( IP .GE. IOCONC .AND. IP .LT. IOCONC+NOSYS ) THEN
                  IIP = (-ISEG-1)*NOSYS + IP-IOCONC+1
                  OUTVAL(IICEL) = BOUND(IIP)
               ELSE
                  OUTVAL(IICEL) = RMISS
               ENDIF
            ELSEIF ( ISEG .EQ. 0 ) THEN
               OUTVAL(IICEL) = RMISS
            ELSE
               IF ( IP .GE. IODEF  ) THEN
                  OUTVAL(IICEL) = DEFAUL(IP-IODEF+1)
               ELSEIF ( IP .GE. IOLOC  ) THEN
                  IIP = (ISEG-1)*NOLOC + IP-IOLOC+1
                  OUTVAL(IICEL) = PROLOC(IIP)
               ELSEIF ( IP .GE. IOCONC ) THEN
                  OUTVAL(IICEL) = CONC(IP-IOCONC+1,ISEG)
               ELSEIF ( IP .GE. IOSFUN ) THEN
                  OUTVAL(IICEL) = SEGFUN(ISEG,IP-IOSFUN+1)
               ELSEIF ( IP .GE. IOFUNC ) THEN
                  OUTVAL(IICEL) = FUNC(IP-IOFUNC+1)
               ELSEIF ( IP .GE. IOPA ) THEN
                  IIP = (ISEG-1)*NOPA + IP-IOPA+1
                  OUTVAL(IICEL) = PARAM(IIP)
               ELSEIF ( IP .GE. IOCONS ) THEN
                  OUTVAL(IICEL) = CONS(IP-IOCONS+1)
               ELSEIF ( IP .EQ. 3 ) THEN
                  OUTVAL(IICEL) = REAL(IDT)
               ELSEIF ( IP .EQ. 2 ) THEN
                  OUTVAL(IICEL) = REAL(ITIME)
               ELSEIF ( IP .EQ. 1 ) THEN
                  OUTVAL(IICEL) = VOLUME(ISEG)
               ELSEIF ( IP .LE. 0 ) THEN
                  OUTVAL(IICEL) = RMISS
               ENDIF
            ENDIF
  100    CONTINUE
  200 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_fioutv

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
      module m_dlwqtr
      use m_waq_precision
      use m_string_utils

      implicit none

      contains


      SUBROUTINE DLWQTR ( NOTOT  , NOSYS  , NOSEG  , NOQ    , NOQ1   , & 
                         NOQ2   , NOQ3   , NOPA   , NOSFUN , NODISP , & 
                         NOVELO , IPOINT , VOLUME , AREA   , FLOW   , & 
                         ALENG  , CONC   , DISP   , CONS   , PARAM  , & 
                         FUNC   , SEGFUN , DISPER , VELO   , ITIME  , & 
                         IDT    , SYNAME , NOCONS , NOFUN  , CONAME , & 
                         PANAME , FUNAME , SFNAME , UPDATR , ILFLAG )
      use m_srstop
      use m_monsys

!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:                 by L.Postma
!     REVISED:    august  1997 by Jan van Beek, Delft3D-WAQ functonality
!
!     FUNCTION            : reads SURFACE from coupling
!                           Sets dispersion length in vertical
!
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     NOSYS   INTEGER       1     INPUT   number of active substances
!     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
!     NOQ     INTEGER       1     INPUT   Total number of exchanges
!     NOQ1    INTEGER       1     INPUT   Nr. of exchanges direction 1
!     NOQ2    INTEGER       1     INPUT   Nr. of exchanges direction 2
!     NOQ3    INTEGER       1     INPUT   Nr. of exchanges direction 3
!     NOPA    INTEGER       1     INPUT   Number of parameters
!     NOSFUN  INTEGER       1     INPUT   Number of segment functions
!     NODISP  INTEGER       1     INPUT   Number of user-dispersions
!     NOVELO  INTEGER       1     INPUT   Number of user-flows
!     IPOINT  INTEGER   4*NOQ     INPUT   1= "From"   segment pointers
!                                 INPUT   2= "To"     segment pointers
!                                 INPUT   3= "From-1" segment pointers
!                                 INPUT   4= "To+1"   segment pointers
!     VOLUME  REAL      NOSEG     INPUT   Segment volumes
!     AREA    REAL        NOQ     INPUT   Exchange surfaces
!     FLOW    REAL        NOQ     INPUT   Flows
!     ALENG a)REAL      2*NOQ     INPUT   1= Length to "From" surface
!                                         2= Length to "To"   surface
!           b)REAL        3       INPUT   3 lengthes in the grid
!     CONC    REAL   NOTOT*NOSEG  INPUT   Model concentrations
!     DISP    REAL        3       IN/OUT  Dispersion in 3 directions
!     CONS    REAL          *     IN/OUT  Model constants
!     PARAM   REAL    NOPA*NOSEG  IN/OUT  Model parameters
!     FUNC    REAL          *     IN/OUT  Model functions at ITIME
!     SEGFUN  REAL   NOSEG*NOSFUN IN/OUT  Segment functions at ITIME
!     DISPER  REAL   NODISP*NOQ   OUTPUT  User defined dispersion
!     VELO    REAL   NOVELO*NOQ   OUTPUT  User defined flows
!     ITIME   INTEGER       1     INPUT   Time in system clock units
!     IDT     INTEGER       1     INPUT   Time step system clock units
!     SYNAME  CHAR*20    NOTOT    INPUT   names of systems
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     CONAME  CHAR*20   NOCONS    INPUT   Constant names
!     PANAME  CHAR*20   NOPA      INPUT   Parameter names
!     FUNAME  CHAR*20   NOFUN     INPUT   Function names
!     SFNAME  CHAR*20   NOSFUN    INPUT   Segment function names
!     UPDATR  LOGICAL       1     IN/OUT  Flag indicating if the transport
!                                         matrix is changed. The user should
!                                         set this flag to .T. if he alters
!                                         part of the matrix and uses integratio
!                                         option 10.xx .
!     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
!
!     ==================================================================
!
!     Save for all the local index pointers and switches
!
      SAVE
!
      integer(kind=int_wp) ::IPOINT(4,NOQ)
      real(kind=real_wp) ::VOLUME(NOSEG)     , AREA(NOQ)         , & 
                  FLOW(NOQ)         , ALENG (2,NOQ)     , & 
                  CONC(NOTOT,NOSEG) , DISP(3)           , & 
                  CONS(*)           , PARAM (NOPA,NOSEG), & 
                  FUNC(*)           , SEGFUN(NOSEG,*)   , & 
                  VELO(*)           , DISPER(*)
      character(len=20) SYNAME (NOTOT)    , CONAME (*)        , & 
                  PANAME (*)        , FUNAME (*)        , & 
                  SFNAME (*)
      LOGICAL      UPDATR
      integer(kind=int_wp) ::NOTOT, ILFLAG, NOSYS, NOSEG, NOQ, NOQ1, IDT, ITIME, & 
                  NOQ2, NOQ3, NOPA, NOSFUN, NODISP, NOVELO, NOCONS, NOFUN
!
!     Local
!
      INTEGER(kind=int_wp) ::LCCCO, ier, ierr, ier2, lunrep, isurf, & 
                nmaxa, mmaxa, nma, idummy, nmt, k, iseg, & 
                ilay, iq, ipos, ifrom, ito, layt
      LOGICAL    FIRST ,  LINIT , LEXI
      DATA       FIRST / .TRUE. /
      DATA       LINIT / .FALSE. /
!
!          check usage w.r.t. parallel computing
!
!          AM:
!          I removed this check, as all the computations set up using
!          the Delft3D user-interface have the SURF parameter.
!          Even if not, then the file should be available on all
!          nodes, as they share the directory.
!
!          check number of parameters
!
!     Initialisation set index pointers, read surface areas
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         IER   = 0
         CALL GETMLU(LUNREP)
         WRITE(LUNREP,*)
         WRITE(LUNREP,2000)
!
!        Set pointers in param array
!
         ISURF = index_in_array( 'SURF      ', PANAME (:NOPA))
!
!          read surface areas
!
         IF ( ISURF > 0 ) THEN
            IF ( ILFLAG == 1 .AND. NOQ3 > 0 ) THEN
               LINIT = .TRUE.
               WRITE(LUNREP,2040)
            ENDIF
            INQUIRE  ( FILE='areachar.dat', EXIST = LEXI )
            IF ( .NOT. LEXI ) THEN
!
!
!              It is assumed the SURF parameter has been set in the input
!
            ELSE
               OPEN ( NEWUNIT = LCCCO, FILE='areachar.dat', FORM  ='UNFORMATTED', & 
                                      STATUS='OLD'       , IOSTAT=IER2         )
               IF ( IER2 /= 0 ) THEN
                  WRITE (LUNREP,2010)
                  WRITE ( *    ,2010)
                  IER = IER + 1
               ELSE
                  WRITE(LUNREP,2030)
                  READ ( LCCCO ) NMAXA, MMAXA, NMA, NMA, NMA, IDUMMY
                  LAYT = NOSEG/NMA
                  NMT = NMA*LAYT
                  IF ( NMT /= NOSEG ) THEN
                     WRITE (LUNREP,2050) NMA,LAYT,NMT,NOSEG
                     WRITE (  *   ,2050) NMA,LAYT,NMT,NOSEG
                     IER = IER + 1
                  ENDIF
                  IF ( IER == 0 ) THEN
                     READ ( LCCCO ) (PARAM(ISURF,K),K=1,NMA)
                     DO ILAY = 2, LAYT
                        DO ISEG = 1, NMA
                           IPOS = (ILAY-1)*NMA + ISEG
                           PARAM(ISURF,IPOS) = PARAM(ISURF,ISEG)
                         end do
                     end do
                  ENDIF
                  CLOSE ( LCCCO )
               ENDIF
            ENDIF
!
            IF ( IER /= 0 ) THEN
               CALL SRSTOP(1)
            ENDIF
         ENDIF
!
         WRITE(LUNREP,2070)
!
      ENDIF
!
!     adapt the length for the third direction
!
      IF ( LINIT ) THEN
         DO IQ = NOQ1 + NOQ2 + 1, NOQ
              IFROM = IPOINT(1,IQ)
              ITO   = IPOINT(2,IQ)
              IF ( IFROM > 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) > 1.0E-15 ) THEN
                      ALENG(1,IQ) = VOLUME(IFROM)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
              IF ( ITO   > 0 ) THEN
                 IF ( PARAM(ISURF,IFROM) > 1.0E-15 ) THEN
                      ALENG(2,IQ) = VOLUME(ITO)/PARAM(ISURF,IFROM)/2.
                 ENDIF
              ENDIF
          end do
      ENDIF
!
!     end of the subroutine
!
      RETURN
!
!     Output formats
!
 2000 FORMAT (' Extra functionality DLWQTR')
 2010 FORMAT (' ERROR: opening file <areachar.dat> !')
 2030 FORMAT (' Surface area''s will be read from file <areachar.dat>')
 2040 FORMAT (' Dispersion length in third dir. will be calculated')
 2050 FORMAT (' ERROR: File areachar.dat does not match.', & 
             ' NMA = ',I8,' LAYT= ',I8,' NMT = ',I8,' NOSEG=',I8)
 2070 FORMAT (' End extra functionality DLWQTR')
!
      END

      end module m_dlwqtr

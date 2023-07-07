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

      SUBROUTINE WRIPRO ( NPROC , NSVAR , IFLUX , NIPMSA, PRVVAR,
     +                    PRVTYP, NOLOC , NODEF , DEFAUL, PRONAM,
     +                    NFLUX , LUWRKP, VERSIO, STOCHI, NOTOT ,
     +                    NOSYS , NDSPX , NVELX , NLOCX , DSTO  ,
     +                    VSTO  , NDSPN , IDPNW , NVELN , IVPNW ,
     +                    PROGRD, PRONDT , NOVAR , VARARR, VARIDX,
     +                    VARTDA, VARDAG , VARTAG, VARAGG, nrref ,
     &                    proref)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: dec -1992 by Jan van Beek
!
!     FUNCTION            : Writes proces intermediate work file
!
!     LOGICAL UNITNUMBERS : LUWRKP , proces wrk file
!
!     SUBROUTINES CALLED  : -
!
!     PARAMETERS          : 15
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     NPROC   INTEGER       1     INPUT   Number of called processes
!     NSVAR   INTEGER       *     INPUT   Number of variables per proces
!     IFLUX   INTEGER       *     INPUT   Pointer in FLUX per proces inst.
!     NIPMSA  INTEGER       1     INPUT   Length IPMSA
!     IPMSA   INTEGER       *     INPUT   Pointer in SSA per proces inst.
!     IPSSA   INTEGER       *     INPUT   Pointer to SSA per proces inst.
!     NOLOC   INTEGER       1     INPUT   Number of local variables
!     NODEF   INTEGER       1     INPUT   Number of used defaults
!     DEFAUL  REAL          *     INPUT   Default values
!     PRONAM  CHA*(*)       *     INPUT   Name of called module
!     NFLUX   INTEGER       1     INPUT   total number of fluxes
!     LUWRKP  INTEGER       1     INPUT   unit number proces work file
!     VERSIO  INTEGER       1     INPUT   Versie number of program
!     STOCHI  REAL   NOTOT*NFLUX  INPUT   Proces stochiometry
!     NOTOT   INTEGER       1     INPUT   Number of substances
!     NOSYS   INTEGER       1     INPUT   Number of active substances
!     NDSPX   INTEGER       1     INPUT   Number of extra dispersion array
!     NVELX   INTEGER       1     INPUT   Number of extra velocity array
!     NLOCX   INTEGER       1     INPUT   No.loc.var.exhange level
!     DSTO    INTEGER NOSYS,*     INPUT   dispersion stochi matrix
!     VSTO    INTEGER NOSYS,*     INPUT   velocity stochi matrix
!     NDSPN   INTEGER       1     INPUT   Number of new dispersion array
!     IDPNW   INTEGER   NOSYS     INPUT   Pointers to new dispersion array
!     NVELN   INTEGER       1     INPUT   Number of new velocity array
!     IVPNW   INTEGER   NOSYS     INPUT   Pointers to new velocity array
!     PROGRD  INTEGER       1     INPUT   Grid number for active processes
!     PRONDT  INTEGER       1     INPUT   Step size for active processes
!
      use timers       !   performance timers

      INTEGER     NPROC , NIPMSA, NOLOC , NODEF , NFLUX ,
     +            LUWRKP, NOTOT , NOSYS , NDSPX , NVELX ,
     +            NLOCX , NDSPN , NVELN , NOVAR , nrref
      INTEGER     NSVAR(*) , IFLUX(*) ,
     +            PRVVAR(*), PRVTYP(*),
     +            IDPNW(*) , IVPNW(*) ,
     +            PROGRD(*), PRONDT(*),
     +            VARARR(*), VARIDX(*),
     +            VARTDA(*), VARDAG(*),
     +            VARTAG(*), VARAGG(*), proref(*)
      REAL        VERSIO
      REAL        DEFAUL(*), STOCHI(*),
     +            DSTO(*)  , VSTO(*)
      CHARACTER*10 PRONAM(*)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "wripro", ithndl )
!
      WRITE (LUWRKP) VERSIO
      WRITE (LUWRKP) NIPMSA,NPROC,NFLUX,NOLOC,NODEF,
     +               NOTOT ,NOSYS,NDSPX,NVELX,NLOCX,
     +               NDSPN ,NVELN,NOVAR, nrref
      WRITE (LUWRKP) ( NSVAR(K) , K = 1 , NPROC )
      WRITE (LUWRKP) ( IFLUX(K) , K = 1 , NPROC )
      WRITE (LUWRKP) ( PRVVAR(K), K = 1 , NIPMSA)
      WRITE (LUWRKP) ( PRVTYP(K), K = 1 , NIPMSA)
      WRITE (LUWRKP) ( DEFAUL(K), K = 1 , NODEF )
      WRITE (LUWRKP) ( STOCHI(K), K = 1 , NOTOT*NFLUX )
      WRITE (LUWRKP) ( DSTO(K)  , K = 1 , NOSYS*NDSPX )
      WRITE (LUWRKP) ( VSTO(K)  , K = 1 , NOSYS*NVELX )
      IF ( NDSPN .GT. 0 ) THEN
         WRITE (LUWRKP) ( IDPNW(K)  , K = 1 , NOSYS )
      ENDIF
      IF ( NVELN .GT. 0 ) THEN
         WRITE (LUWRKP) ( IVPNW(K)  , K = 1 , NOSYS )
      ENDIF
      WRITE (LUWRKP) ( PRONAM(K), K = 1 , NPROC )
      WRITE (LUWRKP) ( PROGRD(K), K = 1 , NPROC )
      WRITE (LUWRKP) ( PRONDT(K), K = 1 , NPROC )
      WRITE (LUWRKP) ( VARARR(K), K = 1 , NOVAR )
      WRITE (LUWRKP) ( VARIDX(K), K = 1 , NOVAR )
      WRITE (LUWRKP) ( VARTDA(K), K = 1 , NOVAR )
      WRITE (LUWRKP) ( VARDAG(K), K = 1 , NOVAR )
      WRITE (LUWRKP) ( VARTAG(K), K = 1 , NOVAR )
      WRITE (LUWRKP) ( VARAGG(K), K = 1 , NOVAR )
      write (luwrkp) ( proref(k), k = 1 , nproc*nrref )
!
      if (timon) call timstop( ithndl )
      RETURN
      END

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
      module m_outbal
      use m_waq_precision


      implicit none

      contains


      SUBROUTINE OUTBAL (LUBAL , FILBAL, ITIME , MONAME, NOTOT , & 
                        NOFLUX, SYNAME, NDMPAR, DANAME, ASMASS, & 
                        FLXINT, NOTOT2, CONC2 , INIT  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : march 1993 by Jan van Beek
!                         ( Modified version of WRIBAL by Jos van Gils )
!
!     FUNCTION            : Writes balance output
!
!     SUBROUTINES CALLED  : -
!
!     FILES               : LUBAL , Balance output file
!
!     COMMON BLOCKS       : -
!
!     PARAMETERS          : 16
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     LUBAL   INTEGER       1     INPUT   Logical unit balance file
!     FILBAL  CHAR*(*)      1     INPUT   Name balance file
!     ITIME   INTEGER       1     INPUT   Simulation time ( scu )
!     MONAME  CHAR*40       4     INPUT   model identification
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
!     SYNAME  CHAR*20   NOTOT     INPUT   names of substances
!     NDMPAR  INTEGER       1     INPUT   Number of dump segments
!     DANAME  CHAR*20   NDMPAR    INPUT   names of monitoring stations
!     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
!     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Integrated fluxes
!     NOTOT2  REAL          1     INPUT   Number of extra variables
!     CONC2   REAL NOTOT2*NDMPAR  INPUT   Extra variables
!     INIT    INTEGER       1     IN/OUT  Init flag (1=yes,!1=no)
!
!     Declaration of arguments
!
      use timers

      INTEGER(kind=int_wp) ::LUBAL , ITIME , INIT  , NOTOT , NOFLUX, & 
                   NDMPAR, NOTOT2, NOPOUT
      REAL(kind=real_wp) ::ASMASS(NOTOT,NDMPAR,6), FLXINT(NOFLUX,NDMPAR), & 
                   CONC2(NOTOT2,NDMPAR)
      CHARACTER*20  SYNAME(*)             , DANAME(*)
      CHARACTER*40  MONAME(4)
      CHARACTER*(*) FILBAL
!
!     Local declarations
!
      INTEGER(kind=int_wp) ::J     , I     , K     , ISYS  , IFLX  , & 
                  IHLP
      integer(kind=int_wp) ::ithandl =0
      if ( timon ) call timstrt ( "outbal", ithandl )
!
!     Initialize file
!
      IF ( INIT == 1 ) THEN
         INIT = 0
!
!        Write header
!
         WRITE (LUBAL) (MONAME(I),I=1,4)
         NOPOUT = 6*NOTOT+NOFLUX+2
         WRITE (LUBAL) NOPOUT,NDMPAR,NOTOT
         WRITE (LUBAL) (SYNAME(I),I=1,NOTOT)
         WRITE (LUBAL) (DANAME(I),I=1,NDMPAR)
      ENDIF
!
!     Perform output
!
      WRITE (LUBAL) ITIME,( & 
               ((ASMASS(ISYS,J,K),K=1,6),ISYS=1,NOTOT) , & 
               (FLXINT(IFLX,J)          ,IFLX=1,NOFLUX), & 
               (CONC2(IHLP,J)           ,IHLP=1,2)     , & 
               J=1,NDMPAR  )
!
      if ( timon ) call timstop ( ithandl )
      RETURN
!
      END

      end module m_outbal

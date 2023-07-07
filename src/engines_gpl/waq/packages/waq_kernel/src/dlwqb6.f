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
      module m_dlwqb6

      implicit none

      contains


      SUBROUTINE DLWQB6 ( CONC   , DERIV  , NOSEG  , NOTOT  , VOLOLD ,
     *                    IDT    , ISYS   , RHS    , NSYS   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: august 1992 by L. Postma
!
!     FUNCTION            : define right hand side
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
!     ----    -----      ------       ------- -----------
!     CONC    REAL     NOTOT*NOSEG    INPUT   concentrations
!     DERIV   REAL     NOTOT*NOSEG    INPUT   derivatives
!     NOSEG   INTEGER       1         INPUT   number of segments
!     NOTOT   INTEGER       1         INPUT   total number of systems
!     VOLOLD  REAL        NOSEG       INPUT   volumes at beginning of step
!     IDT     INTEGER       1         INPUT   timestep in scu's
!     ISYS    INTEGER       1         INPUT   first substance
!     RHS     REAL        NOSEG       OUTPUT  right hand side vector
!     NSYS    INTEGER       1         INPUT   number of substances
!
      use timers

      real    CONC (*)  ,  DERIV(*)  ,  VOLOLD(*) ,  RHS(*)
      integer NOSEG, NOTOT, IDT, ISYS, NSYS

      real    dt
      integer iseg, iset, i1, k1

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqb6", ithandl )
!
!         set the right hand side
!
      DT = REAL(IDT)
      ISET = 1
      DO 10 ISEG = 1 , NOSEG
         I1 = NOTOT*(ISEG-1)
         DO 10 K1 = ISYS, ISYS-1+NSYS
            RHS(ISET) = DERIV(I1+K1) + VOLOLD(ISEG)*CONC(I1+K1)/DT
            ISET = ISET + 1
   10 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwqb6

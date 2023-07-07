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
      module m_dlwqb2

      implicit none

      contains


      SUBROUTINE DLWQB2 ( CONC   , RHS    , NOSEG  , NOTOT  , ISYS   ,
     *                    NSYS   )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED: june 1988 by L.Postma
!
!     FUNCTION            : puts solution from RHS in CONC, zeros RHS
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
!     ----    -----      ------       ------- -----------
!     CONC    REAL     NOTOT*NOSEG    IN/OUT  old/new concentration
!     RHS     REAL        NOSEG       IN/OUT  right hand side matrix
!     NOSEG   INTEGER       1         INPUT   number of segments
!     NOTOT   INTEGER       1         INPUT   total number of systems
!     ISYS    INTEGER       1         INPUT   system considered
!     NSYS    INTEGER       1         INPUT   number of systems
!
      real     CONC(*) , RHS(*)
      integer  NOSEG  , NOTOT  , ISYS, NSYS

      integer  iset, iseg, i1, k1
!
!         put result in concentration array
!
      ISET = 1
      DO 10 ISEG = 1 , NOSEG
      I1 = (ISEG-1)*NOTOT
      DO 10 K1 = ISYS, ISYS+NSYS-1
      CONC (I1+K1) = RHS ( ISET )
      RHS ( ISET ) = 0.0
      ISET = ISET + 1
   10 CONTINUE
!
      RETURN
      END

      end module m_dlwqb2

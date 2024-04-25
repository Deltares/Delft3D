!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

!     MODULE              : PCUXFI
!
!     CREATED             : June 2000  by Jan van Beek
!
!     FUNCTION            : Put DOS/PC variables as HP/UNIX variables
!                           in a file as a byte stream, using (assumed)
!                           'BINARY' file
!
!     REMARKS             : Open output file in FORTRAN without with
!                           FORM options.
!
!     MEMBERS             : UXPUTI, puts an integer variable
!                           UXPUTR, puts a real variable
!                           UXPUTS, puts a string
!                           UXPUTA, puts a real array
!                           UXPUTJ, puts an integer array
!                           ISWAP,  swaps UNIX/PC integer variables
!                           RSWAP,  swaps UNIX/PC real variables
!
      SUBROUTINE UXPUTI ( ILUN  , IVAR  )
!
!     FUNCTION            : Puts an DOS/PC integer as UNIX variable
!                           to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  :
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     IVAR    INTEGER       1     INPUT   Variable to be put to file
!
!     DECLARATION of arguments
!
      INTEGER ILUN  , IVAR
!
!     DECLARATION of local variables
!
      INTEGER     ISWAP
!
      WRITE(ILUN) ISWAP(IVAR)
!
      RETURN
      END
      SUBROUTINE UXPUTR ( ILUN  , RVAR  )
!
!     FUNCTION            : Puts a real DOS/PC variable as UNIX
!                           real variable to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  :
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     RVAR    REAL          1     INPUT   Variable to be put to file
!
!     DECLARATION of arguments
!
      INTEGER ILUN
      REAL    RVAR
!
!     DECLARATION of local variables
!
      CHARACTER*4 RSWAP
!
      WRITE(ILUN) RSWAP(RVAR)
!
      RETURN
      END
      SUBROUTINE UXPUTS ( ILUN  , CHVAR , ICHLEN )
!
!     FUNCTION            : Puts a character DOS variable as UNIX
!                           character variable to unit ILUN as a byte
!                           stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  :
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     CHVAR   CHARACTER*(*) 1     INPUT   Variable to be put to file
!     ICHLEN  INTEGER       1     INPUT   Nf bytes to be written to file
!
!     DECLARATION of arguments
!
      INTEGER       ILUN  , ICHLEN
      CHARACTER*(*) CHVAR
!
      WRITE(ILUN)   CHVAR(1:ICHLEN)
!
      RETURN
      END
      SUBROUTINE UXPUTA ( ILUN  , AVAR  , IALEN )
!
!     FUNCTION            : Puts a real DOS array as UNIX real
!                           array to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  :
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     AVAR    REAL          *     INPUT   Variable to be put to file
!     IALEN   INTEGER       1     INPUT   Length of array
!
!     DECLARATION of arguments
!
      INTEGER       ILUN  , IALEN
      REAL          AVAR(IALEN)
!
!     DECLARATION of local variables
!
      CHARACTER*4   RSWAP
!
      DO I = 1 , IALEN
         WRITE(ILUN) RSWAP(AVAR(I))
      ENDDO
!
      RETURN
      END
      SUBROUTINE UXPUTJ ( ILUN  , IVAR  , IALEN )
!
!     FUNCTION            : Puts an integer DOS array as UNIX integer
!                           array to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  :
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     IVAR    INTEGER       *     INPUT   Variable to be put to file
!     IALEN   INTEGER       1     INPUT   Length of array
!
!     DECLARATION of arguments
!
      INTEGER ILUN  , IALEN
      INTEGER IVAR(IALEN)
!
!     DECLARATION of local variables
!
      INTEGER     ISWAP
!
      DO I = 1 , IALEN
         WRITE(ILUN) ISWAP(IVAR(I))
      ENDDO
!
      RETURN
      END
      INTEGER FUNCTION ISWAP(IVAR)
!
!     Swaps UNIX/PC integer variables
!
      INTEGER       IVAR
!
      INTEGER       ILOC1, ILOC2
      CHARACTER*4   WORD1, WORD2
!
      EQUIVALENCE ( ILOC1, WORD1)
      EQUIVALENCE ( ILOC2, WORD2)
!
      ILOC1 = IVAR
      WORD2(1:1) = WORD1(4:4)
      WORD2(2:2) = WORD1(3:3)
      WORD2(3:3) = WORD1(2:2)
      WORD2(4:4) = WORD1(1:1)
      ISWAP = ILOC2
!
      RETURN
      END
      CHARACTER*4 FUNCTION RSWAP(RVAR)
!
!     Swaps UNIX/PC real variables
!
      REAL          RVAR
!
      REAL          RLOC1, RLOC2
      CHARACTER*4   WORD1, WORD2
!
      EQUIVALENCE ( RLOC1, WORD1)
      EQUIVALENCE ( RLOC2, WORD2)
!
      RLOC1 = RVAR
      WORD2(1:1) = WORD1(4:4)
      WORD2(2:2) = WORD1(3:3)
      WORD2(3:3) = WORD1(2:2)
      WORD2(4:4) = WORD1(1:1)
      RSWAP = WORD2
!
      RETURN
      END

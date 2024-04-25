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

!     MODULE              : JBPUTF
!
!     CREATED             : october 1993 by Jan van Beek
!
!     FUNCTION            : Put HP/UNIX variables as DOS/PC variables
!                           in a file as a byte stream, using the HP
!                           libU77 system routines
!
!     REMARKS             : Open output file in FORTRAN without any
!                           FORM options.
!
!     MEMBERS             : JBPUTI, puts an integer variable
!                           JBPUTR, puts a real variable
!                           JBPUTS, puts a string
!                           JBPUTA, puts a real array
!                           JBPUTJ, puts an integer array
!
      SUBROUTINE JBPUTI ( ILUN  , IVAR  )
!
!     FUNCTION            : Puts an integer HP/UNIX variable as DOS/PC
!                           integer variable to unit ILUN as a byte
!                           stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FPUTC , HP libU77 system function
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
      INTEGER     FPUTC , ILOC  , IB    , IERR  , I
      CHARACTER*1 BYTE
      CHARACTER*4 WORD
!
      EQUIVALENCE ( ILOC , WORD )
!
      ILOC = IVAR
!     DO 100 I = 1 , 4
!        IB   = 5 - I
!        BYTE = WORD(IB:IB)
!        IERR = FPUTC(ILUN,BYTE)
! 100 CONTINUE
      IERR = FPUTC(ILUN,WORD(4:4))
      IERR = FPUTC(ILUN,WORD(3:3))
      IERR = FPUTC(ILUN,WORD(2:2))
      IERR = FPUTC(ILUN,WORD(1:1))
!
      RETURN
      END
      SUBROUTINE JBPUTR ( ILUN  , RVAR  )
!
!     FUNCTION            : Puts a real HP/UNIX variable as DOS/PC
!                           real variable to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FPUTC , HP libU77 system function
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
      INTEGER     FPUTC , IB    , IERR  , I
      REAL        RLOC
      CHARACTER*1 BYTE
      CHARACTER*4 WORD
!
      EQUIVALENCE ( RLOC , WORD )
!
      RLOC = RVAR
!     DO 100 I = 1 , 4
!        IB   = 5 - I
!        BYTE = WORD(IB:IB)
!        IERR = FPUTC(ILUN,BYTE)
! 100 CONTINUE
      IERR = FPUTC(ILUN,WORD(4:4))
      IERR = FPUTC(ILUN,WORD(3:3))
      IERR = FPUTC(ILUN,WORD(2:2))
      IERR = FPUTC(ILUN,WORD(1:1))
!
      RETURN
      END
      SUBROUTINE JBPUTS ( ILUN  , CHVAR , ICHLEN )
!
!     FUNCTION            : Puts a character HP/UNIX variable as DOS/PC
!                           character variable to unit ILUN as a byte
!                           stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FPUTC , HP libU77 system function
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
!     DECLARATION of local variables
!
      INTEGER     FPUTC , IERR  , I     , ICLEN2
      CHARACTER*1 BYTE
!
      ICLEN2 = LEN(CHVAR)
      DO 100 I = 1 , ICHLEN
         IF ( I .LE. ICLEN2 ) THEN
            BYTE = CHVAR(I:I)
         ELSE
            BYTE = ' '
         ENDIF
         IERR = FPUTC(ILUN,BYTE)
  100 CONTINUE
!
      RETURN
      END
      SUBROUTINE JBPUTA ( ILUN  , AVAR  , IALEN )
!
!     FUNCTION            : Puts a real HP/UNIX array as DOS/PC real
!                           array to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : JBPUTR, puts a real variable
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
      INTEGER ILUN  , IALEN
      REAL    AVAR(IALEN)
!
!     DECLARATION of local variables
!
      INTEGER     I     , FPUTC
      REAL        RLOC
      CHARACTER*4 WORD
!
      EQUIVALENCE ( RLOC , WORD )
!
      DO 100 I = 1 , IALEN
!        CALL JBPUTR ( ILUN  , AVAR(I) )
         RLOC = AVAR(I)
         IERR = FPUTC(ILUN,WORD(4:4))
         IERR = FPUTC(ILUN,WORD(3:3))
         IERR = FPUTC(ILUN,WORD(2:2))
         IERR = FPUTC(ILUN,WORD(1:1))
  100 CONTINUE
!
      RETURN
      END
      SUBROUTINE JBPUTJ ( ILUN  , IVAR  , IALEN )
!
!     FUNCTION            : Puts an integer HP/UNIX array as DOS/PC real
!                           array to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : JBPUTI, puts an integer variable
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
      INTEGER     I     , ILOC  , FPUTC , IERR
      CHARACTER*4 WORD
!
      EQUIVALENCE ( ILOC , WORD )
!
      DO 100 I = 1 , IALEN
!        CALL JBPUTI ( ILUN  , IVAR(I) )
         ILOC = IVAR(I)
         IERR = FPUTC(ILUN,WORD(4:4))
         IERR = FPUTC(ILUN,WORD(3:3))
         IERR = FPUTC(ILUN,WORD(2:2))
         IERR = FPUTC(ILUN,WORD(1:1))
  100 CONTINUE
!
      RETURN
      END
!     MODULE              : JBGETF
!
!     CREATED             : october 1993 by Jan van Beek
!
!     FUNCTION            : Get HP/UNIX variables as DOS/PC variables
!                           from a file as a byte stream, using the HP
!                           libU77 system routines
!
!     REMARKS             : Open output file in FORTRAN without any
!                           FORM options.
!
!     MEMBERS             : JBGETI, gets an integer variable
!                           JBGETR, gets a real variable
!                           JBGETS, gets a string
!                           JBGETA, gets a real array
!
      SUBROUTINE JBGETI ( ILUN  , IVAR  , IEND  )
!
!     FUNCTION            : Gets an integer HP/UNIX variable as DOS/PC
!                           integer variable to unit ILUN as a byte
!                           stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FGETC , HP libU77 system function
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     IVAR    INTEGER       1     INPUT   Variable to be put to file
!     IEND    INTEGER       1     OUTPUT  Error indication
!
!     DECLARATION of arguments
!
      INTEGER ILUN  , IVAR  , IEND
!
!     DECLARATION of local variables
!
      INTEGER     FGETC , ILOC  , IB    , IERR  , I
      CHARACTER*1 BYTE
      CHARACTER*4 WORD
!
      EQUIVALENCE ( ILOC , WORD )
!
      DO 100 I = 1 , 4
         IB   = 5 - I
         IERR = FGETC(ILUN,BYTE)
         WORD(IB:IB) = BYTE
  100 CONTINUE
      IVAR = ILOC
      IEND = ABS(IERR)
!
      RETURN
      END
      SUBROUTINE JBGETR ( ILUN  , RVAR  , IEND  )
!
!     FUNCTION            : Gets a real HP/UNIX variable as DOS/PC
!                           real variable to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FGETC , HP libU77 system function
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     RVAR    REAL          1     INPUT   Variable to be put to file
!     IEND    INTEGER       1     OUTPUT  Error indication
!
!     DECLARATION of arguments
!
      INTEGER ILUN  , IEND
      REAL    RVAR
!
!     DECLARATION of local variables
!
      INTEGER     FGETC , IB    , IERR  , I
      REAL        RLOC
      CHARACTER*1 BYTE
      CHARACTER*4 WORD
!
      EQUIVALENCE ( RLOC , WORD )
!
      DO 100 I = 1 , 4
         IB   = 5 - I
         IERR = FGETC(ILUN,BYTE)
         WORD(IB:IB) = BYTE
  100 CONTINUE
      RVAR = RLOC
      IEND = ABS(IERR)
!
      RETURN
      END
      SUBROUTINE JBGETS ( ILUN  , CHVAR , ICHLEN , IEND  )
!
!     FUNCTION            : Gets a character HP/UNIX variable as DOS/PC
!                           character variable to unit ILUN as a byte
!                           stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : FGETC , HP libU77 system function
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     CHVAR   CHARACTER*(*) 1     INPUT   Variable to be put to file
!     ICHLEN  INTEGER       1     INPUT   Nf bytes to be written to file
!     IEND    INTEGER       1     OUTPUT  Error indication
!
!     DECLARATION of arguments
!
      INTEGER       ILUN  , ICHLEN , IEND
      CHARACTER*(*) CHVAR
!
!     DECLARATION of local variables
!
      INTEGER     FGETC , IERR  , I     , ICLEN2
      CHARACTER*1 BYTE
!
      ICLEN2 = LEN(CHVAR)
      DO 100 I = 1 , ICHLEN
         IERR = FGETC(ILUN,BYTE)
         IF ( I .LE. ICLEN2 ) THEN
            CHVAR(I:I) = BYTE
         ENDIF
  100 CONTINUE
!
      IEND = ABS(IERR)
!
      RETURN
      END
      SUBROUTINE JBGETA ( ILUN  , AVAR  , IALEN , IEND  )
!
!     FUNCTION            : Gets a real HP/UNIX array as DOS/PC real
!                           array to unit ILUN as a byte stream.
!
!     LOGICAL UNITNUMBERS : ILUN  , output byte stream
!
!     SUBROUTINES CALLED  : JBGETR, gets a real variable
!
!     ARGUMENTS           :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     ILUN    INTEGER       1     INPUT   Unit number output file
!     AVAR    REAL          *     INPUT   Variable to be put to file
!     IALEN   INTEGER       1     INPUT   Length of array
!     IEND    INTEGER       1     OUTPUT  Error indication
!
!     DECLARATION of arguments
!
      INTEGER ILUN  , IALEN , IEND
      REAL    AVAR(IALEN)
!
!     DECLARATION of local variables
!
      INTEGER     I
!
      DO 100 I = 1 , IALEN
         CALL JBGETR ( ILUN  , AVAR(I) , IEND )
  100 CONTINUE
!
      RETURN
      END

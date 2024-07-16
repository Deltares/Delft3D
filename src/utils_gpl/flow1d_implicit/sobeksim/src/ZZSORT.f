      SUBROUTINE ZZSORT ( NPNTRS, PNTNMS, PNTADR, PNTLEN )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             ZZSORT (ZZ SORT)
c
c Module description: Sort names in a particular memory pool
c
c                     This routine is called to sort the names in a
c                     particular memory pool (Shell sort method).
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 npntrs            I  Number of variable names
c  3 pntadr            IO Variable addresses in pool
c  4 pntlen            IO Variable lengths in pool
c  2 pntnms            IO Allocated variable names
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zzsort.pf,v $
c Revision 1.3  1999/03/15  15:52:40  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:03:48  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:59  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:05  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      INTEGER         NPNTRS
      CHARACTER       PNTNMS(*)*(*)
      INTEGER         PNTADR(*)
      INTEGER         PNTLEN(*)
c
c     Local variables
c
      INTEGER         M, N, JUMP
      CHARACTER*16    TNAM
      INTEGER         TLEN, TADR
      LOGICAL         ALDONE


      JUMP = NPNTRS

c
c     While jump > 1
c
 100  CONTINUE
c
c        jump := jump div 2
c
         JUMP = JUMP / 2
c
c        Repeat
c
 200     CONTINUE
            ALDONE = .TRUE.
            DO 300 M = 1, NPNTRS - JUMP
               N = M + JUMP
               IF (PNTNMS(M) .GT. PNTNMS(N)) THEN
c
c                 Swap names
c
                  TNAM      = PNTNMS(M)
                  PNTNMS(M) = PNTNMS(N)
                  PNTNMS(N) = TNAM
c
c                 Swap lengths
c
                  TLEN      = PNTLEN(M)
                  PNTLEN(M) = PNTLEN(N)
                  PNTLEN(N) = TLEN
c
c                 Swap addresses
c
                  TADR      = PNTADR(M)
                  PNTADR(M) = PNTADR(N)
                  PNTADR(N) = TADR
c
c                 Not all done
c
                  ALDONE    = .FALSE.
               ENDIF
 300        CONTINUE
c
c        Until alldone
c
         IF (.NOT. ALDONE) GOTO 200
c
c     End while
c
      IF (JUMP .GT. 1) GOTO 100

      END

SUBROUTINE ZZSORT ( NPNTRS, PNTNMS, PNTADR, PNTLEN )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZSORT (ZZ SORT)
!
! Module description: Sort names in a particular memory pool
!
!                     This routine is called to sort the names in a
!                     particular memory pool (Shell sort method).
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 npntrs            I  Number of variable names
!  3 pntadr            IO Variable addresses in pool
!  4 pntlen            IO Variable lengths in pool
!  2 pntnms            IO Allocated variable names
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zzsort.pf,v $
! Revision 1.3  1999/03/15  15:52:40  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:03:48  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:59  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:05  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   INTEGER         NPNTRS
   CHARACTER       PNTNMS(*)*(*)
   INTEGER         PNTADR(*)
   INTEGER         PNTLEN(*)
!
!     Local variables
!
   INTEGER         M, N, JUMP
   CHARACTER*16    TNAM
   INTEGER         TLEN, TADR
   LOGICAL         ALDONE


   JUMP = NPNTRS

!
!     While jump > 1
!
100 CONTINUE
!
!        jump := jump div 2
!
   JUMP = JUMP / 2
!
!        Repeat
!
200 CONTINUE
   ALDONE = .TRUE.
   DO 300 M = 1, NPNTRS - JUMP
      N = M + JUMP
      IF (PNTNMS(M) .GT. PNTNMS(N)) THEN
!
!                 Swap names
!
         TNAM      = PNTNMS(M)
         PNTNMS(M) = PNTNMS(N)
         PNTNMS(N) = TNAM
!
!                 Swap lengths
!
         TLEN      = PNTLEN(M)
         PNTLEN(M) = PNTLEN(N)
         PNTLEN(N) = TLEN
!
!                 Swap addresses
!
         TADR      = PNTADR(M)
         PNTADR(M) = PNTADR(N)
         PNTADR(N) = TADR
!
!                 Not all done
!
         ALDONE    = .FALSE.
      ENDIF
300 CONTINUE
!
!        Until alldone
!
   IF (.NOT. ALDONE) GOTO 200
!
!     End while
!
   IF (JUMP .GT. 1) GOTO 100

END

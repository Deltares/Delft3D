c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling system
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory management routines
c
c Programmer:         A. Hoekstra
c
c Module:             mempool.i
c
c Module description: Include file for memory management routines
c
c Pre condition:
c
c Post condition:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id: mempool.i,v 1.6 1996/12/24 15:02:07 scherjo Exp $
c
c History:
c $Log: mempool.i,v $
c Revision 1.6  1996/12/24  15:02:07  scherjo
c Size of character array extended to include id and names
c
c Revision 1.5  1996/10/31  10:31:53  kuipe_j
c Extra resistance finished
c
c Revision 1.4  1996/09/03  14:54:31  kuipe_j
c frequency time hist,etc
c
c Revision 1.3  1995/05/30  09:55:44  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:03:36  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:48  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:13  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      INTEGER   CPSIZE,
     +          DPSIZE,
     +          IPSIZE,
     +          RPSIZE,
     +          LPSIZE

C     PARAMETER (CPSIZE=2000,
C    +           DPSIZE=140000,
C    +           IPSIZE=50000,
C    +           LPSIZE=2000,
C    +           RPSIZE=1200000 )
c      PARAMETER (CPSIZE=500000, op 1-7-2002 veranderd ivm nat model
      PARAMETER (CPSIZE=2000000,
     +           DPSIZE=1500000,
     +           IPSIZE=200000,
     +           LPSIZE=20000,
c     +           RPSIZE=5000000 ) op 1-11 veranderd ivm grondwater 
c     +           RPSIZE=10000000 ) op 27-11 veranderd ivm grondwater 
c     +           RPSIZE=20000000 ) op 22-05-03 veranderd ivm Kalman 
     +           RPSIZE=30000000 )

      CHARACTER        CP(CPSIZE)
      DOUBLE PRECISION DP(DPSIZE)
      INTEGER          IP(IPSIZE)
      REAL             RP(RPSIZE)
      LOGICAL          LP(LPSIZE)

      COMMON /CPOOLC/ CP
      COMMON /DPOOLC/ DP
      COMMON /IPOOLC/ IP
      COMMON /RPOOLC/ RP
      COMMON /LPOOLC/ LP

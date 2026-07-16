!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling system
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory management routines
!
! Programmer:         A. Hoekstra
!
! Module:             mempool.i
!
! Module description: Include file for memory management routines
!
! Pre condition:
!
! Post condition:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id: mempool.i,v 1.6 1996/12/24 15:02:07 scherjo Exp $
!
! History:
! $Log: mempool.i,v $
! Revision 1.6  1996/12/24  15:02:07  scherjo
! Size of character array extended to include id and names
!
! Revision 1.5  1996/10/31  10:31:53  kuipe_j
! Extra resistance finished
!
! Revision 1.4  1996/09/03  14:54:31  kuipe_j
! frequency time hist,etc
!
! Revision 1.3  1995/05/30  09:55:44  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:03:36  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:48  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:13  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:04  kuipe_j
! Initial version
!
!
!***********************************************************************
!
INTEGER   CPSIZE,&
&DPSIZE,&
&IPSIZE,&
&RPSIZE,&
&LPSIZE

!     PARAMETER (CPSIZE=2000,
!    +           DPSIZE=140000,
!    +           IPSIZE=50000,
!    +           LPSIZE=2000,
!    +           RPSIZE=1200000 )
!      PARAMETER (CPSIZE=500000, op 1-7-2002 veranderd ivm nat model
PARAMETER (CPSIZE=2000000,&
&DPSIZE=1500000,&
&IPSIZE=200000,&
&LPSIZE=20000,&
!     +           RPSIZE=5000000 ) op 1-11 veranderd ivm grondwater
!     +           RPSIZE=10000000 ) op 27-11 veranderd ivm grondwater
!     +           RPSIZE=20000000 ) op 22-05-03 veranderd ivm Kalman
&RPSIZE=30000000 )

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

SUBROUTINE SRTMEM ()

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             SRTMEM (SoRT MEMory)
!
! Module description: Sort names in memory in ascending order
!
!                     To speed up the search process allocated names can
!                     be sorted in alphabetical order. The search pro-
!                     cess serially searches through the name tables and
!                     remembers the position of the last name found. If
!                     names are retrieved in alphabetical order the
!                     search routine will be working in the quickest
!                     mode.
!
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzsort  ZZ SORT
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: srtmem.pf,v $
! Revision 1.2  1995/05/30  07:03:42  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:54  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\pointrs.i'

   IF (NCPNTR .GT. 1) THEN
      CALL ZZSORT ( NCPNTR, CPNTRS, CADDRS, CLENGT )
   ENDIF

   IF (NDPNTR .GT. 1) THEN
      CALL ZZSORT ( NDPNTR, DPNTRS, DADDRS, DLENGT )
   ENDIF

   IF (NIPNTR .GT. 1) THEN
      CALL ZZSORT ( NIPNTR, IPNTRS, IADDRS, ILENGT )
   ENDIF

   IF (NRPNTR .GT. 1) THEN
      CALL ZZSORT ( NRPNTR, RPNTRS, RADDRS, RLENGT )
   ENDIF

   IF (NLPNTR .GT. 1) THEN
      CALL ZZSORT ( NLPNTR, LPNTRS, LADDRS, LLENGT )
   ENDIF

END

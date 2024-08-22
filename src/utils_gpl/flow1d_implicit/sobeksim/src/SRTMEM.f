      SUBROUTINE SRTMEM ()

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             SRTMEM (SoRT MEMory)
c
c Module description: Sort names in memory in ascending order
c
c                     To speed up the search process allocated names can
c                     be sorted in alphabetical order. The search pro-
c                     cess serially searches through the name tables and
c                     remembers the position of the last name found. If
c                     names are retrieved in alphabetical order the
c                     search routine will be working in the quickest
c                     mode.
c
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzsort  ZZ SORT
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: srtmem.pf,v $
c Revision 1.2  1995/05/30  07:03:42  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:54  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
c Initial version
c
c
c***********************************************************************
c
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

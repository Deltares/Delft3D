      SUBROUTINE INIMEM ()

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             INIMEM (INItialise MEMory)
c
c Module description: Initialise memory
c
c                     Calling this routine will clear the memory pools
c                     and deletes all names from the allocated name
c                     tables.
c
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: inimem.pf,v $
c Revision 1.2  1995/05/30  07:03:34  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:46  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:02  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\pointrs.i'

      NCPNTR = 0
      NDPNTR = 0
      NIPNTR = 0
      NRPNTR = 0
      NLPNTR = 0

      COLDPT = 1
      DOLDPT = 1
      IOLDPT = 1
      ROLDPT = 1
      LOLDPT = 1

      CLENGT(1) = 0
      DLENGT(1) = 0
      ILENGT(1) = 0
      RLENGT(1) = 0
      LLENGT(1) = 0

      CADDRS(1) = 1
      DADDRS(1) = 1
      IADDRS(1) = 1
      RADDRS(1) = 1
      LADDRS(1) = 1

      END

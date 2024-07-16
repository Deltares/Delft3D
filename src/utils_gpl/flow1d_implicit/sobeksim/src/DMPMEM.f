      subroutine dmpmem (lu)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             DMPMEM (DuMP MEMory)
c
c Module description: Dump names of known variables to file
c
c                     This subroutine writes all the known variable
c                     names to a file. This routine can be used for
c                     debugging purposes. The file will tell you which
c                     names are allocated, their start adresses in the
c                     pool and the length of each variable.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lu                P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c zzdump  ZZ DUMP
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: dmpmem.pf,v $
c Revision 1.2  1995/05/30  07:03:24  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:37  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      integer   lu

      include '..\include\pointrs.i'

      call zzdump ( lu, ncpntr, cpntrs, caddrs, clengt )
      call zzdump ( lu, ndpntr, dpntrs, daddrs, dlengt )
      call zzdump ( lu, nipntr, ipntrs, iaddrs, ilengt )
      call zzdump ( lu, nlpntr, lpntrs, laddrs, llengt )
      call zzdump ( lu, nrpntr, rpntrs, raddrs, rlengt )

      return
      end

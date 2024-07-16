      subroutine zzdump (lu, npntrs, pntnms, pntadr, pntlen)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Memory Management Module
c
c Programmer:         A. Hoekstra / S.L. van der Woude
c
c Module:             ZZDUMP (ZZ DUMP)
c
c Module description: Dump information of a memory pool to file
c
c                     This routine is called by routine DEBMEM and will
c                     report information about a particular memory pool.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lu                I  Logical unitnumber associated with file.
c  2 npntrs            I  Number of variable names
c  4 pntadr            I  Variable addresses in pool
c  5 pntlen            I  Variable lengths in pool
c  3 pntnms            I  Allocated variable names
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: zzdump.pf,v $
c Revision 1.3  1995/10/18  08:59:48  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:03:43  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:08:55  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
c Initial version
c
c
c***********************************************************************
c

      integer   lu
      integer   npntrs
      character pntnms(*)*(*)
      integer   pntadr(*)
      integer   pntlen(*)

      integer   i

      write (lu,*) ' '
      do 100 i = 1, npntrs
         write (lu,200) pntnms(i), pntadr(i), pntlen(i)
 100  continue

 200  format ( a16, ' starts at ', i6, ' length ', i6 )
      return
      end

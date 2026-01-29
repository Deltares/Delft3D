subroutine zzdump (lu, npntrs, pntnms, pntadr, pntlen)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             ZZDUMP (ZZ DUMP)
!
! Module description: Dump information of a memory pool to file
!
!                     This routine is called by routine DEBMEM and will
!                     report information about a particular memory pool.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lu                I  Logical unitnumber associated with file.
!  2 npntrs            I  Number of variable names
!  4 pntadr            I  Variable addresses in pool
!  5 pntlen            I  Variable lengths in pool
!  3 pntnms            I  Allocated variable names
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: zzdump.pf,v $
! Revision 1.3  1995/10/18  08:59:48  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:03:43  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:55  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:03  kuipe_j
! Initial version
!
!
!***********************************************************************
!

   integer   lu
   integer   npntrs
   character pntnms(*)*(*)
   integer   pntadr(*)
   integer   pntlen(*)

   integer   i

   write (lu,*) ' '
   do 100 i = 1, npntrs
      write (lu,200) pntnms(i), pntadr(i), pntlen(i)
100 continue

200 format ( a16, ' starts at ', i6, ' length ', i6 )
   return
end

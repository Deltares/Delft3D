subroutine dmpmem (lu)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             DMPMEM (DuMP MEMory)
!
! Module description: Dump names of known variables to file
!
!                     This subroutine writes all the known variable
!                     names to a file. This routine can be used for
!                     debugging purposes. The file will tell you which
!                     names are allocated, their start adresses in the
!                     pool and the length of each variable.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lu                P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! zzdump  ZZ DUMP
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: dmpmem.pf,v $
! Revision 1.2  1995/05/30  07:03:24  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:37  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   integer   lu

   include '..\include\pointrs.i'

   call zzdump ( lu, ncpntr, cpntrs, caddrs, clengt )
   call zzdump ( lu, ndpntr, dpntrs, daddrs, dlengt )
   call zzdump ( lu, nipntr, ipntrs, iaddrs, ilengt )
   call zzdump ( lu, nlpntr, lpntrs, laddrs, llengt )
   call zzdump ( lu, nrpntr, rpntrs, raddrs, rlengt )

   return
end

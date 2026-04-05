subroutine wqpntr (lupntr ,npntr, pntr)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQPNTR (Water Quality print PoiNTeRs)
!
! Module description: Writes the pointer table to a ascii file.
!                     This is a interface file to delwaq.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lupntr            I  Unit of interface file to Delwaq.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqpntr.pf,v $
! Revision 1.1  1996/10/31  09:51:46  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!
   integer  lupntr ,npntr
   integer  pntr (4,npntr)

   integer       i, j

   do 10 i = 1, npntr
      write(lupntr,100) (pntr(j,i), j=1,2)
10 continue
!
!     Close file
!
   close ( unit = lupntr )
!
!     Integer format
!
100 format ( 2I8,'       0       0')

end

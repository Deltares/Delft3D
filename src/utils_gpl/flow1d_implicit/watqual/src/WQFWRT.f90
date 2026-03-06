subroutine wqfwrt ( lu, time, nrar, rar )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFWRT (Water Quality File WRiTe)
!
! Module description: Append information to binary output file
!
!                     This routine will append information to the output
!                     file. The array containing reals will be accepted
!                     and written to the output file. The real values
!                     will be preceded by one integer usually indicating
!                     time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lu                I  Logical unitnumber associated with file.
!  3 nrar              I  Number of elements in array RA.
!  4 rar(nrar)         I  Real array containing information to be writ-
!                         ten to file.
!  2 time              I  Actual time level tn+1. in sec.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqfwrt.pf,v $
! Revision 1.3  1995/10/18  09:00:49  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:08:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:55  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer        lu,  time, nrar
   real           rar(nrar)
!
!     Variables
!
   integer        j
!
   write(lu) time, (rar(j), j=1,nrar)
!
   return
end

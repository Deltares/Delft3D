subroutine wqhsec ( itime, rtime )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQHSEC (Water Quality Hundredth of SEConds)
!
! Module description: Convert time HHMMSSHH to hundredth of seconds
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 itime             I  time in HHMMSSHH format
!  2 rtime             O  time in hundredth of seconds
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqhsec.pf,v $
! Revision 1.2  1995/05/30  07:08:39  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:00  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer  itime
   integer  rtime
!
!     Local variables
!
   integer  hour, minute, second
!
!     Extract time
!
   hour   = int (itime / 1000000)
   minute = mod (int (itime / 10000), 100)
   second = mod (itime, 10000 )
!
!     Calculate time in seconds
!
   rtime   =  second          +&
   &(minute *   6000) +&
   &(hour   * 360000)

end

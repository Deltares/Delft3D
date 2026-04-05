subroutine wqtime ( itim1, itim2, dt )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQTIME (Water Quality TIME conversion)
!
! Module description: Calculate the delta time between two water quality
!                     time steps in seconds
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 dt                O  Computational time step dt [sec].
!  1 itim1             P  -
!  2 itim2             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqdnum  Water Quality Day NUMber
! wqhsec  Water Quality Hundredth of SEConds
! wqmday  Water Quality Maximum DAY number
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqtime.pf,v $
! Revision 1.5  1997/01/20  13:14:21  kuipe_j
! Remove auz. output
!
! Revision 1.4  1996/12/20  15:44:30  kuipe_j
! rounding error in time step
!
! Revision 1.3  1995/10/18  09:00:50  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:08:45  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:06  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:29  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer itim1(2), itim2(2)
   integer dt
!
!     Local variables
!
   integer day1, day2, hsec1, hsec2, nhsec, diff, mday, difday
!
!     Calculate daynumber of date 1 and 2
!
   call wqdnum ( itim1(1), day1 )
   call wqdnum ( itim2(1), day2 )
!
!     Calculate time in hundreds of seconds
!
   call wqhsec ( itim1(2), hsec1 )
   call wqhsec ( itim2(2), hsec2 )
!
!     Number of seconds between days
!
   if (day2 - day1 .lt. 0) then
      call wqmday ( itim1(1), mday )
      difday = mday - day1 + day2
   else
      difday = day2 - day1
   endif
!
!     Calculate number of 100 th seconds
!
   nhsec = difday * 8640000
!
!     Difference
!
   diff = hsec2 + nhsec - hsec1
!
!     Calculate dt [s]
!
   dt = (diff + 50) / 100
!
end

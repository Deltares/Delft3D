subroutine statim (fd_nefis_rst, grpnam ,nameti ,itim  ,ncel ,&
&kerstt )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Fileio module (interface to nefis)
!
! Programmer:         J.Kuipers
!
! Module:             STATIM (Search resTArt TIMe point)
!
! Module description: Search the cell which contains restart information
!                     at the specified time step number.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grpnam            P  -
!  5 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!  7 kerstt            O  Error code:
!                         0  = No error
!                         <0 = Nefis error code
!                         >0 = Sobek error code (ker)
!  4 nameti            P  -
!  6 ncel              IO Actual cell number of a group.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getiel  GET Integer ELement from nefis file
! inqmxi  INQuire for MaXimum Index of data group
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
! $Log: statim.pf,v $
! Revision 1.4  1996/01/16  15:01:10  kuipe_j
! Restart improvements
!
! Revision 1.3  1995/10/18  08:59:09  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  06:57:18  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:18  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       ncel    ,kerstt
   integer       fd_nefis_rst ,itim(2)
   character(len=*) grpnam    ,nameti
!
!     Declaration of local variables
!
   integer       error     ,icel      ,maxcel
   integer       uindex(3) ,usrord(1) ,itimf(2)
!
!     Declaration of external functions
!
   integer       inqmxi    ,getiel
   external      inqmxi    ,getiel
!
   data          usrord    /1/
!
!     Get maximum cel number.
!
   error = inqmxi (fd_nefis_rst,  grpnam ,maxcel )
   if (error.ne.0) goto 1000
!
!     Search for cel with a time equal to the starting time.
!
   ncel      = 0
   icel      = 0
   uindex(3) = 1

!
! --- Loop over time steps on file -------->
10 continue
   if (icel.lt.maxcel) then
      icel      = icel + 1
      uindex(1) = icel
      uindex(2) = icel
      error = getiel (fd_nefis_rst ,grpnam ,nameti ,&
      &uindex  ,usrord ,8      ,itimf  )
      if (error.ne.0) goto 1000
!
      if (itimf(1).eq.itim(1) .and. itimf(2).eq.itim(2)) then
!           Current time on file is equal to the starting time.
         ncel = icel
         icel = maxcel
!
      else if (itimf(1).gt.itim(1) .or.&
      &(itimf(1).eq.itim(1) .and. itimf(2).gt.itim(2))) then
!           Current time on file is greater then the starting time.
         icel = maxcel
!
      endif
      goto 10
   endif
! <---End loop -----------------------------
!
!     Remark: Looping forwards results in finding of the most recent
!             written cell in case there are 2 different cells with
!             the same time. (Duplicate times may occur if a previous
!             run was not restarted from the last time step of the
!             file.)
!
   if (ncel.eq.0) kerstt = 3
!
   return
!
1000 continue
   kerstt = error
end

      subroutine statim (fd_nefis_rst, grpnam ,nameti ,itim  ,ncel ,
     &                   kerstt )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Fileio module (interface to nefis)
c
c Programmer:         J.Kuipers
c
c Module:             STATIM (Search resTArt TIMe point)
c
c Module description: Search the cell which contains restart information
c                     at the specified time step number.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grpnam            P  -
c  5 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c  7 kerstt            O  Error code:
c                         0  = No error
c                         <0 = Nefis error code
c                         >0 = Sobek error code (ker)
c  4 nameti            P  -
c  6 ncel              IO Actual cell number of a group.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getiel  GET Integer ELement from nefis file
c inqmxi  INQuire for MaXimum Index of data group
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: statim.pf,v $
c Revision 1.4  1996/01/16  15:01:10  kuipe_j
c Restart improvements
c
c Revision 1.3  1995/10/18  08:59:09  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  06:57:18  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:18  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       ncel    ,kerstt
      integer       fd_nefis_rst ,itim(2)
      character(len=*) grpnam    ,nameti
c
c     Declaration of local variables
c
      integer       error     ,icel      ,maxcel
      integer       uindex(3) ,usrord(1) ,itimf(2)
c
c     Declaration of external functions
c
      integer       inqmxi    ,getiel
      external      inqmxi    ,getiel
c
      data          usrord    /1/
c
c     Get maximum cel number.
c
      error = inqmxi (fd_nefis_rst,  grpnam ,maxcel )
      if (error.ne.0) goto 1000
c
c     Search for cel with a time equal to the starting time.
c
      ncel      = 0
      icel      = 0
      uindex(3) = 1

c
c --- Loop over time steps on file -------->
   10 continue
      if (icel.lt.maxcel) then
         icel      = icel + 1
         uindex(1) = icel
         uindex(2) = icel
         error = getiel (fd_nefis_rst ,grpnam ,nameti ,
     &                   uindex  ,usrord ,8      ,itimf  )
         if (error.ne.0) goto 1000
c
         if (itimf(1).eq.itim(1) .and. itimf(2).eq.itim(2)) then
c           Current time on file is equal to the starting time.
            ncel = icel
            icel = maxcel
c
         else if (itimf(1).gt.itim(1) .or.
     &           (itimf(1).eq.itim(1) .and. itimf(2).gt.itim(2))) then
c           Current time on file is greater then the starting time.
            icel = maxcel
c
         endif
         goto 10
      endif
c <---End loop -----------------------------
c
c     Remark: Looping forwards results in finding of the most recent
c             written cell in case there are 2 different cells with
c             the same time. (Duplicate times may occur if a previous
c             run was not restarted from the last time step of the
c             file.)
c
      if (ncel.eq.0) kerstt = 3
c
      return
c
 1000 continue
      kerstt = error
      end

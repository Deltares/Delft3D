subroutine FLREST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,&
&ngrid  ,lwqin  ,ncelst ,nameel ,h2     ,q2    ,&
&conhis ,strhis ,qaggr  ,qlaggr ,arexop ,arexcn,&
&lagstm ,nlags  ,buflag ,lgrwt  ,grhis  ,ker  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLREST (FLow REading of reSTart information)
!
! Module description: Read restart information into memory
!
!                     This routine reads the saved restart information
!                     into memory.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 18 arexcn            P  -
! 17 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
! 13 conhis            P  -
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnamf            P  -
! 11 h2                P  -
! 19 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 10 nameel            P  -
!  9 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
!  6 ncontr            I  Number of controlled structures.
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  7 ngrid             I  Number of grid points in network.
!  5 nstru             I  Number of structures.
! 12 q2                P  -
! 15 qaggr             P  -
! 16 qlaggr            P  -
! 14 strhis            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getiel  GET Integer ELement from nefis file
! getrel  GET Real ELement from a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flrest.pf,v $
! Revision 1.11  1999/03/15  15:50:40  kuipe_j
! tabs removed
!
! Revision 1.10  1998/06/08  12:29:48  kuipe_j
! time lag hydr controller
!
! Revision 1.9  1996/02/09  15:13:28  kuipe_j
! a.o. Restart improvements
!
! Revision 1.8  1995/12/06  08:34:59  kuipe_j
! Declarations
!
! Revision 1.7  1995/11/21  11:08:01  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.6  1995/09/22  10:02:12  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:02  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:36:51  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:25  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:25  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:06  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:31  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   logical       lwqin, lgrwt
   integer       nentri ,nstru   ,ncontr ,ngrid  ,ncelst ,ker
   integer       lagstm ,nlags
   integer       fd_nefis_rst, arexop(*), arexcn(ngrid,2)
   real          conhis(5,*),&
   &strhis(dmstrh,*),qaggr(ngrid,3) ,qlaggr(*)
   real          buflag(lagstm,nlags), grhis(*)
   double precision h2(ngrid)       ,q2(ngrid)
   character*(*) grnamf
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error     ,buflen   ,i
   integer       uindex(3) ,usrord(1)
   real          hqreal(ngrid)
!
!     Declaration of external functions
!
   integer       getrel, getiel
   external      getrel, getiel
!
   data          usrord    /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   buflen    = ngrid*4
!
   error = getrel (fd_nefis_rst ,grnamf ,nameel(2) ,&
   &uindex  ,usrord ,buflen ,hqreal)
   if (error.ne.0) goto 1000
   do i=1, ngrid
      h2(i) = dble(hqreal(i))
   enddo
!
   error = getrel (fd_nefis_rst ,grnamf ,nameel(3) ,&
   &uindex  ,usrord ,buflen ,hqreal)
   if (error.ne.0) goto 1000
   do i=1, ngrid
      q2(i) = dble(hqreal(i))
   enddo

!
   if (nstru .gt. 0) then
!
      buflen = nstru*4*dmstrh
      error = getrel (fd_nefis_rst ,grnamf ,nameel(4) ,&
      &uindex  ,usrord ,buflen ,strhis  )
      if (error.ne.0) goto 1000
   endif

   if (ncontr.gt. 0) then
!
      buflen = ncontr*4*5
      error = getrel (fd_nefis_rst, grnamf ,nameel(5) ,&
      &uindex  ,usrord ,buflen ,conhis  )
      if (error.ne.0) goto 1000
   endif
!
   if ( lwqin ) then
      buflen = ngrid*3*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(6) ,&
      &uindex  ,usrord ,buflen ,qaggr   )
      if (error.ne.0) goto 1000
!
      buflen = ngrid*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(7) ,&
      &uindex  ,usrord ,buflen ,qlaggr  )
      if (error.ne.0) goto 1000
   endif
!
   if (arexop(2) .gt. 0) then
      buflen = ngrid*2*4
      error = getiel (fd_nefis_rst, grnamf ,nameel(8) ,&
      &uindex  ,usrord ,buflen ,arexcn  )
      if (error.ne.0) goto 1000
   endif

   if (nlags .gt. 1) then
!
      buflen = lagstm*nlags*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(9) ,&
      &uindex  ,usrord ,buflen ,buflag  )
!
!        Check if buffer size has been changed
!
      if (error.ne.0)  then
         error = 4
         goto 1000
      else
         if (int(buflag(1,1)) .ne. lagstm .or.&
         &int(buflag(2,1)) .ne. nlags) then
            error = 4
            goto 1000
         endif
      endif
   endif
!
   if (lgrwt) then
!
      buflen = ngrid*(dmgrnd+1)*3*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(10) ,&
      &uindex  ,usrord ,buflen ,grhis  )
!
!        Check if buffer size has been changed
!
      if (error.ne.0)  then
         error = 4
         goto 1000
      endif
   endif
!
   goto 1010
!
1000 continue
   ker = error
1010 continue
end

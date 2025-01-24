subroutine FLWRST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,&
&ngrid  ,lwqin  ,ncelst ,itim   ,curtim ,nameel,&
&h2     ,q2     ,contrl ,conhis ,strhis ,qaggr ,&
&qlaggr ,arexop ,arexcn ,lagstm ,nlags  ,buflag,&
&lgrwt  ,grhis  ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLWRST (FLow WRiting of reSTart information)
!
! Module description: Write restart information from memory to restart
!                     file.
!
!                     This routine writes restart information to the
!                     restart file.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 arexcn            P  -
! 20 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
! 16 conhis            P  -
! 15 contrl            P  -
! 11 curtim            I  Current time.
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnamf            P  -
! 13 h2                P  -
! 10 itim              P  -
!  8 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 12 nameel            P  -
!  9 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
!  6 ncontr            I  Number of controlled structures.
! 22 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  7 ngrid             I  Number of grid points in network.
!  5 nstru             I  Number of structures.
! 14 q2                P  -
! 18 qaggr             P  -
! 19 qlaggr            P  -
! 17 strhis            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flrlti  FLow adapt time ReL. TIme contr.
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flwrst.pf,v $
! Revision 1.10  1998/06/08  12:29:46  kuipe_j
! time lag hydr controller
!
! Revision 1.9  1996/02/09  15:13:29  kuipe_j
! a.o. Restart improvements
!
! Revision 1.8  1996/01/17  14:38:59  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:29  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/09/22  10:02:36  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:09  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:37:03  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:39  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:41  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:19  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
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
   integer       nentri ,nstru   ,ncontr ,ngrid  ,ncelst  ,neferr
   integer       lagstm ,nlags
   integer       fd_nefis_rst        ,itim  (2)
   integer       arexop(*)       ,arexcn(ngrid,2)
   real          curtim
   double precision                    :: h2(ngrid)
   double precision                    :: q2(ngrid)
   real          conhis(5,*)    ,&
   &strhis(dmstrh,*),contrl(17,*)   ,&
   &buflag(lagstm,nlags)            ,&
   &qaggr(ngrid,3)  ,qlaggr(*)      ,grhis(*)
   character*(*) grnamf
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error
   integer       uindex(3) ,usrord(1)

   real, allocatable, dimension(:)             :: sngl_buffer
   integer                                     :: i
!
!     Declaration of external functions
!
   integer       flsdat    ,putrel    ,putiel
   external      flsdat    ,putrel    ,putiel
!
   data          usrord    /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1

!
   error   = putiel (fd_nefis_rst ,grnamf ,nameel(1) ,&
   &uindex  ,usrord ,itim   )
   if (error.ne.0) goto 1000
!
   allocate(sngl_buffer(ngrid))
   do i = 1, ngrid
      sngl_buffer(i) = sngl(h2(i))
   enddo
   error = putrel (fd_nefis_rst, grnamf, nameel(2),&
   &uindex, usrord, sngl_buffer)
   if (error.ne.0) goto 1000
!
   do i = 1, ngrid
      sngl_buffer(i) = sngl(q2(i))
   enddo
   error = putrel (fd_nefis_rst, grnamf, nameel(3),&
   &uindex, usrord, sngl_buffer)
   deallocate(sngl_buffer)
   if (error.ne.0) goto 1000
!
   if (nstru .gt. 0) then
!
      error = putrel (fd_nefis_rst ,grnamf ,nameel(4) ,&
      &uindex  ,usrord ,strhis )
      if (error.ne.0) goto 1000
   endif

   if (ncontr .gt. 0) then
!
!        Adapt start times of active controller phases to the
!        relative time used in a next restart run
!
      call flrlti (ncontr ,-curtim , contrl, conhis)
!
      error = putrel (fd_nefis_rst ,grnamf ,nameel(5) ,&
      &uindex  ,usrord ,conhis )
!
!        The start times of active controller phases will be
!        expressed again in relative time used in this run.
!
      call flrlti (ncontr , curtim , contrl, conhis)
!
      if (error.ne.0) goto 1000
   endif
!
   if ( lwqin ) then
      error = putrel (fd_nefis_rst ,grnamf ,nameel(6) ,&
      &uindex  ,usrord ,qaggr  )
      if (error.ne.0) goto 1000
!
      error = putrel (fd_nefis_rst ,grnamf ,nameel(7) ,&
      &uindex  ,usrord ,qlaggr )
      if (error.ne.0) goto 1000
!
   endif
!
   if (arexop(2) .gt. 0) then
      error = putiel (fd_nefis_rst ,grnamf ,nameel(8) ,&
      &uindex  ,usrord ,arexcn )
      if (error.ne.0) goto 1000
   endif
!
!     Write time lag buffer
!
   if (nlags .gt. 1) then
      error = putrel (fd_nefis_rst ,grnamf ,nameel(9) ,&
      &uindex  ,usrord ,buflag )
      if (error.ne.0) goto 1000
   endif
!
!     Write groundwater history
!
   if (lgrwt) then
      error = putrel (fd_nefis_rst ,grnamf ,nameel(10) ,&
      &uindex  ,usrord ,grhis )
      if (error.ne.0) goto 1000
   endif

   error = flsdat(fd_nefis_rst)
!
1000 continue

   neferr = error

end

      subroutine FLWRST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,
     &                   ngrid  ,lwqin  ,ncelst ,itim   ,curtim ,nameel,
     &                   h2     ,q2     ,contrl ,conhis ,strhis ,qaggr ,
     &                   qlaggr ,arexop ,arexcn ,lagstm ,nlags  ,buflag,
     &                   lgrwt  ,grhis  ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLWRST (FLow WRiting of reSTart information)
c
c Module description: Write restart information from memory to restart
c                     file.
c
c                     This routine writes restart information to the
c                     restart file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 21 arexcn            P  -
c 20 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c 16 conhis            P  -
c 15 contrl            P  -
c 11 curtim            I  Current time.
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnamf            P  -
c 13 h2                P  -
c 10 itim              P  -
c  8 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 12 nameel            P  -
c  9 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c  6 ncontr            I  Number of controlled structures.
c 22 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  7 ngrid             I  Number of grid points in network.
c  5 nstru             I  Number of structures.
c 14 q2                P  -
c 18 qaggr             P  -
c 19 qlaggr            P  -
c 17 strhis            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flrlti  FLow adapt time ReL. TIme contr.
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flwrst.pf,v $
c Revision 1.10  1998/06/08  12:29:46  kuipe_j
c time lag hydr controller
c
c Revision 1.9  1996/02/09  15:13:29  kuipe_j
c a.o. Restart improvements
c
c Revision 1.8  1996/01/17  14:38:59  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:29  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/09/22  10:02:36  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:09  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:03  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:39  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:41  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:19  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      logical       lwqin, lgrwt
      integer       nentri ,nstru   ,ncontr ,ngrid  ,ncelst  ,neferr
      integer       lagstm ,nlags
      integer       fd_nefis_rst        ,itim  (2)
      integer       arexop(*)       ,arexcn(ngrid,2)
      real          curtim
      double precision                    :: h2(ngrid)
      double precision                    :: q2(ngrid)
      real          conhis(5,*)    ,
     &              strhis(dmstrh,*),contrl(17,*)   ,
     &              buflag(lagstm,nlags)            ,
     &              qaggr(ngrid,3)  ,qlaggr(*)      ,grhis(*)
      character(len=*) grnamf
      character(len=*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error
      integer       uindex(3) ,usrord(1)
      
      real, allocatable, dimension(:)             :: sngl_buffer
      integer                                     :: i
c
c     Declaration of external functions
c
      integer       flsdat    ,putrel    ,putiel
      external      flsdat    ,putrel    ,putiel
c
      data          usrord    /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
      
c
      error   = putiel (fd_nefis_rst ,grnamf ,nameel(1) ,
     &                  uindex  ,usrord ,itim   )
      if (error.ne.0) goto 1000
c
      allocate(sngl_buffer(ngrid))
      do i = 1, ngrid
         sngl_buffer(i) = real(h2(i), kind=kind(sngl_buffer))
      enddo
      error = putrel (fd_nefis_rst, grnamf, nameel(2),
     &                uindex, usrord, sngl_buffer)
      if (error.ne.0) goto 1000
c
      do i = 1, ngrid
         sngl_buffer(i) = real(q2(i), kind=kind(sngl_buffer))
      enddo
      error = putrel (fd_nefis_rst, grnamf, nameel(3),
     &                uindex, usrord, sngl_buffer)
      deallocate(sngl_buffer)
      if (error.ne.0) goto 1000
c
      if (nstru .gt. 0) then
c
         error = putrel (fd_nefis_rst ,grnamf ,nameel(4) ,
     &                   uindex  ,usrord ,strhis )
         if (error.ne.0) goto 1000
      endif

      if (ncontr .gt. 0) then
c
c        Adapt start times of active controller phases to the
c        relative time used in a next restart run
c
         call flrlti (ncontr ,-curtim , contrl, conhis)
c
         error = putrel (fd_nefis_rst ,grnamf ,nameel(5) ,
     &                   uindex  ,usrord ,conhis )
c
c        The start times of active controller phases will be
c        expressed again in relative time used in this run.
c
         call flrlti (ncontr , curtim , contrl, conhis)
c
         if (error.ne.0) goto 1000
      endif
c
      if ( lwqin ) then
         error = putrel (fd_nefis_rst ,grnamf ,nameel(6) ,
     &                   uindex  ,usrord ,qaggr  )
         if (error.ne.0) goto 1000
c
         error = putrel (fd_nefis_rst ,grnamf ,nameel(7) ,
     &                   uindex  ,usrord ,qlaggr )
         if (error.ne.0) goto 1000
c
      endif
c
      if (arexop(2) .gt. 0) then
         error = putiel (fd_nefis_rst ,grnamf ,nameel(8) ,
     &                   uindex  ,usrord ,arexcn )
         if (error.ne.0) goto 1000
      endif
c
c     Write time lag buffer
c
      if (nlags .gt. 1) then
         error = putrel (fd_nefis_rst ,grnamf ,nameel(9) ,
     &                   uindex  ,usrord ,buflag )
         if (error.ne.0) goto 1000
      endif
c
c     Write groundwater history
c
      if (lgrwt) then
         error = putrel (fd_nefis_rst ,grnamf ,nameel(10) ,
     &                   uindex  ,usrord ,grhis )
         if (error.ne.0) goto 1000
      endif

      error = flsdat(fd_nefis_rst)
c
 1000 continue
 
      neferr = error

      end

      subroutine FLREST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,
     &                   ngrid  ,lwqin  ,ncelst ,nameel ,h2     ,q2    ,
     &                   conhis ,strhis ,qaggr  ,qlaggr ,arexop ,arexcn,
     &                   lagstm ,nlags  ,buflag ,lgrwt  ,grhis  ,ker  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLREST (FLow REading of reSTart information)
c
c Module description: Read restart information into memory
c
c                     This routine reads the saved restart information
c                     into memory.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 18 arexcn            P  -
c 17 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c 13 conhis            P  -
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnamf            P  -
c 11 h2                P  -
c 19 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 10 nameel            P  -
c  9 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c  6 ncontr            I  Number of controlled structures.
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  7 ngrid             I  Number of grid points in network.
c  5 nstru             I  Number of structures.
c 12 q2                P  -
c 15 qaggr             P  -
c 16 qlaggr            P  -
c 14 strhis            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getiel  GET Integer ELement from nefis file
c getrel  GET Real ELement from a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flrest.pf,v $
c Revision 1.11  1999/03/15  15:50:40  kuipe_j
c tabs removed
c
c Revision 1.10  1998/06/08  12:29:48  kuipe_j
c time lag hydr controller
c
c Revision 1.9  1996/02/09  15:13:28  kuipe_j
c a.o. Restart improvements
c
c Revision 1.8  1995/12/06  08:34:59  kuipe_j
c Declarations
c
c Revision 1.7  1995/11/21  11:08:01  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.6  1995/09/22  10:02:12  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:02  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:36:51  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:25  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:25  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:06  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:31  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
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
      integer       nentri ,nstru   ,ncontr ,ngrid  ,ncelst ,ker
      integer       lagstm ,nlags
      integer       fd_nefis_rst, arexop(*), arexcn(ngrid,2)
      real          conhis(5,*),
     &              strhis(dmstrh,*),qaggr(ngrid,3) ,qlaggr(*)
      real          buflag(lagstm,nlags), grhis(*)
      double precision h2(ngrid)       ,q2(ngrid)
      character*(*) grnamf
      character*(*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error     ,buflen   ,i
      integer       uindex(3) ,usrord(1)
      real          hqreal(ngrid)
c
c     Declaration of external functions
c
      integer       getrel, getiel
      external      getrel, getiel
c
      data          usrord    /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      buflen    = ngrid*4
c
      error = getrel (fd_nefis_rst ,grnamf ,nameel(2) ,
     &                uindex  ,usrord ,buflen ,hqreal)
      if (error.ne.0) goto 1000
      do i=1, ngrid
         h2(i) = dble(hqreal(i))
      enddo
c
      error = getrel (fd_nefis_rst ,grnamf ,nameel(3) ,
     &                uindex  ,usrord ,buflen ,hqreal)
      if (error.ne.0) goto 1000
      do i=1, ngrid
         q2(i) = dble(hqreal(i))
      enddo

c
      if (nstru .gt. 0) then
c
         buflen = nstru*4*dmstrh
         error = getrel (fd_nefis_rst ,grnamf ,nameel(4) ,
     &                   uindex  ,usrord ,buflen ,strhis  )
         if (error.ne.0) goto 1000
      endif

      if (ncontr.gt. 0) then
c
         buflen = ncontr*4*5
         error = getrel (fd_nefis_rst, grnamf ,nameel(5) ,
     &                   uindex  ,usrord ,buflen ,conhis  )
         if (error.ne.0) goto 1000
      endif
c
      if ( lwqin ) then
         buflen = ngrid*3*4
         error = getrel (fd_nefis_rst, grnamf ,nameel(6) ,
     &                   uindex  ,usrord ,buflen ,qaggr   )
         if (error.ne.0) goto 1000
c
         buflen = ngrid*4
         error = getrel (fd_nefis_rst, grnamf ,nameel(7) ,
     &                   uindex  ,usrord ,buflen ,qlaggr  )
         if (error.ne.0) goto 1000
      endif
c
      if (arexop(2) .gt. 0) then
         buflen = ngrid*2*4
         error = getiel (fd_nefis_rst, grnamf ,nameel(8) ,
     &                   uindex  ,usrord ,buflen ,arexcn  )
         if (error.ne.0) goto 1000
      endif

      if (nlags .gt. 1) then
c
         buflen = lagstm*nlags*4
         error = getrel (fd_nefis_rst, grnamf ,nameel(9) ,
     &                   uindex  ,usrord ,buflen ,buflag  )
c 
c        Check if buffer size has been changed
c
         if (error.ne.0)  then
            error = 4
            goto 1000
         else
          if (int(buflag(1,1)) .ne. lagstm .or.
     &        int(buflag(2,1)) .ne. nlags) then
             error = 4
             goto 1000
          endif
         endif 
      endif
c
      if (lgrwt) then
c
         buflen = ngrid*(dmgrnd+1)*3*4
         error = getrel (fd_nefis_rst, grnamf ,nameel(10) ,
     &                   uindex  ,usrord ,buflen ,grhis  )
c 
c        Check if buffer size has been changed
c
        if (error.ne.0)  then
            error = 4
            goto 1000
         endif 
      endif
c
      goto 1010
c
 1000 continue
      ker = error
 1010 continue
      end

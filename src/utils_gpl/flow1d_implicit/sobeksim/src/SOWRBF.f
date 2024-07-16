      subroutine sowrbf ( juresd , justrd , jusold ,
     +                    ibuf   , resbuf , strbuf ,
     +                    nstru  , strtyp , ngrid  ,
     +                    jufrou , solbuf , ker    ,
     +                    conv   , lfrou  , frobuf )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         P.J. van Overloop
c
c Module:             SOWRBF (SObek WRite BuFfer)
c
c Module description: Routine writes buffered information if the model
c                     crashes due to drying or exceeding of maximum
c                     number of iterations.
c                     Also in case of high Froude numbers, the buffered
c                     Froude numbers are written.
c
c                     In case of continuation of the computation after
c                     exceeding of the maximum number of iterations
c                     the amount of information written to file will
c                     be limited.
c
c                        [ residuals:  max 20 times
c                          structures: max 10 times
c                          solution:   max  4 times
c                          Froude:     max 10 times ]
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 conv              I  Switch to indicate convergence of solution:
c                         = 0 no convergence yet
c                         = 1 convergence obtained
c                         = 2 no convergence obtained after exceeding
c                             the maximum number of iterations and
c                             the computation will be continued
c 15 frobuf(8)         IO Buffer to determine the maximum Froude
c                         number during the computation. The buffer
c                         contains the current maximum:
c                         (1) Maximum Froude number in cel,
c                         (2) on time step,
c                         (3) on iteration step,
c                         (4) at grid point.
c                         (5-8) The same but for grid points.
c  4 ibuf(5)           IO Contains pointers in circular buffers:
c                         (1) Pointer for buffers with residuals and
c                             structure data.
c                             (A minus means that the solution buffer
c                             has been written in this step.)
c                         (2) Pointer for buffers with solutions
c                         (3) Pointer for Froude buffers
c                         (4) Counter for number of written buffers
c                             (residuals)
c                         (5) Counter for number of written Froude
c                             buffers
c  1 jufrou            I  dump file handler of froude numbers
c  1 juresd            I  dump file handler of results
c  2 justrd            I  dump file handler of structure data
c  3 jusold            I  dump file handler of solutions
c 12 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 lfrou             I  Flag if Froude numbers during simulation are
c                         high
c  9 ngrid             I  Number of grid points in network.
c  7 nstru             I  Number of structures
c  5 resbuf(dmbuf1,6)  I  Buffer with latest residues
c                         (,1,) : time step    (,2,) : iteration step
c                         (,3,) : Delta h      (,4,) : grid point nr
c                         (,5,) : Delta q      (,5,) : grid point nr
c 11 solbuf
c    (dmbuf2,7,ngrid)  IO Buffer with latest solutions or Froude nrs
c                         *  Solutions, for all grid points:
c                         Index 1 : buffer number
c                         Index 2 : Quantity number
c                         (,1,)   : h            (,2,) : q
c                         (,3,)   : h above slot (,4,) : width flow
c                         (,5,)   : area flow    (,6,) : C**2*R
c                         Index 3 : grid point number
c                         *  High Froude numbers on index (,7,):
c                         Index 1 : Froude numbers or identification:
c                         Time info:
c                         (1,,) : time frame pointer (2,,) : -1
c                         (3,,) : time step          (4,,) : iteration step
c                         Froude numbers:
c                         (1,,) : time frame pointer (2,,) :grid point nr
c                         (3,,) : Froude nr in cel
c                         (4,,) : Froude nr in grid point
c                         Index 3 : Frame index
c                         *  Empty
c                         (2,,) : 0
c  6 strbuf
c    (dmbuf1,2,nstru)  I  Buffer with latest structure data
c                         (,1,) : structure discharge
c                         (,2,) : structure head
c  8 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getloc  GET LOCation of gridpoint
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c $Log: sowrbf.pf,v $
c Revision 1.5  1999/03/15  15:03:32  kuipe_j
c Improve Froude file and Dumpfiles
c
c Revision 1.4  1998/12/11  13:07:31  kuipe_j
c improve annotation in dumps
c
c Revision 1.3  1998/06/08  13:15:42  kuipe_j
c time lag hydr controller
c
c Revision 1.2  1997/11/04  14:11:48  kuipe_j
c Bug structure in lat Q
c
c Revision 1.1  1997/01/23  08:30:18  kuipe_j
c Make flow module robust
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
      include '..\include\sobcon.i'
      include '..\include\errcod.i'
c
c     Parameters
c
      integer juresd, justrd, jusold, jufrou, nstru, conv ,
     +        strtyp(10,*), ngrid, ibuf(*), ker 
      real    resbuf(dmbuf1,6), strbuf(dmbuf1,2,*),
     +        solbuf(dmbuf2,7,ngrid),frobuf(8)
      logical lfrou
 
c
c     local variables
c
      integer ibrh, ibrq, istru, ibr, ib, ib1, igrid,
     +        nresb, ibuf1, ibuf2, ibuf3, istep, iter, kode,
     +        l1,l2, lbrnam ,lstnam
      real    xh, xq, x, xi, froude, froud1
      character*40    branam,braprev
      logical noupda, writen
c
      if (ker .eq. fatal .or. conv .eq. 2) then
c
c     Determine if data was written to the buffers in this
c     iteration step and the iteration step number was also updated
c
      if (ibuf(1).lt.0) then
         ibuf(1) = -ibuf(1)
         noupda = .true.
c        no update, one residual less will be printed (the oldest),
c        as its step annotation is wrong.
         nresb = 19
      else
         noupda = .false.
         nresb = 20
      endif
c
c     write residu buffer
c
      if (ibuf(4).le.20 .or. ker.eq.fatal) then
        do 10 ib = 1 , nresb
          ibuf(1) = ibuf(1) - 1
          if ( ibuf(1) .eq. 0 ) ibuf(1) = 20
          ibuf1 = ibuf(1)
          if (nint(resbuf(ibuf1,2)).ge.0) then
            write(juresd,1000)
     +      nint(resbuf(ibuf1,1)),nint(resbuf(ibuf1,2))
            call getloc (nint(resbuf(ibuf1,4)),ibrh,xh)
            call getbrn (ibrh,branam,lbrnam)
            l1 = max(1,lbrnam-29)
            l2 = l1+29
            write(juresd,1010) branam(l1:l2),xh,resbuf(ibuf1,3)
            call getloc (nint(resbuf(ibuf1,6)),ibrq,xq)
            call getbrn (ibrq,branam,lbrnam)
            l1 = max(1,lbrnam-29)
            l2 = l1+29
            write(juresd,1020) branam(l1:l2),xq,resbuf(ibuf1,5)
          endif
   10   continue
        write(juresd,*) ' '
      endif
c
c     write structure data buffer
c
      if ((ibuf(4).le.10 .or. ker.eq.fatal).and.nstru.gt.0) then
        do 20 ib = 1 , 20
          ibuf(1) = ibuf(1) - 1
          if ( ibuf(1) .eq. 0 ) ibuf(1) = 20
          ibuf1 = ibuf(1)
          if (nint(resbuf(ibuf1,2)).ge.0) then
            write(justrd,1000)
     +      nint(resbuf(ibuf1,1)),nint(resbuf(ibuf1,2))
            do 30 istru = 1 , nstru
              call getstr (istru ,branam, lstnam)
              l1 = max(1,lstnam-29)
              l2 = l1+29
              if (strtyp(2,istru).eq.cstbra) then
                write(justrd,2010) branam(l1:l2),
     +                             strbuf(ibuf1,1,istru),
     +                             strbuf(ibuf1,2,istru)
              else
                write(justrd,2020) branam(l1:l2),
     +                             strbuf(ibuf1,1,istru)
              endif
   30       continue
          endif
   20   continue
        write(justrd,*) ' '
      endif
c
c     write solution buffer
c
      if (ibuf(4).le.4 .or. ker.eq.fatal) then
        if (noupda) ibuf(2) = ibuf(2) + 1
        do 40 ib = 1 , 4
          ibuf(2) = ibuf(2) - 1
          if ( ibuf(2) .eq. 0 ) ibuf(2) = 4
          ib1 = ibuf(1) - ib
          if ( ib1 .le. 0 ) ib1 = ib1 + 20
          if (nint(resbuf(ib1,2)).ge.0) then
            write(jusold,1000)
     +      nint(resbuf(ib1,1)),nint(resbuf(ib1,2))
            ibuf2 = ibuf(2)
            braprev = ' '
            do 50 igrid = 1 , ngrid
              call getloc (igrid,ibr,x)
              call getbrn (ibr,branam,lbrnam)
              if (braprev.ne.branam) then
                 write (jusold,3020) branam(1:lbrnam)
                 braprev = branam
              endif   
              write(jusold,3010) x,
     +                           solbuf(ibuf2,1,igrid),
     +                           solbuf(ibuf2,2,igrid),
     +                           solbuf(ibuf2,3,igrid),
     +                           solbuf(ibuf2,4,igrid),
     +                           solbuf(ibuf2,5,igrid),
     +                           solbuf(ibuf2,6,igrid)
   50       continue
          endif
   40   continue
        write(jusold,*) ' '
      endif
c
c     Empty buffer
c
      do 55 ib = 1 , 20
         resbuf(ib,2) = -1.
  55  continue
      endif
c
c    Determine maximum Froude numbers
c
      ibuf3 = mod(ibuf(3),ngrid)+1
      do 70 ib = 1,ngrid
         kode = nint(solbuf(2,7,ibuf3))
         if (kode.eq.-1) then
            istep = nint(solbuf(3,7,ibuf3))
            iter  = nint(solbuf(4,7,ibuf3))
         else if (kode.gt.0) then
c           Maximum Froude number in gridcel
c               froude
            if (solbuf(3,7,ibuf3) .gt. frobuf(1) ) then
                frobuf(1) = solbuf(3,7,ibuf3)
                frobuf(2) = istep
                frobuf(3) = iter
                frobuf(4) = kode
            endif
c           Maximum Froude number in gridpoint
c               froud1
            if (solbuf(4,7,ibuf3) .gt. frobuf(5) ) then
                frobuf(5) = solbuf(4,7,ibuf3)
                frobuf(6) = istep
                frobuf(7) = iter
                frobuf(8) = kode
            endif
         endif
         ibuf3 = mod(ibuf3,ngrid)+1
  70  continue
c
      if (ibuf(5).le.10 .and. lfrou .or. ker.eq.fatal) then
c
c       Skip to start
c
        ibuf3 = mod(ibuf(3),ngrid)+1
        writen = .false.
c
c       Write Froude buffer

c
        do 60 ib = 1,ngrid
          kode = nint(solbuf(2,7,ibuf3))
          if (kode.eq.-1) then
              write(jufrou,200) nint(solbuf(3,7,ibuf3)),
     +                          nint(solbuf(4,7,ibuf3))
             writen = .true.
          else if (kode.gt.0) then
            call getloc(kode,ibr,xi)
            call getbrn (ibr,branam,lbrnam)
            l1 = max(1,lbrnam-19)
            l2 = l1+19
            froude = solbuf(3,7,ibuf3)
            froud1 = solbuf(4,7,ibuf3)
            if ( min(froud1,froude) .ge. 1. ) then
              write(jufrou,230) branam(l1:l2),xi,froud1,froude
            elseif ( froud1 .gt. 1. ) then
              write(jufrou,210) branam(l1:l2),xi,froud1,froude
            elseif ( froude .gt. 1. ) then
              write(jufrou,220) branam(l1:l2),xi,froud1,froude
c           elseif ( froude .gt. .8 ) then
            else
              write(jufrou,240) branam(l1:l2),xi,froud1,froude
            endif
c         else
c
c           Empty
c
          endif
          solbuf(2,7,ibuf3) = 0
          ibuf3 = mod(ibuf3,ngrid)+1
c
  60    continue
        if (writen) then
           write(jufrou,*) ' '
           ibuf(5) = ibuf(5)+1
        endif
        ibuf(3) = ngrid
        solbuf(1,7,ibuf(3)) = ngrid
      endif

      return
c
  200 format (' Time step =',i6,' Iter. step =',i4)
  210 format
     +(4x,A20,' X=',f10.2,' Froude grdpnt=',f10.5,
     +'*** Froude next cel=',f10.5)
  220 format
     +(4x,A20,' X=',f10.2,' Froude grdpnt=',f10.5,
     +'    Froude next cel=',f10.5,'***')
  230 format
     +(4x,A20,' X=',f10.2,' Froude grdpnt=',f10.5,
     +'*** Froude next cel=',f10.5,'***')
  240 format (4x,A20,' X=',f10.2,' Froude grdpnt=',
     +f10.5,'    Froude next cel=',f10.5)
 
 1000 format (' Time step =',i6,' Nr iter. =',i4)
 1010 format (4x,A30,' X=',f10.2,' DH=',f10.5)
 1020 format (4x,A30,' X=',f10.2,' DQ=',f10.3)
 2010 format (4x,A30,' Q=',f10.3,' DH=',f10.3)
 2020 format (4x,A30,' Q=',f10.3)
 3010 format (4x,' X=',f10.2,' H=',f10.5,
     +        ' Q=',f12.5,' Depth=',f10.5,' Wf=',
     +        f10.5,' Wt=',f10.5,' 1/C/C/R=',f10.5)
 3020 format  (1x,A)
c
      end

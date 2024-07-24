      subroutine FLHYWQ(fd_nefis_waq ,ngrid  ,nqlat  ,dlwqts ,dt     ,
     +                  itim   ,first  ,aggr   ,wqagst ,writim ,istep  ,
     +                  nstep  ,af     ,afs    ,at     ,cp     ,psi    ,
     +                  qaggr  ,qlaggr ,wt     ,wfs    ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHYWQ (FLow HYdrodynamic results to Water Quality)
c
c Module description: Subroutine FLHYWQ stores the (aggregated) hydrody-
c                     namic results on the water quality interface file.
c
c                     In subroutine FLHYWQ the aggregated flow results
c                     will be written to the water quality interface
c                     file to be passed later on to the waterquality sub
c                     system.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 af                P  -
c 15 afs               P  -
c 16 at                P  -
c 17 cp                P  -
c  2 dafdwq            P  -
c  1 defdwq            P  -
c  5 dlwqts            IO Highest cell number on water quality interface
c                         file. (DELWAQ time step number)
c  6 dt                I  Computational time step dt [sec].
c  9 firagg            I  TRUE, if first step of aggregating period.
c  8 first             I  True in case of first call.
c 12 istep             I  Current time step number (t(n+1)).
c  7 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 23 juer              P  -
c 24 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 10 lasagg            I  TRUE, if last step of aggregating period.
c  3 ngrid             I  Number of grid points in network.
c  4 nqlat             P  -
c 13 nstep             I  Last time step number in simulation.
c 18 psi               P  -
c 19 qaggr             P  -
c 20 qlaggr            P  -
c 21 wf                P  -
c 22 wfs               P  -
c 11 writim            I  True when data to the NEFIS file will be
c                         written immediately.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flcfwq  FLow Create File for Water Quality
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
c sotime  SObek TIME
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhywq.pf,v $
c Revision 1.5  1999/03/15  14:19:32  kuipe_j
c improve writing Aggr-file
c
c Revision 1.4  1996/09/03  14:52:01  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.3  1995/05/30  09:55:07  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:05  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:50  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:32  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:04  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       fd_nefis_waq, itim(2)
      integer       ngrid, nqlat, dlwqts, istep, nstep, juer, ker,
     +              wqagst
      logical       first, aggr, writim
      real          af   (ngrid)   ,afs   (ngrid,2) ,cp(ngrid,4) ,
     +              at   (ngrid)   ,
     +              qaggr(ngrid,3) ,qlaggr(*)       ,wt(ngrid)   ,
     +              wfs  (ngrid,2) ,psi
      double  precision  dt
c
c     Declaration of local variables
c
      integer       err
      integer       PUTREL, FLSDAT, PUTIEL
      external      PUTREL, FLSDAT, PUTIEL
c
      integer       nelwqi, mcell, i, nrerr
      parameter     (nelwqi=9)
      integer       ord(5), uindex(3), ibuf(1),itim0(2)
      character*16  elwqi(nelwqi)
      character*16  grnamd, grnamw
      character*16  namdes(2)
      character*64  desdes(2)
      character*8   txt
      double  precision  dtag
c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
      data elwqi /'AF', 'AFS', 'AT' , 'C', 'DLWQTM', 'WT', 'WFS',
     &            'QAGGR', 'QLAGGR' /
c
      data (desdes(i), namdes(i), i=1,2) /
     &      'cel nr'             , 'DLWQTS',
     &      'space weight factor', 'PSI' /
c
      data grnamd / 'WQINT-DES-GROUP' /
      data grnamw / 'WQINT-GROUP' /
c
      nrerr = ewqcre
c
c     Set user order
c
      ord = 1

      if (first) then
c
c        Position on AGGR-file and make Nefis initializations
c
         call FLCFWQ(fd_nefis_waq ,ngrid  ,nqlat  ,grnamd ,grnamw ,
     +               namdes, desdes, itim, psi, elwqi,
     +               dlwqts, err)
         if (err.ne.0) goto 1000
      endif
c
      nrerr = ewqapp
c
c     Finish aggregation period and write flows and lateral
c     discharges
c
      if ( aggr .and. .not.first ) then
c
c        Write Q and Qlat values
c
c        Write each array to the data file
c
         uindex(1) = dlwqts
         uindex(2) = dlwqts
         uindex(3) = 1
c
        err = PUTIEL (fd_nefis_waq, grnamw, elwqi(5),
     +                 uindex, ord   , itim  )
        if (err .ne. 0) goto 1000
c
        err = PUTREL (fd_nefis_waq, grnamw, elwqi(8),
     +                 uindex, ord   , qaggr)
        if (err .ne. 0) goto 1000
c
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(9),
     +                 uindex, ord   , qlaggr)
        if (err .ne. 0) goto 1000

      endif
c
c     Start new aggregation period and write time, Af, Wf and C
c
      if ( aggr .or. (first.and.dlwqts.eq.0) ) then
c
c        Update cel number
c
         dlwqts    = dlwqts + 1
c
         uindex(1) = 1
         uindex(2) = 1
         uindex(3) = 1
c
c        Update time step counter on file
c
        if ( writim ) then
            ibuf(1) = dlwqts
           err   = PUTIEL (fd_nefis_waq ,grnamd ,namdes(1) ,
     &                      uindex  ,ord    ,ibuf   )
            if (err.ne.0) goto 1000
        endif
c
c        Set maximum of cells to write
c
        mcell = 7
c
c        Write each array to the data file
c
        do 200 i = 1, mcell
           goto (110,120,130,140,180,160,170), i
  110       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(1),
     +                       uindex, ord   , af)
              goto 180
  120       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(2),
     +                       uindex, ord   , afs)
              goto 180
  130       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(3),
     +                       uindex, ord   , at)
              goto 180
  140       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(4),
     +                       uindex, ord   , cp)
              goto 180
  160       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(6),
     +                       uindex, ord   , wt)
              goto 180
  170       continue
              err = PUTREL (fd_nefis_waq, grnamw, elwqi(7),
     +                       uindex, ord   , wfs)
              goto 180
  180       continue
           if (err .ne. 0) goto 1000
  200    continue
      endif
c
c     Make last written time step on aggregation file complete
c     (i.e. write available flows and lateral discharges)
c
      if (istep.ge.nstep) then
c
c        Write Q and Qlat values
c
c        Write each array to the data file
c
         uindex(1) = dlwqts
         uindex(2) = dlwqts
         uindex(3) = 1
c
c        Calculate last time
c
         dtag = dt * (wqagst-mod(istep,wqagst))
         itim0(1) = itim(1)
         itim0(2) = itim(2)
         call sotime (itim0,dtag)

         err = PUTIEL (fd_nefis_waq, grnamw, elwqi(5),
     +                 uindex, ord   , itim0 )
         if (err .ne. 0) goto 1000
c
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(8),
     +                 uindex, ord   , qaggr)
         if (err .ne. 0) goto 1000
c
          err = PUTREL (fd_nefis_waq, grnamw, elwqi(9),
     +                 uindex, ord   , qlaggr)
         if (err .ne. 0) goto 1000

      endif
c
      nrerr = ewqtim
c
      if ( istep .ge. nstep ) then
c
c        Update time step counter
c
         ibuf  (1) = dlwqts
         uindex(1) = 1
         uindex(2) = 1
         uindex(3) = 1
c
         err   = PUTIEL (fd_nefis_waq ,grnamd ,namdes(1) ,
     &                   uindex  ,ord    ,ibuf   )
         if (err.ne.0) goto 1000
      endif
c
c     Flush data to file
c
      if ( writim .or. istep .ge. nstep ) then
         err = flsdat (fd_nefis_waq)
         if (err.ne.0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write (txt,'(i8)') err
      call sre_error (juer ,'FLHYWQ @'//txt//'@' ,nrerr ,ker)
c
      end

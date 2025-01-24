subroutine FLHYWQ(fd_nefis_waq ,ngrid  ,nqlat  ,dlwqts ,dt     ,&
&itim   ,first  ,aggr   ,wqagst ,writim ,istep  ,&
&nstep  ,af     ,afs    ,at     ,cp     ,psi    ,&
&qaggr  ,qlaggr ,wt     ,wfs    ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHYWQ (FLow HYdrodynamic results to Water Quality)
!
! Module description: Subroutine FLHYWQ stores the (aggregated) hydrody-
!                     namic results on the water quality interface file.
!
!                     In subroutine FLHYWQ the aggregated flow results
!                     will be written to the water quality interface
!                     file to be passed later on to the waterquality sub
!                     system.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 af                P  -
! 15 afs               P  -
! 16 at                P  -
! 17 cp                P  -
!  2 dafdwq            P  -
!  1 defdwq            P  -
!  5 dlwqts            IO Highest cell number on water quality interface
!                         file. (DELWAQ time step number)
!  6 dt                I  Computational time step dt [sec].
!  9 firagg            I  TRUE, if first step of aggregating period.
!  8 first             I  True in case of first call.
! 12 istep             I  Current time step number (t(n+1)).
!  7 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 23 juer              P  -
! 24 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 10 lasagg            I  TRUE, if last step of aggregating period.
!  3 ngrid             I  Number of grid points in network.
!  4 nqlat             P  -
! 13 nstep             I  Last time step number in simulation.
! 18 psi               P  -
! 19 qaggr             P  -
! 20 qlaggr            P  -
! 21 wf                P  -
! 22 wfs               P  -
! 11 writim            I  True when data to the NEFIS file will be
!                         written immediately.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flcfwq  FLow Create File for Water Quality
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
! sotime  SObek TIME
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhywq.pf,v $
! Revision 1.5  1999/03/15  14:19:32  kuipe_j
! improve writing Aggr-file
!
! Revision 1.4  1996/09/03  14:52:01  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.3  1995/05/30  09:55:07  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:05  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:50  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:32  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:04  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       fd_nefis_waq, itim(2)
   integer       ngrid, nqlat, dlwqts, istep, nstep, juer, ker,&
   &wqagst
   logical       first, aggr, writim
   real          af   (ngrid)   ,afs   (ngrid,2) ,cp(ngrid,4) ,&
   &at   (ngrid)   ,&
   &qaggr(ngrid,3) ,qlaggr(*)       ,wt(ngrid)   ,&
   &wfs  (ngrid,2) ,psi
   double  precision  dt
!
!     Declaration of local variables
!
   integer       err
   integer       PUTREL, FLSDAT, PUTIEL
   external      PUTREL, FLSDAT, PUTIEL
!
   integer       nelwqi, mcell, i, nrerr
   parameter     (nelwqi=9)
   integer       ord(5), uindex(3), ibuf(1),itim0(2)
   character*16  elwqi(nelwqi)
   character*16  grnamd, grnamw
   character*16  namdes(2)
   character*64  desdes(2)
   character*8   txt
   double  precision  dtag
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   data elwqi /'AF', 'AFS', 'AT' , 'C', 'DLWQTM', 'WT', 'WFS',&
   &'QAGGR', 'QLAGGR' /
!
   data (desdes(i), namdes(i), i=1,2) /&
   &'cel nr'             , 'DLWQTS',&
   &'space weight factor', 'PSI' /
!
   data grnamd / 'WQINT-DES-GROUP' /
   data grnamw / 'WQINT-GROUP' /
!
   nrerr = ewqcre
!
!     Set user order
!
   ord = 1

   if (first) then
!
!        Position on AGGR-file and make Nefis initializations
!
      call FLCFWQ(fd_nefis_waq ,ngrid  ,nqlat  ,grnamd ,grnamw ,&
      &namdes, desdes, itim, psi, elwqi,&
      &dlwqts, err)
      if (err.ne.0) goto 1000
   endif
!
   nrerr = ewqapp
!
!     Finish aggregation period and write flows and lateral
!     discharges
!
   if ( aggr .and. .not.first ) then
!
!        Write Q and Qlat values
!
!        Write each array to the data file
!
      uindex(1) = dlwqts
      uindex(2) = dlwqts
      uindex(3) = 1
!
      err = PUTIEL (fd_nefis_waq, grnamw, elwqi(5),&
      &uindex, ord   , itim  )
      if (err .ne. 0) goto 1000
!
      err = PUTREL (fd_nefis_waq, grnamw, elwqi(8),&
      &uindex, ord   , qaggr)
      if (err .ne. 0) goto 1000
!
      err = PUTREL (fd_nefis_waq, grnamw, elwqi(9),&
      &uindex, ord   , qlaggr)
      if (err .ne. 0) goto 1000

   endif
!
!     Start new aggregation period and write time, Af, Wf and C
!
   if ( aggr .or. (first.and.dlwqts.eq.0) ) then
!
!        Update cel number
!
      dlwqts    = dlwqts + 1
!
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
!
!        Update time step counter on file
!
      if ( writim ) then
         ibuf(1) = dlwqts
         err   = PUTIEL (fd_nefis_waq ,grnamd ,namdes(1) ,&
         &uindex  ,ord    ,ibuf   )
         if (err.ne.0) goto 1000
      endif
!
!        Set maximum of cells to write
!
      mcell = 7
!
!        Write each array to the data file
!
      do 200 i = 1, mcell
         goto (110,120,130,140,180,160,170), i
110      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(1),&
         &uindex, ord   , af)
         goto 180
120      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(2),&
         &uindex, ord   , afs)
         goto 180
130      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(3),&
         &uindex, ord   , at)
         goto 180
140      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(4),&
         &uindex, ord   , cp)
         goto 180
160      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(6),&
         &uindex, ord   , wt)
         goto 180
170      continue
         err = PUTREL (fd_nefis_waq, grnamw, elwqi(7),&
         &uindex, ord   , wfs)
         goto 180
180      continue
         if (err .ne. 0) goto 1000
200   continue
   endif
!
!     Make last written time step on aggregation file complete
!     (i.e. write available flows and lateral discharges)
!
   if (istep.ge.nstep) then
!
!        Write Q and Qlat values
!
!        Write each array to the data file
!
      uindex(1) = dlwqts
      uindex(2) = dlwqts
      uindex(3) = 1
!
!        Calculate last time
!
      dtag = dt * (wqagst-mod(istep,wqagst))
      itim0(1) = itim(1)
      itim0(2) = itim(2)
      call sotime (itim0,dtag)

      err = PUTIEL (fd_nefis_waq, grnamw, elwqi(5),&
      &uindex, ord   , itim0 )
      if (err .ne. 0) goto 1000
!
      err = PUTREL (fd_nefis_waq, grnamw, elwqi(8),&
      &uindex, ord   , qaggr)
      if (err .ne. 0) goto 1000
!
      err = PUTREL (fd_nefis_waq, grnamw, elwqi(9),&
      &uindex, ord   , qlaggr)
      if (err .ne. 0) goto 1000

   endif
!
   nrerr = ewqtim
!
   if ( istep .ge. nstep ) then
!
!        Update time step counter
!
      ibuf  (1) = dlwqts
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
!
      err   = PUTIEL (fd_nefis_waq ,grnamd ,namdes(1) ,&
      &uindex  ,ord    ,ibuf   )
      if (err.ne.0) goto 1000
   endif
!
!     Flush data to file
!
   if ( writim .or. istep .ge. nstep ) then
      err = flsdat (fd_nefis_waq)
      if (err.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') err
   call error (juer ,'FLHYWQ @'//txt//'@' ,nrerr ,ker)
!
end

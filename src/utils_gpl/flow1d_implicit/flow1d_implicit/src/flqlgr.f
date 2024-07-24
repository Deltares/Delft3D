      subroutine FLQLGR(iter,istep,dt1,ngrid,mugr, kdgr,grcgr,
     +                  grhis, qlatgr,
     +                  x,  hp, grdh, plrec, pllrec, plave,
     +                  plold,pbal,hbal,juer,ker)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         F.Dirksen
c
c Module:             FLQLGR (FLow Q LATeral Ground Water)
c
c Module description: In subroutine FLQLGR the groundwater flux caused by 
c                 periods of high water-levels is calculated and added
c                 to the array qlatgr.
c                     
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION  
c    iter              I  Iterarion number
c    istep             I  Current time step number
c    dt1               I  Time step flow module
c    ngrid             I  Number of gridpoints
c    mugr              I  Storage factor per gridpoint
c    kdgr              I  Doorlatendheid (m2/d)
c    grcgr             I  Intree weerstand
c    grhis             IO History of waterlevels, delta T and method nr.
c    qlatgr            IO Lateral discharge per gridpoint
c    hp                I  Waterlevels
c    grdh              I  Minimaal verschil in waterstand (m)
c    plrec             I  Lengte periode recente waterstanden (aantal tijdstappen)
c    pllrec            I  Lengte periode minder recente waterstanden (idem)
c    plave             I  Lengte periode waarover gemiddeld wordt (idem)
c    plold             I  Lengte periode waarover oude waterstanden bewaard worden (idem)
c    pbal              I  Tijd tussen evenwichtswaterstand en eerste SOBEK waterstand (idem)
c    hbal              I  Evenwichts waterstand
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
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
      integer istep, ngrid, iter, juer,ker
      integer plrec, pllrec, plave, plold, pbal
      real mugr(ngrid), kdgr(ngrid), grcgr(ngrid) 
      real grhis(0:dmgrnd,ngrid,*)
      real hbal(ngrid)
      real grdh
      double precision hp(ngrid,3)
      real qlatgr(ngrid), x(ngrid)
      double precision dt1

c
c     Declaration of local variables
c
      real qgroun, a, length, wl(ngrid)
      real wlave, deltah
      real gt,standg
      integer i, igrid, snijpnt, period, itel,iber
      logical schuif
      double precision tmean
      parameter ( a = 10./864.)
      
c
c     Include sobek error code file
c
      include '../include/errcod.i'
      include '../include/sobcon.i'
c
c     Compute water levels as the mean value of water level at grid points
c
      do 100 i = 1,ngrid-1
         if (iter .eq. 1) then
            wl(i) = (hp(i,1)+hp(i+1,1)) / 2.0
         else
            wl(i) = (hp(i,2)+hp(i+1,2)) / 2.0
         endif
  100 continue
      wl(ngrid)=0. 
c
c     Add newest water level to array grhis, first move all items one position
c
      schuif=.false.
      if (iter .eq. 1) then
         do 105 i=1,ngrid
            if (grhis(0,i,1).ge.1) then
               do 101 itel =  grhis(0,i,1), 1 ,-1
                  grhis(itel+1,i,1) = grhis (itel,i,1)
                  grhis(itel+1,i,2) = grhis (itel,i,2)
                  grhis(itel+1,i,3) = grhis (itel,i,3)
  101          continue
            endif
            grhis(1,i,1) = wl(i)
            grhis(1,i,2)=1.
            grhis(1,i,3)=1.
c
            if ((grhis(0,i,1)+1) .gt. dmgrnd) then
               ker = fatal
               call sre_error (juer,'FLQLGR' ,eflgrh,ker)
               goto 900
            endif
            
c
            grhis(0,i,1)=grhis(0,i,1)+1
  105    continue
c
c     Restructure array grhis (only at first iteration step)
c 
         do 110 i = 1,ngrid
          do 120 iber=1,grhis(0,i,1)
               wlave=0.
               if (iber .eq. plrec+plave .and. nint(grhis(iber,i,3))
     +               .eq.1) then
                  do 121 itel = plrec+1,plrec+plave
                     wlave=wlave+grhis(itel,i,1)
  121             continue
                  wlave=wlave/plave
                  grhis(plrec+1,i,1)=wlave
                  grhis(plrec+1,i,2)=plave
                  grhis(plrec+1,i,3)=2.
      
                  do 130 itel = plrec+2,dmgrnd-plave+1
                     grhis(itel,i,1)=grhis(itel+plave-1,i,1)
                     grhis(itel,i,2)=grhis(itel+plave-1,i,2)
                     grhis(itel,i,3)=grhis(itel+plave-1,i,3)
  130             continue
                  schuif=.true.
                  grhis(0,i,1)=grhis(0,i,1)-plave+1
               endif
               if (iber.ge.(plrec+pllrec+1).and.nint(grhis(iber,i,3))
     +                   .eq.2
     +              .and. schuif) then
                  if (grhis(iber+1,i,1) .lt. grhis(iber,i,1)-grdh .or.
     +                grhis(iber+1,i,1) .gt. grhis(iber,i,1)+grdh) then
                      grhis(iber,i,3)=3
                  else
                     do 140 itel=iber,(grhis(0,i,1)+1)
                        grhis(itel,i,1)=grhis(itel+1,i,1)
                        grhis(itel,i,2)=grhis(itel+1,i,2)
                        grhis(itel,i,3)=grhis(itel+1,i,3)
  140                continue
      
                     grhis(0,i,1)=grhis(0,i,1)-1
                  endif
                  schuif=.false.
               endif

  120       continue
  110    continue

      
c     Further it is possible to cut off the end of the array
c
         do 170 i=1,ngrid
            period = 0
            do 175 itel =1,grhis(0,i,1)
               period = period + nint(grhis(itel,i,2))
               if ( period .ge. plold) then
                  snijpnt=itel
                  exit
               else
                  snijpnt =0 
               endif
  175       continue
            if (snijpnt+1 < grhis(0,i,1) .and. snijpnt .ne. 0) then
               if ((grhis(snijpnt+1,i,1) .lt. hbal(i) .and. 
     +                           grhis(snijpnt,i,1) .gt. hbal(i)) .or.
     +         (grhis(snijpnt+1,i,1) .gt. hbal(i) .and.
     +                           grhis(snijpnt,i,1) .lt. hbal(i))) then
                  grhis(snijpnt+1,i,1) = hbal(i)
                  grhis(snijpnt+1,i,3) = 4.
                  do 180 itel =snijpnt+2,dmgrnd
                     grhis(itel,i,1) = 0.
                     grhis(itel,i,2) = 0.
                     grhis(itel,i,3) = 0.
  180             continue
                  grhis(0,i,1)=snijpnt+1
               endif
            endif
  170    continue

      else
c
c     Not first iteration, so only water level at first position of grhis is to be replaced
c
         do 190 i=1,ngrid
            grhis(1,i,1)=wl(i)
            grhis(1,i,2)=1.
            grhis(1,i,3)=1.
  190    continue
      endif
c      
c
      do 200 igrid = 1, ngrid
         qgroun = 0.
         if (grhis(0,igrid,1) .ge.2) then
            do 210 itel=1,grhis(0,igrid,1)-1
               tmean=(grhis(itel,igrid,2)*dt1+grhis(itel+1,igrid,2)
     +                    *dt1)/(2.)
               tmean = tmean / 86400.
               gt=SQRT(mugr(igrid)*kdgr(igrid))/SQRT(tmean)*0.5642
               standg=gt/(gt*grcgr(igrid)+1.0)
                   deltah=grhis(itel+1,igrid,1)-grhis(itel,igrid,1)
               qgroun=qgroun + deltah * standg

  210       continue
         endif
         qgroun = qgroun * 2.0 * a
c
c     Compute exchange per gridcell
c
         if (igrid .lt. ngrid) then
            if ( x(igrid+1) .lt. x(igrid) ) then
               length = 0.
            else
               length = x(igrid+1) - x(igrid)
            endif
         else
               length = 0.
         endif
         qlatgr(igrid) = qlatgr(igrid) + qgroun * length / 1000.

  200 continue 
  900 continue
      end

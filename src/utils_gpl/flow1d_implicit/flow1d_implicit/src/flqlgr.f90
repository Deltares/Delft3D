subroutine FLQLGR(iter,istep,dt1,ngrid,mugr, kdgr,grcgr,&
&grhis, qlatgr,&
&x,  hp, grdh, plrec, pllrec, plave,&
&plold,pbal,hbal,juer,ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         F.Dirksen
!
! Module:             FLQLGR (FLow Q LATeral Ground Water)
!
! Module description: In subroutine FLQLGR the groundwater flux caused by
!                 periods of high water-levels is calculated and added
!                 to the array qlatgr.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!    iter              I  Iterarion number
!    istep             I  Current time step number
!    dt1               I  Time step flow module
!    ngrid             I  Number of gridpoints
!    mugr              I  Storage factor per gridpoint
!    kdgr              I  Doorlatendheid (m2/d)
!    grcgr             I  Intree weerstand
!    grhis             IO History of waterlevels, delta T and method nr.
!    qlatgr            IO Lateral discharge per gridpoint
!    hp                I  Waterlevels
!    grdh              I  Minimaal verschil in waterstand (m)
!    plrec             I  Lengte periode recente waterstanden (aantal tijdstappen)
!    pllrec            I  Lengte periode minder recente waterstanden (idem)
!    plave             I  Lengte periode waarover gemiddeld wordt (idem)
!    plold             I  Lengte periode waarover oude waterstanden bewaard worden (idem)
!    pbal              I  Tijd tussen evenwichtswaterstand en eerste SOBEK waterstand (idem)
!    hbal              I  Evenwichts waterstand
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters
!
   integer istep, ngrid, iter, juer,ker
   integer plrec, pllrec, plave, plold, pbal
   real mugr(ngrid), kdgr(ngrid), grcgr(ngrid)
   real grhis(0:dmgrnd,ngrid,*)
   real hbal(ngrid)
   real grdh
   double precision hp(ngrid,3)
   real qlatgr(ngrid), x(ngrid)
   double precision dt1

!
!     Declaration of local variables
!
   real qgroun, a, length, wl(ngrid)
   real wlave, deltah
   real gt,standg
   integer i, igrid, snijpnt, period, itel,iber
   logical schuif
   double precision tmean
   parameter ( a = 10./864.)

!
!     Include sobek error code file
!
   include '../include/errcod.i'
   include '../include/sobcon.i'
!
!     Compute water levels as the mean value of water level at grid points
!
   do 100 i = 1,ngrid-1
      if (iter .eq. 1) then
         wl(i) = (hp(i,1)+hp(i+1,1)) / 2.0
      else
         wl(i) = (hp(i,2)+hp(i+1,2)) / 2.0
      endif
100 continue
   wl(ngrid)=0.
!
!     Add newest water level to array grhis, first move all items one position
!
   schuif=.false.
   if (iter .eq. 1) then
      do 105 i=1,ngrid
         if (grhis(0,i,1).ge.1) then
            do 101 itel =  grhis(0,i,1), 1 ,-1
               grhis(itel+1,i,1) = grhis (itel,i,1)
               grhis(itel+1,i,2) = grhis (itel,i,2)
               grhis(itel+1,i,3) = grhis (itel,i,3)
101         continue
         endif
         grhis(1,i,1) = wl(i)
         grhis(1,i,2)=1.
         grhis(1,i,3)=1.
!
         if ((grhis(0,i,1)+1) .gt. dmgrnd) then
            ker = fatal
            call sre_error (juer,'FLQLGR' ,eflgrh,ker)
            goto 900
         endif

!
         grhis(0,i,1)=grhis(0,i,1)+1
105   continue
!
!     Restructure array grhis (only at first iteration step)
!
      do 110 i = 1,ngrid
         do 120 iber=1,grhis(0,i,1)
            wlave=0.
            if (iber .eq. plrec+plave .and. nint(grhis(iber,i,3))&
            &.eq.1) then
               do 121 itel = plrec+1,plrec+plave
                  wlave=wlave+grhis(itel,i,1)
121            continue
               wlave=wlave/plave
               grhis(plrec+1,i,1)=wlave
               grhis(plrec+1,i,2)=plave
               grhis(plrec+1,i,3)=2.

               do 130 itel = plrec+2,dmgrnd-plave+1
                  grhis(itel,i,1)=grhis(itel+plave-1,i,1)
                  grhis(itel,i,2)=grhis(itel+plave-1,i,2)
                  grhis(itel,i,3)=grhis(itel+plave-1,i,3)
130            continue
               schuif=.true.
               grhis(0,i,1)=grhis(0,i,1)-plave+1
            endif
            if (iber.ge.(plrec+pllrec+1).and.nint(grhis(iber,i,3))&
            &.eq.2&
            &.and. schuif) then
               if (grhis(iber+1,i,1) .lt. grhis(iber,i,1)-grdh .or.&
               &grhis(iber+1,i,1) .gt. grhis(iber,i,1)+grdh) then
                  grhis(iber,i,3)=3
               else
                  do 140 itel=iber,(grhis(0,i,1)+1)
                     grhis(itel,i,1)=grhis(itel+1,i,1)
                     grhis(itel,i,2)=grhis(itel+1,i,2)
                     grhis(itel,i,3)=grhis(itel+1,i,3)
140               continue

                  grhis(0,i,1)=grhis(0,i,1)-1
               endif
               schuif=.false.
            endif

120      continue
110   continue


!     Further it is possible to cut off the end of the array
!
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
175      continue
         if (snijpnt+1 < grhis(0,i,1) .and. snijpnt .ne. 0) then
            if ((grhis(snijpnt+1,i,1) .lt. hbal(i) .and.&
            &grhis(snijpnt,i,1) .gt. hbal(i)) .or.&
            &(grhis(snijpnt+1,i,1) .gt. hbal(i) .and.&
            &grhis(snijpnt,i,1) .lt. hbal(i))) then
               grhis(snijpnt+1,i,1) = hbal(i)
               grhis(snijpnt+1,i,3) = 4.
               do 180 itel =snijpnt+2,dmgrnd
                  grhis(itel,i,1) = 0.
                  grhis(itel,i,2) = 0.
                  grhis(itel,i,3) = 0.
180            continue
               grhis(0,i,1)=snijpnt+1
            endif
         endif
170   continue

   else
!
!     Not first iteration, so only water level at first position of grhis is to be replaced
!
      do 190 i=1,ngrid
         grhis(1,i,1)=wl(i)
         grhis(1,i,2)=1.
         grhis(1,i,3)=1.
190   continue
   endif
!
!
   do 200 igrid = 1, ngrid
      qgroun = 0.
      if (grhis(0,igrid,1) .ge.2) then
         do 210 itel=1,grhis(0,igrid,1)-1
            tmean=(grhis(itel,igrid,2)*dt1+grhis(itel+1,igrid,2)&
            &*dt1)/(2.)
            tmean = tmean / 86400.
            gt=SQRT(mugr(igrid)*kdgr(igrid))/SQRT(tmean)*0.5642
            standg=gt/(gt*grcgr(igrid)+1.0)
            deltah=grhis(itel+1,igrid,1)-grhis(itel,igrid,1)
            qgroun=qgroun + deltah * standg

210      continue
      endif
      qgroun = qgroun * 2.0 * a
!
!     Compute exchange per gridcell
!
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

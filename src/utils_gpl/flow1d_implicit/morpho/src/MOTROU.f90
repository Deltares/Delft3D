subroutine MOTROU(ngrid  ,istep  ,nlev   ,x      ,hp     ,&
&branch ,wft    ,hlev   ,time   ,itim   ,&
&juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOTROU (MOrphology TRaject OUtput file(s))
!
! Module description: Writing of information to h-traject file(s)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  8 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  5 hp(ngrid,3)       I  (i,1) = h1(i) (t=n)
!                         (i,2) = h(i)  (*)
!                         (i,3) = h2(i) (t=n+1)
!  2 istep             I  Current time step number (t(n+1)).
!  9 juer              P  -
! 10 ker               P  -
!  1 ngrid             I  Number of grid points in network.
!  3 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  7 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width      in section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!  4 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
! motrgp  MOrphology TRajectory in GridPoints
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: motrou.pf,v $
! Revision 1.3  1999/03/15  15:53:10  kuipe_j
! tabs removed
!
! Revision 1.2  1997/06/17  11:27:02  kuipe_j
! output in history format
!
! Revision 1.1  1995/10/18  09:00:09  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    dmtrjt
   integer    mxgrid
   integer    mxpar
   parameter (dmtrjt=  20)
   parameter (mxgrid=5500)
   parameter (mxpar = 100)
   integer ngrid, istep, juer, ker , versie
   integer nlev(ngrid), branch(4,*),itim(2)
   real    x(ngrid), wft (ngrid,*), time
   double precision hlev(ngrid,*), hp(ngrid,3)
!
!     Declaration of local variables
!
   integer i     , igp   , igleft, igrght, ilev
   integer ijaar , imaand, idag  , imin  , iuur , isec
   integer nvar  , nloc  , liv   , lnv   , luv  , lil  , lnl
   integer lal   , ngrtot
   real    w1    , w2
!cc      double precision w1, w2
   real wght, hact
!cc	double precision wght, hact
   logical inner
!
   integer ntrjct
   integer trjct(dmtrjt), brnch(dmtrjt)
   real    xb(dmtrjt), xe(dmtrjt), nw(dmtrjt), nd(dmtrjt)
   real    depmin(dmtrjt),deploc(dmtrjt),deltaL(dmtrjt)
   real    depth (mxgrid),nwgr(mxgrid),ndgr(mxgrid)
   common /normtb/ ntrjct, trjct, brnch, xb, xe, nw, nd , versie
   character version*8  , header(4)*40
   character idpar(mxpar )*4,parnam(mxpar )*20
   character idloc(mxgrid)*4,locnam(mxgrid)*20
   character unit*4         ,attr  (mxgrid)*10
!
!     Declaration of function
!
   logical  EPSEQU
   external EPSEQU
!
!     Declaration of local variables
!
   integer gp1, gp2
   integer luntr , lungr
   logical first
   data    first /.true./

   if (ntrjct.ne.0) then
!     write T0-string
      call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
      write ( header(4),1000 ) ijaar,imaand,idag,&
      &iuur,imin,isec

!     luntr =  lun for traject HIS file
!     lungr =  lun for grid    HIS file

      luntr = 70+ntrjct+1
      lungr = 70+ntrjct+2
      if (first) then
!
!        1. Traject HIS file
!
         version   ='his01.01'
         header(1) ='SOBEK                                   '
         header(2) ='Optie voor undocumented traject file    '
         header(3) ='HIS file voor traject-file              '
         nvar   = 5
         nloc   = ntrjct
         if (versie.eq.0) then
            write(luntr) header
         else
            write(luntr) version, header
         endif
         write(luntr) nvar, nloc
!
!        1.1 PARAMETERS
!
!        identifiers
!
         liv    = 4
         lnv    = 20
         luv    = 4
         if (versie.eq.1) then
            write(luntr) liv, lnv, luv
         endif
!
!        names
!
         parnam(1) = 'Geringste diepte    '
         parnam(2) = 'Locatie ger. diepte '
         parnam(3) = 'Lengte: diepte<norm '
         parnam(4) = 'Norm-breedte        '
         parnam(5) = 'Norm-diepte         '
         unit      ='[m]'
         do 10 i=1,nvar
            write(idpar(i),'(i4.4)') i
10       continue
!
         if (versie.eq.0) then
            write(luntr) (parnam(i),i=1,nvar)
         else
            write(luntr) (idpar(i),parnam(i),unit,i=1,nvar)
         endif
!
!        1.2 LOCATIONS
!
!        identifiers
!
         lil = 4
         lnl = 20
         lal = 10
         if (versie.eq.1) write(luntr) lil, lnl, lal
         do 20 i=1,nloc
            write(idloc (i),'(i4.4)') i
            write(locnam(i),'(a,i2.2)') 'Traject ',i
20       continue
!
         if (versie.eq.0) then
            write(luntr) (i,locnam(i),i=1,nloc)
         else
            write(luntr) (idloc(i),locnam(i),' ',i=1,nloc)
         endif
!
!        2. Grid HIS file
!
!        generate names and attributes for grid locations
!
         ngrtot = 0
         do 60 i = 1, ntrjct
            call MOTRGP (ngrid  ,x      ,branch ,brnch(i), xb(i), xe(i),&
            &gp1    ,gp2    ,juer   ,ker )
            do 50 igp = gp1, gp2
               ngrtot = ngrtot + 1
               write(locnam(ngrtot),'(a,i2.2)') 'traject-',i
               write(attr (ngrtot),'(a,i8  )') '_',nint(x(igp)-x(gp1))
50          continue
60       continue
!
         version   ='his01.01'
         header(1) ='SOBEK                                   '
         header(2) ='Optie voor undocumented traject file    '
         header(3) ='HIS file voor grid-file(langs traject)  '
         nvar   = 3
         nloc   = ngrtot
         if (versie.eq.0) then
            write(lungr) header
         else
            write(lungr) version, header
         endif
         write(lungr) nvar, nloc
!
!        2.1 PARAMETERS
!
!        identifiers
!
         liv    = 4
         lnv    = 20
         luv    = 4
         if (versie.eq.1) write(lungr) liv, lnv, luv
         parnam(1) = 'Geringste diepte    '
         parnam(2) = 'Norm-breedte        '
         parnam(3) = 'Norm-diepte         '
         unit      ='[m]'
         do 70 i=1,nvar
            write(idpar(i),'(i4.4)') i
70       continue
!
         if (versie.eq.0) then
            write(lungr) (parnam(i),i=1,nvar)
         else
            write(lungr) (idpar(i),parnam(i),unit,i=1,nvar)
         endif
!
!        1.2 LOCATIONS
!
!        identifiers
!
         lil = 4
         lnl = 20
         lal = 10
         if (versie.eq.1) write(lungr) lil, lnl, lal
         do 80 i=1,nloc
            write(idloc(1),'(i4.4)') i
80       continue
!
         if (versie.eq.0) then
            write(lungr) (i,locnam(i)(1:10)//attr(i)(1:10),i=1,nloc)
         else
            write(lungr) (idloc(i),locnam(i),attr(i),i=1,nloc)
         endif
!
         first = .false.
!
      endif
!
      ngrtot = 0
      do 150 i = 1, ntrjct
         call MOTRGP (ngrid  ,x      ,branch ,brnch(i), xb(i), xe(i),&
         &gp1    ,gp2    ,juer   ,ker )
         depmin(i) = 9999.
         deltaL(i) = 0.
         do 130 igp = gp1, gp2
            ngrtot = ngrtot + 1
            igleft = max ( igp-1, branch(3,brnch(i)) )
            igrght = min ( igp+1, branch(4,brnch(i)) )
!
!           Check for maximum level
!
            if ( nw(i) .ge. wft(igp,nlev(igp)) ) then
               hact = hlev(igp,nlev(igp))
!
!           Check for minimum level
!
            else if ( nw(i) .lt. wft(igp,1) ) then
               hact = hlev(igp,1)
            else
!
!              Search from top to bottom
!
               do 110 ilev = nlev(igp), 2, -1
!
!                 Read table widths
!
                  w2 = wft(igp,ilev)
                  w1 = wft(igp,ilev-1)
!
!                 Check if norm width is inside range
!
                  inner = nw(i) .ge. w1 .and. nw(i) .le. w2
!
                  if (inner) then
                     if ( epsequ(w1,w2,1E-8) ) then
!
!                       Equal widths on two levels, take highest level
!
                        hact = hlev(igp,ilev)
                     else
!
!                       Calculate weight factor
!
                        wght = (nw(i) - w1) / (w2 - w1)
!
!                       Determine water level
!
                        hact = (1d0-wght) * hlev(igp,ilev-1) +&
                        &wght  * hlev(igp,ilev  )
                     endif
                     depth(ngrtot) = hp(igp,3) - hact
                  endif
110            continue
               if ( depth(ngrtot) .lt. depmin(i) ) then
                  depmin(i) = depth(ngrtot)
                  deploc(i) = x(igp)-x(gp1)
               endif
               if ( depth(ngrtot) .lt. nd(i) ) then
                  deltaL(i) = deltaL(i) + (x(igrght) - x(igleft))/2.
               else
                  deltaL(i) = deltaL(i) + 0.
               endif
            endif
            nwgr(ngrtot) = nw(i)
            ndgr(ngrtot) = nd(i)
!           write (70+i, '(2i8,f8.2,2f12.0)')
!    +            istep, igp , depth(ngrtot), nw(i), nd(i)
130      continue
         write (70+i, '(i8,f8.2,2f12.0)')&
         &istep, depmin(i), deploc(i), deltaL(i)
150   continue
!
      write(luntr) nint(time),&
      &(depmin(i), deploc(i), deltaL(i), nw(i), nd(i),i=1,ntrjct)
!
      write(lungr) nint(time),&
      &(depth(igp), nwgr(igp), ndgr(igp),igp=1,ngrtot)
!
   endif
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=       1s)')
!
end

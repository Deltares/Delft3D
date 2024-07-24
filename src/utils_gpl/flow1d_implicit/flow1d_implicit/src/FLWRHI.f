      subroutine flwrhi ( h2     , q2     , ngrid  , itim   , flwini ,
     +                    strhis , nstru  , istep  , nstep  , dt     ,
     +                    qlat   , nqlat  , hyrtim , nhytim , hycpre ,
     +                    q2s    , at     , af     , afs    , c      ,
     +                    cs     , wf     , wfs    , wt     , r      ,
     +                    rs     , g      , nsttim , strtim , stcpre ,
     +                    nlatim , lattim , lacpre , hyrmap , nhymap ,
     +                    gridnm , strunm , qlatnm , buffer , qltpar )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.J. van Overloop
c
c Module:             FLWRHI (FLow WRite HIs-files)
c
c Module description: Routine writes results (map and his output) 
c                     in standard his-format files.
c
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 18 afs(ngrid,2)      I  Actual flow area per section
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 16 at(ngrid)         I  Actual total area at every grid point.
c 19 c(ngrid)          I  Actual Chezy coefficient for total channel in
c                         every grid point.
c 20 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c  9 dt                I  Time step flow module
c  5 flwini(4)         I  Indication array if file is already opened
c  1 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 14 hycpre            I  hycpre(i) = index in block table (1...nentri)
c                         for main code i of the hydrodynamic results.
c 29 hyrmap(nhymap)    I  Parameter list for MAP block with hydrodynamic
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nhymap) = Report parameter n sub code
c 12 hyrtim(nhytim)    I  Parameter list for HIST block with hydrodyna-
c                         mic results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nhytim) = Report parameter n sub code
c  8 istep             I  Current time step number (t(n+1)).
c  4 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 28 lacpre            I  lacpre(i) = index in block table (1...nentri)
c                         for main code i of the lateral discharges
c                         results.
c 27 lattim(nlatim)    I  Parameter list for HIST block with hydrodyna-
c                         mic lateral discharge results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nlatim) = Report parameter n sub code
c  3 ngrid             I  Number of grid points in network.
c 30 nhymap            I  Number of entries in hyrmap.
c 13 nhytim            I  Number of entries in hyrtim.
c 26 nlatim            I  Number of entries in lattim.
c 13 nqlat             I  Number of lateral discharge stations.
c  7 nstru             I  NNumber od structures.
c 23 nsttim            I  Number of entries in strtim.
c  2 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 15 q2s(ngrid,2)      I  Flow through main and sub section 1 at time
c                         t(n+1).
c 10 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c    qltpar(9,nqlat)   I  Lateral discharge parameters
c 25 stcpre            I  stcpre(i) = index in block table (1...nentri)
c                         for main code i of the structures results.
c  8 strhis(10,nstru)  I  For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c                         (13,i)= water level in retention area 
c 24 strtim(nsttim)    I  Parameter list for HIST block with hydrodyna-
c                         mic structure results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nsttim) = Report parameter n sub code
c 21 wf(ngrid)         I  Actual flow width at every grid point.
c 22 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c getloc  GET LOCation of gridpoint
c yesmap  YES MAP results are written
c=======================================================================
c
c
c     Include constants for sobek dimensions
c
      include '../include/sobdim.i'
c
c     Parameters
c
      integer, parameter :: npflw=33  ,npstr=4 ,npqlt=2
      integer          ngrid  , nstru  , istep  , nstep ,nqlat  ,
     +                 nhytim , nsttim , nlatim , nhymap , i
      integer          flwini(*) , itim(*)   , 
     +                 hyrtim(*) , hycpre(*) , strtim(*) , 
     +                 stcpre(*) , lattim(*) , lacpre(*) , 
     +                 hyrmap(*)
      real             g
      real             strhis(dmstrh,nstru) , qlat(nqlat)  , 
     +                 q2s(ngrid,2)         , at(ngrid)    ,
     +                 af(ngrid)            , afs(ngrid,2) , 
     +                 c(ngrid)             , cs(ngrid,3)  , 
     +                 r(ngrid)             , rs(ngrid,3)  , 
     +                 wf(ngrid)            , wfs(ngrid,2) ,
     +                 buffer(npflw,*)      , qltpar(9,*)  ,
     +                 wt(ngrid)
      double precision dt, h2(ngrid), q2(ngrid)
      character*40     gridnm(*), strunm(*), qlatnm(*)
c
c     local variables
c
      integer      igrid  , istru  , iqlat  , nlc   , 
     +             ie     , ifil   , lun    , ivar   , nvar
      integer      ijaar  , imaand , idag   , iuur   , imin  , isec
      integer      istepf , nsk    , istphf , istplf , istpsf
      integer      iloc   , istat  , iscu   , igr1   , skip 
      logical      new    , newuit
      real         qstat  , a2     , w2     , q22 
      double precision    scudt
      character*40 idhis(4)     ,idstr(4)      ,idqlt(4)     ,idmap(4)
      character*20 parflw(npflw),parflw1(npflw),parstr(npstr),
     +             parqlt(npqlt)
      save         istepf, istphf, istplf, istpsf,scudt
c
      include '../include/sobcon.i'
      include '../include/filsim.i'
c
c     Declaration of external functions
c
      real         sub2 ,depth ,froude ,velocity
      logical      yesmap
      external     yesmap ,sub2 ,depth ,froude ,velocity
c
c     map-part
c
c     write map-results for gridpoints
c
      if (nhymap .gt. 3) then
         if (yesmap(hyrmap(1),hyrmap(2),hyrmap(3),istep)) then
            nvar = nint(.5*(nhymap-3))
            ifil = 1
            lun  = 50
            if ( flwini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = flwmap , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = flwmap , form = 'unformatted') 
#else
               open(lun , file = flwmap , form = 'unformatted') 
#endif
#endif
               call headhis (idhis  ,idstr ,idqlt ,idmap ,parflw ,
     +                       parstr ,parqlt ,npflw ,npstr ,npqlt )
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idmap(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idmap(i) , i = 1,4)
               write(lun) nvar,ngrid
               write(lun) ( parflw(hycpre(hyrmap(i))+
     +                      hyrmap(i+1)) ,  i = 4,nhymap,2 )
               write(lun) (igrid , gridnm(igrid)(:20), 
     +                     igrid = 1 , ngrid ) 
               flwini(ifil) = 1
               istepf       = istep
            endif
c
            ivar = 0
            do 80 i = 4,nhymap,2
               ivar = ivar + 1
               ie = hycpre(hyrmap(i))+hyrmap(i+1)
               do 70 igrid = 1 , ngrid
                  if ( ie .eq. 1 ) then
                     buffer(ivar,igrid) = sngl( h2(igrid) )
                  elseif ( ie .eq. 2 ) then
                     buffer(ivar,igrid) = sngl( q2(igrid) )
                  elseif ( ie .eq. 3 ) then
                     buffer(ivar,igrid) = q2s(igrid,1)
                  elseif ( ie .eq. 4 ) then
                     buffer(ivar,igrid) = q2s(igrid,2)
                  elseif ( ie .eq. 5 ) then
                     buffer(ivar,igrid) = sub2( sngl( q2 (igrid) ),
     +                                                q2s(igrid,1),
     +                                                q2s(igrid,2))
                  elseif ( ie .eq. 6 ) then
                     buffer(ivar,igrid) = at(igrid)
                  elseif ( ie .eq. 7 ) then
                     buffer(ivar,igrid) = af(igrid)
                  elseif ( ie .eq. 8 ) then
                     buffer(ivar,igrid) = afs(igrid,1)
                  elseif ( ie .eq. 9 ) then
                     buffer(ivar,igrid) = afs(igrid,2)
                  elseif ( ie .eq. 10 ) then
                     buffer(ivar,igrid) = sub2 (af (igrid)  ,
     +                                          afs(igrid,1),
     +                                          afs(igrid,2))
                  elseif ( ie .eq. 11 ) then
                     buffer(ivar,igrid) = c(igrid)
                  elseif ( ie .eq. 12 ) then
                     buffer(ivar,igrid) = cs(igrid,1)
                  elseif ( ie .eq. 13 ) then
                     buffer(ivar,igrid) = cs(igrid,2)
                  elseif ( ie .eq. 14 ) then
                     buffer(ivar,igrid) = cs(igrid,3)
                  elseif ( ie .eq. 15 ) then
                     buffer(ivar,igrid) = wf(igrid)
                  elseif ( ie .eq. 16 ) then
                     buffer(ivar,igrid) = wfs(igrid,1)
                  elseif ( ie .eq. 17 ) then
                     buffer(ivar,igrid) = wfs(igrid,2)
                  elseif ( ie .eq. 18 ) then
                     buffer(ivar,igrid) = sub2(wf (igrid)    ,
     +                                         wfs(igrid,1)  ,
     +                                         wfs(igrid,2)  )
                  elseif ( ie .eq. 19 ) then
                     buffer(ivar,igrid) = wt(igrid)
                  elseif ( ie .eq. 20 ) then
                     buffer(ivar,igrid) = depth(at(igrid),wt(igrid))   
                  elseif ( ie .eq. 21 ) then
                     buffer(ivar,igrid) = depth(af(igrid),wf(igrid))
                  elseif ( ie .eq. 22 ) then
                     buffer(ivar,igrid) = depth(afs(igrid,1),
     +                                          wfs(igrid,1))      
                  elseif ( ie .eq. 23 ) then
                     buffer(ivar,igrid) = depth(afs(igrid,2),
     +                                          wfs(igrid,2))    
                  elseif ( ie .eq. 24 ) then
                     a2 = sub2 (af(igrid),afs(igrid,1),afs(igrid,2))
                     w2 = sub2 (wf(igrid),wfs(igrid,1),wfs(igrid,2))
                     buffer(ivar,igrid) = depth(a2,w2)
                  elseif ( ie .eq. 25 ) then
                     buffer(ivar,igrid) = velocity(sngl (q2(igrid)),
     +                                                   af(igrid))
                  elseif ( ie .eq. 26 ) then
                     buffer(ivar,igrid) = velocity(q2s(igrid,1),
     +                                             afs(igrid,1))      
                  elseif ( ie .eq. 27 ) then
                     buffer(ivar,igrid) = velocity(q2s(igrid,2),
     +                                             afs(igrid,2))    
                  elseif ( ie .eq. 28 ) then
                     q22= sub2 (sngl(q2(igrid)),q2s(igrid,1),
     +                                          q2s(igrid,2))
                     a2 = sub2 (af(igrid),afs(igrid,1),afs(igrid,2))
                     buffer(ivar,igrid) = velocity(q22,a2)
                  elseif ( ie .eq. 29 ) then
                     buffer(ivar,igrid) = r(igrid)
                  elseif ( ie .eq. 30 ) then
                     buffer(ivar,igrid) = rs(igrid,1)
                  elseif ( ie .eq. 31 ) then
                     buffer(ivar,igrid) = rs(igrid,2)
                  elseif ( ie .eq. 32 ) then
                     buffer(ivar,igrid) = rs(igrid,3)
                  elseif ( ie .eq. 33 ) then
                     buffer(ivar,igrid) = froude( sngl(q2(igrid)),
     +                                                 af(igrid),
     +                                                 wt(igrid),g)      
                  endif  
   70          continue
   80       continue
            write(lun) nint((istep-istepf)*scudt), 
     +                 ((buffer(ivar,igrid), 
     +                 ivar = 1 , nvar),
     +                 igrid = 1 , ngrid)
         endif
      endif
c
c     his-part
c
c     write his-results for gridpoints
c
      nlc = hyrtim(1)
      new = mod(nhytim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nhytim .gt. 1+nsk ) then
         if (new) then
            newuit = yesmap(hyrtim(nlc+2),hyrtim(nlc+3),hyrtim(nlc+4),
     +                      istep)
         else
            newuit = .false.
         endif
         if ((hyrtim(1) .gt.0  .and.  .not. new)  .or. newuit) then
            ifil = 2
            lun  = 51
            if ( flwini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = flwhis , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = flwhis , form = 'unformatted') 
#else
               open(lun , file = flwhis , form = 'unformatted') 
#endif
#endif
               call headhis (idhis  ,idstr ,idqlt ,idmap ,parflw ,
     +                       parstr ,parqlt ,npflw ,npstr ,npqlt )
               ivar = 0
               do i = hyrtim(1)+2+nsk,nhytim,2
                  ie = hycpre(hyrtim(i))+hyrtim(i+1)
                  if (ie.le.npflw) then
                     ivar = ivar + 1
                     parflw1(ivar) = parflw(ie)
                  endif
               enddo   
               nvar  = ivar
               iscu  = nstep*dt/1.0d9+1.d0
               scudt = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idhis(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idhis(i) , i = 1,4)
               write(lun) nvar , hyrtim(1)
               write(lun) (parflw1(ivar),ivar=1,nvar)
               write(lun) (hyrtim(igrid),gridnm(hyrtim(igrid))(:20),
     +                     igrid = 2 , hyrtim(1)+1)
               flwini(ifil) = 1
               istphf       = istep
            endif
c
            ivar = 0   
            do 30 i = hyrtim(1)+2+nsk,nhytim,2
               ivar = ivar + 1
               skip = 0
               ie = hycpre(hyrtim(i))+hyrtim(i+1)
               do 20 igrid = 1 , hyrtim(1)
                  igr1 = hyrtim(igrid+1)
                  if ( ie .eq. 1 ) then
                     buffer(ivar,igrid) = sngl( h2(igr1) )
                  elseif ( ie .eq. 2 ) then
                     buffer(ivar,igrid) = sngl( q2(igr1) )
                  elseif ( ie .eq. 3 ) then
                     buffer(ivar,igrid) = q2s(igr1,1)
                  elseif ( ie .eq. 4 ) then
                     buffer(ivar,igrid) = q2s(igr1,2)
                  elseif ( ie .eq. 5 ) then
                     buffer(ivar,igrid) = sub2( sngl( q2 (igr1)),
     +                                                q2s(igr1,1),
     +                                                q2s(igr1,2))
                  elseif ( ie .eq. 6 ) then
                     buffer(ivar,igrid) = at(igr1)
                  elseif ( ie .eq. 7 ) then
                     buffer(ivar,igrid) = af(igr1)

                  elseif ( ie .eq. 8 ) then
                     buffer(ivar,igrid) = afs(igr1,1)
                  elseif ( ie .eq. 9 ) then
                     buffer(ivar,igrid) = afs(igr1,2)
                  elseif ( ie .eq. 10 ) then
                     buffer(ivar,igrid) = sub2(af (igr1)  ,
     +                                         afs(igr1,1),
     +                                         afs(igr1,2))
                  elseif ( ie .eq. 11 ) then
                     buffer(ivar,igrid) = c(igr1)
                  elseif ( ie .eq. 12 ) then
                     buffer(ivar,igrid) = cs(igr1,1)
                  elseif ( ie .eq. 13 ) then
                     buffer(ivar,igrid) = cs(igr1,2)
                  elseif ( ie .eq. 14 ) then
                     buffer(ivar,igrid) = cs(igr1,3)
                  elseif ( ie .eq. 15 ) then
                     buffer(ivar,igrid) = wf(igr1)
                  elseif ( ie .eq. 16 ) then
                     buffer(ivar,igrid) = wfs(igr1,1)
                  elseif ( ie .eq. 17 ) then
                     buffer(ivar,igrid) = wfs(igr1,2)
                  elseif ( ie .eq. 18 ) then
                     buffer(ivar,igrid) = sub2(wf (igr1)  ,
     +                                         wfs(igr1,1),
     +                                         wfs(igr1,2))
                  elseif ( ie .eq. 19 ) then
                     buffer(ivar,igrid) = wt(igr1)
                  elseif ( ie .eq. 20 ) then
                     buffer(ivar,igrid) = depth(at(igr1),wt(igr1))   
                  elseif ( ie .eq. 21 ) then
                     buffer(ivar,igrid) = depth(af(igr1),wf(igr1))
                  elseif ( ie .eq. 22 ) then
                     buffer(ivar,igrid) = depth(afs(igr1,1),wfs(igr1,1))      
                  elseif ( ie .eq. 23 ) then
                     buffer(ivar,igrid) = depth(afs(igr1,2),wfs(igr1,2))    
                  elseif ( ie .eq. 24 ) then
                     a2 = sub2 (af(igr1),afs(igr1,1),afs(igr1,2))
                     w2 = sub2 (wf(igr1),wfs(igr1,1),wfs(igr1,2))
                     buffer(ivar,igrid) = depth(a2,w2)
                  elseif ( ie .eq. 25 ) then
                     buffer(ivar,igrid) = velocity( sngl( q2(igr1)),
     +                                                    af(igr1))
                  elseif ( ie .eq. 26 ) then
                     buffer(ivar,igrid) = velocity(q2s(igr1,1),
     +                                             afs(igr1,1))      
                  elseif ( ie .eq. 27 ) then
                     buffer(ivar,igrid) = velocity(q2s(igr1,2),
     +                                             afs(igr1,2))    
                  elseif ( ie .eq. 28 ) then
                     q22= sub2 (sngl(q2(igr1)),q2s(igr1,1),
     +                                         q2s(igr1,2))
                     a2 = sub2 (af(igr1),afs(igr1,1),afs(igr1,2))
                     buffer(ivar,igrid) = velocity(q22,a2)
                  elseif ( ie .eq. 29 ) then
                     buffer(ivar,igrid) = r(igr1)
                  elseif ( ie .eq. 30 ) then
                     buffer(ivar,igrid) = rs(igr1,1)
                  elseif ( ie .eq. 31 ) then
                     buffer(ivar,igrid) = rs(igr1,2)
                  elseif ( ie .eq. 32 ) then
                     buffer(ivar,igrid) = rs(igr1,3)
                  elseif ( ie .eq. 33 ) then
                     buffer(ivar,igrid) = froude( sngl(q2(igr1)),
     +                                                 af(igr1),
     +                                                 wt(igr1),g)
                  else
                     skip = 1
                  endif
   20          continue
               ivar = ivar - skip
   30       continue
            nvar = ivar
            write(lun) nint((istep-istphf)*scudt),
     +                 ((buffer(ivar,igrid),
     +                 ivar = 1,nvar),
     +                 igrid = 1,hyrtim(1))
         endif
      endif
c
c     write his-results for structures
c     
      nlc = strtim(1)
      new = mod(nsttim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nsttim .gt. 1+nsk ) then
         if (new) then
            newuit = yesmap(strtim(nlc+2),strtim(nlc+3),strtim(nlc+4),
     +                      istep)
         else
            newuit = .false.
         endif 
         if ((strtim(1) .gt.0  .and.  .not. new)  .or. newuit) then
            nvar = nint(.5*(nsttim-strtim(1)-2-nsk+1))
            ifil = 3
            lun  = 52 
            if ( flwini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fstrhs , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = fstrhs , form = 'unformatted') 
#else
               open(lun , file = fstrhs , form = 'unformatted') 
#endif
#endif
               call headhis (idhis  ,idstr ,idqlt ,idmap ,parflw ,
     +                       parstr ,parqlt ,npflw ,npstr ,npqlt )
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idstr(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idstr(i) , i = 1,4)
               write(lun) nvar,strtim(1)
               write(lun) (parstr(stcpre(strtim(i)) + strtim(i+1)),
     +                     i = strtim(1)+2+nsk,nsttim,2)
               write(lun) (strtim(istru),strunm(strtim(istru))(:20),
     +                     istru = 2 , strtim(1)+1)
               flwini(ifil) = 1
               istpsf       = istep
            endif
            write(lun) nint((istep-istpsf)*scudt),
     +        ((strhis(stcpre(strtim(i))+strtim(i+1),strtim(istru)),
     +                 i     = strtim(1)+2+nsk,nsttim,2),
     +                 istru = 2,strtim(1)+1           )
         endif
      endif
c
c
c     write his-results for lateral discharges
c
      nlc = lattim(1)
      new = mod(nlatim-nlc,2) .eq. 0
      if (new) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nlatim .gt. 1+nsk ) then
         if (new) then
            newuit = yesmap(lattim(nlc+2),lattim(nlc+3),lattim(nlc+4),
     +                      istep)
         else
            newuit = .false.
         endif 
         if ((lattim(1) .gt.0  .and.  .not. new)  .or. newuit) then
            nvar = nint(.5*(nlatim-lattim(1)-2-nsk+1))
            ifil = 4
            lun  = 53
            if ( flwini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
               open(lun , file = fqlths , form = 'binary')      
#else
#if defined (USE_HPUX)
               open(lun , file = fqlths , form = 'unformatted') 
#else
               open(lun , file = fqlths , form = 'unformatted') 
#endif
#endif
               call headhis (idhis  ,idstr ,idqlt ,idmap ,parflw ,
     +                       parstr ,parqlt ,npflw ,npstr ,npqlt )
               iscu   = nstep*dt/1.0d9+1.d0
               scudt  = dt/dble(iscu)
               call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
               write ( idqlt(4),1000 ) ijaar,imaand,idag,
     +                                   iuur,imin,isec,iscu
               write(lun) (idqlt(i) , i = 1,4)
               write(lun) nvar,lattim(1)
               write(lun) (parqlt(lacpre(lattim(i)) + lattim(i+1)),
     +                    i = lattim(1)+2+nsk,nlatim,2)
               write(lun) (lattim(iqlat),qlatnm(lattim(iqlat))(:20),
     +                    iqlat = 2 , lattim(1)+1)
               flwini(ifil) = 1
               istplf       = istep
            endif
c
            ivar = 0
            do 50 i=lattim(1)+2+nsk,nlatim,2
               ivar = ivar + 1
               ie = lacpre(lattim(i)) + lattim(i+1)
               do 40 iloc=1,lattim(1)
                  if (ie .eq. 1) then
                     istat = lattim(iloc+1)
                     qstat = qlat(istat)
                     if (qstat .gt. 1.1e+20) qstat = 0.
c
c                    Convert from M2/s to M3/s
c
                     if (nint(qltpar(4,istat)).eq.ctd1gc .or. 
     +                  nint(qltpar(4,istat)).eq.ctdmgc ) then
                        qstat = qstat * ( qltpar(8,istat) 
     +                                  - qltpar(7,istat))
                     endif
                     buffer(ivar,iloc) = qstat
c                
                  else if (ie .eq. 2) then
                     istat = lattim(iloc+1)
                     if (INT(qltpar(2,istat)).eq.cqlret) then
                       istru = MOD(INT(qltpar(9,istat)), 1000)
                       buffer(ivar,iloc) = strhis(13,istru)
                     else               
                       buffer(ivar,iloc) = 0.
                     endif      
                  endif
 40            continue
 50         continue
            write(lun) nint((istep-istplf)*scudt),
     +                     ((buffer(ivar,iloc),
     +                     ivar=1,nvar), iloc=1,lattim(1)) 
         endif
      endif
c
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')
c
      return
      end
      subroutine headhis (idhis  ,idstr ,idqlt ,idmap ,parflw ,parstr ,
     +                    parqlt ,npflw ,npstr ,npqlt )
c
c     Initialize arrays to be written to headers
c
      integer      npflw  ,npstr ,npqlt  
      character*40 idhis(4)      ,idstr(4)      ,idqlt(4)     ,idmap(4)
      character*20 parflw(npflw) ,parstr(npstr) ,parqlt(npqlt)
c      
      idhis(1)   = 'SOBEK                                   '
      idhis(2)   = 'History at gridpoints                   '
      idhis(3)   = '                                        '
      idhis(4)   = '                                        '
      parflw(1)  = 'Water level         '
      parflw(2)  = 'Discharge Total     '
      parflw(3)  = 'Disch. main channel '
      parflw(4)  = 'Disch. floodplain 1 '
      parflw(5)  = 'Disch. floodplain 2 '
      parflw(6)  = 'Total Area          '
      parflw(7)  = 'Flow Area           '
      parflw(8)  = 'Flow Area main ch.l '
      parflw(9)  = 'Flow Area floodp. 1 '
      parflw(10) = 'Flow Area floodp. 2 '
      parflw(11) = 'Chezy               '
      parflw(12) = 'Chezy main channel  '
      parflw(13) = 'Chezy floodplain 1  '
      parflw(14) = 'Chezy floodplain 2  '
      parflw(15) = 'Flow Width          '
      parflw(16) = 'Width main channel '
      parflw(17) = 'Width floodplain 1  '
      parflw(18) = 'Width floodplain 2  '
      parflw(19) = 'Total Width         '
      parflw(20) = 'Total average Depth '  
      parflw(21) = 'Average Depth flow  '
      parflw(22) = 'Average Depth main  '
      parflw(23) = 'Average Depth fp1   '
      parflw(24) = 'Average Depth fp2   '
      parflw(25) = 'AverageVelocity flow'
      parflw(26) = 'AverageVelocity main'
      parflw(27) = 'Average Velocity fp1'
      parflw(28) = 'Average Velocity fp2'
      parflw(29) = 'Hydraulic rad flow  '
      parflw(30) = 'Hydraulic rad main  '
      parflw(31) = 'Hydraulic rad fp1   ' 
      parflw(32) = 'Hydraulic rad fp2   '
      parflw(33) = 'Froude number       '
      idstr(1)   = 'SOBEK                                   '
      idstr(2)   = 'History at structure                    '
      idstr(3)   = '                                        '
      idstr(4)   = '                                        '
      parstr(1)  = 'Gate height         '
      parstr(2)  = 'Crest level         '
      parstr(3)  = 'Crest width         '
      parstr(4)  = 'Discharge           '
      idqlt(1)   = 'SOBEK                                   '
      idqlt(2)   = 'History of lateral discharges           '
      idqlt(3)   = '                                        '
      idqlt(4)   = '                                        '
      parqlt(1)  = 'Lateral discharge   '
      parqlt(2)  = 'Retention area level'
      idmap(1)   = 'SOBEK                                   '
      idmap(2)   = 'Map-results at gridpoints               '
      idmap(3)   = '                                        '
      idmap(4)   = '                                        '
c
      end
      real function sub2(flow,main,sub1)
c
c     calculate parameter in floodplain 2
c
      real flow, main, sub1
c 
      sub2 = flow - main - sub1
c
      end
      real function depth(a,w)
c
c     calculate average depth
c
      real a ,w
 
      if (w.gt.1.e-6) then
         depth = a/w
      else
         depth = 0.0
      endif
      end
      real function velocity(q,a)
c
c     calculate average velocity
c
      real q ,a
 
      if (a.gt.1.e-6) then
         velocity = q/a
      else
         velocity = 0.0
      endif      
      end
      real function froude(q,af,wt,g)
c
c     calculate Froude number
c
      real     q ,af ,wt ,g ,u
      real     velocity
      external velocity
 
      u = velocity(q,af)
      if (wt.gt.1.e-6) then
         froude = abs(u)/sqrt(g*af/wt)
      else
         froude = 0.0
      endif
      end

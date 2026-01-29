subroutine SOSCAV ( steps  ,nbran  ,ngrid ,flwdir,celer ,sedtr  ,&
&dissed ,slat   ,asedtr,adissd,aslat ,juer   ,&
&maxlev ,nlev   ,hlev  ,wft   ,ws    ,alfab  ,&
&prslot ,branch ,sedpar,g     ,grsize,nucoef ,&
&uscoef ,trform ,forcon,engpar,sectv ,ahp    ,&
&cp     ,rp     ,waoft ,afwfqs,lkalm ,typcr  ,&
&bfrict ,bfricp ,aft   ,overlp,arex  ,arexcn ,&
&arexop ,of     ,maxtab,ntabm ,ntab  ,table  ,&
&psltvr ,scifri ,nnf   ,pfa   ,hqav  ,sectc  ,&
&wtt    ,att    ,ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!                     A.W.J.Koster (revision)
!
! Module:             SOSCAV (SObek Sediment Celerity AVarage)
!
! Module description: This routine avarages the calculated sediment
!                     transports and celerities and fills array flwdir.
!
!                     This routine avarages the calculated sediment
!                     transports and celerities. Notice that sedredge
!                     branches are not allowed in an estuary morphology
!                     case ! The avaraged values are returned in the
!                     'normal' arrays for calculated sediment transport
!                     and celerities.
!                     The array flwdir is filled for each gridpoint
!                     depending on the celerity at that gridpoint:
!                     Positive celerity -> flwdir(igp) = 1
!                     No celerity       -> flwdir(igp) = 0
!                     Negative celerity -> flwdir(igp) = -1
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 adissd(4,nbran)   I  Avaraged distributed sediment transport in
!                         nodes and boundaries
!  9 asedtr(ngrid)     I  Avaraged sediment transport
! 12 aslat(ngrid)      I  Avaraged lateral sediment
!  6 celer(ngrid,*)    IO Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
!  7 dissed(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  4 flwdir(ngrid)     O  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  5 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
!  8 slat(ngrid,*)     O  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
!  1 steps             IO Number of aggregation steps (estuary morpho-
!                         logy)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: soscav.pf,v $
! Revision 1.11  1999/06/01  13:42:50  kuipe_j
! names in messages substituted + message template
!
! Revision 1.10  1998/12/18  09:34:34  kuipe_j
! remove aux output
!
! Revision 1.9  1998/12/11  13:09:45  kuipe_j
! Improve celerity calculation in estuaries
!
! Revision 1.8  1998/06/11  11:47:50  kuipe_j
! Estuary special integrated
!
! Revision 1.7  1996/04/11  08:16:38  kuipe_j
! Kalman module added
!
! Revision 1.6  1996/03/08  09:40:51  kuipe_j
! Headers
!
! Revision 1.5  1996/03/07  10:44:32  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.4  1995/09/22  10:04:33  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:06  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:00  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:26  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:10:07  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Parameters
!
   integer  steps,  nbran, ngrid ,maxlev, maxtab, ntabm, nnf ,&
   &nucoef, ker
   integer  ntab(4,maxtab) ,typcr(nbran), bfrict(3,nbran) ,&
   &arexcn(ngrid,2),arexop(2)   , scifri(ngrid)   ,&
   &flwdir(ngrid)   , branch(4,nbran) , nlev(ngrid)
   real     sedtr  (ngrid,*),&
   &celer  (ngrid,*),&
   &dissed (4,nbran),&
   &slat   (ngrid,*),&
   &asedtr (ngrid)  ,&
   &adissd (4,nbran),&
   &aslat  (ngrid)   ,&
   &afwfqs (ngrid,8 ),&
   &waoft  (ngrid,*) ,&
   &sectc  (ngrid,3) ,&
   &sectv  (ngrid,dmsecv),&
   &wft    (ngrid,maxlev), aft    (ngrid,maxlev),&
   &wtt    (ngrid,maxlev), att    (ngrid,maxlev),&
   &ws     (ngrid)       ,&
!     +         hqav   (ngrid,2)     ,
   &alfab  (ngrid)
   real     g                ,overlp
   real     uscoef(nrcoefs,nucoef)            ,trform(3,nbran) ,&
   &grsize(4,ngrid,*),forcon(4,ngrid,*),&
   &sedpar(*)        ,engpar(9)       ,prslot(3,nbran)  ,&
   &bfricp(6,ngrid)  ,arex(ngrid,4)   ,of(ngrid,maxlev) ,&
   &psltvr(7,ngrid)  ,pfa(nnf)        ,table(ntabm)
   real     ahp(ngrid,3)
   real     cp(ngrid,4) ,rp(ngrid,4)
   double precision hlev (ngrid,maxlev), hqav(ngrid,2)
!
!     Variables
!
   integer  igp,  icnt  ,errno  ,iter
   integer  ibr  ,igr   ,ind    ,&
   &i1    ,i2    ,juer  ,iter1 ,iter2
   real     pacfac,relden,kinvis, depth, u1, u2, um,&
   &f1    ,f2    ,fm
   real     sedtr1 ,sedtr2 ,sedtrm , sedabs ,ueps
   logical  incall , tolq , lkalm

   logical  epsequ
   external epsequ
!
   logical  first
!
   data first /.true./
!
   if (first) then
!        Initialize averaged discharge
      do 5 igr=1,ngrid
         hqav (igr,2) = 0D0
5     continue
      first = .false.
   endif
!
!     Average
!       - sediment transports
!       - lateral sediment
!       - water level
!
   do 10 igp = 1, ngrid
      sedtr (igp,1) = asedtr (igp)    / steps
      slat  (igp,1) = aslat  (igp)    / steps
!        <H-aver>
      hqav  (igp,1) = dble( ahp    (igp,3) )  / steps
!        Store At (at end of tide) temporarily in At1, because At
!        will be overruled by At at averaged water level over tide.
      waoft (igp,5) = waoft  (igp,4)
10 continue
!
!     Average distributed sediment transport
!     ARS 9774 / Replace velocity determined transport by
!     distributed transports
   do 30 ibr = 1, nbran
      do 20 icnt = 1, 4
         dissed(icnt,ibr) = adissd(icnt,ibr) / steps
20    continue
      i1     = branch(3,ibr)
      i2     = branch(4,ibr)
      sedtr (i1,1) = dissed(1,ibr)
      sedtr (i2,1) = dissed(3,ibr)
30 continue
!
!     Calculate hydraulic parameters for the flow-averaged
!     water level <h>.
!     The discharges <Q-aver> are from previous morph. time step !
!
   call flnp1( lkalm  ,   nbran  ,   ngrid  ,   nnf    ,&
   &branch ,   typcr  ,   bfrict ,   bfricp ,&
!                 <H-aver> <Q-'aver'>
   &hqav(1,1),hqav(1,2),  maxlev ,   nlev   ,&
   &hlev   ,   wft    ,   aft    ,   overlp ,&
   &arex   ,   arexcn ,   arexop ,   of     ,&
   &maxtab ,   ntabm  ,   ntab   ,   table  ,&
   &sectc  ,   sectv  ,   prslot ,   psltvr ,&
   &waoft  ,   grsize ,   engpar ,   scifri ,&
   &pfa    ,   juer   ,   cp     ,   rp     ,&
   &afwfqs ,   alfab  ,   wtt    ,   att    ,&
   &ker    )
!
!     Calculation of velocity related to the flow-averaged
!     sediment transport.
!
!     Given the averaged sediment transport (as computed above) ,
!     the velocity will be calculated by solving the
!     sediment transport equation :
!
!          F(u) = S - f(u) = 0
!
!     for the unknown velocity u.
!     S = flow-averaged sediment transport <asedtr>
!     u = velocity related to flow-averaged sediment transport
!     f = total sediment transport formula
!         (e.g. Ackers-White)
!
!     The root solver is the bisection method.
!     As the transport formulaes are (odd) power functions a
!     smooth convergence may be expected.
!
   kinvis = sedpar(1)
   relden = sedpar(2)
   pacfac = sedpar(3)
!
!     Loop over branches
!
   do 60 ibr=1,nbran
!
      ind    = max(int(trform(2,ibr)),1)
      incall = .true.
!
      i1     = branch(3,ibr)
      i2     = branch(4,ibr)
!
      do 50 igr = i1 , i2
!
!                        <Af,main>      <Wf,main>
         depth   = afwfqs(igr,1) / afwfqs(igr,3)
!
!           Prepare the bisection method for the interval [u1,u2]
!
         sedabs = abs(sedtr(igr,1))
!           The interval is limited by super critical flow
         u2   = max(10.0,sqrt(g*depth))
         ueps = 1.e-6 * u2
         u1   =    0.0
         um   = 0.5 * (u1+u2)
!
!           Initialize sediment formula
!
         call setrfo(&
         &incall    ,g   ,pacfac      ,relden   ,kinvis   ,&
!                                 <C-main>               <R-main>
         &grsize(1,igr,1),cp(igr,2)   ,u1,depth ,rp(igr,2),&
         &uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)   ,&
         &sedtr1         )
!
         incall = .false.
!
!           Calculate sediment transport for u=u1
!
         call setrfo(&
         &incall    ,g   ,pacfac       ,relden   ,kinvis   ,&
!                                 <C-main>               <R-main>
         &grsize(1,igr,1),cp(igr,2)    ,u1,depth ,rp(igr,2),&
         &uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)   ,&
         &sedtr1         )
!
!           Calculate sediment transport for u=u2
!
         call setrfo(&
         &incall    ,g   ,pacfac       ,relden   ,kinvis   ,&
!                                 <C-main>               <R-main>
         &grsize(1,igr,1),cp(igr,2)    ,u2,depth ,rp(igr,2),&
         &uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)    ,&
         &sedtr2         )
!
         F1  = sedabs - sedtr1 * ws(igr)
         F2  = sedabs - sedtr2 * ws(igr)
         Fm  = 0.5*(F1 + F2)
!
!           Solve the transport equation for actual grid point
!           through iteration
!
         iter1  = 0
         iter2  = 0
         iter   = 0
40       iter   = iter + 1
!                      <Af,main>
         tolq   = u2-u1 .le. ueps
         if (max(iter1,iter2).gt.30) tolq = .true.
         if (.not. tolq )  Then
!
!                        <Af,main>        <Wf,main>
            depth   = afwfqs(igr,1) / afwfqs(igr,3)
!
!              Calculate sediment transport for middle of interval
!              ( u=um )
!
            um = 0.5 * (u1+u2)
            call setrfo(&
            &incall    ,g   ,pacfac       ,relden   ,kinvis   ,&
!                                 <av. Cs>                <av. Rs>
            &grsize(1,igr,1),cp(igr,2)    ,um,depth ,rp(igr,2),&
            &uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)    ,&
            &sedtrm         )
!
            Fm  = sedabs - sedtrm * ws(igr)
!
!              Redefine interval [u1,u2] enclosing the zero to be
!              found.
!
            if (Fm * F1 .le. 0) Then
               u2 = um
               F2 = Fm
               iter1 = iter1+1
               iter2 = 0
            else
               u1 = um
               F1 = Fm
               iter2 = iter2+1
               iter1 = 0
            endif
            go to 40
         else if (iter.gt.100) then
            go to 100
         endif
         if (sedtr(igr,1).lt.0) um = -um
!           <Q,gridp.>             <Af,main>
         hqav(igr,2)   = um *  dble(afwfqs(igr,1))
!           <Q,main>               <Af,main>
         afwfqs(igr,7) = um *  afwfqs(igr,1)
!
50    continue

60 continue
!
!     Calculation of the bed celerities.
!
!     In case of estuary morphology the celerity is only
!     proper defined in case of no subsections
!
   do igr=1,ngrid
      sectv(igr,1) = 0.
   enddo

   do 70 ibr = 1, nbran
      call senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden ,&
      &kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar ,&
      &nucoef ,uscoef ,trform ,prslot ,&
!                     <av. Af>    <av.Af,s>   <av. Wf>   <av. Wf,s>
      &waoft(1,3),afwfqs(1,1) ,waoft(1,1) ,afwfqs(1,3),&
      &nlev      ,hlev        ,wft        ,ws         ,&
!                     <secth0>   <av. Rs>    <av. Cs>    <asubsc>
      &sectv(1,8) ,rp(1,2)    ,cp(1,2)    ,sectv(1,1) ,&
!                                 <av. Q>    <av. Qs>    <av. H>
      &alfab      ,hqav(1,2)  ,afwfqs(1,7),hqav(1,1)  ,&
!                                          <work space>
      &grsize     ,forcon     ,asedtr     ,celer      ,&
      &.false.    ,psltvr     ,ker        )
70 continue
!
!     Determine flow direction for morphology module
!
   do 90 igr = 1, ngrid
!        if (epsequ(celer(igr,1),0.,1.0E-10)) then
      if (epsequ(celer(igr,1),0.,1.0E-30)) then
         flwdir(igr) = 0
      elseif (celer(igr,1) .gt. 0.) then
         flwdir(igr) = 1
      else
         flwdir(igr) = -1
      endif
90 continue

!     Set back of At from temporarily store in At1

   do igp = 1, ngrid
      waoft (igp,4) = waoft  (igp,5)
   enddo

!
!     Reset steps
!
   steps = 0
!

   return
100 ker   = fatal
   errno = emrbis
   call error (juer ,&
   &'SOSCAV Too many iterations for bisection',errno ,ker)
end


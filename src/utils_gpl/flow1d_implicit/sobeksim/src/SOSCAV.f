      subroutine SOSCAV ( steps  ,nbran  ,ngrid ,flwdir,celer ,sedtr  ,
     +                    dissed ,slat   ,asedtr,adissd,aslat ,juer   ,
     +                    maxlev ,nlev   ,hlev  ,wft   ,ws    ,alfab  ,
     +                    prslot ,branch ,sedpar,g     ,grsize,nucoef ,
     +                    uscoef ,trform ,forcon,engpar,sectv ,ahp    ,
     +                    cp     ,rp     ,waoft ,afwfqs,lkalm ,typcr  ,
     +                    bfrict ,bfricp ,aft   ,overlp,arex  ,arexcn ,
     +                    arexop ,of     ,maxtab,ntabm ,ntab  ,table  ,
     +                    psltvr ,scifri ,nnf   ,pfa   ,hqav  ,sectc  ,
     +                    wtt    ,att    ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c                     A.W.J.Koster (revision)
c
c Module:             SOSCAV (SObek Sediment Celerity AVarage)
c
c Module description: This routine avarages the calculated sediment
c                     transports and celerities and fills array flwdir.
c
c                     This routine avarages the calculated sediment
c                     transports and celerities. Notice that sedredge
c                     branches are not allowed in an estuary morphology
c                     case ! The avaraged values are returned in the
c                     'normal' arrays for calculated sediment transport
c                     and celerities.
c                     The array flwdir is filled for each gridpoint
c                     depending on the celerity at that gridpoint:
c                     Positive celerity -> flwdir(igp) = 1
c                     No celerity       -> flwdir(igp) = 0
c                     Negative celerity -> flwdir(igp) = -1
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 adissd(4,nbran)   I  Avaraged distributed sediment transport in
c                         nodes and boundaries
c  9 asedtr(ngrid)     I  Avaraged sediment transport
c 12 aslat(ngrid)      I  Avaraged lateral sediment
c  6 celer(ngrid,*)    IO Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c  7 dissed(4,nbran)   O  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c  4 flwdir(ngrid)     O  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  5 sedtr(ngrid,*)    O  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  8 slat(ngrid,*)     O  Actual lateral sediment transport in grid
c              1|2        point i+1/2 for:
c                         (i,1) = Main or Left channel.
c                         (i,2) = Right channel.
c  1 steps             IO Number of aggregation steps (estuary morpho-
c                         logy)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: soscav.pf,v $
c Revision 1.11  1999/06/01  13:42:50  kuipe_j
c names in messages substituted + message template
c
c Revision 1.10  1998/12/18  09:34:34  kuipe_j
c remove aux output
c
c Revision 1.9  1998/12/11  13:09:45  kuipe_j
c Improve celerity calculation in estuaries
c
c Revision 1.8  1998/06/11  11:47:50  kuipe_j
c Estuary special integrated
c
c Revision 1.7  1996/04/11  08:16:38  kuipe_j
c Kalman module added
c
c Revision 1.6  1996/03/08  09:40:51  kuipe_j
c Headers
c
c Revision 1.5  1996/03/07  10:44:32  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/09/22  10:04:33  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:06  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:00  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:26  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:10:07  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\errcod.i'
      include '..\include\sobdim.i'
c
c     Parameters
c
      integer  steps,  nbran, ngrid ,maxlev, maxtab, ntabm, nnf ,
     +         nucoef, ker
      integer  ntab(4,maxtab) ,typcr(nbran), bfrict(3,nbran) ,
     +         arexcn(ngrid,2),arexop(2)   , scifri(ngrid)   ,
     +         flwdir(ngrid)   , branch(4,nbran) , nlev(ngrid)
      real     sedtr  (ngrid,*),
     +         celer  (ngrid,*),
     +         dissed (4,nbran),
     +         slat   (ngrid,*),
     +         asedtr (ngrid)  ,
     +         adissd (4,nbran),
     +         aslat  (ngrid)   ,
     +         afwfqs (ngrid,8 ),
     +         waoft  (ngrid,*) ,
     +         sectc  (ngrid,3) ,
     +         sectv  (ngrid,dmsecv),
     +         wft    (ngrid,maxlev), aft    (ngrid,maxlev),
     +         wtt    (ngrid,maxlev), att    (ngrid,maxlev),
     +         ws     (ngrid)       ,
c     +         hqav   (ngrid,2)     ,
     +         alfab  (ngrid)
      real     g                ,overlp  
      real     uscoef(nrcoefs,nucoef)            ,trform(3,nbran) ,
     &         grsize(4,ngrid,*),forcon(4,ngrid,*),
     &         sedpar(*)        ,engpar(9)       ,prslot(3,nbran)  ,
     &         bfricp(6,ngrid)  ,arex(ngrid,4)   ,of(ngrid,maxlev) ,
     &         psltvr(7,ngrid)  ,pfa(nnf)        ,table(ntabm)
      real     ahp(ngrid,3)
      real     cp(ngrid,4) ,rp(ngrid,4)
      double precision hlev (ngrid,maxlev), hqav(ngrid,2)
c
c     Variables
c
      integer  igp,  icnt  ,errno  ,iter
      integer  ibr  ,igr   ,ind    ,
     &         i1    ,i2    ,juer  ,iter1 ,iter2 
      real     pacfac,relden,kinvis, depth, u1, u2, um,
     &         f1    ,f2    ,fm
      real     sedtr1 ,sedtr2 ,sedtrm , sedabs ,ueps
      logical  incall , tolq , lkalm

      logical  epsequ
      external epsequ
c
      logical  first
c
      data first /.true./
c
      if (first) then
c        Initialize averaged discharge
         do 5 igr=1,ngrid
            hqav (igr,2) = 0D0
    5    continue
         first = .false.
      endif
c
c     Average
c       - sediment transports
c       - lateral sediment
c       - water level
c
      do 10 igp = 1, ngrid
         sedtr (igp,1) = asedtr (igp)    / steps
         slat  (igp,1) = aslat  (igp)    / steps
c        <H-aver>
         hqav  (igp,1) = dble( ahp    (igp,3) )  / steps
c        Store At (at end of tide) temporarily in At1, because At 
c        will be overruled by At at averaged water level over tide.
         waoft (igp,5) = waoft  (igp,4)
 10   continue
c
c     Average distributed sediment transport 
c     ARS 9774 / Replace velocity determined transport by 
c     distributed transports
      do 30 ibr = 1, nbran
         do 20 icnt = 1, 4
            dissed(icnt,ibr) = adissd(icnt,ibr) / steps
 20      continue
         i1     = branch(3,ibr)
         i2     = branch(4,ibr)
         sedtr (i1,1) = dissed(1,ibr)
         sedtr (i2,1) = dissed(3,ibr)
 30   continue
c
c     Calculate hydraulic parameters for the flow-averaged
c     water level <h>.
c     The discharges <Q-aver> are from previous morph. time step !
c
      call flnp1( lkalm  ,   nbran  ,   ngrid  ,   nnf    ,
     +            branch ,   typcr  ,   bfrict ,   bfricp ,
c                 <H-aver> <Q-'aver'>
     +            hqav(1,1),hqav(1,2),  maxlev ,   nlev   ,
     +            hlev   ,   wft    ,   aft    ,   overlp ,
     +            arex   ,   arexcn ,   arexop ,   of     ,
     +            maxtab ,   ntabm  ,   ntab   ,   table  ,
     +            sectc  ,   sectv  ,   prslot ,   psltvr ,
     +            waoft  ,   grsize ,   engpar ,   scifri ,
     +            pfa    ,   juer   ,   cp     ,   rp     ,
     +            afwfqs ,   alfab  ,   wtt    ,   att    ,
     +            ker    )
c
c     Calculation of velocity related to the flow-averaged
c     sediment transport.
c
c     Given the averaged sediment transport (as computed above) ,
c     the velocity will be calculated by solving the
c     sediment transport equation :
c
c          F(u) = S - f(u) = 0
c
c     for the unknown velocity u.
c     S = flow-averaged sediment transport <asedtr>
c     u = velocity related to flow-averaged sediment transport
c     f = total sediment transport formula
c         (e.g. Ackers-White)
c
c     The root solver is the bisection method.
c     As the transport formulaes are (odd) power functions a
c     smooth convergence may be expected.
c
      kinvis = sedpar(1)
      relden = sedpar(2)
      pacfac = sedpar(3)
c
c     Loop over branches
c
      do 60 ibr=1,nbran
c
         ind    = max(int(trform(2,ibr)),1)
         incall = .true.
c
         i1     = branch(3,ibr)
         i2     = branch(4,ibr)
c
         do 50 igr = i1 , i2
c
c                        <Af,main>      <Wf,main>
            depth   = afwfqs(igr,1) / afwfqs(igr,3)
c
c           Prepare the bisection method for the interval [u1,u2]
c
            sedabs = abs(sedtr(igr,1))
c           The interval is limited by super critical flow 
            u2   = max(10.0,sqrt(g*depth))
            ueps = 1.e-6 * u2
            u1   =    0.0
            um   = 0.5 * (u1+u2)
c
c           Initialize sediment formula
c
            call setrfo(
     &            incall    ,g   ,pacfac      ,relden   ,kinvis   ,
c                                 <C-main>               <R-main>
     &            grsize(1,igr,1),cp(igr,2)   ,u1,depth ,rp(igr,2),
     &            uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)   ,
     &            sedtr1         )
c
            incall = .false.
c
c           Calculate sediment transport for u=u1
c
            call setrfo(
     &            incall    ,g   ,pacfac       ,relden   ,kinvis   ,
c                                 <C-main>               <R-main>
     &            grsize(1,igr,1),cp(igr,2)    ,u1,depth ,rp(igr,2),
     &            uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)   ,
     &            sedtr1         )
c
c           Calculate sediment transport for u=u2
c
            call setrfo(
     &            incall    ,g   ,pacfac       ,relden   ,kinvis   ,
c                                 <C-main>               <R-main>
     &            grsize(1,igr,1),cp(igr,2)    ,u2,depth ,rp(igr,2),
     &            uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)    ,
     &            sedtr2         )
c
            F1  = sedabs - sedtr1 * ws(igr)
            F2  = sedabs - sedtr2 * ws(igr)
            Fm  = 0.5*(F1 + F2)
c
c           Solve the transport equation for actual grid point
c           through iteration
c
            iter1  = 0
            iter2  = 0
            iter   = 0
   40       iter   = iter + 1
c                      <Af,main>
            tolq   = u2-u1 .le. ueps
            if (max(iter1,iter2).gt.30) tolq = .true.
            if (.not. tolq )  Then
c
c                        <Af,main>        <Wf,main>
               depth   = afwfqs(igr,1) / afwfqs(igr,3)
c
c              Calculate sediment transport for middle of interval
c              ( u=um )
c
               um = 0.5 * (u1+u2)
               call setrfo(
     &            incall    ,g   ,pacfac       ,relden   ,kinvis   ,
c                                 <av. Cs>                <av. Rs>
     &            grsize(1,igr,1),cp(igr,2)    ,um,depth ,rp(igr,2),
     &            uscoef(1,ind)  ,trform(1,ibr),forcon(1,igr,1)    ,
     &            sedtrm         )
c
               Fm  = sedabs - sedtrm * ws(igr)
c
c              Redefine interval [u1,u2] enclosing the zero to be
c              found.
c
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
c           <Q,gridp.>             <Af,main>
            hqav(igr,2)   = um *  dble(afwfqs(igr,1))
c           <Q,main>               <Af,main>
            afwfqs(igr,7) = um *  afwfqs(igr,1)
c
   50    continue
         
   60 continue
c
c     Calculation of the bed celerities.
c
c     In case of estuary morphology the celerity is only
c     proper defined in case of no subsections
c
      do igr=1,ngrid
         sectv(igr,1) = 0.  
      enddo

      do 70 ibr = 1, nbran
         call senorb (nbran  ,ngrid  ,maxlev ,g      ,pacfac ,relden ,
     &                kinvis ,ibr    ,juer   ,branch ,bfrict ,engpar ,
     &                nucoef ,uscoef ,trform ,prslot ,
c                     <av. Af>    <av.Af,s>   <av. Wf>   <av. Wf,s>
     &                waoft(1,3),afwfqs(1,1) ,waoft(1,1) ,afwfqs(1,3),
     &                nlev      ,hlev        ,wft        ,ws         ,
c                     <secth0>   <av. Rs>    <av. Cs>    <asubsc>
     &                sectv(1,8) ,rp(1,2)    ,cp(1,2)    ,sectv(1,1) ,
c                                 <av. Q>    <av. Qs>    <av. H>
     &                alfab      ,hqav(1,2)  ,afwfqs(1,7),hqav(1,1)  ,
c                                          <work space>
     &                grsize     ,forcon     ,asedtr     ,celer      ,
     &                .false.    ,psltvr     ,ker        )
 70   continue
c
c     Determine flow direction for morphology module
c
      do 90 igr = 1, ngrid
c        if (epsequ(celer(igr,1),0.,1.0E-10)) then
         if (epsequ(celer(igr,1),0.,1.0E-30)) then
            flwdir(igr) = 0
         elseif (celer(igr,1) .gt. 0.) then
            flwdir(igr) = 1
         else
            flwdir(igr) = -1
         endif
   90 continue

c     Set back of At from temporarily store in At1

      do igp = 1, ngrid
         waoft (igp,4) = waoft  (igp,5)
      enddo

c
c     Reset steps
c
      steps = 0
c

      return
  100 ker   = fatal
      errno = emrbis
      call error (juer ,
     *       'SOSCAV Too many iterations for bisection',errno ,ker)
      end


subroutine FLAUTO ( g     ,time  ,ngrid ,x     ,hp    ,qp    ,&
&strtyp,strpar,maxtab,ntabm ,ntab  ,table ,&
&maxlev,hlev  ,nlev  ,nqlat ,qltpar,juer  ,&
&qlat  ,qlatgr,strclo,strhis,nbran ,branch,&
&nhstat,hbdpar,hstat ,nqstat,qbdpar,qstat ,&
&th2   ,dtf   ,lambda,dhstru,ker   ,&
&nnode ,nbrnod,brnode,typcr ,prslot,&
&wft   ,aft   ,wtt   ,att   ,&
&arex  ,arexcn,arexop,grid  ,&
&wf    ,af    ,wt    ,at    ,of    ,o     ,&
&psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAUTO (FLow AUTOstart procedure)
!
! Module description: Determine initial conditions based on boundary
!                     conditions.
!
!                     The auto start procedure is implemented by iterat-
!                     ing a steady state on time = 0. Initial conditions
!                     are required and they are determined as follows:
!
!                     -   calculate avaraged depth at h-boundaries;
!                     -   calculate maximum boundary discharge (abso-
!                         lute);
!                     -   fill network with values;
!                     -   calculate lateral discharges based on condi-
!                         tions described above;
!                     -   increase discharge with maximum lateral dis-
!                         charge on every point of the network.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 49 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 41 aft               P  -
! 44 arex              P  -
! 45 arexcn            P  -
! 46 arexop            P  -
! 51 at                P  -
! 43 att               P  -
! 24 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 37 brnode            P  -
! 32 dtf               P  -
!  1 g                 P  -
! 47 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 26 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
!                         (1,i) = Location [grid point] for H-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : h = f(t)
!                                 cbfqoh (2) : h = h(Q)
!                                 cbfour (3) : h = fourier
!                                 cbtidl (4) : h = tidal components
!                         (3,i) = Table number for f(t), h(Q), fourier
!                                 or tidal components table.
! 14 hlev              P  -
!  5 hp(ngrid,3)       IO (i,1) = h1(i) (t=n)
!                         (i,2) = h(i)  (*)
!                         (i,3) = h2(i) (t=n+1)
! 27 hstat(nhstat)     I  Actual water level in every H-station.
! 18 juer              P  -
! 34 ker               P  -
! 33 lambda            P  -
! 13 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  9 maxtab            I  Maximum number of defined tables.
! 23 nbran             I  Number of branches.
! 36 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  3 ngrid             I  Number of grid points in network.
! 25 nhstat            I  Number of H-boundary stations.
! 15 nlev              P  -
! 35 nnode             I  Number of nodes.
! 16 nqlat             P  -
! 28 nqstat            I  Number of Q-boundary stations.
! 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 10 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 53 o                 P  -
! 52 of                P  -
! 39 prslot            P  -
! 54 psltvr            P  -
! 29 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
!                         (1,i) = Location [grid point] for Q-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : Q = f(t)
!                                 cbfqoh (2) : Q = Q(h)
!                                 cbfour (3) : Q = fourier
!                                 cbtidl (4) : Q = tidal components
!                         (3,i) = Table number for f(t), Q(h), fourier
!                                 or tidal components table.
! 19 qlat              P  -
! 20 qlatgr(ngrid)     I  (i) = Actual lateral discharge in grid point
!                         i+1/2.
! 17 qltpar            P  -
!  6 qp(ngrid,3)       IO (i,1) = q1(i) (t=n)
!                         (i,2) = q(i)  (*)
!                         (i,3) = q2(i) (t=n+1)
! 30 qstat(nqstat)     I  Actual discharge in every Q-station.
! 21 strclo            P  -
! 22 strhis            P  -
!  8 strpar            P  -
!  7 strtyp            P  -
! 12 table             P  -
! 31 th2               P  -
!  2 time              P  -
! 38 typcr             P  -
! 48 wf                P  -
! 40 wft               P  -
! 50 wt                P  -
! 42 wtt               P  -
!  4 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flaunh  FLow AUtostart Node H adaptation
! flaunq  FLow AUtostart Node Q adaptation
! flausm  FLow AUtostart SMoothing
! flinia  FLow INItialisation Areas and widths
! flqasm  FLow QqA will be SMoothed
! flqlat  FLow Q LATeral
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flauto.pf,v $
! Revision 1.12  1999/03/15  15:49:28  kuipe_j
! tabs removed
!
! Revision 1.11  1997/11/04  14:07:14  kuipe_j
! dhstru in lat structures
!
! Revision 1.10  1997/01/23  08:28:56  kuipe_j
! Make flow module robust
!
! Revision 1.9  1995/10/18  08:59:15  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.8  1995/09/29  11:22:09  hoeks_a
! Undeclared variable declared
!
! Revision 1.7  1995/09/29  10:36:12  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.6  1995/09/22  10:00:54  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:10:46  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:36:23  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:54:43  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:39  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:26  hoeks_a
! Initial check-in
!
! Revision 1.5  1994/12/05  13:49:13  kuipe_j
! Release 0.06
!
! Revision 1.4  1994/12/02  13:21:32  kuipe_j
! Look at Q-H relations when making initial conditions.
!
! Revision 1.2  1993/11/26  15:30:27  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Parameters
!
   integer ngrid ,maxtab,ntabm ,nqlat ,maxlev,juer ,ker   ,&
   &nbran ,nhstat,nqstat,nnode ,nbrnod
   integer strtyp(10,*), ntab(4,maxtab), branch(4,nbran) ,&
   &hbdpar(3,*) , qbdpar(3,*)   , nlev   (ngrid)  ,&
   &brnode(nnode,nbrnod+1)      ,&
   &typcr(nbran)   ,grid(ngrid) ,&
   &arexcn(ngrid,2), arexop(2)
!
   real    g     ,lambda   ,th2  ,dhstru
   real    qltpar(9,*)     , qlat(*)    , qlatgr(ngrid),&
   &x(ngrid)     ,&
   &table(ntabm)    , strpar(dmstrpar,*)        ,&
   &strhis(dmstrh,*),&
   &hstat(*)        , qstat(*)                  ,&
   &prslot(3,nbran) , psltvr(7,ngrid)           ,&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &wtt(ngrid,maxlev), att(ngrid,maxlev),&
   &of (ngrid,maxlev),&
   &arex(ngrid,4),&
   &wf(ngrid), af(ngrid),&
   &wt(ngrid), at(ngrid),&
   &o (ngrid)
!
   double  precision  time     ,dtf , hlev(ngrid,maxlev),&
   &hp(ngrid,3), qp(ngrid,3)
!
   logical strclo(*)
!
!     Local variables
!
   integer   ndepth, i1,    i2,     igr,   ibr,    iopt, istat ,&
   &itab  , i,     iter
   real      adepth, disch, qlatmx, depth, watlev, qh  ,maxare ,&
   &overlp, omqlat, relstr
   double precision qmin
   logical   depest, disest ,inires
!
!     mozart dummy declarations
   integer      nstmoz
   logical      lmozad
   character(len=40) qlatidd(1)
!
!     External functions
!
   real      FLBOTT ,FLTOPL
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Determine avaraged water depth based on h-boundaries
!
   depest = .false.
   adepth = 0.
   ndepth = 0
   do 10 istat = 1, nhstat
      igr  = hbdpar(1,istat)
      iopt = hbdpar(2,istat)
!
!        Functions h=f(Q) or Q=f(h) can not be processed
!
      if (iopt .ne. cbfhoq) then
         depth  = hstat(istat) - FLBOTT(igr,ngrid,maxlev,hlev)
         adepth = adepth + depth
         ndepth = ndepth + 1
         depest = .true.
      endif
10 continue
!
!     Calculate avaraged depth
!
   if (ndepth .gt. 0) then
      adepth = adepth / real(ndepth)
   endif
!
!     Determine maximum discharge based on q-boundaries
!
   disest = .false.
   disch  = 0.
   do 20 istat = 1, nqstat
      iopt = qbdpar(2,istat)
      if (iopt .ne. cbfhoq) then
         disch  = max ( disch, abs(qstat(istat)))
         disest = .true.
      endif
20 continue
!
!     If only a depth or a discharge but not both is obtained,
!     search in the Q-H relations
!
   if (depest) then
      if (.not.disest) then
!
!           No discharge available.
!           Start with search for discharge in Q=f(H) table
!
         disch = 0.
         do 30 istat = 1, nqstat
            igr  = qbdpar(1,istat)
            iopt = qbdpar(2,istat)
            if (iopt .eq. cbfhoq) then
               itab   = qbdpar(3,istat)
               watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
!
               call INTTAB (ntab(1,itab), ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &dble(watlev), qh   )
!
               disch  = max ( disch, qh)
               disest = .true.
            endif
30       continue
!
!           If not yet found, search for discharge in H=f(Q) table
!
         if (.not.disest) then
            do 40 istat = 1, nhstat
               igr  = hbdpar(1,istat)
               iopt = hbdpar(2,istat)
               if (iopt .eq. cbfhoq) then
                  itab   = hbdpar(3,istat)
                  watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
!
!                    Inverse interpolation by exchanging arguments
!                    and function values.
!
                  call INTTAB (ntab(1,itab), ntab(4,itab),&
                  &table(ntab(3,itab)),&
                  &table(ntab(2,itab)),&
                  &dble(watlev),qh    )
!
                  disch  = max ( disch, qh)
                  disest = .true.
               endif
40          continue
         endif
      endif
   else
      if (disest) then
!
!           No depth available.
!           Start with search for water level in H=f(Q) table
!
         adepth = 0.
         ndepth = 0
         do 50 istat = 1, nhstat
            igr  = hbdpar(1,istat)
            iopt = hbdpar(2,istat)
            if (iopt .eq. cbfhoq) then
               itab   = hbdpar(3,istat)
!
               call INTTAB (ntab(1,itab), ntab(4,itab),&
               &table(ntab(2,itab)),&
               &table(ntab(3,itab)),&
               &dble(disch) ,watlev)
!
               depth  = watlev - FLBOTT(igr,ngrid,maxlev,hlev)
               adepth = adepth + depth
               ndepth = ndepth + 1
               depest = .true.
            endif
50       continue
!
!           If not yet found, search for water level in Q=f(H) table
!
         if (.not.depest) then
            do 60 istat = 1, nqstat
               igr  = qbdpar(1,istat)
               iopt = qbdpar(2,istat)
               if (iopt .eq. cbfhoq) then
                  itab   = qbdpar(3,istat)
                  watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
!
!                    Inverse interpolation by exchanging arguments
!                    and function values.
!
                  call INTTAB (ntab(1,itab), ntab(4,itab),&
                  &table(ntab(3,itab)),&
                  &table(ntab(2,itab)),&
                  &dble(disch) ,watlev)
!
                  depth  = watlev - FLBOTT(igr,ngrid,maxlev,hlev)
                  adepth = adepth + depth
                  ndepth = ndepth + 1
                  depest = .true.
               endif
60          continue
         endif
!
!           Calculate avaraged depth
!
         if (ndepth .gt. 0) then
            adepth = adepth / real(ndepth)
         endif
      endif
   endif
!
!     Test if no depth could be obtained or if the depth appeares
!     to be zero.
!     If so the depth will be defined as the half of the difference
!     between top and bottom level in a cross section. This depth will
!     be averaged over all grid points.
!
   if (adepth .lt. .001) then
      adepth = 0.
      do 70 igr=1,ngrid
         adepth =  adepth +&
         &(FLTOPL(igr,ngrid,maxlev,hlev,nlev) -&
         &FLBOTT(igr,ngrid,maxlev,hlev)) *.5
70    continue
      adepth = adepth / real(ngrid)
   endif
!
!     Make estimation of water levels in every grid point
!     ===================================================
!
!     Fill data structures with avaraged depth
!
   do 80 igr = 1, ngrid
      hp(igr,3) = dble( adepth + FLBOTT(igr,ngrid,maxlev,hlev) )
80 continue
!
!     water levels at grid points at nodes are made equal
!
   call flaunh (nnode ,nbran ,nbrnod ,ngrid ,branch ,&
   &brnode,hp(1,3))
!
!     Assign water level boundary conditions
!
   do 83  istat = 1, nhstat
      igr  = hbdpar(1,istat)
      iopt = hbdpar(2,istat)
      if (iopt .ne. cbfhoq) then
         hp(igr,3) = dble(hstat(istat))
      endif
83 continue
!
!     Smooth water levels at all intermediate grid points
!     Smoothing will be performed several times, so
!     a more or less linear distribution will be obtained
!
   do 84 i=1,5
      call flausm (nbran,ngrid,branch,hp(1,3),hp(1,2),0.5)
      do igr = 1, ngrid
         hp(igr,3) = max(dble(adepth*.05 +&
         &FLBOTT(igr,ngrid,maxlev,hlev)),&
         &hp(igr,3))
      enddo
84 continue

   do 85 igr = 1, ngrid
      hp(igr,2) = hp(igr,3)
      hp(igr,1) = hp(igr,3)
85 continue
!
!     Make estimation of discharges in every grid point
!     =================================================
!
!     Start with calculation of lateral discharges
   iter = 1
   omqlat = 0.0
   relstr = 1.0
!                                                          <h_n>
   lmozad    = .false.
   nstmoz    = 0
   qlatidd(1)= 'AUTO'
   call FLQLAT(g      ,time   ,ngrid  ,lambda ,x      ,hp(1,1),&
!                 <h_*>
   &hp(2,1),strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,&
!                 mozart dummy parameters
   &lmozad ,nstmoz ,qlatidd,&
   &table  ,nqlat  ,qltpar ,juer   ,qlat   ,&
   &qlatgr ,strclo ,strhis ,th2    ,dtf    ,ker    ,&
   &omqlat ,dhstru ,relstr ,iter   ,.false.)
!     in case of SRW                              yet no action
!
!     Determine maximum lateral discharge
!
   qlatmx = 0.
   do 100 ibr = 1, nbran
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
      do 90 igr = i1, i2-1
         qlatmx = max ( qlatmx, abs(qlatgr(igr)))
90    continue
100 continue
!
!     Max discharge = disch + qlatmx
!
   disch = disch + qlatmx
!
!     Determine flow area of cross sections
!
   inires = .true.
   overlp = 0.
!
   call FLINIA(inires ,nbran  ,ngrid  ,branch ,typcr  ,&
   &hp(1,1),maxlev ,nlev   ,hlev   ,prslot ,&
   &overlp ,arex   ,arexcn ,arexop ,&
   &wft    ,aft    ,wtt    ,att    ,of     ,&
   &wf     ,af     ,wt     ,at     ,o      ,&
   &juer   ,ker    ,psltvr)
!
   call flausn (nbran,ngrid,branch,af,qp(1,2),0.1)
!
!     Fill data structures with discharges.
!     The discharges are proportional to the current flow area.
!
   maxare = 0.
   do 105  i=1,ngrid
      maxare = max (maxare,af(i))
105 continue
!
   do 110 igr = 1, ngrid
      qp(igr,3) = dble( disch * af(igr) / maxare )
110 continue
!
!     Smooth discharges at all intermediate grid points
!
   call flausm (nbran,ngrid,branch,qp(1,3),qp(1,2),0.5)
!
!     Discharges left and right of structures should be equal
!
   do 111 igr = 2, ngrid
      if (grid(igr-1) .eq. 2 ) then
         qmin = min(qp(igr-1,3),qp(igr,3))
         qp(igr-1,3) = qmin
         qp(igr,3)   = qmin
      endif
111 continue
!
!     The discharges at nodes should satisfy the continuity equation
!
   call flaunq (nnode ,nbran ,nbrnod ,ngrid ,branch ,&
   &brnode,qp(1,3))
!
!     Assign discharge boundary conditions
!
   do 112 istat = 1, nqstat
      igr  = qbdpar(1,istat)
      iopt = qbdpar(2,istat)
      if (iopt .ne. cbfhoq) then
         qp(igr,3) = dble( qstat(istat) )
      endif
112 continue
!
!     Calculate Q*Q/A in every point
!
   do 113 igr = 1, ngrid
      qp(igr,3) = qp(igr,3) * qp(igr,3) / af(igr)
113 continue
!
!     Smooth Q*Q/A at intermediate grid points,
!     structure grid points excluded
!
!     Smoothing will be performed several times, so
!     a more or less linear distribution will be obtained.
!
   do 114 i=1,5
      call flqasm (nbran,ngrid,branch,grid,qp(1,3),qp(1,2))
114 continue
!
!     Calculate discharge in every point
!
   do 115 igr = 1, ngrid
      qp(igr,3) = sqrt(qp(igr,3) * af(igr))
115 continue
!
!      DO IGR=1,NGRID
!          bodem =  FLBOTT(igr,ngrid,maxlev,hlev)
!          WRITE (*,*) igr,qp(igr,3),hp(igr,3),bodem
!      ENDDO
   do 116 igr = 1, ngrid
      qp(igr,2) = qp(igr,3)
      qp(igr,1) = qp(igr,3)
116 continue

end

      subroutine FLAUTO ( g     ,time  ,ngrid ,x     ,hp    ,qp    ,
     +                    strtyp,strpar,maxtab,ntabm ,ntab  ,table ,
     +                    maxlev,hlev  ,nlev  ,nqlat ,qltpar,juer  ,
     +                    qlat  ,qlatgr,strclo,strhis,nbran ,branch,
     +                    nhstat,hbdpar,hstat ,nqstat,qbdpar,qstat ,
     +                    th2   ,dtf   ,lambda,dhstru,ker   ,
     +                    nnode ,nbrnod,brnode,typcr ,prslot,
     +                    wft   ,aft   ,wtt   ,att   ,
     +                    arex  ,arexcn,arexop,grid  ,
     +                    wf    ,af    ,wt    ,at    ,of    ,o     ,
     +                    psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAUTO (FLow AUTOstart procedure)
c
c Module description: Determine initial conditions based on boundary
c                     conditions.
c
c                     The auto start procedure is implemented by iterat-
c                     ing a steady state on time = 0. Initial conditions
c                     are required and they are determined as follows:
c
c                     -   calculate avaraged depth at h-boundaries;
c                     -   calculate maximum boundary discharge (abso-
c                         lute);
c                     -   fill network with values;
c                     -   calculate lateral discharges based on condi-
c                         tions described above;
c                     -   increase discharge with maximum lateral dis-
c                         charge on every point of the network.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 49 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 41 aft               P  -
c 44 arex              P  -
c 45 arexcn            P  -
c 46 arexop            P  -
c 51 at                P  -
c 43 att               P  -
c 24 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 37 brnode            P  -
c 32 dtf               P  -
c  1 g                 P  -
c 47 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 26 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c 14 hlev              P  -
c  5 hp(ngrid,3)       IO (i,1) = h1(i) (t=n)
c                         (i,2) = h(i)  (*)
c                         (i,3) = h2(i) (t=n+1)
c 27 hstat(nhstat)     I  Actual water level in every H-station.
c 18 juer              P  -
c 34 ker               P  -
c 33 lambda            P  -
c 13 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  9 maxtab            I  Maximum number of defined tables.
c 23 nbran             I  Number of branches.
c 36 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  3 ngrid             I  Number of grid points in network.
c 25 nhstat            I  Number of H-boundary stations.
c 15 nlev              P  -
c 35 nnode             I  Number of nodes.
c 16 nqlat             P  -
c 28 nqstat            I  Number of Q-boundary stations.
c 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 10 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 53 o                 P  -
c 52 of                P  -
c 39 prslot            P  -
c 54 psltvr            P  -
c 29 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
c                         (1,i) = Location [grid point] for Q-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : Q = f(t)
c                                 cbfqoh (2) : Q = Q(h)
c                                 cbfour (3) : Q = fourier
c                                 cbtidl (4) : Q = tidal components
c                         (3,i) = Table number for f(t), Q(h), fourier
c                                 or tidal components table.
c 19 qlat              P  -
c 20 qlatgr(ngrid)     I  (i) = Actual lateral discharge in grid point
c                         i+1/2.
c 17 qltpar            P  -
c  6 qp(ngrid,3)       IO (i,1) = q1(i) (t=n)
c                         (i,2) = q(i)  (*)
c                         (i,3) = q2(i) (t=n+1)
c 30 qstat(nqstat)     I  Actual discharge in every Q-station.
c 21 strclo            P  -
c 22 strhis            P  -
c  8 strpar            P  -
c  7 strtyp            P  -
c 12 table             P  -
c 31 th2               P  -
c  2 time              P  -
c 38 typcr             P  -
c 48 wf                P  -
c 40 wft               P  -
c 50 wt                P  -
c 42 wtt               P  -
c  4 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flaunh  FLow AUtostart Node H adaptation
c flaunq  FLow AUtostart Node Q adaptation
c flausm  FLow AUtostart SMoothing
c flinia  FLow INItialisation Areas and widths
c flqasm  FLow QqA will be SMoothed
c flqlat  FLow Q LATeral
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flauto.pf,v $
c Revision 1.12  1999/03/15  15:49:28  kuipe_j
c tabs removed
c
c Revision 1.11  1997/11/04  14:07:14  kuipe_j
c dhstru in lat structures
c
c Revision 1.10  1997/01/23  08:28:56  kuipe_j
c Make flow module robust
c
c Revision 1.9  1995/10/18  08:59:15  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/09/29  11:22:09  hoeks_a
c Undeclared variable declared
c
c Revision 1.7  1995/09/29  10:36:12  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.6  1995/09/22  10:00:54  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:10:46  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:36:23  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:54:43  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:39  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:26  hoeks_a
c Initial check-in
c
c Revision 1.5  1994/12/05  13:49:13  kuipe_j
c Release 0.06
c
c Revision 1.4  1994/12/02  13:21:32  kuipe_j
c Look at Q-H relations when making initial conditions.
c
c Revision 1.2  1993/11/26  15:30:27  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Parameters
c
      integer ngrid ,maxtab,ntabm ,nqlat ,maxlev,juer ,ker   ,
     +        nbran ,nhstat,nqstat,nnode ,nbrnod
      integer strtyp(10,*), ntab(4,maxtab), branch(4,nbran) ,
     +        hbdpar(3,*) , qbdpar(3,*)   , nlev   (ngrid)  ,
     +        brnode(nnode,nbrnod+1)      ,
     +        typcr(nbran)   ,grid(ngrid) ,
     +        arexcn(ngrid,2), arexop(2)
c
      real    g     ,lambda   ,th2  ,dhstru
      real    qltpar(9,*)     , qlat(*)    , qlatgr(ngrid),
     +        x(ngrid)     ,
     +        table(ntabm)    , strpar(dmstrpar,*)        ,
     +        strhis(dmstrh,*), 
     +        hstat(*)        , qstat(*)                  ,
     +        prslot(3,nbran) , psltvr(7,ngrid)           ,
     +        wft(ngrid,maxlev), aft(ngrid,maxlev),
     +        wtt(ngrid,maxlev), att(ngrid,maxlev),
     +        of (ngrid,maxlev),
     +        arex(ngrid,4),
     +        wf(ngrid), af(ngrid),
     +        wt(ngrid), at(ngrid),
     +        o (ngrid)
c
      double  precision  time     ,dtf , hlev(ngrid,maxlev),
     +        hp(ngrid,3), qp(ngrid,3)
c
      logical strclo(*)
c
c     Local variables
c
      integer   ndepth, i1,    i2,     igr,   ibr,    iopt, istat ,
     +          itab  , i,     iter
      real      adepth, disch, qlatmx, depth, watlev, qh  ,maxare ,
     +          overlp, omqlat, relstr
      double precision qmin
      logical   depest, disest ,inires
c
c     mozart dummy declarations
      integer      nstmoz 
      logical      lmozad
      character*40 qlatidd(1)
c
c     External functions
c
      real      FLBOTT ,FLTOPL
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     Determine avaraged water depth based on h-boundaries
c
      depest = .false.
      adepth = 0.
      ndepth = 0
      do 10 istat = 1, nhstat
         igr  = hbdpar(1,istat)
         iopt = hbdpar(2,istat)
c
c        Functions h=f(Q) or Q=f(h) can not be processed
c
         if (iopt .ne. cbfhoq) then
            depth  = hstat(istat) - FLBOTT(igr,ngrid,maxlev,hlev)
            adepth = adepth + depth
            ndepth = ndepth + 1
            depest = .true.
         endif
 10   continue
c
c     Calculate avaraged depth
c
      if (ndepth .gt. 0) then
         adepth = adepth / real(ndepth)
      endif
c
c     Determine maximum discharge based on q-boundaries
c
      disest = .false.
      disch  = 0.
      do 20 istat = 1, nqstat
         iopt = qbdpar(2,istat)
         if (iopt .ne. cbfhoq) then
            disch  = max ( disch, abs(qstat(istat)))
            disest = .true.
         endif
 20   continue
c
c     If only a depth or a discharge but not both is obtained,
c     search in the Q-H relations
c
      if (depest) then
         if (.not.disest) then
c
c           No discharge available.
c           Start with search for discharge in Q=f(H) table
c
            disch = 0.
            do 30 istat = 1, nqstat
               igr  = qbdpar(1,istat)
               iopt = qbdpar(2,istat)
               if (iopt .eq. cbfhoq) then
                  itab   = qbdpar(3,istat)
                  watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
c
                  call INTTAB (ntab(1,itab), ntab(4,itab),
     +                         table(ntab(2,itab)),
     +                         table(ntab(3,itab)),
     +                         dble(watlev), qh   )
c
                  disch  = max ( disch, qh)
                  disest = .true.
               endif
 30         continue
c
c           If not yet found, search for discharge in H=f(Q) table
c
            if (.not.disest) then
               do 40 istat = 1, nhstat
                  igr  = hbdpar(1,istat)
                  iopt = hbdpar(2,istat)
                  if (iopt .eq. cbfhoq) then
                     itab   = hbdpar(3,istat)
                     watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
c
c                    Inverse interpolation by exchanging arguments
c                    and function values.
c
                     call INTTAB (ntab(1,itab), ntab(4,itab),
     +                            table(ntab(3,itab)),
     +                            table(ntab(2,itab)),
     +                            dble(watlev),qh    )
c
                     disch  = max ( disch, qh)
                     disest = .true.
                  endif
   40          continue
           endif
        endif
      else
         if (disest) then
c
c           No depth available.
c           Start with search for water level in H=f(Q) table
c
            adepth = 0.
            ndepth = 0
            do 50 istat = 1, nhstat
               igr  = hbdpar(1,istat)
               iopt = hbdpar(2,istat)
               if (iopt .eq. cbfhoq) then
                  itab   = hbdpar(3,istat)
c
                  call INTTAB (ntab(1,itab), ntab(4,itab),
     +                         table(ntab(2,itab)),
     +                         table(ntab(3,itab)),
     +                         dble(disch) ,watlev)
c
                  depth  = watlev - FLBOTT(igr,ngrid,maxlev,hlev)
                  adepth = adepth + depth
                  ndepth = ndepth + 1
                  depest = .true.
               endif
   50       continue
c
c           If not yet found, search for water level in Q=f(H) table
c
            if (.not.depest) then
               do 60 istat = 1, nqstat
                  igr  = qbdpar(1,istat)
                  iopt = qbdpar(2,istat)
                  if (iopt .eq. cbfhoq) then
                     itab   = qbdpar(3,istat)
                     watlev = adepth + FLBOTT(igr,ngrid,maxlev,hlev)
c
c                    Inverse interpolation by exchanging arguments
c                    and function values.
c
                     call INTTAB (ntab(1,itab), ntab(4,itab),
     +                            table(ntab(3,itab)),
     +                            table(ntab(2,itab)),
     +                            dble(disch) ,watlev)
c
                     depth  = watlev - FLBOTT(igr,ngrid,maxlev,hlev)
                     adepth = adepth + depth
                     ndepth = ndepth + 1
                     depest = .true.
                  endif
   60          continue
            endif
c
c           Calculate avaraged depth
c
            if (ndepth .gt. 0) then
               adepth = adepth / real(ndepth)
            endif
        endif
      endif
c
c     Test if no depth could be obtained or if the depth appeares
c     to be zero.
c     If so the depth will be defined as the half of the difference
c     between top and bottom level in a cross section. This depth will
c     be averaged over all grid points.
c
      if (adepth .lt. .001) then
        adepth = 0.
        do 70 igr=1,ngrid
           adepth =  adepth +
     +               (FLTOPL(igr,ngrid,maxlev,hlev,nlev) -
     +                FLBOTT(igr,ngrid,maxlev,hlev)) *.5
   70    continue
         adepth = adepth / real(ngrid)
      endif
c
c     Make estimation of water levels in every grid point
c     ===================================================
c
c     Fill data structures with avaraged depth
c
      do 80 igr = 1, ngrid
         hp(igr,3) = dble( adepth + FLBOTT(igr,ngrid,maxlev,hlev) )
 80   continue
c
c     water levels at grid points at nodes are made equal
c
      call flaunh (nnode ,nbran ,nbrnod ,ngrid ,branch ,
     &                   brnode,hp(1,3))
c
c     Assign water level boundary conditions
c
      do 83  istat = 1, nhstat
         igr  = hbdpar(1,istat)
         iopt = hbdpar(2,istat)
         if (iopt .ne. cbfhoq) then
            hp(igr,3) = dble(hstat(istat))
         endif
 83   continue
c
c     Smooth water levels at all intermediate grid points
c     Smoothing will be performed several times, so
c     a more or less linear distribution will be obtained
c
      do 84 i=1,5
         call flausm (nbran,ngrid,branch,hp(1,3),hp(1,2),0.5)
         do igr = 1, ngrid
            hp(igr,3) = dble( max(adepth*.05 + 
     +                      FLBOTT(igr,ngrid,maxlev,hlev),
     +                      hp(igr,3)) )
         enddo 
 84   continue

      do 85 igr = 1, ngrid
         hp(igr,2) = hp(igr,3)
         hp(igr,1) = hp(igr,3)
 85   continue
c
c     Make estimation of discharges in every grid point
c     =================================================
c
c     Start with calculation of lateral discharges
      iter = 1
      omqlat = 0.0
      relstr = 1.0
c                                                          <h_n>
      lmozad    = .false.
      nstmoz    = 0
      qlatidd(1)= 'AUTO'
      call FLQLAT(g      ,time   ,ngrid  ,lambda ,x      ,hp(1,1),
c                 <h_*>
     +            hp(2,1),strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,
c                 mozart dummy parameters
     +            lmozad ,nstmoz ,qlatidd,
     +            table  ,nqlat  ,qltpar ,juer   ,qlat   ,
     +            qlatgr ,strclo ,strhis ,th2    ,dtf    ,ker    ,
     +            omqlat ,dhstru ,relstr ,iter   ,.false.)
c     in case of SRW                              yet no action
c
c     Determine maximum lateral discharge
c
      qlatmx = 0.
      do 100 ibr = 1, nbran
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 90 igr = i1, i2-1
            qlatmx = max ( qlatmx, abs(qlatgr(igr)))
 90      continue
100   continue
c
c     Max discharge = disch + qlatmx
c
      disch = disch + qlatmx
c
c     Determine flow area of cross sections
c
      inires = .true.
      overlp = 0.
c
      call FLINIA(inires ,nbran  ,ngrid  ,branch ,typcr  ,
     +            hp(1,1),maxlev ,nlev   ,hlev   ,prslot ,
     +            overlp ,arex   ,arexcn ,arexop ,
     +            wft    ,aft    ,wtt    ,att    ,of     ,
     +            wf     ,af     ,wt     ,at     ,o      ,
     +            juer   ,ker    ,psltvr)
c
      call flausn (nbran,ngrid,branch,af,qp(1,2),0.1) 
c
c     Fill data structures with discharges.
c     The discharges are proportional to the current flow area.
c
      maxare = 0.
      do 105  i=1,ngrid
          maxare = max (maxare,af(i))
  105 continue
c
      do 110 igr = 1, ngrid
         qp(igr,3) = dble( disch * af(igr) / maxare )
 110  continue
c
c     Smooth discharges at all intermediate grid points
c
      call flausm (nbran,ngrid,branch,qp(1,3),qp(1,2),0.5)
c
c     Discharges left and right of structures should be equal
c
      do 111 igr = 2, ngrid
         if (grid(igr-1) .eq. 2 ) then
            qmin = min(qp(igr-1,3),qp(igr,3))
            qp(igr-1,3) = qmin
            qp(igr,3)   = qmin
         endif
 111  continue
c
c     The discharges at nodes should satisfy the continuity equation
c
      call flaunq (nnode ,nbran ,nbrnod ,ngrid ,branch ,
     &                   brnode,qp(1,3))
c
c     Assign discharge boundary conditions
c
      do 112 istat = 1, nqstat
         igr  = qbdpar(1,istat)
         iopt = qbdpar(2,istat)
         if (iopt .ne. cbfhoq) then
            qp(igr,3) = dble( qstat(istat) )
         endif
 112  continue
c
c     Calculate Q*Q/A in every point
c
      do 113 igr = 1, ngrid
         qp(igr,3) = qp(igr,3) * qp(igr,3) / af(igr)
 113  continue
c
c     Smooth Q*Q/A at intermediate grid points,
c     structure grid points excluded
c
c     Smoothing will be performed several times, so
c     a more or less linear distribution will be obtained.
c
      do 114 i=1,5
        call flqasm (nbran,ngrid,branch,grid,qp(1,3),qp(1,2))
 114  continue
c
c     Calculate discharge in every point
c
      do 115 igr = 1, ngrid
         qp(igr,3) = dsqrt(qp(igr,3) * af(igr))
 115  continue
c
c      DO IGR=1,NGRID
c          bodem =  FLBOTT(igr,ngrid,maxlev,hlev)
c          WRITE (*,*) igr,qp(igr,3),hp(igr,3),bodem
c      ENDDO
      do 116 igr = 1, ngrid
         qp(igr,2) = qp(igr,3)
         qp(igr,1) = qp(igr,3)
116   continue

      end

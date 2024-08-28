      subroutine FLINI(lkalm  ,lfilt  ,lwqin  ,lauto  ,
     &                 lgrwt  ,     g   ,rhow   ,
     &                 nqlat  ,ncontr ,nstru  ,ngrid  ,
     &                 itim   ,juer   ,newres ,lambda ,dtf    ,dhstru ,
     &                 lagstm ,nlags  ,fd_nefis_rst       ,
     &                 fd_nefis_new   ,hp     ,qp     ,conhis ,strpar ,
     &                 strtyp ,contrl ,strclo ,x      ,qaggr  ,qlaggr ,
     &                 ncelfl ,inires ,nbran  ,branch ,nnode  ,nbrnod ,
     &                 brnode ,nodnod ,numnod ,typcr  ,prslot ,psltvr ,
     &                 bfrict ,bfricp ,grsize ,engpar ,qltpar ,qlat   ,
     &                 qlatgr ,cp     ,rp     ,maxlev ,nlev   ,hlev   ,
     &                 wft    ,aft    ,wtt    ,att    ,overlp ,arex   ,
     &                 arexcn ,arexop ,waoft  ,afwfqs ,sectc  ,sectv  ,
     &                 of     ,maxtab ,ntabm  ,ntab   ,table  ,nhstat ,
     &                 hstat  ,hbdpar ,nqstat ,qstat  ,qbdpar ,alfab  ,
     &                 pfa    ,pmua   ,pw     ,scifri ,scimu  ,sclceq ,
     &                 sclmeq ,sclqhs ,sclnod ,scceq  ,scmeq  ,scqhs  ,
     &                 scnode ,rho    ,ncsrel ,
     &                 strhis ,cnstrl ,grid   ,nexres ,exres  ,ker    ,
     &                 ibuf   ,resbuf ,strbuf ,solbuf ,itstat ,flwini ,
     &                 lhisgp ,gridnm ,strunm ,qlatnm ,delh   ,buflag ,
     &                 grhis  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLINI (FLow INItialisation)
c
c Module description: In subroutine FLINI the initial water flow at the
c                     begin time of the simulation period will be
c                     assigned. For the initial water flow there are
c                     three options:
c
c                     Option 1:    Initial water flow will be read from
c                                  the restart file created in a
c                                  previous SOBEK run;
c                     Option 2:    Initial conditions are available from
c                                  the user;
c                     Option 3:    The initial conditions will be
c                                  calculated by an auto procedure.
c
c                     In subroutine FLINI the initial water flow will be
c                     assigned for the options mentioned above. For
c                     option 1, the restart-option, the water flow will
c                     be read and assigned in subroutine FLRSTA. If a
c                     restart file has not been found the boundary
c                     conditions will be assigned on time T = 0.
c                     Because the equations have been written in delta h
c                     the water levels in a node must be equal.
c                     Therefore in a node with two or more connected
c                     branches the water level will be avaraged.
c
c                     In subroutine FLISTR the initialization concerning
c                     controlled parameters of structures, such as gate
c                     heigth, crest width etc. will take place.
c
c                     The next step is calculating the initial flow
c                     areas, flow widths etc. If the salt module is
c                     activated in the application the chezy values have
c                     to be known also for calculating initial terms of
c                     the Thatcher-Harleman sum. In that case the
c                     subroutine FLVNP1 will be called also.
c
c                     If the user did not specify initial conditions
c                     these conditions must be calculated by the auto
c                     start procedure. The auto start will be completed
c                     by the main program by calling the subroutine
c                     SONOMO [S-DO-000] with the steady state option
c                     switched on.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 57 aft               P  -
c 65 afwfqs            P  -
c 79 alfab             P  -
c 61 arex              P  -
c 62 arexcn            P  -
c 63 arexop            P  -
c 59 att               P  -
c 45 bfricp            P  -
c 44 bfrict            P  -
c 35 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 38 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
c        ,nnode)          contains the number of connected branches
c                         (index 1) for each node. The second index
c                         contains the first connected branch number
c                         etc.
c 93 cnstrl            P  -
c 24 conhis            P  -
c 27 contrl            P  -
c 51 cp                P  -
c 21 dafdrn            P  -
c 19 dafdst            P  -
c 20 defdrn            P  -
c 18 defdst            P  -
c 17 dtf               P  -
c 47 engpar            P  -
c  5 g                 P  -
c 94 grid              P  -
c 46 grsize            P  -
c 75 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c 55 hlev              P  -
c 22 hp(ngrid,3)       IO (i,1) = h1(i) (t=n)
c                         (i,2) = h(i)  (*)
c                         (i,3) = h2(i) (t=n+1)
c 74 hstat(nhstat)     I  Actual water level in every H-station.
c 96 ibuf              O  (1) Pointer in circular buffers with residues
c                         and structure data
c                         (2) Pointer in circular buffers with
c                             solutions
c                         (3) Counter for number of no convergences
c
c 33 inires            I  True when no restart info of this module has
c                         been written before.
c 13 itim              P  -
c 101itstat            O  Statistical information on iterationsteps
c                         during simulation
c 14 juer              P  -
c 95 ker               P  -
c 16 lambda            P  -
c  4 lauto             I  Switch to execute autostart procedure
c  2 lfilt             P  -
c  1 lkalm             I  -
c  3 lwqin             P  -
c 53 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 69 maxtab            I  Maximum number of defined tables.
c 34 nbran             I  Number of branches.
c 37 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 32 ncelfl            P  -
c  7 ncontr            P  -
c 91 ncsrel            P  -
c 15 newres            P  -
c 12 ngrid             I  Number of grid points in network.
c 73 nhstat            I  Number of H-boundary stations.
c 54 nlev              P  -
c 39 nodnod(nnode,     IO Administration nodal administration matrix.
c      nbrnod+1)
c  6 nqlat             P  -
c 76 nqstat            I  Number of Q-boundary stations.
c  8 nstru             I  Number of structures.
c 71 ntab              P  -
c 70 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 40 numnod(nnode)     IO Number of coefficients in nodal administration
c                         matrix.
c 68 of                P  -
c 60 overlp            P  -
c 80 pfa               P  -
c 81 pmua              P  -
c 42 prslot            P  -
c 43 psltvr            P  -
c 82 pw                P  -
c 30 qaggr             P  -
c 78 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
c                         (1,i) = Location [grid point] for Q-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : Q = f(t)
c                                 cbfqoh (2) : Q = Q(h)
c                                 cbfour (3) : Q = fourier
c                                 cbtidl (4) : Q = tidal components
c                         (3,i) = Table number for f(t), Q(h), fourier
c                                 or tidal components table.
c 31 qlaggr            P  -
c 49 qlat              P  -
c 50 qlatgr            P  -
c 48 qltpar            P  -
c 23 qp(ngrid,3)       O  (i,1) = q1(i) (t=n)
c                         (i,2) = q(i)  (*)
c                         (i,3) = q2(i) (t=n+1)
c 77 qstat(nqstat)     I  Actual discharge in every Q-station.
c 97 resbuf(dmbuf1,6)  O  Buffer with latest residues
c 90 rho(ngrid)        O  Density of diluted water per grid point.
c 52 rp                P  -
c 85 scifri            P  -
c 86 scimu             P  -
c 88 sclnod            P  -
c 89 scnode            P  -
c 66 sectc             P  -
c 67 sectv             P  -
c 100solbuf
c    (dmbuf2,7,ngrid)  O  Buffer with latest solutions
c 98 strbuf
c    (dmbuf1,2,nstru)  O  Buffer with latest structure data
c 28 strclo            P  -
c 92 strhis            P  -
c 25 strpar            P  -
c 26 strtyp            P  -
c 72 table             P  -
c 41 typcr             P  -
c 64 waoft             P  -
c 56 wft               P  -
c 58 wtt               P  -
c 29 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flauto  FLow AUTOstart procedure
c flboun  FLow BOUNdary conditions
c flchkh  FLow CHeck H (Water levels > bottom)
c flinia  FLow INItialisation Areas and widths
c flistr  FLow Initialize STRucture related data
c flkain
c flqsec  FLow Q in SECtions
c flrsta  FLow read or write of ReSTArt information
c flvnp1  FLow Variables on time level N+1
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flini.pf,v $
c Revision 1.29  1999/06/01  13:42:18  kuipe_j
c names in messages substituted + message template
c
c Revision 1.28  1999/03/15  14:23:28  kuipe_j
c Improve writing Froude file + Bug fix lat Q on t=0
c
c Revision 1.27  1998/12/11  13:06:58  kuipe_j
c improve annotation in dumps
c
c Revision 1.26  1998/06/08  12:29:40  kuipe_j
c time lag hydr controller
c
c Revision 1.25  1998/02/25  12:48:51  kuipe_j
c Check on grain size added
c
c Revision 1.24  1997/11/04  14:07:17  kuipe_j
c dhstru in lat structures
c
c Revision 1.23  1997/10/03  06:39:35  kuipe_j
c criterium for flow drection changed
c
c Revision 1.22  1997/07/11  10:38:08  kuipe_j
c Structure length set to zero
c
c Revision 1.21  1997/06/17  11:26:35  kuipe_j
c output in history format
c
c Revision 1.20  1997/06/04  11:18:11  kuipe_j
c Initialize arrays
c
c Revision 1.19  1997/05/26  07:44:44  kuipe_j
c Small changes
c
c Revision 1.18  1997/05/20  09:39:25  kuipe_j
c hstat/qstat set in case of restart
c
c Revision 1.17  1997/02/17  10:20:50  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.16  1997/01/23  08:29:06  kuipe_j
c Make flow module robust
c
c Revision 1.15  1996/09/03  14:52:02  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.14  1996/04/12  13:03:51  kuipe_j
c headers, minor changes
c
c Revision 1.13  1996/04/11  08:23:21  kuipe_j
c Kalman module added
c
c Revision 1.12  1996/01/17  14:38:31  kuipe_j
c header update
c
c Revision 1.11  1996/01/16  15:01:18  kuipe_j
c Restart improvements
c
c Revision 1.10  1995/11/21  11:07:53  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.9  1995/10/18  08:59:21  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/09/29  10:36:15  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.7  1995/09/22  10:01:45  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:55  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:40  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:20  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:10  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:09  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:52  hoeks_a
c Initial check-in
c
c Revision 1.5  1994/12/02  13:21:34  kuipe_j
c Look at Q-H relations when making initial conditions.
c
c Revision 1.4  1994/11/28  08:37:34  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.3  1993/12/13  16:01:39  kuipe_j
c Initialization of PID controller changed (statements disabled)
c
c Revision 1.2  1993/11/26  15:31:08  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      use flow_in_datools
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
      include '../include/errcod.i'
c
c     Declaration of parameters
c
      integer  nqlat  ,ncontr ,ngrid  ,nstru  ,juer   ,
     &         ker    ,maxlev ,nbran  ,maxtab ,ntabm  ,
     &         nhstat ,nqstat ,nnode  ,nbrnod ,ncsrel ,
     &         nexres ,ibuf  (*),
     &         lagstm ,nlags
      logical  lkalm  ,lfilt  ,lwqin  ,lauto  ,inires ,newres ,
     &         strclo(*)      ,lhisgp , lgrwt
      integer  branch(4,nbran) ,typcr(nbran)    ,
     &         nlev(ngrid)     ,ntab(4,maxtab)  ,
     &         arexcn(ngrid,2) ,arexop(2)       ,
     &         grid  (ngrid)   ,
     &         hbdpar(3,*)     ,qbdpar(3,*)     ,
     &         bfrict(3,nbran) ,brnode(nbrnod+1,nnode),
     &         numnod(nnode)   ,nodnod(nnode,nbrnod+1),
     &         fd_nefis_rst, fd_nefis_new ,
     &         ncelfl(*)       ,itim(2)         ,
     &         strtyp(10,*)    ,cnstrl(2,*)     ,
     &         scceq(*)   ,scmeq(*)   ,scqhs(*)  ,scnode(*),
     &         scifri(*) , scimu (*)   ,
     &         sclnod(*) ,sclceq(*),sclmeq(*) ,sclqhs(*),
     &         itstat(4)
      integer  flwini(*)
      real     g     ,lambda   ,overlp ,dhstru ,rhow
      real     prslot(3,nbran)  ,
     &         wft (ngrid,maxlev) ,aft(ngrid,maxlev),
     &         wtt (ngrid,maxlev) ,att(ngrid,maxlev),
     &         of  (ngrid,maxlev) ,x(ngrid)         ,
     &         waoft(ngrid,*)     ,conhis( 5,*)     ,
     &         strhis(dmstrh,*)   ,exres(3,*)       ,
     &         strpar(dmstrpar,*) ,contrl(17,*)     ,
     &         qaggr(ngrid,3)     ,qlaggr(*)        ,
     &         table(ntabm)       ,hstat(nhstat)    ,
     &         qstat(nqstat)      ,bfricp(6,ngrid)  ,
     &         sectc(ngrid,3)     ,sectv(ngrid,dmsecv),
     &         grsize(4,ngrid,*)  ,engpar(9)        ,
     &         psltvr(7,ngrid)    ,afwfqs(ngrid,8)  ,
     &         cp(ngrid,4)        ,rp(ngrid,4)      ,
     &         qltpar(9,*)        ,qlat(*)          ,
     &         qlatgr(ngrid)      ,alfab(ngrid)     ,
     &         arex(ngrid,4)      ,
     &         rho(ngrid)         ,
     &         pfa(*)           ,pmua(*)       ,pw(1),
     &         grhis(0:dmgrnd,ngrid,*) 
      real     buflag(lagstm,nlags)
      real     resbuf(dmbuf1,6)   ,strbuf(dmbuf1,2,*)     ,
     &         solbuf(dmbuf2,7,ngrid)
      character(len=40) gridnm(*) , strunm(*), qlatnm(*)

      double precision dtf, hlev(ngrid,maxlev)
      double precision hp(ngrid,3), qp(ngrid,3)
      double precision delh(nnode)
c
c     Declaration of local variables
c
      integer igr, iopt, istat, inode, nconn, ibr,
     &        bno, i1  , i2   , n1   , i    , j
      integer ii , k   , l    , m    , max  ,istru,
     &        igrid    , ifil , iqlat, nsec ,
     &        nmess    ,d90,  lbrnam, igrpnt, ipnt
      parameter (d90=3)
      real    havg,  th2 ,omboun ,xc, cdum
      double precision   t0
      character(len=40)  branam
      character(len=11)  xtxt
c
c     Include sobek constants
c
      include '../include/sobcon.i'

c
c     Set initial his-file flags to zero
c
      do 999 ifil=1,4
         flwini(ifil) = 0
 999  continue
c
c     Initialize right hand side vector for BICGST routine
c
      do 1000 i = 1 , nnode
         delh(i) = 0.D0
 1000 continue
c
c     Set residue and structure buffers to zero
c
      do 1010 i = 1 , 20
         resbuf(i,1) = 0.0
c        A minus sign for the iteration step means
c        that that particular buffer is empty
         resbuf(i,2) = -1.0
         resbuf(i,3) = 0.0
         resbuf(i,4) = 0.0
         resbuf(i,5) = 0.0
         resbuf(i,6) = 0.0
         do 1020 istru = 1 , nstru
            strbuf(i,1,istru) = 0.0
            strbuf(i,2,istru) = 0.0
 1020    continue
 1010 continue
      ibuf(1) = 1
      ibuf(2) = 1
      ibuf(3) = ngrid
      ibuf(4) = 0
      ibuf(5) = 1
c
c     Set solution buffers to zero
c
      do 1030 i = 1 , 4
         do 1040 igrid = 1 , ngrid
            solbuf(i,1,igrid) = 0.0
            solbuf(i,2,igrid) = 0.0
            solbuf(i,7,igrid) = 0.0
 1040    continue
 1030 continue
      solbuf(1,7,ngrid) = ngrid
c
c     statistical information on iteration steps set to initial
c     value
c
      itstat(1) = 10000
      itstat(2) = 0
      itstat(3) = 0
      itstat(4) = 0

c
      do 10 i=1,ngrid
         rho(i)=rhow
   10 continue
c
c     Set flag to indicate that no initialization took
c     place
c
      do 11 i=1,nqlat
         qlat(i)=1.111111e+20
   11 continue
c
c     Initialize rougness check
      call flroulim (-1 ,cdum ,juer,ker)
c
c  Make coordinates at structure cell eqaul
c
      do 15 ibr=1,nbran
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 12 i=i2-1,i1,-1
            if (grid(i).eq.cstrcl) then
               x(i) = x(i+1)
            else
               goto 13
            endif
  12     continue
  13     continue
         do 14 i=i1,i2-2
            if (grid(i).eq.cstrcl) then
               x(i+1) = x(i)
            endif
  14     continue
  15  continue
c
c     Convert coordinates into gridcells - FLERSI -
c
      call FLERSI (branch ,x      ,nexres ,exres  ,juer   ,ker  )
c
c     Check and update administration of lateral stations
c
      call CHLATA (ngrid  ,nqlat ,nbran, qltpar ,grid  ,branch)
c
         do 30 i = 1,nnode
            numnod(i) = 0
            do 20 j = 1,nbrnod+1
               nodnod(i,j) = 0
   20       continue
   30   continue

        do 100 i = 1,nnode
           numnod(i) = brnode(1,i)+1
           do 40 j = 2,numnod(i)
              ii = brnode(j,i)
              if (branch(1,ii) .ne. i) then
                  nodnod(i,j) = branch(1,ii)
                  nodnod(i,1) = branch(2,ii)
              else
                  nodnod(i,j) = branch(2,ii)
                  nodnod(i,1) = branch(1,ii)
              endif
   40      continue
c dubbel voorkomende knooppuntnrs. eruit
   50      continue
           max = numnod(i)
           do 80 k = 1,max
              do 70 l = k+1,max
                 if (nodnod(i,l) .eq. nodnod(i,k)) then
                    do 60 m = l,max-1
                       nodnod(i,m) = nodnod(i,m+1)
   60               continue
                    nodnod(i,numnod(i)) = 0
                    numnod(i) = numnod(i)-1
                 goto 50
c                =======
                 endif
   70         continue
   80      continue
c hoofdiagonaal element als laatste in nodnod matrix plaatsen
           k = nodnod(i,1)
           do 90 j=1,numnod(i)-1
              nodnod(i,j) = nodnod(i,j+1)
   90      continue
           nodnod(i,numnod(i)) = k

CJK   write(11,*) ' FLINI:  i = ',i,' numnod(i) = ',numnod(i)
CJK   write(11,*) ' nodnod(i,j) = ',(nodnod(i,j),j=1,numnod(i))
  100    continue
      do 95 i=1,nstru
         do 95 j=1,dmstrh
            strhis(j,i) = 0.
  95  continue
c
c     Add administration for compound structures 
c     ARS 7170 (18-4-2001)
c   
      do i=1,nstru
         if (nint(strhis(9,i)).eq.0) then
            strhis(9,i)=i       
            do j=i+1,nstru      
               if (strtyp(3,i).eq.strtyp(3,j)) then
                  strhis(9,j)=i
               endif
            enddo   
         endif
      enddo
      do i=1,nstru
         if (strtyp(1,i).eq.csweir) strhis(9,i) = -strhis(9,i)
      enddo   
c
      call FLRSTA(lwqin  ,nqlat  ,nstru  ,ncontr ,ngrid  ,itim   ,
     &            0.0    ,juer   ,.true. ,newres ,fd_nefis_rst,
     &            fd_nefis_new ,hp(1,3),qp(1,3),contrl ,conhis ,
     &            strhis ,qaggr  ,qlaggr ,ncelfl(3)      ,inires ,
     &            arexop ,arexcn ,lagstm ,nlags  ,
     &            lgrwt  ,grhis  ,buflag ,ker    )
c
c     If no restart file found replace initial conditions with
c     boundary conditions (condition = f(t))
c
      if (inires) then
         t0   = 0D0
         if ( da_running_in_da_tools() ) then
            t0 = da_get_seconds_from_org_start()
         endif
         omboun = 0.0
         call FLBOUN ( t0     ,
     &                 maxtab ,ntabm ,ntab   ,table  ,
     &                 nhstat ,hstat ,hbdpar ,
     &                 nqstat ,qstat ,qbdpar ,omboun , 0)
c
c set initial value for retention areas
c
         do 105 iqlat = 1, nqlat
            if (INT(qltpar(2,iqlat)) .eq. cqlret) then
               istru = MOD(INT(qltpar(9,iqlat)), 1000)
               strhis(13,istru) = qltpar(8,iqlat)
            endif
 105     continue

c
c        If auto start call FLAUTO to determine initial h and Q
c
         if (lauto) then
            th2 = 1.
            call FLAUTO ( g     ,t0    ,ngrid ,x     ,hp    ,qp    ,
     +                    strtyp,strpar,maxtab,ntabm ,ntab  ,table ,
     +                    maxlev,hlev  ,nlev  ,nqlat ,qltpar,juer  ,
     +                    qlat  ,qlatgr,strclo,strhis,nbran ,branch,
     +                    nhstat,hbdpar,hstat ,nqstat,qbdpar,qstat ,
     +                    th2   ,dtf   ,lambda,dhstru,ker   ,
     +                    nnode ,nbrnod,brnode,typcr ,prslot,
     +                    wft   ,aft   ,wtt   ,att   ,
     +                    arex  ,arexcn,arexop,grid  ,
c    +                   <wf>       <af>       <wt>       <at>     ,
     +                    waoft(1,1),waoft(1,3),waoft(1,2),waoft(1,4),
c    +                          <o>
     +                    of    ,waoft(1,6)    ,psltvr )
         endif
c
c        Assign boundary conditions
c
         do 110 istat = 1, nhstat
            igr  = hbdpar(1,istat)
            iopt = hbdpar(2,istat)
            if (iopt .ne. cbfhoq) then
               hp(igr,3) = hstat(istat)
            endif
 110     continue
c
         do 120 istat = 1, nqstat
            igr  = qbdpar(1,istat)
            iopt = qbdpar(2,istat)
            if (iopt .ne. cbfhoq) then
               qp(igr,3) = qstat(istat)
            endif
 120     continue
c
c        In a node the water levels must be equal (equations are written
c        in dh)
c
         do 150 inode = 1, nnode
            nconn = brnode(1,inode)
            if (nconn .gt. 1) then
               havg = 0.
               do 130 bno = 1, nconn
                  ibr = brnode(1+bno,inode)
                  n1  = branch(1,ibr)
                  i1  = branch(3,ibr)
                  i2  = branch(4,ibr)
                  if (n1 .eq. inode) then
                     havg = havg + hp(i1,3)
                  else
                     havg = havg + hp(i2,3)
                  endif
 130           continue
c
c              Determine avaraged water level
c
               havg = havg / real(nconn)
c
c              Assign values
c
               do 140 bno = 1, nconn
                  ibr = brnode(1+bno,inode)
                  n1  = branch(1,ibr)
                  i1  = branch(3,ibr)
                  i2  = branch(4,ibr)
                  if (n1 .eq. inode) then
                     hp(i1,3) = havg
                  else
                     hp(i2,3) = havg
                  endif
 140           continue
            endif
 150     continue
c
c        Initialize the array for the groundwater history (waterlevels on 
c        every gridpoint
c
         if (lgrwt) then
            do 152 ipnt=0,dmgrnd
             do 153 igrpnt=1,ngrid
                grhis(ipnt,igrpnt,1)=0.
                grhis(ipnt,igrpnt,2)=0.
                grhis(ipnt,igrpnt,3)=0.
 153           continue
 152        continue
         endif
c
c    
      else
c
c        Restart file found
c
         do 155 iqlat = 1, nqlat
            if (INT(qltpar(2,iqlat)) .eq. cqlret) then
               istru = MOD(INT(qltpar(9,iqlat)), 1000)
               qltpar(8,iqlat) = strhis(13,iqlat)
            endif
 155     continue
         do 160 istat = 1, nhstat
            igr  = hbdpar(1,istat)
            iopt = hbdpar(2,istat)
            if (iopt .ne. cbfhoq) then
               hstat(istat) = hp(igr,3)
            endif
 160     continue
c
         do 170 istat = 1, nqstat
            igr  = qbdpar(1,istat)
            iopt = qbdpar(2,istat)
            if (iopt .ne. cbfhoq) then
               qstat(istat) = qp(igr,3)
            endif
 170     continue
         lauto = .false.
      endif
c
c     Check if water level is above bottom
c
      call FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev,
     +             hp(1,3),juer   ,ker    ,prslot ,psltvr )
c
c     Initialise controllers
c
      if ( ker.ne.fatal) 
     &call FLISTR(inires, nstru , ncsrel, ncontr, strtyp, strpar,
     &            strhis, cnstrl, conhis, contrl, juer  , ker   )
c
c     Calculate Af, Wf and At
c
      if ( ker.ne.fatal) then
      call FLINIA(inires ,nbran  ,ngrid  ,branch ,typcr  ,
     &            hp(1,3),maxlev ,nlev   ,hlev   ,prslot ,
     &            overlp ,arex   ,arexcn ,arexop ,
     &            wft    ,aft    ,wtt    ,att    ,of     ,
c                  <Wf>            <Af>
     &            waoft(1,1)     ,waoft(1,3)     ,
c                  <Wt>            <At>            <O>
     &            waoft(1,2),     waoft(1,4)     ,waoft(1,6),
     &            juer      ,     ker            ,psltvr )
      endif
c
c     Check if grainsize is specified in case of roughness predictor
c
      nmess = 0
      do 185 ibr=1,nbran
         if (bfrict(1,ibr) .eq. cfreng) then
            i1 = branch(3,ibr)
            i2 = branch(4,ibr)
            if (typcr(ibr) .eq. ccrsed) then
               nsec = 2
            else
               nsec = 1
            endif
            do 180 i = i1,i2
               if (nmess .lt. 11) then
                  do 175 j = 1,nsec
                     if (grsize(d90,i,j) .lt. 1.e-6) then
                        nmess = nmess + 1
                        ker   = fatal
                        call getloc (i,ibr,xc)
                        write (xtxt,'(f10.2)') xc
                        call getbrn (ibr,branam,lbrnam)
                        call sre_error (juer ,
     +                 'FLINI grain size d90 too small'//
     +                 ' at branch @'//branam(:lbrnam)//
     +                 '@ X= @'//xtxt//'@',
     +                  eflgrn , ker )
                     endif
  175             continue
               endif
  180       continue
         endif
  185 continue
c
c     In case of autostart no water values are known yet. They
c     will be calculated in soinit by calling SONOMO including
c     routine FLVNP1.
c
      if (.not. lauto.and. ker.ne.fatal) then
c
c        For the salt module calculate Chezy coefficients etc.
c
         call FLVNP1 (nbran      ,ngrid      ,branch      ,typcr      ,
     +                bfrict     ,bfricp     ,hp(1,3)     ,qp(1,3)    ,
     +                maxlev     ,hlev       ,wft         ,maxtab     ,
c                                                          <Subsec>
     +                ntabm      ,ntab       ,table       ,sectc(1,1) ,
c                     <secth0>    <secth1>    <Wf>         <Wfh0>
     +                sectv(1,2) ,sectv(1,3) ,waoft(1,1)  ,sectc(1,2) ,
c                     <Wfh1>                               <Af>
     +                sectc(1,3) ,grsize     ,engpar      ,waoft(1,3) ,
c                     <O>         <Afh0>      <Afh1>       <Oh0>
     +                waoft(1,6) ,sectv(1,4) ,sectv(1,5)  ,sectv(1,6) ,
c                     <Oh1>       <Asubsc>
     +                sectv(1,7) ,sectv(1,1) ,prslot      ,psltvr     ,
c                     <C channel> <R channel> <C section>  <R section>
     +                cp(1,1)    ,rp(1,1)    ,cp(1,2)     ,rp(1,2)    ,
c                     <Af sec   > <Wf sec >
     +                afwfqs(1,1),afwfqs(1,3),alfab       )
c
c        Calculate Q distribution for water quality interface
c
c                                 <Q>          <Af>        <Asubsc>
         call FLQSEC(ngrid       ,qp(1,3)     ,waoft(1,3) ,sectv(1,1) ,
c                    <C channel>  <R channel>  <C section> <R section>
     +               cp(1,1)     ,rp(1,1)     ,cp(1,2)    ,rp(1,2)    ,
c                    <Af sec   >  <Q distr section on time n !>
     +               afwfqs(1,1) ,afwfqs(1,5) )
      endif
c
      if ( .not. lkalm ) then
         call FLKAIN( pfa    ,pmua   ,pw     ,scceq ,scmeq  ,scqhs ,
     +                scifri ,scimu  ,sclceq ,sclmeq,sclqhs ,sclnod,
     +                scnode ,lfilt  )
      endif
c
c     Generate identifiers for gridpoints, structures and lateral
c     discharge stations, if not present on NEFIS file from SOBEK
c     User Interface.
c
      if (.not.lhisgp) then
         do 210 ibr=1,nbran
            do 200 igr=branch(3,ibr),branch(4,ibr)
               write(gridnm(igr),'(a7,i2,a1,f10.2)')
     +                     'Branch ',ibr,'_',x(igr)
  200       continue
  210    continue
c
         do 220 istru=1,nstru
            write(strunm(istru),'(a10,i2)') 'Structure ',istru
  220    continue
c
         do 230 iqlat=1,nqlat
            write(qlatnm(iqlat),'(a10,i2)') 'Q-lateral ',iqlat
  230    continue
      endif
      return
      end

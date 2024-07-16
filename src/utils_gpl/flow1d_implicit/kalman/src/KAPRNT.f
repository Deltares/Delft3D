      subroutine KAPRNT(g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,
     +                  ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,
     +                  x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +                  lsalt  ,rho    ,rhow   ,nstru  ,nosdim ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  ngridm ,prslot ,strclo ,nnode  ,node   ,
     +                  nbrnod ,brnode ,hbdpar ,qbdpar ,juer   ,eta    ,
     +                  dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,
     +                  detadh ,af2    ,wf2    ,pw     ,pfa    ,
     +                  pmua   ,abcd1  ,kabcd1 ,kabcd2 ,
     +                  indx   ,scnode ,snnode ,nnn    ,sclnod ,wfrict ,
     +                  rfv1   ,rfv2   ,kbeta  ,mat    ,nnc    ,nnm    ,
     +                  nns    ,nnf    ,nnmu   ,scifri ,scimu  ,
     +                  snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,
     +                  sclceq ,sclmeq ,sclqhs ,sclfri ,sclmu  ,
     +                  scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,
     +                  rhs    ,np     ,p1     ,p2     ,pcol   ,ker    ,
     +                  psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAPRNT (KAlman PRedict Next Time step)
c
c Module description: The predicted covariances P(n+1|n) are calculated
c                     using the covariance P(n|n) and the system noice.
c                     The resulting covariances will be obtained by 3
c                     successive steps.
c                     In each step a system of equations will be made and
c                     solved. The solutions of these steps are Pp, Pq and
c                     P(n+1|n) respectively. To perform step 3 the
c                     solutions of the 2 previous steps are used.
c
c                     In a particular step a system of ngrid*2 equations
c                     with ngrid*2 unknowns is solved for N different'
c                     right hand sides, resulting in N solution vectors.
c                     N=ngrid*2+Nnf+Nnmu+1
c
c                     A complete system of equations consists of a large
c                     matrix of coefficients with N right hand sides. The
c                     large matrix will be transformed to a smaller matrix
c                     (the nodal administration matrix) by eliminating the
c                     unknowns at grid points inside the branches. The
c                     same elimination process is performed on all right
c                     hand sides. This smaller system of equations will
c                     be solved for each reduced right hand side. To speed
c                     up the computation a LU decomposition is performed
c                     once on the nodal administration matrix. The finally
c                     resulting large solution matrix is obtained by back
c                     substitution. The same LU decomposition will be
c                     used in all 3 steps.
c
c                     1.   Calculate the coefficients ABCD etc. in all
c                          grid points. Store some coefficients to be
c                          able to calculate corresponding right hand
c                          sides. Eliminate coefficients at inner grid
c                          points. (KADSCO)
c
c                     2.   Calculate nodal administration matrix. Store
c                          coefficients to be able to calculate corres-
c                          ponding right hand side. (KASBCO).
c
c                     3.   Make LU decomposition and check for singu-
c                          larity. (LUDCMP)
c
c                     4.   Calculate right hande side matrix for cova-
c                          riances B(.)Pp. As matrix P(n|n) is over-
c                          written by B(.)Pp, the 'lower rows' are
c                          correct already, so there is no need to copy
c                          them.  (KARHCV). Calculate Solution Pp in
c                            A(.) * Pp = B(.) * P(n|n)
c                          by calling routine KASOEQ.
c
c                     5.   Calculate right hande side matrix for system
c                          noise GQG^t. Also the 'lower rows' have to be
c                          calculated. But the the first ngrid*2 of the
c                          lower rows contain only zero's.  (KARHSN).
c                          Calculate Solution Pq in
c                            A(.) * Pq = G * Q * G^t
c                          by calling routine KASOEQ.
c
c                     6.   Calculate right hand side matrix for covari-
c                          ances B(.)Pp^t+Pq^t using the next steps:
c                          a)   Transpose Pp in the same matrix. (MATRAN)
c                          b)   Calculate B(.)Pp^t (KARHCV)
c                          c)   Add Pq^t  (KARHP1)
c                          As matrix P(n+1|n) is overwritten by
c                          B(.)Pp^t+Pq^t, the 'lower rows' are correct
c                          already, so there is no need to copy them.
c                          Calculate Solution P(n+1|n) in
c                            A(.) * P(n+1|n) = B(.) * Pp^t + Pq^t
c                          by calling routine KASOEQ.
c
c                     7.   Make solution symmetric (KACVSM)
c
c
c                     Data structure:
c                     --------------
c                     There are 2 N X N matrices physically available
c                     (calling level of this routine):
c                     1)   P(n|n), after filter step P(n+1|n+1)
c                          Array P1
c                     2)   P(n+1|n)
c                          Array P2
c                     These arrays are also used for intermediate results.
c                     The succesive contents are show in next scheme:
c                     1)   P1:
c                     P(n|n) --> B(.)*Pp --> Pp --> Pp^t --> B(.)*Pp^t
c                     2)   P2:
c                     GQGt --> Pq --> B(.)*Pp^t+Pq^t --> P(n+1|n)
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 55 abcd1             P  -
c 50 af2               P  -
c 17 alfab             P  -
c  6 branch            P  -
c 38 brnode            P  -
c 15 cp                P  -
c 44 dalfdh            P  -
c 45 dcdh              P  -
c 47 dcdq              P  -
c 49 detadh            P  -
c  9 dhstru            P  -
c 43 dqltdh            P  -
c 46 drdh              P  -
c  2 dt1               P  -
c 48 dwfdh             P  -
c 42 eta               P  -
c  1 g                 P  -
c 12 grid              P  -
c 39 hbdpar            P  -
c 10 hp                P  -
c 58 indx              P  -
c 41 juer              P  -
c 56 kabcd1            P  -
c 57 kabcd2            P  -
c 66 kbeta             P  -
c 98 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 lambda            P  -
c 19 lsalt             P  -
c 67 mat               P  -
c 27 maxtab            I  Maximum number of defined tables.
c  5 nbran             I  Number of branches.
c 37 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  7 ngrid             I  Number of grid points in network.
c 31 ngridm            I  Maximum number of gridpoints in a branch.
c 68 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c 71 nnf               I  Number of uncertain bed friction parameters.
c 69 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c 72 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 61 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c 35 nnode             I  Number of nodes.
c 70 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 36 node              P  -
c 94 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 22 nstru             I  Number of structures.
c 29 ntab              P  -
c 28 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 95 p1                P  -
c 96 p2(np,np)         O  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c 97 pcol              P  -
c 53 pfa               P  -
c 54 pmua              P  -
c 32 prslot            P  -
c 99 psltvr            P  -
c  3 psi               P  -
c 52 pw                P  -
c 40 qbdpar            P  -
c 11 qp                P  -
c 64 rfv1              P  -
c 65 rfv2              P  -
c 20 rho               P  -
c 21 rhow              P  -
c 93 rhs               P  -
c 16 rp                P  -
c 88 scceq             P  -
c 91 scfric            P  -
c 75 scifri            P  -
c 76 scimu             P  -
c 83 sclceq            P  -
c 86 sclfri            P  -
c 84 sclmeq            P  -
c 87 sclmu             P  -
c 62 sclnod            P  -
c 85 sclqhs            P  -
c 89 scmeq             P  -
c 92 scmu              P  -
c 59 scnode            P  -
c 90 scqhs             P  -
c 77 snceq             P  -
c 80 snfric            P  -
c 78 snmeq             P  -
c 81 snmu              P  -
c 60 snnode            P  -
c 79 snqhs             P  -
c 82 snwind            P  -
c 34 strclo            P  -
c 26 strpar            P  -
c 25 strtyp            P  -
c 30 table             P  -
c 18 tauwi             P  -
c  4 theta             P  -
c 14 waoft             P  -
c 51 wf2               P  -
c 63 wfrict            P  -
c 13 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c kacvsm  KALman make CoVariance SyMmetric
c kadsco  Kalman Double Sweep COefficients
c karhcv  KAlman make Right Hand side CoVariance matrix
c karhp1  KAlman add to Right Hand side Pq^t
c karhsn  KAlman make Right Hand side for System Noise
c kasbco  KALman Substitute Boundary COefficients
c kasoeq  KAlman SOlve EQuations
c ludcmp  LU DeCOMposition
c matran  MAtrix TRANsposition
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaprnt.pf,v $
c Revision 1.7  1999/03/15  15:52:07  kuipe_j
c tabs removed
c
c Revision 1.6  1997/06/17  11:24:40  kuipe_j
c Initialize vars
c
c Revision 1.5  1997/01/23  08:29:43  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/12/05  10:00:05  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.3  1996/09/03  14:54:28  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:17  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:50  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer nbran, ngrid, ngridm, nstru, maxtab, ntabm
      integer nnode, nbrnod, juer, ker, nnn, nosdim
      integer branch(4,nbran), ntab(4,maxtab), grid(ngrid)
      integer strtyp(10,*)
      integer hbdpar(3,*), qbdpar(3,*)
      integer node(4,nnode), indx(nnode), brnode(nbrnod+1,nnode)
      integer wfrict(3,nbran)
      integer sclnod(nnn+1), scnode(*)
      integer nnc, nnm, nns, nnf, nnmu, np
      integer scifri(ngrid), scimu(nstru)
      integer sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +        sclfri(nnf+1), sclmu(nnmu+1)
      integer scceq(*), scmeq(*), scqhs(*),
     +        scfric(ngrid), scmu(nstru)
c
      logical lsalt, strclo(*)
      real    g, psi, theta, rhow, pw, lambda ,dhstru
      real    x(ngrid), waoft(ngrid,dmwaof)
      real    cp(ngrid,4), rp(ngrid,4), alfab(ngrid)
      real    tauwi(ngrid), eta(ngrid), rho(ngrid)
      real    strpar(dmstrpar,*), table(ntabm)
      real    prslot(3,nbran), af2(ngrid), wf2(ngrid)
      real    pfa(nnf), pmua(nnmu)
      real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),
     +        drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),
     +        detadh(ngrid)
      real    snnode(nosdim,nnn)
      real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),
     +        snfric(2,nnf), snmu(2,nnmu), snwind(2)
      real    p1(np,np), p2(np,np), pcol(np), psltvr(7,ngrid)
c
      double precision dt1, hp(ngrid,3), qp(ngrid,3) 
      double precision abcd1(ngridm,5)
      double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
      double precision kbeta(2,nbran)
c
c     Declaration of local variables:
c
      integer kerlu, calsol, i, j, scceql ,scmeql ,scqhsl, scnodl
      real    d    , stheta
      character*4    txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     store and overwrite theta
c
      stheta = theta
      theta  = 1.0
c
      call KADSCO (g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,
     +             ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,
     +             x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +             lsalt  ,rho    ,rhow   ,nstru  ,
     +             strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +             ngridm ,prslot ,strclo ,eta    ,dqltdh ,
     +             dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,detadh ,
     +             af2    ,wf2    ,pw     ,nnf    ,pfa    ,scifri ,
     +             nnmu   ,pmua   ,scimu  ,abcd1  ,kabcd1 ,kabcd2 ,
     +             rfv1   ,rfv2   ,psltvr )
c
      call KASBCO (nnode  ,node   ,nbran  ,branch ,
     +             ngrid  ,hp(1,3),qp(1,3),
     +             maxtab ,ntabm  ,ntab   ,table  ,
     +             hbdpar ,qbdpar ,kbeta  ,
     +             rfv1   ,rfv2   ,mat    ,rhs    )
c
c     Solve the matrix by using LU decomposition. The result is stored
c     in rhs (= rhsvv(1,1))
c
      call LUDCMP (mat    ,nnode  ,nnode  ,indx   ,d      ,rhs    ,
     +             kerlu  )
c
      if ( kerlu .ne. 0 ) then
c        Matrix singular
         ker = fatal
         write (txt,'(i4)') kerlu
         call ERROR (juer ,'KAPRNT Matrix singular ( node@'
     +                     //txt//'@ )' ,ekamat ,ker)
c
         goto 1000
      endif
c
      call KARHCV (ngrid          ,kabcd1(1,5)    ,kabcd1(1,6)    ,
     +             kabcd1(1,7)    ,kabcd1(1,8)    ,kabcd2(1,8)    ,
     +             kabcd2(1,9)    ,kabcd2(1,10)   ,kabcd2(1,11)   ,
     +             kabcd2(1,12)   ,kabcd2(1,14)   ,kabcd2(1,13)   ,
     +             nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,scmu   ,
     +             nstru  ,strtyp ,nbran  ,branch ,wfrict ,
     +             np     ,p1     ,pcol   )
c
      calsol = 1
      scnodl = sclnod(nnn+1)-1
      call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,
     +             branch ,ngridm ,rfv1   ,
     +             rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,
     +             brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,
     +             kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p1     ,qp(1,3))
c
c    In order to use the adaptation with respect to system noise
C    in the right hand side vector of the nodal administration matrix
c    the matrix has to be initialized at zero.

      do 200 i = 1,np
         do 100 j = 1,np
            p2(i,j) = 0.0
 100     continue
 200  continue

      scceql = sclceq(nnc+1)-1
      scmeql = sclmeq(nnm+1)-1
      scqhsl = sclqhs(nns+1)-1
      call KARHSN (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,
     +             nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,
     +             snceq  ,snmeq  ,snqhs  ,snfric ,
     +             snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,
     +             sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,
     +             strtyp ,branch ,wfrict ,x      ,
     +             kabcd2(1,5)    ,kabcd2(1,7)    ,kabcd2(1,6)    ,
     +             np     ,p2     ,qp(1,3))
c
      calsol = 2
      scnodl = sclnod(nnn+1)-1
      call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,
     +             branch ,ngridm ,rfv1   ,
     +             rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,
     +             brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,
     +             kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p2     ,qp(1,3))
c
      call MATRAN (np     ,p1     )
c
      call KARHCV (ngrid          ,kabcd1(1,5)    ,kabcd1(1,6)    ,
     +             kabcd1(1,7)    ,kabcd1(1,8)    ,kabcd2(1,8)    ,
     +             kabcd2(1,9)    ,kabcd2(1,10)   ,kabcd2(1,11)   ,
     +             kabcd2(1,12)   ,kabcd2(1,14)   ,kabcd2(1,13)   ,
     +             nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,scmu   ,
     +             nstru  ,strtyp ,nbran  ,branch ,wfrict ,
     +             np     ,p1     ,pcol   )
c
      call KARHP1 (np     ,p1     ,p2     )
c
      calsol = 3
      scnodl = sclnod(nnn+1)-1
      call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,
     +             branch ,ngridm ,rfv1   ,
     +             rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,
     +             brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,
     +             kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p2     ,qp(1,3))
c
      call KACVSM (np     ,p2     )
c
 1000 continue
c
c     restore theta
c
      theta = stheta
c
      end

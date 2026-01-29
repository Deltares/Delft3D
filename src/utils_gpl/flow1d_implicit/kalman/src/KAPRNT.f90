subroutine KAPRNT(g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,&
&ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,&
&x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
&lsalt  ,rho    ,rhow   ,nstru  ,nosdim ,&
&strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
&ngridm ,prslot ,strclo ,nnode  ,node   ,&
&nbrnod ,brnode ,hbdpar ,qbdpar ,juer   ,eta    ,&
&dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,&
&detadh ,af2    ,wf2    ,pw     ,pfa    ,&
&pmua   ,abcd1  ,kabcd1 ,kabcd2 ,&
&indx   ,scnode ,snnode ,nnn    ,sclnod ,wfrict ,&
&rfv1   ,rfv2   ,kbeta  ,mat    ,nnc    ,nnm    ,&
&nns    ,nnf    ,nnmu   ,scifri ,scimu  ,&
&snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,&
&sclceq ,sclmeq ,sclqhs ,sclfri ,sclmu  ,&
&scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,&
&rhs    ,np     ,p1     ,p2     ,pcol   ,ker    ,&
&psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAPRNT (KAlman PRedict Next Time step)
!
! Module description: The predicted covariances P(n+1|n) are calculated
!                     using the covariance P(n|n) and the system noice.
!                     The resulting covariances will be obtained by 3
!                     successive steps.
!                     In each step a system of equations will be made and
!                     solved. The solutions of these steps are Pp, Pq and
!                     P(n+1|n) respectively. To perform step 3 the
!                     solutions of the 2 previous steps are used.
!
!                     In a particular step a system of ngrid*2 equations
!                     with ngrid*2 unknowns is solved for N different'
!                     right hand sides, resulting in N solution vectors.
!                     N=ngrid*2+Nnf+Nnmu+1
!
!                     A complete system of equations consists of a large
!                     matrix of coefficients with N right hand sides. The
!                     large matrix will be transformed to a smaller matrix
!                     (the nodal administration matrix) by eliminating the
!                     unknowns at grid points inside the branches. The
!                     same elimination process is performed on all right
!                     hand sides. This smaller system of equations will
!                     be solved for each reduced right hand side. To speed
!                     up the computation a LU decomposition is performed
!                     once on the nodal administration matrix. The finally
!                     resulting large solution matrix is obtained by back
!                     substitution. The same LU decomposition will be
!                     used in all 3 steps.
!
!                     1.   Calculate the coefficients ABCD etc. in all
!                          grid points. Store some coefficients to be
!                          able to calculate corresponding right hand
!                          sides. Eliminate coefficients at inner grid
!                          points. (KADSCO)
!
!                     2.   Calculate nodal administration matrix. Store
!                          coefficients to be able to calculate corres-
!                          ponding right hand side. (KASBCO).
!
!                     3.   Make LU decomposition and check for singu-
!                          larity. (LUDCMP)
!
!                     4.   Calculate right hande side matrix for cova-
!                          riances B(.)Pp. As matrix P(n|n) is over-
!                          written by B(.)Pp, the 'lower rows' are
!                          correct already, so there is no need to copy
!                          them.  (KARHCV). Calculate Solution Pp in
!                            A(.) * Pp = B(.) * P(n|n)
!                          by calling routine KASOEQ.
!
!                     5.   Calculate right hande side matrix for system
!                          noise GQG^t. Also the 'lower rows' have to be
!                          calculated. But the the first ngrid*2 of the
!                          lower rows contain only zero's.  (KARHSN).
!                          Calculate Solution Pq in
!                            A(.) * Pq = G * Q * G^t
!                          by calling routine KASOEQ.
!
!                     6.   Calculate right hand side matrix for covari-
!                          ances B(.)Pp^t+Pq^t using the next steps:
!                          a)   Transpose Pp in the same matrix. (MATRAN)
!                          b)   Calculate B(.)Pp^t (KARHCV)
!                          c)   Add Pq^t  (KARHP1)
!                          As matrix P(n+1|n) is overwritten by
!                          B(.)Pp^t+Pq^t, the 'lower rows' are correct
!                          already, so there is no need to copy them.
!                          Calculate Solution P(n+1|n) in
!                            A(.) * P(n+1|n) = B(.) * Pp^t + Pq^t
!                          by calling routine KASOEQ.
!
!                     7.   Make solution symmetric (KACVSM)
!
!
!                     Data structure:
!                     --------------
!                     There are 2 N X N matrices physically available
!                     (calling level of this routine):
!                     1)   P(n|n), after filter step P(n+1|n+1)
!                          Array P1
!                     2)   P(n+1|n)
!                          Array P2
!                     These arrays are also used for intermediate results.
!                     The succesive contents are show in next scheme:
!                     1)   P1:
!                     P(n|n) --> B(.)*Pp --> Pp --> Pp^t --> B(.)*Pp^t
!                     2)   P2:
!                     GQGt --> Pq --> B(.)*Pp^t+Pq^t --> P(n+1|n)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 55 abcd1             P  -
! 50 af2               P  -
! 17 alfab             P  -
!  6 branch            P  -
! 38 brnode            P  -
! 15 cp                P  -
! 44 dalfdh            P  -
! 45 dcdh              P  -
! 47 dcdq              P  -
! 49 detadh            P  -
!  9 dhstru            P  -
! 43 dqltdh            P  -
! 46 drdh              P  -
!  2 dt1               P  -
! 48 dwfdh             P  -
! 42 eta               P  -
!  1 g                 P  -
! 12 grid              P  -
! 39 hbdpar            P  -
! 10 hp                P  -
! 58 indx              P  -
! 41 juer              P  -
! 56 kabcd1            P  -
! 57 kabcd2            P  -
! 66 kbeta             P  -
! 98 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 lambda            P  -
! 19 lsalt             P  -
! 67 mat               P  -
! 27 maxtab            I  Maximum number of defined tables.
!  5 nbran             I  Number of branches.
! 37 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  7 ngrid             I  Number of grid points in network.
! 31 ngridm            I  Maximum number of gridpoints in a branch.
! 68 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
! 71 nnf               I  Number of uncertain bed friction parameters.
! 69 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
! 72 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 61 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
! 35 nnode             I  Number of nodes.
! 70 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 36 node              P  -
! 94 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 22 nstru             I  Number of structures.
! 29 ntab              P  -
! 28 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 95 p1                P  -
! 96 p2(np,np)         O  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
! 97 pcol              P  -
! 53 pfa               P  -
! 54 pmua              P  -
! 32 prslot            P  -
! 99 psltvr            P  -
!  3 psi               P  -
! 52 pw                P  -
! 40 qbdpar            P  -
! 11 qp                P  -
! 64 rfv1              P  -
! 65 rfv2              P  -
! 20 rho               P  -
! 21 rhow              P  -
! 93 rhs               P  -
! 16 rp                P  -
! 88 scceq             P  -
! 91 scfric            P  -
! 75 scifri            P  -
! 76 scimu             P  -
! 83 sclceq            P  -
! 86 sclfri            P  -
! 84 sclmeq            P  -
! 87 sclmu             P  -
! 62 sclnod            P  -
! 85 sclqhs            P  -
! 89 scmeq             P  -
! 92 scmu              P  -
! 59 scnode            P  -
! 90 scqhs             P  -
! 77 snceq             P  -
! 80 snfric            P  -
! 78 snmeq             P  -
! 81 snmu              P  -
! 60 snnode            P  -
! 79 snqhs             P  -
! 82 snwind            P  -
! 34 strclo            P  -
! 26 strpar            P  -
! 25 strtyp            P  -
! 30 table             P  -
! 18 tauwi             P  -
!  4 theta             P  -
! 14 waoft             P  -
! 51 wf2               P  -
! 63 wfrict            P  -
! 13 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! kacvsm  KALman make CoVariance SyMmetric
! kadsco  Kalman Double Sweep COefficients
! karhcv  KAlman make Right Hand side CoVariance matrix
! karhp1  KAlman add to Right Hand side Pq^t
! karhsn  KAlman make Right Hand side for System Noise
! kasbco  KALman Substitute Boundary COefficients
! kasoeq  KAlman SOlve EQuations
! ludcmp  LU DeCOMposition
! matran  MAtrix TRANsposition
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaprnt.pf,v $
! Revision 1.7  1999/03/15  15:52:07  kuipe_j
! tabs removed
!
! Revision 1.6  1997/06/17  11:24:40  kuipe_j
! Initialize vars
!
! Revision 1.5  1997/01/23  08:29:43  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/12/05  10:00:05  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.3  1996/09/03  14:54:28  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:17  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:50  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
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
   integer sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &sclfri(nnf+1), sclmu(nnmu+1)
   integer scceq(*), scmeq(*), scqhs(*),&
   &scfric(ngrid), scmu(nstru)
!
   logical lsalt, strclo(*)
   real    g, psi, theta, rhow, pw, lambda ,dhstru
   real    x(ngrid), waoft(ngrid,dmwaof)
   real    cp(ngrid,4), rp(ngrid,4), alfab(ngrid)
   real    tauwi(ngrid), eta(ngrid), rho(ngrid)
   real    strpar(dmstrpar,*), table(ntabm)
   real    prslot(3,nbran), af2(ngrid), wf2(ngrid)
   real    pfa(nnf), pmua(nnmu)
   real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),&
   &drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),&
   &detadh(ngrid)
   real    snnode(nosdim,nnn)
   real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),&
   &snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real    p1(np,np), p2(np,np), pcol(np), psltvr(7,ngrid)
!
   double precision dt1, hp(ngrid,3), qp(ngrid,3)
   double precision abcd1(ngridm,5)
   double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
   double precision kbeta(2,nbran)
!
!     Declaration of local variables:
!
   integer kerlu, calsol, i, j, scceql ,scmeql ,scqhsl, scnodl
   real    d    , stheta
   character*4    txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     store and overwrite theta
!
   stheta = theta
   theta  = 1.0
!
   call KADSCO (g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,&
   &ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,&
   &x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
   &lsalt  ,rho    ,rhow   ,nstru  ,&
   &strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
   &ngridm ,prslot ,strclo ,eta    ,dqltdh ,&
   &dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,detadh ,&
   &af2    ,wf2    ,pw     ,nnf    ,pfa    ,scifri ,&
   &nnmu   ,pmua   ,scimu  ,abcd1  ,kabcd1 ,kabcd2 ,&
   &rfv1   ,rfv2   ,psltvr )
!
   call KASBCO (nnode  ,node   ,nbran  ,branch ,&
   &ngrid  ,hp(1,3),qp(1,3),&
   &maxtab ,ntabm  ,ntab   ,table  ,&
   &hbdpar ,qbdpar ,kbeta  ,&
   &rfv1   ,rfv2   ,mat    ,rhs    )
!
!     Solve the matrix by using LU decomposition. The result is stored
!     in rhs (= rhsvv(1,1))
!
   call LUDCMP (mat    ,nnode  ,nnode  ,indx   ,d      ,rhs    ,&
   &kerlu  )
!
   if ( kerlu .ne. 0 ) then
!        Matrix singular
      ker = fatal
      write (txt,'(i4)') kerlu
      call ERROR (juer ,'KAPRNT Matrix singular ( node@'&
      &//txt//'@ )' ,ekamat ,ker)
!
      goto 1000
   endif
!
   call KARHCV (ngrid          ,kabcd1(1,5)    ,kabcd1(1,6)    ,&
   &kabcd1(1,7)    ,kabcd1(1,8)    ,kabcd2(1,8)    ,&
   &kabcd2(1,9)    ,kabcd2(1,10)   ,kabcd2(1,11)   ,&
   &kabcd2(1,12)   ,kabcd2(1,14)   ,kabcd2(1,13)   ,&
   &nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,scmu   ,&
   &nstru  ,strtyp ,nbran  ,branch ,wfrict ,&
   &np     ,p1     ,pcol   )
!
   calsol = 1
   scnodl = sclnod(nnn+1)-1
   call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,&
   &branch ,ngridm ,rfv1   ,&
   &rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,&
   &brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,&
   &kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p1     ,qp(1,3))
!
!    In order to use the adaptation with respect to system noise
!    in the right hand side vector of the nodal administration matrix
!    the matrix has to be initialized at zero.

   do 200 i = 1,np
      do 100 j = 1,np
         p2(i,j) = 0.0
100   continue
200 continue

   scceql = sclceq(nnc+1)-1
   scmeql = sclmeq(nnm+1)-1
   scqhsl = sclqhs(nns+1)-1
   call KARHSN (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,&
   &nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,&
   &snceq  ,snmeq  ,snqhs  ,snfric ,&
   &snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,&
   &sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,&
   &strtyp ,branch ,wfrict ,x      ,&
   &kabcd2(1,5)    ,kabcd2(1,7)    ,kabcd2(1,6)    ,&
   &np     ,p2     ,qp(1,3))
!
   calsol = 2
   scnodl = sclnod(nnn+1)-1
   call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,&
   &branch ,ngridm ,rfv1   ,&
   &rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,&
   &brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,&
   &kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p2     ,qp(1,3))
!
   call MATRAN (np     ,p1     )
!
   call KARHCV (ngrid          ,kabcd1(1,5)    ,kabcd1(1,6)    ,&
   &kabcd1(1,7)    ,kabcd1(1,8)    ,kabcd2(1,8)    ,&
   &kabcd2(1,9)    ,kabcd2(1,10)   ,kabcd2(1,11)   ,&
   &kabcd2(1,12)   ,kabcd2(1,14)   ,kabcd2(1,13)   ,&
   &nnf    ,sclfri ,scfric ,nnmu   ,sclmu  ,scmu   ,&
   &nstru  ,strtyp ,nbran  ,branch ,wfrict ,&
   &np     ,p1     ,pcol   )
!
   call KARHP1 (np     ,p1     ,p2     )
!
   calsol = 3
   scnodl = sclnod(nnn+1)-1
   call KASOEQ (nnode  ,nnn    ,nbran  ,nosdim ,scnodl ,&
   &branch ,ngridm ,rfv1   ,&
   &rfv2   ,calsol ,scnode ,snnode ,sclnod ,nbrnod ,&
   &brnode ,mat    ,rhs    ,indx   ,ngrid  ,np     ,&
   &kbeta  ,abcd1  ,kabcd1 ,kabcd2 ,p2     ,qp(1,3))
!
   call KACVSM (np     ,p2     )
!
1000 continue
!
!     restore theta
!
   theta = stheta
!
end

      subroutine FLSOEQ (lkalm  ,ngrid  ,nstru  ,nnc    ,nnm    ,nns   ,
     +                   nnf    ,nnmu   ,nnn    ,nosdim ,
     +                   qtyp   ,qlatnm ,sclceq ,sclmeq ,sclqhs ,
     +                   scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,snceq ,
     +                   snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,strtyp,
     +                   wfrict ,scnode ,snnode ,sclnod ,nnode  ,node  ,
     +                   nbrnod ,nodnod ,numnod ,nbran  ,branch ,maxtab,
     +                   ntabm  ,ntab   ,table  ,hstat  ,hbdpar ,qstat ,
     +                   qbdpar ,urelax ,rfv1   ,rfv2   ,mat    ,rhsvv ,
     +                   hp     ,qp     ,iterbc ,resid  ,delh   ,work  ,
     +                   ker    ,steady ,nqlat  ,qlat   ,qltpar ,strhis,
     +                   relstr ,theta  ,dt1    ,indx   ,juer   ,bicg  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLSOEQ (FLow SOlve EQuations)
c
c Module description: Calculates the new flow and water level values for
c                     the current iteration step.
c
c                     The ABCDE coefficients are now known and are
c                     applied to calculate a new discharge and water
c                     level for each gridpoint. First a nodal
c                     administration matrix is created by calling rou-
c                     tine FLSBCO. This matrix is solved using a general
c                     LU decomposition routine. The solution of this
c                     routine is used to calculate new discharges and
c                     water levels in FLBRAN. The new hydraulic
c                     parameters are used to determine the correct
c                     discharges and levels in retention areas in FLSBRA
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 32 branch            P  -
c 52 delh(nnode)       I  Work array nodal administration matrix.
c                         odd (-1).
c 38 hbdpar            P  -
c 48 hp                P  -
c 37 hstat             P  -
c 50 iterbc            P  -
c 54 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 lkalm             I  -
c 45 mat(nnode,nnode)  I  (i,j) contains coefficient j of the equation
c                         at node i of the set of node equations.
c 33 maxtab            I  Maximum number of defined tables.
c 31 nbran             I  Number of branches.
c 28 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  2 ngrid             I  Number of grid points in network.
c  4 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c  7 nnf               I  Number of uncertain bed friction parameters.
c  5 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c  8 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  9 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c 26 nnode             I  Number of nodes.
c  6 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 27 node              P  -
c 29 nodnod            P  -
c  3 nstru             I  Number of structures.
c 35 ntab              P  -
c 34 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 30 numnod            P  -
c 40 qbdpar            P  -
c 49 qp                P  -
c 39 qstat             P  -
c 51 resid             P  -
c 43 rfv1              P  -
c 44 rfv2              P  -
c 46 rhsvv(ngrid,2)    IO (i,1) = rhs(i)
c                         (i,2) = vv(i)
c 10 scceq             P  -
c 13 scifri            P  -
c 11 scmeq             P  -
c 14 scimu             P  -
c 12 scqhs             P  -
c 25 sclnod            P  -
c 23 scnode            P  -
c 15 snceq             P  -
c 18 snfric            P  -
c 16 snmeq             P  -
c 19 snmu              P  -
c 24 snnode            P  -
c 17 snqhs             P  -
c 20 snwind            P  -
c 21 strtyp            P  -
c 36 table             P  -
c 42 urelax            P  -
c 22 wfrict            P  -
c 53 work              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c bicgst  Iterative template routine BICGSTAB
c dscal   Double prec. SCALing of matrix
c flbran  FLow in BRANches
c flksn1  FLow Kalman add System Noise 1
c flksn2  FLow Kalman add System Noise 2
c flsbco  FLow Substitute Boundary COnditions
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flsoeq.pf,v $
c Revision 1.19  1999/03/15  15:50:47  kuipe_j
c tabs removed
c
c Revision 1.18  1998/06/24  11:10:23  kuipe_j
c Try direct solver if BICGST fails
c
c Revision 1.17  1998/06/08  14:41:56  kuipe_j
c comment leader
c
c Revision 1.15  1997/11/04  14:17:31  kuipe_j
c Retention basin
c
c Revision 1.14  1997/05/26  07:41:33  kuipe_j
c dicretization Q(H), H(Q) boundaries improved
c
c Revision 1.13  1997/02/17  10:06:37  kuipe_j
c Q and H coefficients are interchaged
c
c Revision 1.12  1996/04/12  13:04:22  kuipe_j
c headers, minor changes
c
c Revision 1.11  1996/04/11  08:23:53  kuipe_j
c Kalman module added
c
c Revision 1.10  1996/01/17  14:38:49  kuipe_j
c header update
c
c Revision 1.9  1995/11/21  11:08:02  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.8  1995/10/18  08:59:27  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.7  1995/09/22  10:02:15  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:04  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:53  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:29  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:28  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:29  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:09  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nnode, ngrid, nstru,  maxtab, ntabm, nbran, nbrnod
      integer nnc,   nnm,   nnn,    nns,   nnf,   nnmu, nosdim
      integer juer,  ker
      integer node(4,nnode),   ntab(4,maxtab)
      integer numnod(nnode),   nodnod(nnode,  nbrnod+1)
      integer indx  (nnode)
      integer sclceq(nnc+1),   sclmeq(nnm+1), sclqhs(nns+1)
      integer scceq(*)     ,   scmeq(*)     , scqhs(*)
      integer scifri(ngrid),   scimu(nstru)
      integer wfrict(3,nbran), scnode(*), sclnod(nnn+1)
      integer branch(4,nbran), strtyp(10,nstru)
      integer hbdpar(3,*), qbdpar(3,*)
      integer iterbc
 
      real    qtyp ,theta
      real    hstat(*), qstat(*)
      real    table(ntabm) , urelax
      real    snceq(nosdim,nnc),  snmeq(nosdim,nnm), snqhs(nosdim,nns)
      real    snfric(2,nnf), snmu(2,nnmu), snwind(2)
      real    snnode(nosdim,nnn)

      double precision hp(ngrid,3), qp(ngrid,3)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhsvv(nnode,2)
      double precision resid,delh(nnode),work(nnode,*)

      logical lkalm
      logical steady, bicg

      integer nqlat
      real    qlat(nqlat,9), qltpar(9,nqlat), relstr, strhis(13,nstru)
      double precision dt1
      character*40 qlatnm(*)

c
c     Declaration of local variables:
c
      integer i ,infobc ,kerlu, scceql ,scmeql ,scqhsl, scnodl

      real    d
      double precision hulp
c
c     Declaration of external functions
c
      external MATVC1,MATVC2,PSOLVE
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      if ( lkalm ) then
         scceql = sclceq(nnc+1)-1
         scmeql = sclmeq(nnm+1)-1
         scqhsl = sclqhs(nns+1)-1
         call FLKSN1 (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,
     +                nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,
     +                sclceq ,sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,
     +                scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,
     +                snmu   ,snwind ,strtyp ,branch ,wfrict ,
     +                rfv1(1,3)      ,rfv2(1,3)      )
      endif
c
c     Substitute boundary conditions to create a nodal administration
c     matrix -mat- and the right hand side vector -rhs-
c

      call FLSBCO (nnode  ,node    ,nbran   ,branch  ,qtyp    ,
     +             ngrid  ,hp(1,1) ,hp(1,2) ,qp(1,1) ,qp(1,2) ,
     +             maxtab ,ntabm   ,ntab    ,table   ,
     +             hstat  ,hbdpar  ,qstat   ,qbdpar  ,
     +             rfv1   ,rfv2    ,mat     ,rhsvv(1,1)       )

      if ( lkalm ) then
          scnodl = sclnod(nnn+1)-1
          call FLKSN2 (nnode  ,nnn ,nosdim, scnodl, scnode ,snnode,
     +                 sclnod ,rhsvv(1,1) )
      endif

      if (bicg) then
c
c--     the explicit left preconditioning step --c
        do 20 i = 1,nnode
           hulp = mat(i,i)
           call DSCAL (nnode,1.d0/hulp,mat(i,1),nnode)
           rhsvv(i,1) = rhsvv(i,1) / hulp
   20   continue
c
        call BICGST (nnode ,rhsvv(1,1) ,delh  ,work  ,nnode ,iterbc ,
     +       resid ,MATVC1 ,MATVC2 ,PSOLVE ,mat ,infobc ,nodnod ,numnod)
        if (infobc .ne. 0) bicg = .false.

      endif
      if (.not.bicg) then
c
c        If BICGST fails
c        solve the matrix by using LU decomposition.
c
         call ludcmp (mat    ,nnode  ,nnode  ,indx   ,d ,rhsvv(1,2) ,
     +                kerlu)
         if (kerlu.ne.0) then
c           Matrix singular
            ker = fatal
            call error (juer ,'FLSOEQ Matrix singular' ,eflmat ,ker)
            goto 1000
         endif
         call lubksb (mat    ,nnode  ,nnode  ,indx   ,rhsvv(1,1)    )
      else
         do 30 i = 1,nnode
            rhsvv(i,1) = delh(i)
   30    continue
      endif
c
c     Calculate new discharges and water levels
c
     
      call FLBRAN (nbran   ,nnode   ,ngrid   ,
     +             branch  ,urelax  ,
     +             rfv1    ,rfv2    ,
     +             rhsvv(1,1)       ,
     +             hp(1,1) ,hp(1,3) ,qp(1,1) ,qp(1,3) )
c
c     Substitute back to obtain correct Q and H in retention areas
c
      call FLSBRA( steady, nqlat , ngrid  , qlat   , qlatnm, qltpar, 
     &             strhis, relstr, theta , hp(1,1), hp(1,3), dt1   , 
     &             nstru  )
c
 1000 continue
      end

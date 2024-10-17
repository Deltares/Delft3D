subroutine FLSOEQ (lkalm  ,ngrid  ,nstru  ,nnc    ,nnm    ,nns   ,&
&nnf    ,nnmu   ,nnn    ,nosdim ,&
&qtyp   ,qlatnm ,sclceq ,sclmeq ,sclqhs ,&
&scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,snceq ,&
&snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,strtyp,&
&wfrict ,scnode ,snnode ,sclnod ,nnode  ,node  ,&
&nbrnod ,nodnod ,numnod ,nbran  ,branch ,maxtab,&
&ntabm  ,ntab   ,table  ,hstat  ,hbdpar ,qstat ,&
&qbdpar ,urelax ,rfv1   ,rfv2   ,mat    ,rhsvv ,&
&hp     ,qp     ,iterbc ,resid  ,delh   ,work  ,&
&ker    ,steady ,nqlat  ,qlat   ,qltpar ,strhis,&
&relstr ,theta  ,dt1    ,indx   ,juer   ,bicg  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLSOEQ (FLow SOlve EQuations)
!
! Module description: Calculates the new flow and water level values for
!                     the current iteration step.
!
!                     The ABCDE coefficients are now known and are
!                     applied to calculate a new discharge and water
!                     level for each gridpoint. First a nodal
!                     administration matrix is created by calling rou-
!                     tine FLSBCO. This matrix is solved using a general
!                     LU decomposition routine. The solution of this
!                     routine is used to calculate new discharges and
!                     water levels in FLBRAN. The new hydraulic
!                     parameters are used to determine the correct
!                     discharges and levels in retention areas in FLSBRA
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 32 branch            P  -
! 52 delh(nnode)       I  Work array nodal administration matrix.
!                         odd (-1).
! 38 hbdpar            P  -
! 48 hp                P  -
! 37 hstat             P  -
! 50 iterbc            P  -
! 54 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 lkalm             I  -
! 45 mat(nnode,nnode)  I  (i,j) contains coefficient j of the equation
!                         at node i of the set of node equations.
! 33 maxtab            I  Maximum number of defined tables.
! 31 nbran             I  Number of branches.
! 28 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  2 ngrid             I  Number of grid points in network.
!  4 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
!  7 nnf               I  Number of uncertain bed friction parameters.
!  5 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
!  8 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  9 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
! 26 nnode             I  Number of nodes.
!  6 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 27 node              P  -
! 29 nodnod            P  -
!  3 nstru             I  Number of structures.
! 35 ntab              P  -
! 34 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 30 numnod            P  -
! 40 qbdpar            P  -
! 49 qp                P  -
! 39 qstat             P  -
! 51 resid             P  -
! 43 rfv1              P  -
! 44 rfv2              P  -
! 46 rhsvv(ngrid,2)    IO (i,1) = rhs(i)
!                         (i,2) = vv(i)
! 10 scceq             P  -
! 13 scifri            P  -
! 11 scmeq             P  -
! 14 scimu             P  -
! 12 scqhs             P  -
! 25 sclnod            P  -
! 23 scnode            P  -
! 15 snceq             P  -
! 18 snfric            P  -
! 16 snmeq             P  -
! 19 snmu              P  -
! 24 snnode            P  -
! 17 snqhs             P  -
! 20 snwind            P  -
! 21 strtyp            P  -
! 36 table             P  -
! 42 urelax            P  -
! 22 wfrict            P  -
! 53 work              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! bicgst  Iterative template routine BICGSTAB
! dscal   Double prec. SCALing of matrix
! flbran  FLow in BRANches
! flksn1  FLow Kalman add System Noise 1
! flksn2  FLow Kalman add System Noise 2
! flsbco  FLow Substitute Boundary COnditions
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flsoeq.pf,v $
! Revision 1.19  1999/03/15  15:50:47  kuipe_j
! tabs removed
!
! Revision 1.18  1998/06/24  11:10:23  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.17  1998/06/08  14:41:56  kuipe_j
! comment leader
!
! Revision 1.15  1997/11/04  14:17:31  kuipe_j
! Retention basin
!
! Revision 1.14  1997/05/26  07:41:33  kuipe_j
! dicretization Q(H), H(Q) boundaries improved
!
! Revision 1.13  1997/02/17  10:06:37  kuipe_j
! Q and H coefficients are interchaged
!
! Revision 1.12  1996/04/12  13:04:22  kuipe_j
! headers, minor changes
!
! Revision 1.11  1996/04/11  08:23:53  kuipe_j
! Kalman module added
!
! Revision 1.10  1996/01/17  14:38:49  kuipe_j
! header update
!
! Revision 1.9  1995/11/21  11:08:02  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.8  1995/10/18  08:59:27  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.7  1995/09/22  10:02:15  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:04  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:53  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:29  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:28  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:29  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:09  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:55  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
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

!
!     Declaration of local variables:
!
   integer i ,infobc ,kerlu, scceql ,scmeql ,scqhsl, scnodl

   real    d
   double precision hulp
!
!     Declaration of external functions
!
   external MATVC1,MATVC2,PSOLVE
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   if ( lkalm ) then
      scceql = sclceq(nnc+1)-1
      scmeql = sclmeq(nnm+1)-1
      scqhsl = sclqhs(nns+1)-1
      call FLKSN1 (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnf    ,&
      &nnmu   ,nbran  ,nosdim ,scceql ,scmeql ,scqhsl ,&
      &sclceq ,sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,&
      &scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,&
      &snmu   ,snwind ,strtyp ,branch ,wfrict ,&
      &rfv1(1,3)      ,rfv2(1,3)      )
   endif
!
!     Substitute boundary conditions to create a nodal administration
!     matrix -mat- and the right hand side vector -rhs-
!

   call FLSBCO (nnode  ,node    ,nbran   ,branch  ,qtyp    ,&
   &ngrid  ,hp(1,1) ,hp(1,2) ,qp(1,1) ,qp(1,2) ,&
   &maxtab ,ntabm   ,ntab    ,table   ,&
   &hstat  ,hbdpar  ,qstat   ,qbdpar  ,&
   &rfv1   ,rfv2    ,mat     ,rhsvv(1,1)       )

   if ( lkalm ) then
      scnodl = sclnod(nnn+1)-1
      call FLKSN2 (nnode  ,nnn ,nosdim, scnodl, scnode ,snnode,&
      &sclnod ,rhsvv(1,1) )
   endif

   if (bicg) then
!
!--     the explicit left preconditioning step --c
      do 20 i = 1,nnode
         hulp = mat(i,i)
         call DSCAL (nnode,1.d0/hulp,mat(i,1),nnode)
         rhsvv(i,1) = rhsvv(i,1) / hulp
20    continue
!
      call BICGST (nnode ,rhsvv(1,1) ,delh  ,work  ,nnode ,iterbc ,&
      &resid ,MATVC1 ,MATVC2 ,PSOLVE ,mat ,infobc ,nodnod ,numnod)
      if (infobc .ne. 0) bicg = .false.

   endif
   if (.not.bicg) then
!
!        If BICGST fails
!        solve the matrix by using LU decomposition.
!
      call ludcmp (mat    ,nnode  ,nnode  ,indx   ,d ,rhsvv(1,2) ,&
      &kerlu)
      if (kerlu.ne.0) then
!           Matrix singular
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
!
!     Calculate new discharges and water levels
!

   call FLBRAN (nbran   ,nnode   ,ngrid   ,&
   &branch  ,urelax  ,&
   &rfv1    ,rfv2    ,&
   &rhsvv(1,1)       ,&
   &hp(1,1) ,hp(1,3) ,qp(1,1) ,qp(1,3) )
!
!     Substitute back to obtain correct Q and H in retention areas
!
   call FLSBRA( steady, nqlat , ngrid  , qlat   , qlatnm, qltpar,&
   &strhis, relstr, theta , hp(1,1), hp(1,3), dt1   ,&
   &nstru  )
!
1000 continue
end

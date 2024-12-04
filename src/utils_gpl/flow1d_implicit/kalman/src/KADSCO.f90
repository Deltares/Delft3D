subroutine KADSCO(g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,&
&ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,&
&x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
&lsalt  ,rho    ,rhow   ,nstru  ,&
&strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
&ngridm ,prslot ,strclo ,eta    ,dqltdh ,&
&dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,detadh ,&
&af2    ,wf2    ,pw     ,nnf    ,pfa    ,scifri ,&
&nnmu   ,pmua   ,scimu  ,abcd1  ,kabcd1 ,kabcd2 ,&
&rfv1   ,rfv2   ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KADSCO (Kalman Double Sweep COefficients)
!
! Module description: Subroutine KADSCO computes the ABCDE coefficients
!                     and the double sweeped ABCDE coefficients r1,f1 and
!                     r2,f2 for all grid points in the network.
!
!                     In subroutine KAABCD the matrix coefficients
!                     (diagonals) A,B,C,D and E will be computed, for the
!                     'normal' gridpoints and the gridpoints around the
!                     structures.
!
!                     Subroutine KASWPC will perform a double sweep
!                     operation on the just computed ABCDE matrix,
!                     resulting in coefficients r1,f1 and r2,f2 for each
!                     grid point. These double sweeped coefficients are
!                     the final result of subroutine KADSCO.
!
!                     The coefficients r1,f1 and r2,f2 are calculated
!                     once and will be used for every column in the
!                     covariance matrix. The coefficients v1 and v2 will
!                     be calculeted for every column in the covariance
!                     matrix in a double sweep process in routine KASWPR.
!
!
!                     The right hand side of the nodal administration
!                     system must be filled for a paticular column of the
!                     P-matrix.  Use arrays v1,v2 and KBETA.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 52 abcd1             P  -
! 43 af2               P  -
! 17 alfab             P  -
!  6 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 15 cp                P  -
! 37 dalfdh            P  -
! 38 dcdh              P  -
! 40 dcdq              P  -
! 42 detadh            P  -
!  9 dhstru            P  -
! 36 dqltdh            P  -
! 39 drdh              P  -
!  2 dt1               P  -
! 41 dwfdh             P  -
! 35 eta               P  -
!  1 g                 P  -
! 12 grid              P  -
! 10 hp                P  -
! 53 kabcd1            P  -
! 54 kabcd2            P  -
!  8 lambda            P  -
! 19 lsalt             P  -
! 27 maxtab            I  Maximum number of defined tables.
!  5 nbran             I  Number of branches.
!  7 ngrid             I  Number of grid points in network.
! 31 ngridm            I  Maximum number of gridpoints in a branch.
! 46 nnf               I  Number of uncertain bed friction parameters.
! 49 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 22 nstru             I  Number of structures.
! 29 ntab              P  -
! 28 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 47 pfa               P  -
! 50 pmua              P  -
! 32 prslot            P  -
! 57 psltvr            P  -
!  3 psi               P  -
! 45 pw                P  -
! 11 qp                P  -
! 55 rfv1              P  -
! 56 rfv2              P  -
! 20 rho               P  -
! 21 rhow              P  -
! 16 rp                P  -
! 48 scifri            P  -
! 51 scimu             P  -
! 34 strclo            P  -
! 26 strpar            P  -
! 25 strtyp            P  -
! 30 table             P  -
! 18 tauwi             P  -
!  4 theta             P  -
! 14 waoft             P  -
! 44 wf2               P  -
! 13 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kaabcd  KAlman ABCDe coefficients
! kaswpc  KAlman double SWeeP coefficients
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kadsco.pf,v $
! Revision 1.5  1999/03/15  15:51:41  kuipe_j
! tabs removed
!
! Revision 1.4  1997/06/17  11:23:44  kuipe_j
! Initialize vars
!
! Revision 1.3  1997/01/23  08:29:36  kuipe_j
! Make flow module robust
!
! Revision 1.2  1996/04/12  13:04:47  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:23  kuipe_j
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
   integer nnf, nnmu
   integer branch(4,nbran), ntab(4,maxtab), grid(ngrid)
   integer strtyp(10,*), scifri(ngrid), scimu(nstru)
   logical lsalt, strclo(*)
   real    lambda ,dhstru
   real    g, psi, theta, rhow, pw
   real    x(ngrid), waoft(ngrid,dmwaof)
   real    cp(ngrid,4), rp(ngrid,4), alfab(ngrid)
   real    tauwi(ngrid), eta(ngrid), rho(ngrid)
   real    strpar(dmstrpar,*), table(ntabm)
   real    prslot(3,nbran), af2(ngrid), wf2(ngrid)
   real    pfa(nnf), pmua(nnmu)
   real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),&
   &drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),&
   &detadh(ngrid)
   real    psltvr(7,ngrid)
!
   double precision dt1, hp(ngrid,3), qp(ngrid,3)
   double precision abcd1(ngridm,5)
   double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
!
!     Declaration of local variables:
!
   integer ibr, i1, i2, n
!
!     Loop over branches
!
   do 10 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
!        Compute ABCDE coefficients for actual branch
!
      call KAABCD (i1     ,i2     ,ibr    ,nbran  ,g      ,dt1    ,&
      &psi    ,theta  ,ngrid  ,lambda ,dhstru ,hp     ,&
      &qp     ,grid   ,x      ,waoft  ,cp     ,rp     ,&
      &alfab  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,&
      &strtyp ,strpar ,maxtab ,ntabm  ,&
      &ntab   ,table  ,ngridm ,prslot ,strclo ,&
      &eta    ,dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,&
      &dwfdh  ,detadh ,af2    ,wf2    ,pw     ,nnf    ,&
      &pfa    ,scifri ,nnmu   ,pmua   ,scimu  ,kabcd1 ,&
      &kabcd2 ,psltvr )
!
!        Perform double sweep operation on ABCDE coefficients
!
      n = i2 - i1
      call KASWPC (n      ,ngridm ,&
      &kabcd1(i1,1)   ,kabcd1(i1,2)   ,kabcd1(i1,3)   ,&
      &kabcd1(i1,4)   ,&
      &kabcd2(i1,1)   ,kabcd2(i1,2)   ,kabcd2(i1,3)   ,&
      &kabcd2(i1,4)   ,&
      &rfv1 (i1,1)    ,rfv1 (i1,2)    ,abcd1(1,1)     ,&
      &rfv2 (i1,1)    ,rfv2 (i1,2)    ,abcd1(1,2)     )
10 continue
end

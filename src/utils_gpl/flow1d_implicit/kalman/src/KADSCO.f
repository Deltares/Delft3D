      subroutine KADSCO(g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,
     +                  ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,
     +                  x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +                  lsalt  ,rho    ,rhow   ,nstru  ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  ngridm ,prslot ,strclo ,eta    ,dqltdh ,
     +                  dalfdh ,dcdh   ,drdh   ,dcdq   ,dwfdh  ,detadh ,
     +                  af2    ,wf2    ,pw     ,nnf    ,pfa    ,scifri ,
     +                  nnmu   ,pmua   ,scimu  ,abcd1  ,kabcd1 ,kabcd2 ,
     +                  rfv1   ,rfv2   ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KADSCO (Kalman Double Sweep COefficients)
c
c Module description: Subroutine KADSCO computes the ABCDE coefficients
c                     and the double sweeped ABCDE coefficients r1,f1 and
c                     r2,f2 for all grid points in the network.
c
c                     In subroutine KAABCD the matrix coefficients
c                     (diagonals) A,B,C,D and E will be computed, for the
c                     'normal' gridpoints and the gridpoints around the
c                     structures.
c
c                     Subroutine KASWPC will perform a double sweep
c                     operation on the just computed ABCDE matrix,
c                     resulting in coefficients r1,f1 and r2,f2 for each
c                     grid point. These double sweeped coefficients are
c                     the final result of subroutine KADSCO.
c
c                     The coefficients r1,f1 and r2,f2 are calculated
c                     once and will be used for every column in the
c                     covariance matrix. The coefficients v1 and v2 will
c                     be calculeted for every column in the covariance
c                     matrix in a double sweep process in routine KASWPR.
c
c
c                     The right hand side of the nodal administration
c                     system must be filled for a paticular column of the
c                     P-matrix.  Use arrays v1,v2 and KBETA.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 52 abcd1             P  -
c 43 af2               P  -
c 17 alfab             P  -
c  6 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 15 cp                P  -
c 37 dalfdh            P  -
c 38 dcdh              P  -
c 40 dcdq              P  -
c 42 detadh            P  -
c  9 dhstru            P  -
c 36 dqltdh            P  -
c 39 drdh              P  -
c  2 dt1               P  -
c 41 dwfdh             P  -
c 35 eta               P  -
c  1 g                 P  -
c 12 grid              P  -
c 10 hp                P  -
c 53 kabcd1            P  -
c 54 kabcd2            P  -
c  8 lambda            P  -
c 19 lsalt             P  -
c 27 maxtab            I  Maximum number of defined tables.
c  5 nbran             I  Number of branches.
c  7 ngrid             I  Number of grid points in network.
c 31 ngridm            I  Maximum number of gridpoints in a branch.
c 46 nnf               I  Number of uncertain bed friction parameters.
c 49 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 22 nstru             I  Number of structures.
c 29 ntab              P  -
c 28 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 47 pfa               P  -
c 50 pmua              P  -
c 32 prslot            P  -
c 57 psltvr            P  -
c  3 psi               P  -
c 45 pw                P  -
c 11 qp                P  -
c 55 rfv1              P  -
c 56 rfv2              P  -
c 20 rho               P  -
c 21 rhow              P  -
c 16 rp                P  -
c 48 scifri            P  -
c 51 scimu             P  -
c 34 strclo            P  -
c 26 strpar            P  -
c 25 strtyp            P  -
c 30 table             P  -
c 18 tauwi             P  -
c  4 theta             P  -
c 14 waoft             P  -
c 44 wf2               P  -
c 13 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kaabcd  KAlman ABCDe coefficients
c kaswpc  KAlman double SWeeP coefficients
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kadsco.pf,v $
c Revision 1.5  1999/03/15  15:51:41  kuipe_j
c tabs removed
c
c Revision 1.4  1997/06/17  11:23:44  kuipe_j
c Initialize vars
c
c Revision 1.3  1997/01/23  08:29:36  kuipe_j
c Make flow module robust
c
c Revision 1.2  1996/04/12  13:04:47  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:23  kuipe_j
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
      real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),
     +        drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),
     +        detadh(ngrid)
      real    psltvr(7,ngrid)
c
      double precision dt1, hp(ngrid,3), qp(ngrid,3) 
      double precision abcd1(ngridm,5)
      double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
c
c     Declaration of local variables:
c
      integer ibr, i1, i2, n
c
c     Loop over branches
c
      do 10 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
c        Compute ABCDE coefficients for actual branch
c
         call KAABCD (i1     ,i2     ,ibr    ,nbran  ,g      ,dt1    ,
     +                psi    ,theta  ,ngrid  ,lambda ,dhstru ,hp     ,
     +                qp     ,grid   ,x      ,waoft  ,cp     ,rp     ,
     +                alfab  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,
     +                strtyp ,strpar ,maxtab ,ntabm  ,
     +                ntab   ,table  ,ngridm ,prslot ,strclo ,
     +                eta    ,dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,
     +                dwfdh  ,detadh ,af2    ,wf2    ,pw     ,nnf    ,
     +                pfa    ,scifri ,nnmu   ,pmua   ,scimu  ,kabcd1 ,
     +                kabcd2 ,psltvr )
c
c        Perform double sweep operation on ABCDE coefficients
c
         n = i2 - i1
         call KASWPC (n      ,ngridm ,
     +                kabcd1(i1,1)   ,kabcd1(i1,2)   ,kabcd1(i1,3)   ,
     +                kabcd1(i1,4)   ,
     +                kabcd2(i1,1)   ,kabcd2(i1,2)   ,kabcd2(i1,3)   ,
     +                kabcd2(i1,4)   ,
     +                rfv1 (i1,1)    ,rfv1 (i1,2)    ,abcd1(1,1)     ,
     +                rfv2 (i1,1)    ,rfv2 (i1,2)    ,abcd1(1,2)     )
   10 continue
      end

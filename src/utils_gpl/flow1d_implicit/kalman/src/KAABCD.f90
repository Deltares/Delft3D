subroutine KAABCD(i1     ,i2     ,ibr    ,nbran  ,g      ,dt1    ,&
&psi    ,theta  ,ngrid  ,lambda ,dhstru ,hp     ,&
&qp     ,grid   ,x      ,waoft  ,cp     ,rp     ,&
&alfab  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,&
&strtyp ,strpar ,maxtab ,ntabm  ,&
&ntab   ,table  ,ngridm ,prslot ,strclo ,&
&eta    ,dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,&
&dwfdh  ,detadh ,af2    ,wf2    ,pw     ,nnf    ,&
&pfa    ,scifri ,nnmu   ,pmua   ,scimu  ,kabcd1 ,&
&kabcd2 ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAABCD (KAlman ABCDe coefficients)
!
! Module description: Subroutine KAABCD computes the matrix ABCDE coef-
!                     ficients for all grid points including structures.
!
!                     The set of ABCDE coefficients consists of the coef-
!                     ficients A1-D1 and EA1-ED1 from the continuity
!                     equation and the A2-F2 and EA2-EF2 coefficients
!                     from the momentum c.q. stage-discharge equation.
!
!                     For the 'normal' gridpoints the coefficients A2-F2
!                     and EA2-EM2 belong to the momentum equation. For
!                     the structures these coefficients follow from the
!                     specific stage-discharge relation for the structure.
!
!                     In subroutine KANORM the coefficients are computed
!                     for the 'normal' gridpoints whereas subroutine
!                     KASTRU performs the same calculation for the grid
!                     points around structures, with the momentum
!                     equation replaced by the stage-discharge equation.
!
!                     The coefficients F, W, M, EF, EW and EM are the
!                     contributions to the equations of the uncertain
!                     modelparameters.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 45 af2               P  -
! 19 alfab             P  -
! 17 cp                P  -
! 39 dalfdh            P  -
! 40 dcdh              P  -
! 42 dcdq              P  -
! 44 detadh            P  -
! 11 dhstru            P  -
! 38 dqltdh            P  -
! 41 drdh              P  -
!  6 dt1               P  -
! 43 dwfdh             P  -
! 37 eta               P  -
!  5 g                 P  -
! 14 grid              P  -
! 12 hp                P  -
!  1 i1                I  Index of first grid point in actual branch.
!  2 i2                P  -
!  3 ibr               P  -
! 54 kabcd1            P  -
! 55 kabcd2            P  -
! 10 lambda            P  -
! 21 lsalt             P  -
! 29 maxtab            I  Maximum number of defined tables.
!  4 nbran             I  Number of branches.
!  9 ngrid             I  Number of grid points in network.
! 33 ngridm            P  -
! 48 nnf               I  Number of uncertain bed friction parameters.
! 51 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 24 nstru             I  Number of structures.
! 31 ntab              P  -
! 30 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 49 pfa               P  -
! 52 pmua              P  -
! 34 prslot            P  -
! 56 psltvr            P  -
!  7 psi               P  -
! 47 pw                P  -
! 13 qp                P  -
! 22 rho               P  -
! 23 rhow              P  -
! 18 rp                P  -
! 50 scifri            P  -
! 53 scimu             P  -
! 36 strclo            P  -
! 28 strpar            P  -
! 27 strtyp            P  -
! 32 table             P  -
! 20 tauwi             P  -
!  8 theta             P  -
! 16 waoft             P  -
! 46 wf2               P  -
! 15 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kanorm  KAlman abcde coefficients 'NORMal' gridpoints
! kastru  KAlman abcde coefficients for STRUctures
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaabcd.pf,v $
! Revision 1.4  1997/06/17  11:23:40  kuipe_j
! Initialize vars
!
! Revision 1.3  1997/01/23  08:29:29  kuipe_j
! Make flow module robust
!
! Revision 1.2  1996/04/12  13:04:33  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:10  kuipe_j
! Kalman module added
!
! Revision 1.2  1996/02/14  16:32:07  kuipe_j
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer i1, i2, ngrid, ngridm, nstru, maxtab, ntabm, nbran, ibr
   integer nnf, nnmu
   integer ntab(4,maxtab), grid(ngrid), strtyp(10,*)
   integer scifri(ngrid), scimu(nstru)
   logical lsalt, strclo(*)
   real    lambda ,dhstru
   real    g, psi, theta, rhow, pw
   real    x(ngrid), waoft(ngrid,dmwaof), cp(ngrid,4), rp(ngrid,4)
   real    alfab(ngrid), tauwi(ngrid), eta(ngrid), rho(ngrid)
   real    strpar(dmstrpar,*), table(ntabm)
   real    prslot(3,nbran), af2(ngrid), wf2(ngrid)
   real    pfa(nnf), pmua(nnmu)
   real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),&
   &drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),&
   &detadh(ngrid)
   real    psltvr(7,ngrid)

   double  precision dt1, hp(ngrid,3), qp(ngrid,3)
   double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
!
!     Compute ABCDE coefficients for normal grid points
!
   call KANORM (ngrid   ,ngridm  ,nbran  ,ibr      ,i1      ,i2    ,&
   &g       ,dt1     ,psi     ,theta   ,&
   &hp(1,1) ,qp(1,1) ,hp(1,3) ,qp(1,3) ,&
   &grid    ,x       ,waoft(1,1)       ,waoft(1,3)     ,&
   &cp(1,1) ,rp(1,1) ,alfab   ,tauwi   ,eta    ,lsalt  ,&
   &rho     ,rhow    ,prslot  ,dqltdh  ,dalfdh ,dcdh   ,&
   &drdh    ,dcdq    ,dwfdh   ,detadh  ,&
   &scifri  ,nnf     ,pfa     ,pw      ,&
   &kabcd1(i1,1)     ,kabcd1(i1,2)     ,kabcd1(i1,3)   ,&
   &kabcd1(i1,4)     ,kabcd1(i1,5)     ,kabcd1(i1,6)   ,&
   &kabcd1(i1,7)     ,kabcd1(i1,8)     ,&
   &kabcd2(i1,1)     ,kabcd2(i1,2)     ,kabcd2(i1,3)   ,&
   &kabcd2(i1,4)     ,kabcd2(i1,5)     ,kabcd2(i1,6)   ,&
   &kabcd2(i1,7)     ,kabcd2(i1,8)     ,kabcd2(i1,9)   ,&
   &kabcd2(i1,10)    ,kabcd2(i1,11)    ,kabcd2(i1,12)  ,&
   &kabcd2(i1,13)    ,kabcd2(i1,14)    ,psltvr )
!
!     Compute ABCDE coefficients for grid points connected to structures
!
   call KASTRU (i1     ,i2     ,g      ,nstru  ,strtyp ,strpar     ,&
   &ngrid  ,hp(1,3),hp(1,1),qp(1,3),af2    ,wf2        ,&
   &maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt      ,&
   &rho    ,strclo ,nnmu   ,pmua   ,scimu  ,lambda     ,&
   &dhstru         ,kabcd2(i1,1)   ,&
   &kabcd2(i1, 2)  ,kabcd2(i1, 3)  ,kabcd2(i1, 4)      ,&
   &kabcd2(i1, 7)  ,kabcd2(i1, 8)  ,kabcd2(i1, 9)      ,&
   &kabcd2(i1,10)  ,kabcd2(i1,11)  ,kabcd2(i1,14)      )
!
end

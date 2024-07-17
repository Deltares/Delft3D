      subroutine KAABCD(i1     ,i2     ,ibr    ,nbran  ,g      ,dt1    ,
     +                  psi    ,theta  ,ngrid  ,lambda ,dhstru ,hp     ,
     +                  qp     ,grid   ,x      ,waoft  ,cp     ,rp     ,
     +                  alfab  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,
     +                  ntab   ,table  ,ngridm ,prslot ,strclo ,
     +                  eta    ,dqltdh ,dalfdh ,dcdh   ,drdh   ,dcdq   ,
     +                  dwfdh  ,detadh ,af2    ,wf2    ,pw     ,nnf    ,
     +                  pfa    ,scifri ,nnmu   ,pmua   ,scimu  ,kabcd1 ,
     +                  kabcd2 ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAABCD (KAlman ABCDe coefficients)
c
c Module description: Subroutine KAABCD computes the matrix ABCDE coef-
c                     ficients for all grid points including structures.
c
c                     The set of ABCDE coefficients consists of the coef-
c                     ficients A1-D1 and EA1-ED1 from the continuity
c                     equation and the A2-F2 and EA2-EF2 coefficients
c                     from the momentum c.q. stage-discharge equation.
c
c                     For the 'normal' gridpoints the coefficients A2-F2
c                     and EA2-EM2 belong to the momentum equation. For
c                     the structures these coefficients follow from the
c                     specific stage-discharge relation for the structure.
c
c                     In subroutine KANORM the coefficients are computed
c                     for the 'normal' gridpoints whereas subroutine
c                     KASTRU performs the same calculation for the grid
c                     points around structures, with the momentum
c                     equation replaced by the stage-discharge equation.
c
c                     The coefficients F, W, M, EF, EW and EM are the
c                     contributions to the equations of the uncertain
c                     modelparameters.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 45 af2               P  -
c 19 alfab             P  -
c 17 cp                P  -
c 39 dalfdh            P  -
c 40 dcdh              P  -
c 42 dcdq              P  -
c 44 detadh            P  -
c 11 dhstru            P  -
c 38 dqltdh            P  -
c 41 drdh              P  -
c  6 dt1               P  -
c 43 dwfdh             P  -
c 37 eta               P  -
c  5 g                 P  -
c 14 grid              P  -
c 12 hp                P  -
c  1 i1                I  Index of first grid point in actual branch.
c  2 i2                P  -
c  3 ibr               P  -
c 54 kabcd1            P  -
c 55 kabcd2            P  -
c 10 lambda            P  -
c 21 lsalt             P  -
c 29 maxtab            I  Maximum number of defined tables.
c  4 nbran             I  Number of branches.
c  9 ngrid             I  Number of grid points in network.
c 33 ngridm            P  -
c 48 nnf               I  Number of uncertain bed friction parameters.
c 51 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 24 nstru             I  Number of structures.
c 31 ntab              P  -
c 30 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 49 pfa               P  -
c 52 pmua              P  -
c 34 prslot            P  -
c 56 psltvr            P  -
c  7 psi               P  -
c 47 pw                P  -
c 13 qp                P  -
c 22 rho               P  -
c 23 rhow              P  -
c 18 rp                P  -
c 50 scifri            P  -
c 53 scimu             P  -
c 36 strclo            P  -
c 28 strpar            P  -
c 27 strtyp            P  -
c 32 table             P  -
c 20 tauwi             P  -
c  8 theta             P  -
c 16 waoft             P  -
c 46 wf2               P  -
c 15 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kanorm  KAlman abcde coefficients 'NORMal' gridpoints
c kastru  KAlman abcde coefficients for STRUctures
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaabcd.pf,v $
c Revision 1.4  1997/06/17  11:23:40  kuipe_j
c Initialize vars
c
c Revision 1.3  1997/01/23  08:29:29  kuipe_j
c Make flow module robust
c
c Revision 1.2  1996/04/12  13:04:33  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:10  kuipe_j
c Kalman module added
c
c Revision 1.2  1996/02/14  16:32:07  kuipe_j
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
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
      real    dqltdh(ngrid), dalfdh(ngrid), dcdh(ngrid),
     +        drdh(ngrid), dcdq(ngrid), dwfdh(ngrid),
     +        detadh(ngrid)
      real    psltvr(7,ngrid)

      double  precision dt1, hp(ngrid,3), qp(ngrid,3)      
      double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
c
c     Compute ABCDE coefficients for normal grid points
c
      call KANORM (ngrid   ,ngridm  ,nbran  ,ibr      ,i1      ,i2    ,
     +             g       ,dt1     ,psi     ,theta   ,
     +             hp(1,1) ,qp(1,1) ,hp(1,3) ,qp(1,3) ,
     +             grid    ,x       ,waoft(1,1)       ,waoft(1,3)     ,
     +             cp(1,1) ,rp(1,1) ,alfab   ,tauwi   ,eta    ,lsalt  ,
     +             rho     ,rhow    ,prslot  ,dqltdh  ,dalfdh ,dcdh   ,
     +             drdh    ,dcdq    ,dwfdh   ,detadh  ,
     +             scifri  ,nnf     ,pfa     ,pw      ,
     +             kabcd1(i1,1)     ,kabcd1(i1,2)     ,kabcd1(i1,3)   ,
     +             kabcd1(i1,4)     ,kabcd1(i1,5)     ,kabcd1(i1,6)   ,
     +             kabcd1(i1,7)     ,kabcd1(i1,8)     ,
     +             kabcd2(i1,1)     ,kabcd2(i1,2)     ,kabcd2(i1,3)   ,
     +             kabcd2(i1,4)     ,kabcd2(i1,5)     ,kabcd2(i1,6)   ,
     +             kabcd2(i1,7)     ,kabcd2(i1,8)     ,kabcd2(i1,9)   ,
     +             kabcd2(i1,10)    ,kabcd2(i1,11)    ,kabcd2(i1,12)  ,
     +             kabcd2(i1,13)    ,kabcd2(i1,14)    ,psltvr )
c
c     Compute ABCDE coefficients for grid points connected to structures
c
      call KASTRU (i1     ,i2     ,g      ,nstru  ,strtyp ,strpar     ,
     +             ngrid  ,hp(1,3),hp(1,1),qp(1,3),af2    ,wf2        ,
     +             maxtab ,ntabm  ,ntab   ,table  ,ngridm ,lsalt      ,
     +             rho    ,strclo ,nnmu   ,pmua   ,scimu  ,lambda     ,
     +             dhstru         ,kabcd2(i1,1)   ,
     +             kabcd2(i1, 2)  ,kabcd2(i1, 3)  ,kabcd2(i1, 4)      ,
     +             kabcd2(i1, 7)  ,kabcd2(i1, 8)  ,kabcd2(i1, 9)      ,
     +             kabcd2(i1,10)  ,kabcd2(i1,11)  ,kabcd2(i1,14)      )
c
      end

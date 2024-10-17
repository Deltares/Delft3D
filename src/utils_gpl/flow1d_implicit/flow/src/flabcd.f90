subroutine FLABCD(i1     ,i2     ,g      ,iter   ,dt1    ,steady ,&
&psi    ,theta  ,exrstp ,ngrid  ,lambda ,relstr ,&
&dhstru ,hp     ,qp     ,qlatgr ,grid   ,&
&x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
&ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,&
&strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
&ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,theta2 ,&
&omr    ,cflpse ,omcfl  ,dhtyp  ,ibuf   ,istep  ,&
&lfrou  ,qlat   ,qltpar ,nqlat  ,hlev   ,maxlev ,&
&solbuf ,stdbq  ,nstdb  ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLABCD (FLow ABCDe coefficients)
!
! Module description: Subroutine FLABCD computes the matrix ABCDE coef-
!                     ficients for all grid points including structures.
!
!                     The set of ABCDE coefficients consists of the
!                     coefficients A1-E1 from the continuity equation
!                     and the A2-E2 coefficients from the momentum c.q.
!                     stage-discharge equation.
!
!                     For the 'normal' gridpoints the coefficients A2-E2
!                     belong to the momentum equation. For the struc-
!                     tures these coefficients follow from the specific
!                     stage-discharge relation for the structure.
!
!                     In subroutine FLNORM the coefficients A1-E1 and
!                     A2-E2 are computed for the 'normal' gridpoints
!                     whereas subroutine FLSTRU performs the same calcu-
!                     lation for the gridpoints connected to structures,
!                     with the momentum equation replaced by the
!                     stage-discharge equation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 27 a1m               P  -
! 38 abcd1             P  -
! 39 abcd2             P  -
! 21 alfab             P  -
! 42 cflpse            P  -
! 19 cp                P  -
! 12 dhstru            P  -
! 44 dhtyp             P  -
!  5 dt1               P  -
!  3 g                 P  -
! 16 grid              P  -
! 13 hp                P  -
!  1 i1                P  -
!  2 i2                P  -
! 46 istep             P  -
!  4 iter              P  -
! 45 jufrou            P  -
! 23 ksi               P  -
! 10 lambda            P  -
! 49 lfrou             P  -
! 24 lsalt             P  -
! 31 maxtab            I  Maximum number of defined tables.
!  9 ngrid             I  Number of grid points in network.
! 35 ngridm            I  Maximum number of gridpoints in a branch.
! 28 nstru             P  -
! 33 ntab              P  -
! 32 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 43 omcfl             P  -
! 41 omr               P  -
!  7 psi               P  -
! 15 qlatgr            P  -
! 14 qp                P  -
! 11 relstr            P  -
! 25 rho               P  -
! 26 rhow              P  -
! 20 rp                P  -
!  6 steady            P  -
! 36 strclo            P  -
! 37 strhis            P  -
! 30 strpar            P  -
! 29 strtyp            P  -
! 34 table             P  -
! 22 tauwi             P  -
! 40 theta2            P  -
!  8 theta             P  -
! 18 waoft             P  -
! 17 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flnorm  FLow abcde coefficients 'NORMal'gridpnts.
! flstru  FLow abcde coefficients for STRUctures
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flabcd.pf,v $
! Revision 1.17  1999/03/15  14:21:39  kuipe_j
! Improve writing Froude file
!
! Revision 1.16  1998/05/25  18:59:59  kuipe_j
! Wendy structures
!
! Revision 1.15  1997/11/04  14:17:21  kuipe_j
! Retention basin
!
! Revision 1.14  1997/10/03  06:39:32  kuipe_j
! criterium for flow drection changed
!
! Revision 1.13  1997/05/26  07:44:41  kuipe_j
! Small changes
!
! Revision 1.12  1997/02/17  10:20:47  kuipe_j
! Lateral Q in m3/s in cont equation now
!
! Revision 1.11  1997/01/23  08:28:53  kuipe_j
! Make flow module robust
!
! Revision 1.10  1996/10/31  10:30:15  kuipe_j
! Extra resistance finished
!
! Revision 1.9  1996/01/17  14:38:08  kuipe_j
! header update
!
! Revision 1.8  1995/11/21  11:07:41  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:00:44  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:43  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:21  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:10  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:54:41  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:36  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:24  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  08:37:14  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer i1, i2, iter, ngrid, ngridm, nstru, maxtab, ntabm, exrstp
   integer istep, nqlat, maxlev, nstdb, juer   ,ker
   integer ntab(4,maxtab), grid(ngrid), strtyp(10,*), ibuf(*)
   logical lsalt, steady,  strclo(*), lfrou
   real    g, psi, theta, rhow ,omcfl  ,dhtyp
   real    lambda ,relstr ,dhstru
   real    qlatgr(ngrid)
   real    x(ngrid), waoft(ngrid,*), cp(ngrid,4), rp(ngrid,4)
   real    alfab(ngrid), tauwi(ngrid), ksi(ngrid), rho(ngrid)
   real    a1m(ngrid)
   real    strpar(dmstrpar,*), table(ntabm),stdbq(nstdb)
   real    theta2,omr,cflpse
   real    strhis(dmstrh,*)
   real    qltpar(9,nqlat), qlat(nqlat,9)
   real    solbuf(dmbuf2,7,ngrid)

   double precision  dt1, hlev(ngrid,maxlev)
   double precision  hp(ngrid,3), qp(ngrid,3)
   double precision abcd1(ngridm,5), abcd2(ngridm,5)
!
!     Compute ABCDE coefficients for normal grid points
!
   call FLNORM (ngrid   ,ngridm  ,i1     ,i2      ,g       ,dt1     ,&
   &steady  ,psi     ,theta  ,exrstp  ,hp(1,1) ,qp(1,1) ,&
!                                                    <wf>
   &hp(1,2) ,qlatgr  ,grid   ,x       ,waoft(1,1)       ,&
!                  <af>              <wt>
   &waoft(1,3)       ,waoft(1,2)       ,&
!                  <at>              <at1>
   &waoft(1,4)       ,waoft(1,5)       ,&
!                  <chz>    <r>
   &cp(1,1) ,rp(1,1) ,alfab   ,tauwi   ,ksi    ,&
   &lsalt   ,rho     ,rhow    ,a1m     ,&
!                  <a1>              <b1>              <c1>
   &abcd1(1,1)       ,abcd1(1,2)       ,abcd1(1,3)     ,&
!                  <d1>              <e1>
   &abcd1(1,4)       ,abcd1(1,5)       ,&
!                  <a2>              <b2>              <c2>
   &abcd2(1,1)       ,abcd2(1,2)       ,abcd2(1,3)     ,&
!                  <d2>              <e2>              <o>
   &abcd2(1,4)       ,abcd2(1,5)       ,waoft(1,6)     ,&
!                  <wfp>
   &waoft(1,9)       ,omr              ,&
!                                    <q>
   &cflpse           ,qp(1,2)          ,&
!                  <wfex>            <wtex>
   &waoft(1,13)      ,waoft(1,14)      ,hp(1,3)        ,&
!                  <cflpsa>
   &waoft(1,17)      ,omcfl    ,dhtyp  ,iter           ,&
   &ibuf(3), istep   ,lfrou    ,solbuf , qp(1,3))
!
!     Compute ABCDE coefficients for grid points connected to structures
!
   call FLSTRU (i1     ,i2     ,g      ,iter   ,nstru  ,strtyp     ,&
   &strpar ,ngrid  ,lambda ,relstr ,dhstru ,&
!                  <h>     <h1>    <q>     <q1>    <q2>    <af>
   &hp(1,2),hp(1,1),qp(1,2),qp(1,1),qp(1,3),waoft(1,3) ,&
!                  <wf>
   &waoft(1,1)     ,maxtab ,ntabm  ,ntab   ,table      ,&
   &ngridm         ,lsalt  ,rho    ,strclo ,strhis     ,&
!                  <a2>            <b2>            <c2>
   &abcd2(1,1)     ,abcd2(1,2)     ,abcd2(1,3)         ,&
!                  <d2>            <e2>
   &abcd2(1,4)     ,abcd2(1,5)     ,&
   &hlev           ,maxlev         ,stdbq  ,nstdb      ,&
   &juer   ,ker    )

!
!
!     Compute ABCDE coefficients for lateral discharges
!
   call FLQLAB( steady  , ngrid   , ngridm  , i1      , i2         ,&
   &theta   , theta2  , relstr  ,&
!                  <q>                <q1>
   &qp(1,2)           , qp(1,1)           ,&
   &qltpar            ,&
!                  <cflpa>
   &waoft(1,17)       , x       , qlat       ,&
!                  <a1>                <b1>
   &abcd1(1,1)        , abcd1(1,2)        ,&
!                  <c1>                <d1>               <e1>
   &abcd1(1,3)        , abcd1(1,4)        , abcd1(1,5) ,&
   &dt1     , nqlat   )

end

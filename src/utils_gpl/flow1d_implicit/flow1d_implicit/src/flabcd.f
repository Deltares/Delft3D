      subroutine FLABCD(i1     ,i2     ,g      ,iter   ,dt1    ,steady ,
     +                  psi    ,theta  ,exrstp ,ngrid  ,lambda ,relstr ,
     +                  dhstru ,hp     ,qp     ,qlatgr ,grid   ,
     +                  x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +                  ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,theta2 ,
     +                  omr    ,cflpse ,omcfl  ,dhtyp  ,ibuf   ,istep  ,
     +                  lfrou  ,qlat   ,qltpar ,nqlat  ,hlev   ,maxlev ,
     +                  solbuf ,stdbq  ,nstdb  ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLABCD (FLow ABCDe coefficients)
c
c Module description: Subroutine FLABCD computes the matrix ABCDE coef-
c                     ficients for all grid points including structures.
c
c                     The set of ABCDE coefficients consists of the
c                     coefficients A1-E1 from the continuity equation
c                     and the A2-E2 coefficients from the momentum c.q.
c                     stage-discharge equation.
c
c                     For the 'normal' gridpoints the coefficients A2-E2
c                     belong to the momentum equation. For the struc-
c                     tures these coefficients follow from the specific
c                     stage-discharge relation for the structure.
c
c                     In subroutine FLNORM the coefficients A1-E1 and
c                     A2-E2 are computed for the 'normal' gridpoints
c                     whereas subroutine FLSTRU performs the same calcu-
c                     lation for the gridpoints connected to structures,
c                     with the momentum equation replaced by the
c                     stage-discharge equation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 27 a1m               P  -
c 38 abcd1             P  -
c 39 abcd2             P  -
c 21 alfab             P  -
c 42 cflpse            P  -
c 19 cp                P  -
c 12 dhstru            P  -
c 44 dhtyp             P  -
c  5 dt1               P  -
c  3 g                 P  -
c 16 grid              P  -
c 13 hp                P  -
c  1 i1                P  -
c  2 i2                P  -
c 46 istep             P  -
c  4 iter              P  -
c 45 jufrou            P  -
c 23 ksi               P  -
c 10 lambda            P  -
c 49 lfrou             P  -
c 24 lsalt             P  -
c 31 maxtab            I  Maximum number of defined tables.
c  9 ngrid             I  Number of grid points in network.
c 35 ngridm            I  Maximum number of gridpoints in a branch.
c 28 nstru             P  -
c 33 ntab              P  -
c 32 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 43 omcfl             P  -
c 41 omr               P  -
c  7 psi               P  -
c 15 qlatgr            P  -
c 14 qp                P  -
c 11 relstr            P  -
c 25 rho               P  -
c 26 rhow              P  -
c 20 rp                P  -
c  6 steady            P  -
c 36 strclo            P  -
c 37 strhis            P  -
c 30 strpar            P  -
c 29 strtyp            P  -
c 34 table             P  -
c 22 tauwi             P  -
c 40 theta2            P  -
c  8 theta             P  -
c 18 waoft             P  -
c 17 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flnorm  FLow abcde coefficients 'NORMal'gridpnts.
c flstru  FLow abcde coefficients for STRUctures
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flabcd.pf,v $
c Revision 1.17  1999/03/15  14:21:39  kuipe_j
c Improve writing Froude file
c
c Revision 1.16  1998/05/25  18:59:59  kuipe_j
c Wendy structures
c
c Revision 1.15  1997/11/04  14:17:21  kuipe_j
c Retention basin
c
c Revision 1.14  1997/10/03  06:39:32  kuipe_j
c criterium for flow drection changed
c
c Revision 1.13  1997/05/26  07:44:41  kuipe_j
c Small changes
c
c Revision 1.12  1997/02/17  10:20:47  kuipe_j
c Lateral Q in m3/s in cont equation now
c
c Revision 1.11  1997/01/23  08:28:53  kuipe_j
c Make flow module robust
c
c Revision 1.10  1996/10/31  10:30:15  kuipe_j
c Extra resistance finished
c
c Revision 1.9  1996/01/17  14:38:08  kuipe_j
c header update
c
c Revision 1.8  1995/11/21  11:07:41  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/22  10:00:44  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:43  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:21  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:10  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:54:41  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:36  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:24  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/11/28  08:37:14  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
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
c
c     Compute ABCDE coefficients for normal grid points
c
      call FLNORM (ngrid   ,ngridm  ,i1     ,i2      ,g       ,dt1     ,
     +             steady  ,psi     ,theta  ,exrstp  ,hp(1,1) ,qp(1,1) ,
c                                                    <wf>
     +             hp(1,2) ,qlatgr  ,grid   ,x       ,waoft(1,1)       ,
c                  <af>              <wt>
     +             waoft(1,3)       ,waoft(1,2)       ,
c                  <at>              <at1>
     +             waoft(1,4)       ,waoft(1,5)       ,
c                  <chz>    <r>
     +             cp(1,1) ,rp(1,1) ,alfab   ,tauwi   ,ksi    ,
     +             lsalt   ,rho     ,rhow    ,a1m     ,
c                  <a1>              <b1>              <c1>
     +             abcd1(1,1)       ,abcd1(1,2)       ,abcd1(1,3)     ,
c                  <d1>              <e1>
     +             abcd1(1,4)       ,abcd1(1,5)       ,
c                  <a2>              <b2>              <c2>
     +             abcd2(1,1)       ,abcd2(1,2)       ,abcd2(1,3)     ,
c                  <d2>              <e2>              <o>
     +             abcd2(1,4)       ,abcd2(1,5)       ,waoft(1,6)     ,
c                  <wfp>
     +             waoft(1,9)       ,omr              ,
c                                    <q>
     +             cflpse           ,qp(1,2)          ,
c                  <wfex>            <wtex>
     +             waoft(1,13)      ,waoft(1,14)      ,hp(1,3)        ,
c                  <cflpsa>
     +             waoft(1,17)      ,omcfl    ,dhtyp  ,iter           ,
     +             ibuf(3), istep   ,lfrou    ,solbuf , qp(1,3))
c
c     Compute ABCDE coefficients for grid points connected to structures
c
      call FLSTRU (i1     ,i2     ,g      ,iter   ,nstru  ,strtyp     ,
     +             strpar ,ngrid  ,lambda ,relstr ,dhstru ,
c                  <h>     <h1>    <q>     <q1>    <q2>    <af>
     +             hp(1,2),hp(1,1),qp(1,2),qp(1,1),qp(1,3),waoft(1,3) ,
c                  <wf>
     +             waoft(1,1)     ,maxtab ,ntabm  ,ntab   ,table      ,
     +             ngridm         ,lsalt  ,rho    ,strclo ,strhis     ,
c                  <a2>            <b2>            <c2>
     +             abcd2(1,1)     ,abcd2(1,2)     ,abcd2(1,3)         ,
c                  <d2>            <e2>
     +             abcd2(1,4)     ,abcd2(1,5)     ,
     +             hlev           ,maxlev         ,stdbq  ,nstdb      ,
     +             juer   ,ker    )

c
c
c     Compute ABCDE coefficients for lateral discharges
c
      call FLQLAB( steady  , ngrid   , ngridm  , i1      , i2         ,
     &             theta   , theta2  , relstr  ,
c                  <q>                <q1>
     &             qp(1,2)           , qp(1,1)           ,
     &             qltpar            ,
c                  <cflpa>
     &             waoft(1,17)       , x       , qlat       ,
c                  <a1>                <b1>
     &             abcd1(1,1)        , abcd1(1,2)        ,
c                  <c1>                <d1>               <e1>
     &             abcd1(1,3)        , abcd1(1,4)        , abcd1(1,5) ,
     &             dt1     , nqlat   )

      end

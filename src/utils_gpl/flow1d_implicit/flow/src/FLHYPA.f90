subroutine FLHYPA(time   ,iter   ,nbran  ,ngrid  ,branch ,typcr  ,&
&h      ,h1     ,maxlev ,nlev   ,hlev   ,wft    ,&
&aft    ,wtt    ,att    ,overlp ,arex   ,arexcn ,&
&arexop ,of     ,bfrict ,bfricp ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,&
&grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,&
&x      ,nexres ,exres  ,lsalt  ,izwft  ,juer   ,&
&prslot ,psltvr ,waoft  ,c      ,r      ,alfab  ,&
&tauwi  ,ksi    ,a1m    ,ker    ,dt1    ,theta2 ,&
&exrstp ,omalfa ,omc    ,omr    ,omw    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHYPA (FLow HYdraulic PArameters)
!
! Module description: In subroutine FLHYPA several hydraulic parameters
!                     are computed, needed for the computation of the
!                     ABCDE coefficients.
!
!                     In subroutine FLARWI the actual values will be
!                     computed for the flow area, the total area, the
!                     flow width, the total width for each iteration
!                     step.
!
!                     Depending on the chosen option the wetted perime-
!                     ter, the Boussinesq's constant alfab and the Chezy
!                     coefficients C will be computed.
!
!                     In subroutine FLBOCH the Boussinesq's constant
!                     alfab and the Chezy coefficients C will be deter-
!                     mined for the water level h(0)n+1/2 as computed in
!                     the latest iteration level.
!
!                     In subroutine FLWIFR the wind friction parameter
!                     tauwi will be computed. The extra resistance term
!                     is calculated in routine FLERES. In case the salt
!                     module is included in the application the integral
!                     A1M will be calculated by routine FLA1M.
!
!                     Three different types of schematisation are to be
!                     processed. These types are the table of levels and
!                     widths, the circle and the sedredge branch.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 51 a1m               P  -
! 13 aft               P  -
! 48 alfab             P  -
! 17 arex              P  -
! 18 arexcn            P  -
! 19 arexop            P  -
! 15 att               P  -
! 22 bfricp            P  -
! 21 bfrict            P  -
!  5 branch            P  -
! 46 c                 P  -
! 53 dt1               P  -
! 32 engpar            P  -
! 39 exres             P  -
! 33 gangle            P  -
! 31 grsize            P  -
!  7 h1                P  -
!  8 h                 P  -
! 11 hlev              P  -
!  2 iter              P  -
! 41 izwft             P  -
! 42 juer              P  -
! 52 ker               P  -
! 50 ksi               P  -
! 40 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 25 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
! 38 nexres            P  -
!  4 ngrid             I  Number of grid points in network.
! 10 nlev              P  -
! 27 ntab              P  -
! 26 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 20 of                P  -
! 55 omalfa            P  -
! 56 omc               P  -
! 57 omr               P  -
! 58 omw               P  -
! 16 overlp            P  -
! 43 prslot            P  -
! 44 psltvr            P  -
! 23 q1                P  -
! 24 q                 P  -
! 47 r                 P  -
! 29 sectc             P  -
! 30 sectv             P  -
! 28 table             P  -
! 49 tauwi             P  -
! 54 theta2            P  -
!  1 time              P  -
!  6 typcr             P  -
! 45 waoft             P  -
! 35 wfrict            P  -
! 12 wft               P  -
! 34 wndpar            P  -
! 36 wshld             P  -
! 14 wtt               P  -
! 37 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! fla1m   FLow A1M
! flarwi  FLow AReas and WIdths
! flboch  FLow BOussinesq and CHezy coefficient
! fleres  FLow Extra RESistance
! flwetp  FLow Wetted Perimeter
! flwifr  FLow WInd FRiction
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhypa.pf,v $
! Revision 1.11  1997/09/30  09:25:24  kuipe_j
! density term improved for Preisman slot
!
! Revision 1.10  1997/05/26  07:43:33  kuipe_j
! Check on R andC added
!
! Revision 1.9  1997/01/23  08:29:04  kuipe_j
! Make flow module robust
!
! Revision 1.8  1996/11/05  13:47:49  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.7  1996/10/31  10:30:19  kuipe_j
! Extra resistance finished
!
! Revision 1.6  1996/09/03  14:52:00  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.5  1995/09/22  10:01:42  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:54  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1994/12/02  13:19:29  kuipe_j
! Prevention against devide by zero.
!
! Revision 1.3  1994/11/28  08:37:31  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:02  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants
!
   include '..\include\errcod.i'
   include '..\include\sobdim.i'
!
!     Declaration of Parameters:
!
   integer iter, nbran, ngrid, maxlev, nexres, exrstp, juer, ker,&
   &branch(4,nbran), bfrict(3,nbran), wfrict(3,nbran),&
   &typcr(nbran),&
   &maxtab, ntabm, ntab(4,maxtab),&
   &nlev(ngrid), arexcn(ngrid,2), arexop(2)
   real    overlp,&
   &table(ntabm),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &wtt(ngrid,maxlev), att(ngrid,maxlev),&
   &of (ngrid,maxlev), izwft(ngrid,maxlev),&
   &waoft(ngrid,*), arex(ngrid,4),&
   &c(ngrid,4), r(ngrid,4), alfab(ngrid), gangle(ngrid),&
   &wshld(ngrid), bfricp(6,ngrid),&
   &grsize(4,ngrid,*), engpar(9), wndpar(3), exres(3,*),&
   &sectc(ngrid,3), sectv(ngrid,dmsecv),&
   &ksi(ngrid), x(ngrid), a1m(ngrid), tauwi(ngrid),&
   &prslot(3,nbran), psltvr(7,ngrid)
   real    theta2 ,omalfa ,omc    ,omr    ,omw
   double precision time, dt1, hlev(ngrid,maxlev)
   double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
   logical lsalt
!
!     Declaration of parameters
!
!     Computation of 1. flow width       Wf(1:ngrid)
!                    2. flow area        Af(1:ngrid)
!                    3. total width      Wt(1:ngrid)
!                    4. total area       At(1:ngrid)
!
   call FLARWI (iter   ,nbran  ,ngrid  ,branch ,typcr  ,prslot ,&
   &h1     ,h      ,maxlev ,nlev   ,hlev   ,&
   &wft    ,aft    ,wtt    ,att    ,juer   ,&
   &overlp ,arex   ,arexcn ,arexop ,&
!                  <Wf>            <Af>            <Wt>
   &waoft(1,1)     ,waoft(1,3)     ,waoft(1,2)     ,&
!                  <At>                            <W2>
   &waoft(1,4)     ,ker    ,theta2 ,waoft(1,10)    ,&
!                  <Wfex>          <Wtex>
   &waoft(1,13)    ,waoft(1,14)    ,psltvr         )
!
!     Computation of 5. wetted perimeter O (1:ngrid)
!
   call FLWETP (nbran  ,ngrid  ,branch ,typcr  ,h1     ,h      ,&
   &maxlev ,nlev   ,hlev   ,wft    ,of     ,&
!                  <Wf>                            <O>
   &waoft(1,1)     ,prslot ,juer   ,waoft(1,6)     ,&
   &ker    ,theta2 ,psltvr )
!
!     Computation of 6. Hydraulic radius R
!                    7. Chezy coefficient C
!                    8. Boussinesq constant ALFAB(1:ngrid)
!
   if (ker .eq.fatal) goto 1000
!
   call FLBOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,&
   &h1     ,h      ,q1     ,q      ,maxlev ,hlev   ,&
   &wft    ,maxtab ,ntabm  ,ntab   ,table  ,&
!                  <subsec>        <secth0>        <secth1>
   &sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,&
!                  <wfh0>          <wfh1>
   &sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,&
!                  <Af>            <O>             <Afh0>
   &waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,&
!                  <Afh1>          <Oh0>           <Oh1>
   &sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,&
!                                  <Asubsc>
   &prslot ,psltvr ,sectv(1,1)     ,c(1,1) ,r(1,1) ,&
   &c(1,2) ,r(1,2) ,alfab          ,&
   &iter   ,theta2 ,omalfa         ,omr    ,omw    ,&
!                  <alfabp>        <c2rp>           <wfp>
   &waoft(1,7)     ,waoft(1,8)     ,waoft(1,9)     ,&
!                  <wf>
   &waoft(1,1)     ,juer   ,ker    )
!
!     Computation of 9. wind stress
!
   call FLWIFR (nbran  ,ngrid  ,branch ,time   ,&
   &maxtab ,ntabm  ,ntab   ,table  ,gangle ,&
   &wndpar ,wfrict ,wshld  ,tauwi  ,dt1    ,theta2 )
!
!     Computation of 10. extra resistance
!
   call FLERES (ngrid  ,h1     ,h      ,q1     ,q      ,maxtab ,&
   &ntabm  ,ntab   ,table  ,x      ,nexres ,exres  ,&
!                         <ksip>                  <omega>
   &ksi    ,waoft(1,18)    ,exrstp ,omc    ,iter   ,&
   &juer   ,ker    ,theta2 )
!
!     Computation of 11. first order momentum cross section A1m
!
   if ( lsalt ) then
      call FLA1M (ngrid  ,nbran  ,branch ,typcr  ,&
      &h1     ,h      ,maxlev ,nlev   ,hlev   ,wft    ,&
!                   <Af>
      &waoft(1,3)     ,izwft  ,a1m    ,theta2 )
   endif
1000 continue
!
end

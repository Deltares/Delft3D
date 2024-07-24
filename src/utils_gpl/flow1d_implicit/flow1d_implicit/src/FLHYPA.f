      subroutine FLHYPA(time   ,iter   ,nbran  ,ngrid  ,branch ,typcr  ,
     +                  h      ,h1     ,maxlev ,nlev   ,hlev   ,wft    ,
     +                  aft    ,wtt    ,att    ,overlp ,arex   ,arexcn ,
     +                  arexop ,of     ,bfrict ,bfricp ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     +                  grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,
     +                  x      ,nexres ,exres  ,lsalt  ,izwft  ,juer   ,
     +                  prslot ,psltvr ,waoft  ,c      ,r      ,alfab  ,
     +                  tauwi  ,ksi    ,a1m    ,ker    ,dt1    ,theta2 ,
     +                  exrstp ,omalfa ,omc    ,omr    ,omw    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHYPA (FLow HYdraulic PArameters)
c
c Module description: In subroutine FLHYPA several hydraulic parameters
c                     are computed, needed for the computation of the
c                     ABCDE coefficients.
c
c                     In subroutine FLARWI the actual values will be
c                     computed for the flow area, the total area, the
c                     flow width, the total width for each iteration
c                     step.
c
c                     Depending on the chosen option the wetted perime-
c                     ter, the Boussinesq's constant alfab and the Chezy
c                     coefficients C will be computed.
c
c                     In subroutine FLBOCH the Boussinesq's constant
c                     alfab and the Chezy coefficients C will be deter-
c                     mined for the water level h(0)n+1/2 as computed in
c                     the latest iteration level.
c
c                     In subroutine FLWIFR the wind friction parameter
c                     tauwi will be computed. The extra resistance term
c                     is calculated in routine FLERES. In case the salt
c                     module is included in the application the integral
c                     A1M will be calculated by routine FLA1M.
c
c                     Three different types of schematisation are to be
c                     processed. These types are the table of levels and
c                     widths, the circle and the sedredge branch.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 51 a1m               P  -
c 13 aft               P  -
c 48 alfab             P  -
c 17 arex              P  -
c 18 arexcn            P  -
c 19 arexop            P  -
c 15 att               P  -
c 22 bfricp            P  -
c 21 bfrict            P  -
c  5 branch            P  -
c 46 c                 P  -
c 53 dt1               P  -
c 32 engpar            P  -
c 39 exres             P  -
c 33 gangle            P  -
c 31 grsize            P  -
c  7 h1                P  -
c  8 h                 P  -
c 11 hlev              P  -
c  2 iter              P  -
c 41 izwft             P  -
c 42 juer              P  -
c 52 ker               P  -
c 50 ksi               P  -
c 40 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 25 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c 38 nexres            P  -
c  4 ngrid             I  Number of grid points in network.
c 10 nlev              P  -
c 27 ntab              P  -
c 26 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 20 of                P  -
c 55 omalfa            P  -
c 56 omc               P  -
c 57 omr               P  -
c 58 omw               P  -
c 16 overlp            P  -
c 43 prslot            P  -
c 44 psltvr            P  -
c 23 q1                P  -
c 24 q                 P  -
c 47 r                 P  -
c 29 sectc             P  -
c 30 sectv             P  -
c 28 table             P  -
c 49 tauwi             P  -
c 54 theta2            P  -
c  1 time              P  -
c  6 typcr             P  -
c 45 waoft             P  -
c 35 wfrict            P  -
c 12 wft               P  -
c 34 wndpar            P  -
c 36 wshld             P  -
c 14 wtt               P  -
c 37 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c fla1m   FLow A1M
c flarwi  FLow AReas and WIdths
c flboch  FLow BOussinesq and CHezy coefficient
c fleres  FLow Extra RESistance
c flwetp  FLow Wetted Perimeter
c flwifr  FLow WInd FRiction
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhypa.pf,v $
c Revision 1.11  1997/09/30  09:25:24  kuipe_j
c density term improved for Preisman slot
c
c Revision 1.10  1997/05/26  07:43:33  kuipe_j
c Check on R andC added
c
c Revision 1.9  1997/01/23  08:29:04  kuipe_j
c Make flow module robust
c
c Revision 1.8  1996/11/05  13:47:49  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.7  1996/10/31  10:30:19  kuipe_j
c Extra resistance finished
c
c Revision 1.6  1996/09/03  14:52:00  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.5  1995/09/22  10:01:42  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:54  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1994/12/02  13:19:29  kuipe_j
c Prevention against devide by zero.
c
c Revision 1.3  1994/11/28  08:37:31  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:02  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants 
c
      include '../include/errcod.i'
      include '../include/sobdim.i'
c
c     Declaration of Parameters:
c
      integer iter, nbran, ngrid, maxlev, nexres, exrstp, juer, ker,
     +        branch(4,nbran), bfrict(3,nbran), wfrict(3,nbran),
     +        typcr(nbran),
     +        maxtab, ntabm, ntab(4,maxtab),
     +        nlev(ngrid), arexcn(ngrid,2), arexop(2)
      real    overlp,
     +        table(ntabm),
     +        wft(ngrid,maxlev), aft(ngrid,maxlev),
     +        wtt(ngrid,maxlev), att(ngrid,maxlev),
     +        of (ngrid,maxlev), izwft(ngrid,maxlev),
     +        waoft(ngrid,*), arex(ngrid,4),
     +        c(ngrid,4), r(ngrid,4), alfab(ngrid), gangle(ngrid),
     +        wshld(ngrid), bfricp(6,ngrid),
     +        grsize(4,ngrid,*), engpar(9), wndpar(3), exres(3,*),
     +        sectc(ngrid,3), sectv(ngrid,dmsecv),
     +        ksi(ngrid), x(ngrid), a1m(ngrid), tauwi(ngrid),
     +        prslot(3,nbran), psltvr(7,ngrid)
      real    theta2 ,omalfa ,omc    ,omr    ,omw
      double precision time, dt1, hlev(ngrid,maxlev)
      double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
      logical lsalt
c
c     Declaration of parameters
c
c     Computation of 1. flow width       Wf(1:ngrid)
c                    2. flow area        Af(1:ngrid)
c                    3. total width      Wt(1:ngrid)
c                    4. total area       At(1:ngrid)
c
c     FM1DIMP2DO: remove debug
c      write(52,*) 'a'
c      write(52,*) waoft(:,3)
c      write(52,*) h
c      write(52,*) h1
      
      call FLARWI (iter   ,nbran  ,ngrid  ,branch ,typcr  ,prslot ,
     +             h1     ,h      ,maxlev ,nlev   ,hlev   ,
     +             wft    ,aft    ,wtt    ,att    ,juer   ,
     +             overlp ,arex   ,arexcn ,arexop ,
c                  <Wf>            <Af>            <Wt>
     +             waoft(1,1)     ,waoft(1,3)     ,waoft(1,2)     ,
c                  <At>                            <W2>
     +             waoft(1,4)     ,ker    ,theta2 ,waoft(1,10)    ,
c                  <Wfex>          <Wtex>       
     +             waoft(1,13)    ,waoft(1,14)    ,psltvr         )
      
c     FM1DIMP2DO: remove debug
c      write(42,*) 'a'
c      write(42,*) waoft(:,3)
c      write(42,*) h
c      write(42,*) h1
c
c     Computation of 5. wetted perimeter O (1:ngrid)
c
      call FLWETP (nbran  ,ngrid  ,branch ,typcr  ,h1     ,h      ,
     +             maxlev ,nlev   ,hlev   ,wft    ,of     ,
c                  <Wf>                            <O>
     +             waoft(1,1)     ,prslot ,juer   ,waoft(1,6)     ,
     +             ker    ,theta2 ,psltvr )
c
c     Computation of 6. Hydraulic radius R
c                    7. Chezy coefficient C
c                    8. Boussinesq constant ALFAB(1:ngrid)
c
      if (ker .eq.fatal) goto 1000
c
      call FLBOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +             h1     ,h      ,q1     ,q      ,maxlev ,hlev   ,
     +             wft    ,maxtab ,ntabm  ,ntab   ,table  ,
c                  <subsec>        <secth0>        <secth1>
     +             sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,
c                  <wfh0>          <wfh1>
     +             sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,
c                  <Af>            <O>             <Afh0>
     +             waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,
c                  <Afh1>          <Oh0>           <Oh1>
     +             sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,
c                                  <Asubsc>
     +             prslot ,psltvr ,sectv(1,1)     ,c(1,1) ,r(1,1) ,
     +             c(1,2) ,r(1,2) ,alfab          ,
     +             iter   ,theta2 ,omalfa         ,omr    ,omw    ,
c                  <alfabp>        <c2rp>           <wfp>
     +             waoft(1,7)     ,waoft(1,8)     ,waoft(1,9)     ,
c                  <wf>
     +             waoft(1,1)     ,juer   ,ker    )
      
c     FM1DIMP2DO: remove debug
c      write(42,*) 'a'
c      write(42,*) waoft(:,3)
      
c
c     Computation of 9. wind stress
c
      call FLWIFR (nbran  ,ngrid  ,branch ,time   ,
     +             maxtab ,ntabm  ,ntab   ,table  ,gangle ,
     +             wndpar ,wfrict ,wshld  ,tauwi  ,dt1    ,theta2 )
c
c     Computation of 10. extra resistance
c
      call FLERES (ngrid  ,h1     ,h      ,q1     ,q      ,maxtab ,
     +             ntabm  ,ntab   ,table  ,x      ,nexres ,exres  ,
c                         <ksip>                  <omega>
     +             ksi    ,waoft(1,18)    ,exrstp ,omc    ,iter   ,
     +             juer   ,ker    ,theta2 )
c
c     Computation of 11. first order momentum cross section A1m
c
      if ( lsalt ) then
        call FLA1M (ngrid  ,nbran  ,branch ,typcr  ,
     +              h1     ,h      ,maxlev ,nlev   ,hlev   ,wft    ,
c                   <Af>
     +              waoft(1,3)     ,izwft  ,a1m    ,theta2 )
      endif
1000  continue
c
      end

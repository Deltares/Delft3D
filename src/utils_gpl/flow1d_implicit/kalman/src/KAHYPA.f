      subroutine KAHYPA(nbran  ,ngrid  ,nnf    ,branch ,typcr  ,h1     ,
     +                  h      ,h2     ,maxlev ,nlev   ,hlev   ,wft    ,
     +                  aft    ,of     ,bfrict ,bfricp ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     +                  grsize ,engpar ,x      ,nexres ,exres  ,juer   ,
     +                  prslot ,waoft  ,af2    ,wf2    ,grid   ,c      ,
     +                  r      ,alfab  ,scifri ,pfa    ,afacc  ,oacc   ,
     +                  dwfdh  ,dcdh   ,drdh   ,dalfdh ,dcdq   ,eta    ,
     +                  detadh ,theta2 ,overlp ,arex   ,arexcn ,arexop ,
     +                  ker    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAHYPA (KAlman HYdraulic PArameters)
c
c Module description: In subroutine KAHYPA several hydraulic parameters
c                     are computed, needed for the computation of deri-
c                     vatives used to determine the ABCDE coefficients.
c
c                     The following hydraulic parameters and derivatives
c                     are calculated:
c                                                          in routine
c                     flow area, flow width:                   KAAWWP
c                     wetted perimeter:                        KAAWWP
c                     derivative of wetted perimeter:          KABOCH
c                     derivative of bousinessq coefficient :   KABOCH
c                     derivatives of Chezy coefficient: KABOCH, KADCDQ
c                     extra resistance and derivative:         KAERES
c                     flow area, flow width for structures: KAAWST
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 33 af2               P  -
c 41 afacc             P  -
c 13 aft               P  -
c 38 alfab             P  -
c 52 arex              P  -
c 53 arexcn            P  -
c 54 arexop            P  -
c 16 bfricp            P  -
c 15 bfrict            P  -
c  4 branch            P  -
c 36 c                 P  -
c 46 dalfdh            P  -
c 44 dcdh              P  -
c 47 dcdq              P  -
c 49 detadh            P  -
c 45 drdh              P  -
c 43 dwfdh             P  -
c 26 engpar            P  -
c 48 eta               P  -
c 29 exres             P  -
c 35 grid              P  -
c 25 grsize            P  -
c  6 h1                P  -
c  8 h2                P  -
c  7 h                 P  -
c 11 hlev              P  -
c 30 juer              P  -
c 55 ker               P  -
c  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 19 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c 28 nexres            P  -
c  2 ngrid             I  Number of grid points in network.
c 10 nlev              P  -
c  3 nnf               I  Number of uncertain bed friction parameters.
c 21 ntab              P  -
c 20 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 42 oacc              P  -
c 14 of                P  -
c 51 overlp            P  -
c 40 pfa               P  -
c 31 prslot            P  -
c 56 psltvr            P  -
c 17 q1                P  -
c 18 q                 P  -
c 37 r                 P  -
c 39 scifri            P  -
c 23 sectc             P  -
c 24 sectv             P  -
c 22 table             P  -
c 50 theta2            P  -
c  5 typcr             P  -
c 32 waoft             P  -
c 34 wf2               P  -
c 12 wft               P  -
c 27 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kaawst  KAlman Wetted Perimeter
c kaawwp  KAlman Areas Widths Wetted Perimeter
c kaboch  KAlman Boussinesq and Ch‚zy coefficient
c kadcdq  KAlman Derivative of Ch‚zy to Discharge Q
c kaeres  KAlman Extra RESistance
c kaflcp  Kalman Flow Chezy correction Parameter
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kahypa.pf,v $
c Revision 1.5  1997/06/17  11:23:45  kuipe_j
c Initialize vars
c
c Revision 1.4  1997/01/23  08:29:37  kuipe_j
c Make flow module robust
c
c Revision 1.3  1996/11/05  13:48:00  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.2  1996/04/12  13:05:00  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:36  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of Parameters:
c
      integer nbran, ngrid, nnf, maxlev, nexres, juer, ker,
     +        branch(4,nbran), bfrict(3,nbran),
     +        typcr(nbran),grid(ngrid),
     +        maxtab, ntabm, ntab(4,maxtab),
     +        nlev(ngrid), scifri(ngrid),
     +        arexcn(ngrid,2), arexop(2)
      real    theta2, overlp
      real    table(ntabm),
     +        af2(ngrid), wf2(ngrid),
     +        wft(ngrid,maxlev), aft(ngrid,maxlev),
     +        of (ngrid,maxlev),
     +        waoft(ngrid,dmwaof),
     +        c(ngrid,4), r(ngrid,4), alfab(ngrid),
     +        bfricp(6,ngrid),
     +        grsize(4,ngrid,*), engpar(9), exres(3,*),
     +        sectc(ngrid,3), sectv(ngrid,dmsecv),
     +        x(ngrid), arex(ngrid,4),
     +        prslot(3,nbran), pfa(nnf), psltvr(7,ngrid)
      real    afacc(ngrid), oacc(ngrid), dwfdh(ngrid),
     +        dcdh(ngrid), drdh(ngrid), dalfdh(ngrid),
     +        dcdq(ngrid), eta(ngrid), detadh(ngrid)
      double precision hlev(ngrid,maxlev), h1(ngrid), h(ngrid), 
     +        h2(ngrid), q1(ngrid), q(ngrid)

c
      real       dh
      parameter (dh=.001)
c
c     Computation of 1. flow area        Af'(1:ngrid)
c                    2. wetted perimeter O' (1:ngrid)
c                    3. dWf/dh on time n+1/2
c
      call KAAWWP (nbran  ,ngrid  ,branch ,typcr  ,prslot ,
     +             h1     ,h      ,maxlev ,nlev   ,hlev   ,
     +             wft    ,aft    ,of     ,juer   ,theta2 ,
c                  <Wf>           <Af>
     +             waoft(1,1)     ,waoft(1,3)     ,afacc  ,
c                         <Wt>
     +             oacc   ,dwfdh  ,overlp ,arex   ,arexcn ,
     +             arexop ,ker    ,psltvr )
c
c     Remove correction on total bottom friction
c
      call KAFLCP (ngrid  ,nnf    ,scifri ,pfa    ,c      )
c
c     Computation of 4. dC/dh on time n+theta
c                    5. dR/dh on time n+theta
c                    6. dALFAb/dh on time n+theta
c
      call KABOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +             h1     ,h      ,q1     ,q      ,theta2 ,0.0    ,
     +             maxtab ,ntabm  ,ntab   ,table  ,
c                  <subsec>        <secth0>        <secth1>
     +             sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,
c                  <wfh0>          <wfh1>
     +             sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,
c                  <Af>            <O>             <Afh0>
     +             waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,
c                  <Afh1>          <Oh0>           <Oh1>
     +             sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,
c                                  <Asubsc>
     +             prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,
     +             c(1,2) ,alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )
c
      call KABOCH (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +             h1     ,h      ,q1     ,q      ,theta2 ,dh     ,
     +             maxtab ,ntabm  ,ntab   ,table  ,
c                  <subsec>        <secth0>        <secth1>
     +             sectc(1,1)     ,sectv(1,2)     ,sectv(1,3)     ,
c                  <wfh0>          <wfh1>
     +             sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,
c                  <Af'>           <O'>            <Afh0>
     +             afacc          ,oacc           ,sectv(1,4)     ,
c                  <Afh1>          <Oh0>           <Oh1>
     +             sectv(1,5)     ,sectv(1,6)     ,sectv(1,7)     ,
c                                  <Asubsc>
     +             prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,
     +             c(1,2) ,alfab  ,dcdh   ,drdh   ,dalfdh ,psltvr )
c
c     Computation of 7. dC/dQ on time n+theta
c
      call KADCDQ (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +             h1     ,h      ,q1     ,q      ,
     +             maxtab ,ntabm  ,ntab   ,table  ,theta2 ,0.0    ,
c                  <secth0>        <secth1>
     +             sectv(1,2)     ,sectv(1,3)     ,
c                  <wfh0>          <wfh1>
     +             sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,
c                  <Af>            <O>             <Afh0>
     +             waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,
c                  <Afh1>
     +             sectv(1,5)     ,
c                                  <Asubsc>
     +             prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,
     +             c(1,2) ,r(1,2) ,dcdq   ,psltvr )
c
      call KADCDQ (nbran  ,ngrid  ,branch ,typcr  ,bfrict ,bfricp ,
     +             h1     ,h      ,q1     ,q      ,
     +             maxtab ,ntabm  ,ntab   ,table  ,theta2 ,dh     ,
c                  <secth0>        <secth1>
     +             sectv(1,2)     ,sectv(1,3)     ,
c                  <wfh0>          <wfh1>
     +             sectc(1,2)     ,sectc(1,3)     ,grsize ,engpar ,
c                  <Af>            <O>             <Afh0>
     +             waoft(1,3)     ,waoft(1,6)     ,sectv(1,4)     ,
c                  <Afh1>
     +             sectv(1,5)     ,
c                                  <Asubsc>
     +             prslot         ,sectv(1,1)     ,c(1,1) ,r(1,1) ,
     +             c(1,2) ,r(1,2) ,dcdq   ,psltvr )
c
c
c     Computation of 8. extra resistance eta for every gridpoint
c                    9. deta/dh on time n+1/2
c
      call KAERES (nbran  ,ngrid  ,branch ,h1     ,h      ,
     +             theta2 ,maxtab ,ntabm  ,ntab   ,table  ,
     +             x      ,nexres ,exres  ,eta    ,detadh )
c
c     Computation of 10. flow area and width for structures on t=n+1.
c
      call KAAWST (nbran  ,ngrid  ,branch ,typcr  ,prslot ,maxlev ,
     +             nlev   ,hlev   ,wft    ,aft    ,h2     ,wf2    ,
     +             af2    ,grid   ,overlp ,arex   ,arexcn ,arexop ,
     +             juer   ,ker    ,psltvr )

      end

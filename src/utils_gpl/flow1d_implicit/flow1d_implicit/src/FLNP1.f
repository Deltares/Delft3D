      subroutine FLNP1(lkalm  ,nbran  ,ngrid  ,nnf    ,branch ,typcr  ,
     +                 bfrict ,bfricp ,h2     ,q2     ,maxlev ,nlev   ,
     +                 hlev   ,wft    ,aft    ,overlp ,arex   ,arexcn ,
     +                 arexop ,of     ,maxtab ,ntabm  ,ntab   ,table  ,
     +                 sectc  ,sectv  ,prslot ,psltvr ,waoft  ,grsize ,
     +                 engpar ,scifri ,pfa    ,juer   ,cp     ,rp     ,
     +                 afwfqs ,alfab  ,wtt,att ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLNP1 (FLow variables for time level N + 1)
c
c Module description: Calculate the different variables for time level
c                     n+1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 15 aft               P  -
c 37 afwfqs            P  -
c 38 alfab             P  -
c 17 arex              P  -
c 18 arexcn            P  -
c 19 arexop            P  -
c  8 bfricp            P  -
c  7 bfrict            P  -
c  5 branch            P  -
c 35 cp                P  -
c 31 engpar            P  -
c 30 grsize            P  -
c 13 hlev              P  -
c  9 hp                P  -
c 34 juer              P  -
c 39 ker               P  -
c  1 lkalm             I  -
c 11 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 21 maxtab            I  Maximum number of defined tables.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 12 nlev              P  -
c  4 nnf               I  Number of uncertain bed friction parameters.
c 23 ntab              P  -
c 22 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 20 of                P  -
c 16 overlp            P  -
c 33 pfa               P  -
c 27 prslot            P  -
c 28 psltvr            P  -
c 10 qp                P  -
c 36 rp                P  -
c 32 scifri            P  -
c 25 sectc             P  -
c 26 sectv             P  -
c 24 table             P  -
c  6 typcr             P  -
c 29 waoft             P  -
c 14 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flawn1  FLow Areas and Widths on time level N+1
c flkac2  FLow Kalman Chezy correction 2
c flqsec  FLow Q in SECtions
c flvnp1  FLow Variables on time level N+1
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flnp1.pf,v $
c Revision 1.10  1998/06/11  11:46:56  kuipe_j
c Estuary special integrated
c
c Revision 1.9  1998/04/10  09:18:24  kuipe_j
c total area recalculated
c
c Revision 1.8  1997/01/23  08:29:11  kuipe_j
c Make flow module robust
c
c Revision 1.7  1996/04/12  13:04:08  kuipe_j
c headers, minor changes
c
c Revision 1.6  1996/04/11  08:23:43  kuipe_j
c Kalman module added
c
c Revision 1.5  1995/09/22  10:01:56  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:58  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:31:15  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants 
c
      include '../include/sobdim.i'
c
c     Declaration of Parameters:
c
      integer maxtab, ntabm, juer, ker, nnf, ntab(4,maxtab)
      integer nbran, ngrid, maxlev, nlev(ngrid), branch(4,nbran)
      integer typcr(nbran), bfrict(3,nbran), arexcn(ngrid,2), arexop(2)
      integer scifri(ngrid)
      real    pfa(nnf)
      real    overlp, wft(ngrid,maxlev)
      real    of(ngrid,maxlev), aft(ngrid,maxlev)
      real    wtt(ngrid,maxlev), att(ngrid,maxlev)
      real    table(ntabm), arex(ngrid,4)
      real    cp(ngrid,4), rp(ngrid,4)
      real    grsize(4,ngrid,*), engpar(9)
      real    sectc(ngrid,3), sectv(ngrid,dmsecv)
      real    afwfqs(ngrid,8), waoft(ngrid,*) , alfab(ngrid)
      real    bfricp(6,ngrid), prslot(3,nbran), psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev)
      double precision h2(ngrid), q2(ngrid)
      logical lkalm
c
c     Calculate variables on time level n+1
c
c     Af an Wf
c
      call FLAWN1 (nbran   ,ngrid  ,branch     ,typcr      ,prslot     ,
     +             h2      ,maxlev ,nlev       ,hlev       ,wft        ,
     +             aft     ,overlp ,arex       ,arexcn     ,arexop     ,
     +             of      ,juer   ,waoft(1,1) ,waoft(1,3) ,waoft(1,6) ,
     +             wtt     ,att    ,waoft(1,4) ,
     +             ker     ,psltvr )
c
c     Chezy values, hydraulic radius, area and widths for each gridpoint
c     and section
c
      call FLVNP1 (nbran      ,ngrid      ,branch      ,typcr      ,
     +             bfrict     ,bfricp     ,h2          ,q2         ,
     +             maxlev     ,hlev       ,wft         ,maxtab     ,
c                                                       <subsec>
     +             ntabm      ,ntab       ,table       ,sectc(1,1) ,
c                  <secth0>    <secth1>    <Wf>         <Wfh0>
     +             sectv(1,2) ,sectv(1,3) ,waoft(1,1)  ,sectc(1,2) ,
c                  <Wfh1>                               <Af>
     +             sectc(1,3) ,grsize     ,engpar      ,waoft(1,3) ,
c                  <O>         <Afh0>      <Afh1>       <Oh0>
     +             waoft(1,6) ,sectv(1,4) ,sectv(1,5)  ,sectv(1,6) ,
c                  <Oh1>       <asubsc>
     +             sectv(1,7) ,sectv(1,1) ,prslot      ,psltvr     ,
     +             cp(1,1)    ,rp(1,1)    ,cp(1,2)     ,rp(1,2)    ,
     +             afwfqs(1,1),afwfqs(1,3),alfab       )
c
c     Q distribution for each gridpoint and section
c
      call FLQSEC(ngrid       ,q2          ,waoft(1,3) ,sectv(1,1) ,
     +            cp(1,1)     ,rp(1,1)     ,cp(1,2)    ,rp(1,2)    ,
     +            afwfqs(1,1) ,afwfqs(1,7) )
c
c     Correct Chezy values
c
      if ( lkalm ) then
         call FLKAC2 (ngrid  ,nnf    ,scifri ,pfa    ,sectv(1,1)   ,
     +                cp(1,1)        ,cp(1,2) )
      endif
c
      end

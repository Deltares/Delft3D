      subroutine FLINIA(inires ,nbran  ,ngrid  ,branch ,typcr  ,
     +                  h      ,maxlev ,nlev   ,hlev   ,prslot ,
     +                  overlp ,arex   ,arexcn ,arexop ,
     +                  wft    ,aft    ,wtt    ,att    ,of     ,
     +                  wf     ,af     ,wt     ,at     ,o      ,
     +                  juer   ,ker    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLINIA (FLow INItialisation Areas and widths)
c
c Module description: Routine FLINIA calculates the initial areas and
c                     widths.
c
c                     The following areas and widths are initially re-
c                     quired:
c
c                     Af, the flow area in case the salt module has been
c                     activated
c                     At on time level n, flow width Wf and total width
c                     Wt.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 21 af(ngrid)         IO Flow area at every grid point at time t(n+1)
c 16 aft               P  -
c 12 arex              P  -
c 13 arexcn            P  -
c 14 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c 23 at(ngrid)         IO Actual total area at every grid point.
c 18 att               P  -
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 h                 P  -
c  9 hlev              P  -
c  1 inires            I  True when no restart info of this module has
c                         been written before.
c 25 juer              P  -
c 26 ker               P  -
c  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  8 nlev              P  -
c 24 o                 P  -
c 19 of                P  -
c 11 overlp            P  -
c 10 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 27 psltvr(7,ngrid)   P  -  
c  5 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 20 wf(ngrid)         O  Actual flow width at every grid point.
c 15 wft               P  -
c 22 wt(ngrid)         I  Actual total width at every grid point.
c 17 wtt               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flarei  FLow correction for ARea Extra Initial
c flarex  FLow correction for ARea EXtra
c flcirc  FLow CIRCLe cross section
c flinaw  FLow INterpolate Area and Width
c flperi  FLow PERImeter
c flsedr  FLow SEDRedge branch
c indwgh  Compute INDex and WeiGHt factor
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flinia.pf,v $
c Revision 1.7  1999/03/15  15:50:06  kuipe_j
c tabs removed
c
c Revision 1.6  1997/01/23  08:29:08  kuipe_j
c Make flow module robust
c
c Revision 1.5  1995/09/22  10:01:47  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:56  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:31:10  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      logical  inires
      integer  ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),
     +         arexcn(ngrid,2), arexop(2)
      real     overlp, prslot(3,nbran),
     +         psltvr(7,ngrid),
     +         wft(ngrid,maxlev), aft(ngrid,maxlev),
     +         wtt(ngrid,maxlev), att(ngrid,maxlev),
     +         of (ngrid,maxlev), arex(ngrid,4),
     +         wf(ngrid), af(ngrid),
     +         wt(ngrid), at(ngrid),
     +         o (ngrid)
      double precision hlev(ngrid,maxlev), h(ngrid)
c
c     Declaration of local variables:
c
      integer iter, ilev, ibr, i1, i2, i
      logical lslot
      real    delA, dummy
      double precision wght
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Computation of
c          - actual total width wt(1:ngrid) at h=h(n)
c          - actual total area  at(1:ngrid) at h=h(n)
c          - actual flow  width wf(1:ngrid) at h=h(n)
c          - actual flow  area  af(1:ngrid) at h=h(n)
c
c     for given interval and weight factor.
c
      iter = 0
c
      do 40 ibr = 1, nbran
c
c        i1 = global grid point number at node n1
c        i2 = global grid point number at node n2
c
         i1 = branch (3,ibr)
         i2 = branch (4,ibr)
c
         lslot = int(prslot(1,ibr)) .eq. cslena
c
         if      ( typcr(ibr) .eq. ccrtab ) then
c
c           Arbitrary tabulated cross section for this branch
c
            do 10 i = i1, i2
c
c              Compute index (ilev) and weight factor (wght) w.r.t.
c              tables of widths.
c
               call INDWGH (ngrid  ,i      ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h(i)   ,ilev   ,wght   )
c
c              Compute of actual total width and area
c
               call FLINAW (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h(i)   ,ilev   ,wght   ,
     +                      wtt    ,att    ,
     +                      wt(i)  ,at(i)  ,psltvr )
c
c              Compute of actual total extra area
c
               if (arexop(2) .ne. 0) then
                  if (inires) then
                     call FLAREI (sngl(h(i))   ,arex(i,1)   ,arex(i,4) ,
     +                            overlp,delA  ,arexcn(i,2) )
                  else
                     call FLAREX (iter      ,arexop(2)  ,sngl(h(i)) ,
     +                            arex(i,1) ,arex(i,2)  ,arex(i,4)  ,
     +                            overlp    ,delA       ,arexcn(i,2),
     +                            dummy)
                  endif
                  at(i) = at(i) + delA
               endif
c
c              Compute actual flow width, area and wetted perimeter
c
               call FLINAW (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h(i)   ,ilev   ,wght   ,
     +                      wft    ,aft    ,
     +                      wf(i)  ,af(i)  ,psltvr )
c
c              Compute actual flow extra area
c
               if (arexop(1) .ne. 0) then
                  if (inires) then
                     call FLAREI (sngl(h(i))  ,arex(i,1)   ,arex(i,3) ,
     +                            overlp,delA  ,arexcn(i,1) )
                  else
                     call FLAREX (iter      ,arexop(1)  ,sngl(h(i)) ,
     +                            arex(i,1) ,arex(i,2)  ,arex(i,3)  ,
     +                            overlp    ,delA       ,arexcn(i,1),
     +                            dummy)
                  endif
                  af(i) = af(i) + delA
               endif
c
c              Compute wetted perimeter
c
               call FLPERI (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h(i)   ,ilev   ,
     +                      wft    ,wf(i)  ,
     +                      of     ,o(i)   ,psltvr )

   10       continue

         else if ( typcr(ibr) .eq. ccrcir ) then
c
c           Circle used as cross section in this branch
c
            do 20 i = i1, i2
c
c              reflev : hlev(i,1)
c              radius : wft(i,1)
c
c              Compute of actual total/flow width and area
c
               call FLCIRC (h(i)   ,
     +                      hlev(i,1) ,wft(i,1),juer   ,
     +                      at(i)     ,wt(i)   ,o(i)   ,ker    )
c
               af(i) = at(i)
               wf(i) = wt(i)
c
   20       continue

         else if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 30 i = i1, i2
               call FLSEDR (ibr      ,h(i)     ,hlev(i,1) ,hlev(i,2) ,
     +                      wft(i,1) ,wft(i,2) ,juer      ,at(i)     ,
     +                      wt(i)    ,o(i)     ,ker       )
c
               af(i) = at(i)
               wf(i) = wt(i)
c
   30       continue
         endif
   40 continue
      end

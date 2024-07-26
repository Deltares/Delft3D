      subroutine FLAWN1(nbran  ,ngrid  ,branch ,typcr  ,prslot ,
     +                  h2     ,maxlev ,nlev   ,hlev   ,wft    ,
     +                  aft    ,overlp ,arex   ,arexcn ,arexop ,
     +                  of     ,juer   ,wf     ,af     ,o      ,
     +                  wtt    ,att    ,at     ,ker    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAWN1 (FLow Areas and Widths on time level N+1)
c
c Module description: In subroutine FLAWN1 the flow area, flow width and
c                     the wetted perimeter will be computed on time
c                     level n+1.
c
c                     These variables are needed for usage in other
c                     modules on time level n+1.
c
c                     The flow areas and widths will be computed for
c                     gridpoint and the main section, sub section 1 and
c                     sub section 2 (if available). The shapes of the
c                     cross sections are given as a function (circle) or
c                     in tabular form. The same calculation routines
c                     will be used as in the flow iteration process
c                     except that a water level from time level n+1 will
c                     be passed as parameter.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 af(ngrid)         IO Flow area at every grid point at time t(n+1)
c 11 aft               P  -
c 13 arex              P  -
c 14 arexcn            P  -
c 15 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 h2                P  -
c  9 hlev              P  -
c 17 juer              P  -
c 21 ker               P  -
c  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  8 nlev              P  -
c 20 o                 P  -
c 16 of                P  -
c 12 overlp            P  -
c  5 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 22 psltvr            P  -
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 18 wf                P  -
c 10 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
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
c $Log: flawn1.pf,v $
c Revision 1.8  1999/03/15  15:49:32  kuipe_j
c tabs removed
c
c Revision 1.7  1998/04/10  09:18:23  kuipe_j
c total area recalculated
c
c Revision 1.6  1997/01/23  08:28:57  kuipe_j
c Make flow module robust
c
c Revision 1.5  1995/09/22  10:00:57  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:48  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:30:30  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),
     +         arexcn(ngrid,2), arexop(2)
      real     overlp
      real     arex(ngrid,4),
     +         wft(ngrid,maxlev), aft(ngrid,maxlev), of(ngrid,maxlev),
     +         wtt(ngrid,maxlev), att(ngrid,maxlev), at(ngrid),
     +         prslot(3,nbran), wf(ngrid), af(ngrid), o(ngrid),
     +         psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev), h2(ngrid)
c
c     Declaration of local variables:
c
      integer ilev, ibr, i1, i2, i, iter
      logical lslot
      real    delA, dummy
      double precision wght
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     Computation of
c          - actual flow  width     wf(1:ngrid) at h=h(n+1)
c          - actual flow  area      af(1:ngrid) at h=h(n+1)
c          - actual wetted perimeter o(1:ngrid) at h=h(n+1)
c
c     given interval and weight factor.
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
     +                      h2(i)  ,ilev   ,wght   )
c---
c
c             Compute actual total width and area
c
              call FLINAW (ngrid  ,i      ,lslot  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     h2(i)  ,ilev   ,wght   ,
     +                     wtt    ,att    ,
     +                     dummy  ,at(i)  ,psltvr )
c
c             Compute actual total extra area
c
              if (arexop(2) .ne. 0) then
                 call FLAREX (iter      ,arexop(2)  ,sngl(h2(i)),
     +                        arex(i,1) ,arex(i,2)  ,arex(i,4)  ,
     +                        overlp    ,delA       ,arexcn(i,2),
     +                        dummy     )
c
c                Linearization 
c
                 at(i)    = at(i) + delA
              endif
c---
c
c              Compute of actual flow width and area
c
               call FLINAW (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h2(i)  ,ilev   ,wght   ,
     +                      wft    ,aft    ,
     +                      wf(i)  ,af(i)  ,psltvr )
c
c              Compute extra area
c
               if (arexop(1) .ne. 0) then
                  iter = 0
                  call FLAREX (iter      ,arexop(1)  ,sngl(h2(i)),
     +                         arex(i,1) ,arex(i,2)  ,arex(i,3)  ,
     +                         overlp    ,delA       ,arexcn(i,1),
     +                         dummy)
                  af(i) = af(i) + delA
               endif
c
c              Compute of actual wetted perimeter
c
               call FLPERI (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      h2(i)  ,ilev   ,
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
c              Compute of actual flow width and area
c
               call FLCIRC (h2(i)     ,
     +                      hlev(i,1) ,wft(i,1),juer   ,
     +                      af(i)     ,wf(i)   ,o(i)   ,ker    )
   20       continue

         else if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 30 i = i1, i2
               call FLSEDR (ibr      ,h2(i)    ,hlev(i,1) ,hlev(i,2) ,
     +                      wft(i,1) ,wft(i,2) ,juer      ,af(i)     ,
     +                      wf(i)    ,o(i)     ,ker       )
   30       continue
         endif
   40 continue
      end

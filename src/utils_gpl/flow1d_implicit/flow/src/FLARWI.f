      subroutine FLARWI(iter   ,nbran  ,ngrid  ,branch ,typcr  ,
     +                  prslot ,h1     ,h      ,maxlev ,nlev   ,
     +                  hlev   ,wft    ,aft    ,wtt    ,att    ,
     +                  juer   ,overlp ,arex   ,arexcn ,arexop ,
     +                  wf     ,af     ,wt     ,at     ,ker    ,
     +                  theta2 ,w2     ,wfex   ,wtex   ,psltvr )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLARWI (FLow AReas and WIdths)
c
c Module description: In subroutine FLARWI the total area, total width,
c                     the flow area and the flow width will be computed.
c
c                     For the computation of the matrix coefficients
c                     A,B,C,D and E are the following parameters are
c                     needed in each grid point:
c
c                     -      total area At
c                     -      total width Wt
c                     -      flow area Af
c                     -      flow width Wf
c
c                     The total area will be computed for each
c                     gridpoint. The flow areas will be computed for
c                     gridpoint and the main section, sub section 1 and
c                     sub section 2 (if available). The shapes of the
c                     cross sections are given as a function (circle) or
c                     in tabular form.
c
c                     If a cross section is defined as a circle, the
c                     hydraulic parameters from the list above follow
c                     immediately from subroutine FLCIRC for a given
c                     water level h. The bottom is defined in hlev(i,1)
c                     and the radius is defined in wft(i,1).
c
c                     Arbitrary cross sections can be defined in a tabu-
c                     lar form. For a given water level h, an interpo-
c                     lation will be made in order to compute the hy-
c                     draulic parameters defined above.
c
c                     Sedredge branches are defined by two bed levels
c                     and two widths. In this case the left channel will
c                     be defined as the main section and the right chan-
c                     nel will be defined as sub section 1. The cross
c                     section dimensions are defined in the following
c                     way:
c
c                     hlev(i,1)    Bed level of left channel
c                     wft(i,1)     Width of left channel
c
c                     hlev(i,2)    Bed level of right channel
c                     wft(i,2)     Width of right channel
c
c                     The function FLBOTT computes the lowest bed level
c                     of each grid point for this type of cross section.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 af(ngrid)         IO Flow area at every grid point at time t(n+1)
c 32 afexp(ngrid)      IO Extra flow area due to zomerkaden in every
c                         grid point on previous iteration step.
c 13 aft               P  -
c 18 arex              P  -
c 19 arexcn            P  -
c 20 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c 24 at(ngrid)         IO Actual total area at every grid point.
c 33 atexp(ngrid)      IO Extra total area due to zomerkaden in every
c                         grid point on previous iteration step.
c 15 att               P  -
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  7 h1(ngrid)         I  Water level in every grid point at time t(n).
c  8 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 11 hlev              P  -
c  1 iter              I  Iteration step.
c 16 juer              P  -
c 25 ker               P  -
c  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 10 nlev              P  -
c 17 overlp            P  -
c  6 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 35 psltvr            p  -
c 26 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  5 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 27 w2                P  -
c 21 wf                P  -
c 30 wfex(ngrid)       IO Extra flow width due to zomerkaden in every
c                         grid point.
c 28 wfexp(ngrid)      O  Extra flow width due to zomerkaden in every
c                         grid point on previous iteration step.
c 12 wft               P  -
c 23 wt                P  -
c 31 wtex(ngrid)       IO Extra total width due to zomerkaden in every
c                         grid point.
c 29 wtexp(ngrid)      O  Extra total width due to zomerkaden in every
c                         grid point on previous iteration step.
c 14 wtt               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flarex  FLow correction for ARea EXtra
c flcirc  FLow CIRCLe cross section
c flinaw  FLow INterpolate Area and Width
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
c $Log: flarwi.pf,v $
c Revision 1.8  1997/05/26  07:38:52  kuipe_j
c linearizatiom summerdikes introduced
c
c Revision 1.7  1997/01/23  08:28:54  kuipe_j
c Make flow module robust
c
c Revision 1.6  1996/04/11  08:23:06  kuipe_j
c Kalman module added
c
c Revision 1.5  1995/09/22  10:00:48  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:45  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:30:25  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer  iter, ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),
     +         arexcn(ngrid,2), arexop(2)
      real     overlp, theta2 , prslot(3,nbran), psltvr(7,ngrid),
     +         wft(ngrid,maxlev), aft(ngrid,maxlev),
     +         wtt(ngrid,maxlev), att(ngrid,maxlev),
     +         arex(ngrid,4),
     +         wf(ngrid), af(ngrid),
     +         wt(ngrid), at(ngrid),
     +         w2(ngrid),
     +         wfex(ngrid), wtex(ngrid)
      double precision hlev(ngrid,maxlev), h(ngrid), h1(ngrid)
c
c     Declaration of local variables:
c
      integer ilev, ibr, i1, i2, i
      logical lslot
      real    dummy, delA, dummy2, delW
      double precision wght, hact
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Computation of
c          - actual total width wt(1:ngrid) at h=h(n+1  )
c          - actual total area  at(1:ngrid) at h=h(n+1  )
c          - actual flow  width wf(1:ngrid) at h=h(n+theta2)
c          - actual flow  area  af(1:ngrid) at h=h(n+theta2)
c          - specially for a pseudo time technique
c            actual total width w2(1:ngrid) at h=h(n+theta2)
c
c     for given interval and weight factor.
c
c     Remark:
c     ------
c     Notice that flow widths and total widths are defined for different
c     water levels
c     Flow widths are defined for water level h=h(n+theta2)
c     Total widths are defined for water level h=h(n+1)
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
c          Arbitrary tabulated cross section for this branch
c
           do 10 i = i1, i2
c
c             wfex and wtex temporarily set to zero
c
              wfex(i) = 0.0
              wtex(i) = 0.0
c
c             Compute index (ilev) and weight factor (wght) w.r.t.
c             tables of widths.
c
              call INDWGH (ngrid  ,i      ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     h(i)   ,ilev   ,wght   )
c
c             Compute actual total width and area
c
              call FLINAW (ngrid  ,i      ,lslot  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     h(i)   ,ilev   ,wght   ,
     +                     wtt    ,att    ,
     +                     wt(i)  ,at(i)  ,psltvr )
c
c             Compute actual total extra area
c
              if (arexop(2) .ne. 0) then
                 call FLAREX (iter      ,arexop(2)  ,sngl(h(i)) ,
     +                        arex(i,1) ,arex(i,2)  ,arex(i,4)  ,
     +                        overlp    ,delA       ,arexcn(i,2),
     +                        delW      )
c
c                Linearization 
c
                 wtex (i) = delW 
                 at(i)    = at(i) + delA
              endif
c
              hact = theta2*h(i)+(1.-theta2)*h1(i)
c
c              Compute index (ilev) and weight factor (wght) w.r.t.
c              tables of widths.
c
              call INDWGH (ngrid  ,i      ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     hact   ,ilev   ,wght   )
c
c              Compute of actual flow width and area
c
              call FLINAW (ngrid  ,i      ,lslot  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     hact   ,ilev   ,wght   ,
     +                     wft    ,aft    ,
     +                     wf(i)  ,af(i)  ,psltvr )
c
c              Compute extra area
c
              if (arexop(1) .ne. 0) then
                 call FLAREX (iter      ,arexop(1)  ,sngl(hact) ,
     +                        arex(i,1) ,arex(i,2)  ,arex(i,3)  ,
     +                        overlp    ,delA       ,arexcn(i,1),
     +                        delW      )
c
c                Linearization 
c
                 wfex (i) = delW
                 af(i)    = af(i) + delA
              endif
c
c              Compute of actual total width at level n+theta2
c
              call FLINAW (ngrid  ,i      ,lslot  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     hact   ,ilev   ,wght   ,
     +                     wtt    ,att    ,
     +                     w2(i)  ,dummy  ,psltvr )

   10      continue

        else if ( typcr(ibr) .eq. ccrcir ) then
c
c          Circle used as cross section in this branch
c
           do 20 i = i1, i2
c
c             reflev : hlev(i,1)
c             radius : wft(i,1)
c
c             Compute of actual total width and area
c
              call FLCIRC (h(i)   ,
     +                     hlev(i,1) ,wft(i,1),juer  ,
     +                     at(i)     ,wt(i)   ,dummy ,ker)

c
c             Compute of actual flow width and area
c
              call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,
     +                      hlev(i,1) ,wft(i,1),juer  ,
     +                      af(i)     ,wf(i)   ,dummy ,ker)
c
c             Compute of actual total width at level n+theta2
c
              call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,
     +                      hlev(i,1) ,wft(i,1),juer  ,
     +                      dummy2    ,w2(i)   ,dummy ,ker)

   20      continue

        else if ( typcr(ibr) .eq. ccrsed ) then
c
c          Sedredge branch
c
           do 30 i = i1, i2
              call FLSEDR (ibr      ,h(i)     ,hlev(i,1) ,hlev(i,2) ,
     +                     wft(i,1) ,wft(i,2) ,juer      ,at(i)     ,
     +                     wt(i)    ,dummy    ,ker       )
              hact = theta2*h(i)+(1.-theta2)*h1(i)
              call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,
     +                     wft(i,1) ,wft(i,2) ,juer      ,af(i)     ,
     +                     wf(i)    ,dummy    ,ker       )
              call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,
     +                     wft(i,1) ,wft(i,2) ,juer      ,dummy2    ,
     +                     w2(i)    ,dummy    ,ker       )
   30       continue
        endif
   40 continue
      end

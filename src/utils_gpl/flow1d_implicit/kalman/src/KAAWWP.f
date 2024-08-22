      subroutine KAAWWP(nbran  ,ngrid  ,branch ,typcr  ,prslot ,
     +                  h1     ,h      ,maxlev ,nlev   ,hlev   ,
     +                  wft    ,aft    ,of     ,juer   ,theta2 ,
     +                  wf     ,af     ,afacc  ,oacc   ,dwfdh  ,
     +                  overlp ,arex   ,arexcn ,arexop ,ker    ,
     +                  psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAAWWP (KAlman Areas Widths Wetted Perimeter)
c
c Module description: Calculate the flow area, flow width and wetted
c                     perimeter for an increment to the waterlevel at
c                     t(n+1/2). Also calculate the derivative of the flow
c                     width by numerical differentiation.
c
c                     Three different types of shapes are possible: the
c                     table, the circle and the sedredge cross section.
c                     Arbitrary cross sections are defined in tabular form.
c                     For a given water level h, an interpolation will be
c                     made in order to compute the hydraulic parameters
c                     defined above.
c
c                     If a cross section is defined as a circle, the
c                     hydraulic parameters follow immediately from sub-
c                     routine FLCIRC for a given water level h.
c
c                     Sedredge branches are defined by two bed levels and
c                     two widths. In this case the left channel will be
c                     defined as the main section and the right channel
c                     will be defined as sub section 1.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 af(ngrid)         O  Flow area at every grid point at time t(n+1)
c 18 afacc(ngrid)      O  Flow area in every grid point i on time
c                         n+1/2+dh.
c 12 aft               P  -
c 22 arex              P  -
c 23 arexcn            P  -
c 24 arexop(2)         I  Option to calculate flow(1) and total(2) area.
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
c 20 dwfdh(ngrid)      O  Derivative of flow width to waterlevel in every
c                         grid point i on time n+1/2 (dWf/dh).
c  6 h1(ngrid)         I  Water level in every grid point at time t(n).
c  7 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 10 hlev              P  -
c 14 juer              P  -
c 25 ker               P  -
c  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  9 nlev              P  -
c 19 oacc              P  -
c 13 of                P  -
c 21 overlp            P  -
c  5 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 26 psltvr            P  -
c 15 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 16 wf(ngrid)         I  Actual flow width at every grid point.
c 11 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flarex  FLow correction for ARea EXtra
c flcirc  FLow CIRCLe cross section
c flinaw  FLow INterpolate Area and Width
c flperi
c flsedr  FLow SEDRedge branch
c indwgh  Compute INDex and WeiGHt factor
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaawwp.pf,v $
c Revision 1.4  1999/03/15  15:51:35  kuipe_j
c tabs removed
c
c Revision 1.3  1997/01/23  08:29:31  kuipe_j
c Make flow module robust
c
c Revision 1.2  1996/04/12  13:04:38  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:15  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),
     +         arexcn(ngrid,2), arexop(2)
      real     overlp, theta2
      real     wft(ngrid,maxlev), aft(ngrid,maxlev), of(ngrid,maxlev),
     +         prslot(3,nbran), wf(ngrid), afacc(ngrid), oacc(ngrid),
     +         af(ngrid), dwfdh(ngrid), arex(ngrid,4), psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev), h1(ngrid), h(ngrid)
c
c     Declaration of local variables:
c
      integer ilev, ibr, i1, i2, i, iter
      logical lslot
      real    hi, hiacc, dh, wfacc, dela1, dela2, delw,
     +        af1, af2, dum
      double precision wght
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      parameter (dh = 0.001)
      iter = 0
c
c     Computation of
c          - actual flow  width     wf(1:ngrid) at h=h(n+1/2)+dh
c          - actual flow  area      af(1:ngrid) at h=h(n+1/2)+dh
c          - actual wetted perimeter o(1:ngrid) at h=h(n+1/2)+dh
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
c              tables of widths on n+theta2
c
               hi    = theta2*h(i)+(1.-theta2)*h1(i)
               hiacc = hi + dh
c
               call INDWGH (ngrid   ,i      ,
     +                      maxlev  ,nlev   ,hlev   ,
     +                      dble(hi),ilev   ,wght   )
c
c              Compute actual flow area on n+theta2
c
               call FLINAW (ngrid   ,i      ,lslot  ,
     +                      maxlev  ,nlev   ,hlev   ,
     +                      dble(hi),ilev   ,wght   ,
     +                      wft     ,aft    ,
     +                      dum     ,af1    ,psltvr )
c
c              Compute index (ilev) and weight factor (wght) w.r.t.
c              tables of widths on n+theta2+dh
c
               call INDWGH (ngrid       ,i      ,
     +                      maxlev      ,nlev   ,hlev   ,
     +                      dble(hiacc) ,ilev   ,wght   )
c
c              Compute actual flow width and area on (n+theta2)+dh
c
               call FLINAW (ngrid   ,i      ,lslot  ,
     +                      maxlev  ,nlev   ,hlev   ,
     +                      dble(hiacc)   ,ilev   ,wght   ,
     +                      wft     ,aft    ,
     +                      wfacc   ,af2    ,psltvr )

               if (arexop(1) .ne. 0) then
c
c                 Calculate extra area due to summerdikes
c                 At time level n+theta2
c
                  call FLAREX (iter      ,arexop(1)  ,hi  ,
     +                         arex(i,1) ,arex(i,2)  ,arex(i,3) ,
     +                         overlp    ,delA1      ,arexcn(i,1),
     +                         delW      )
c
c                 At time level n+theta2+dh
c
                  call FLAREX (iter      ,arexop(1)  ,hiacc,
     +                         arex(i,1) ,arex(i,2)  ,arex(i,3) ,
     +                         overlp    ,delA2      ,arexcn(i,1),
     +                         delW      )
c
c                 In case of summerdikes A-flow at n+theta2 must be
c                 recalculated to avoid the effect of relaxation
c                 in A-flow
c
                  af   (i) = af1 + delA1
                  afacc(i) = af2 + delA2
               else
                  afacc(i) = af2
               endif
c
c              Compute of actual wetted perimeter
c
               call FLPERI (ngrid  ,i      ,lslot  ,
     +                      maxlev ,nlev   ,hlev   ,
     +                      dble(hiacc)    ,ilev   ,
     +                      wft    ,wfacc  ,
     +                      of     ,oacc(i),psltvr )
c
               dwfdh(i) = (wfacc - wf(i)) / dh
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
               hi = theta2*h(i)+(1.-theta2)*h1(i) + dh
c
               call FLCIRC (hi        ,
     +                      hlev(i,1) ,wft(i,1) ,juer    ,
     +                      afacc(i)  ,wfacc    ,oacc(i) ,ker    )
c
               dwfdh(i) = (wfacc - wf(i)) / dh
   20       continue

         else if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 30 i = i1, i2
c
               hi = theta2*h(i)+(1.-theta2)*h1(i) + dh
c
               call FLSEDR (ibr       ,hi        ,hlev(i,1),
     +                      hlev(i,2) ,wft(i,1)  ,wft(i,2) ,juer      ,
     +                      afacc(i)  ,wfacc     ,oacc(i)  ,ker       )
c
               dwfdh(i) = (wfacc - wf(i)) / dh
   30       continue
         endif
   40 continue
      end

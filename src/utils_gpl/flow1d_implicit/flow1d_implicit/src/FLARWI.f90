subroutine FLARWI(iter   ,nbran  ,ngrid  ,branch ,typcr  ,&
&prslot ,h1     ,h      ,maxlev ,nlev   ,&
&hlev   ,wft    ,aft    ,wtt    ,att    ,&
&juer   ,overlp ,arex   ,arexcn ,arexop ,&
&wf     ,af     ,wt     ,at     ,ker    ,&
&theta2 ,w2     ,wfex   ,wtex   ,psltvr )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLARWI (FLow AReas and WIdths)
!
! Module description: In subroutine FLARWI the total area, total width,
!                     the flow area and the flow width will be computed.
!
!                     For the computation of the matrix coefficients
!                     A,B,C,D and E are the following parameters are
!                     needed in each grid point:
!
!                     -      total area At
!                     -      total width Wt
!                     -      flow area Af
!                     -      flow width Wf
!
!                     The total area will be computed for each
!                     gridpoint. The flow areas will be computed for
!                     gridpoint and the main section, sub section 1 and
!                     sub section 2 (if available). The shapes of the
!                     cross sections are given as a function (circle) or
!                     in tabular form.
!
!                     If a cross section is defined as a circle, the
!                     hydraulic parameters from the list above follow
!                     immediately from subroutine FLCIRC for a given
!                     water level h. The bottom is defined in hlev(i,1)
!                     and the radius is defined in wft(i,1).
!
!                     Arbitrary cross sections can be defined in a tabu-
!                     lar form. For a given water level h, an interpo-
!                     lation will be made in order to compute the hy-
!                     draulic parameters defined above.
!
!                     Sedredge branches are defined by two bed levels
!                     and two widths. In this case the left channel will
!                     be defined as the main section and the right chan-
!                     nel will be defined as sub section 1. The cross
!                     section dimensions are defined in the following
!                     way:
!
!                     hlev(i,1)    Bed level of left channel
!                     wft(i,1)     Width of left channel
!
!                     hlev(i,2)    Bed level of right channel
!                     wft(i,2)     Width of right channel
!
!                     The function FLBOTT computes the lowest bed level
!                     of each grid point for this type of cross section.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 af(ngrid)         IO Flow area at every grid point at time t(n+1)
! 32 afexp(ngrid)      IO Extra flow area due to zomerkaden in every
!                         grid point on previous iteration step.
! 13 aft               P  -
! 18 arex              P  -
! 19 arexcn            P  -
! 20 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
! 24 at(ngrid)         IO Actual total area at every grid point.
! 33 atexp(ngrid)      IO Extra total area due to zomerkaden in every
!                         grid point on previous iteration step.
! 15 att               P  -
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  7 h1(ngrid)         I  Water level in every grid point at time t(n).
!  8 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 11 hlev              P  -
!  1 iter              I  Iteration step.
! 16 juer              P  -
! 25 ker               P  -
!  9 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 10 nlev              P  -
! 17 overlp            P  -
!  6 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 35 psltvr            p  -
! 26 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  5 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 27 w2                P  -
! 21 wf                P  -
! 30 wfex(ngrid)       IO Extra flow width due to zomerkaden in every
!                         grid point.
! 28 wfexp(ngrid)      O  Extra flow width due to zomerkaden in every
!                         grid point on previous iteration step.
! 12 wft               P  -
! 23 wt                P  -
! 31 wtex(ngrid)       IO Extra total width due to zomerkaden in every
!                         grid point.
! 29 wtexp(ngrid)      O  Extra total width due to zomerkaden in every
!                         grid point on previous iteration step.
! 14 wtt               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flarex  FLow correction for ARea EXtra
! flcirc  FLow CIRCLe cross section
! flinaw  FLow INterpolate Area and Width
! flsedr  FLow SEDRedge branch
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flarwi.pf,v $
! Revision 1.8  1997/05/26  07:38:52  kuipe_j
! linearizatiom summerdikes introduced
!
! Revision 1.7  1997/01/23  08:28:54  kuipe_j
! Make flow module robust
!
! Revision 1.6  1996/04/11  08:23:06  kuipe_j
! Kalman module added
!
! Revision 1.5  1995/09/22  10:00:48  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:45  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:30:25  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer  iter, ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   real     overlp, theta2 , prslot(3,nbran), psltvr(7,ngrid),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &wtt(ngrid,maxlev), att(ngrid,maxlev),&
   &arex(ngrid,4),&
   &wf(ngrid), af(ngrid),&
   &wt(ngrid), at(ngrid),&
   &w2(ngrid),&
   &wfex(ngrid), wtex(ngrid)
   double precision hlev(ngrid,maxlev), h(ngrid), h1(ngrid)
!
!     Declaration of local variables:
!
   integer ilev, ibr, i1, i2, i
   logical lslot
   real    dummy, delA, dummy2, delW
   double precision wght, hact
!
!     FM1DIMP2DO: remove debug
   real dbg1
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Computation of
!          - actual total width wt(1:ngrid) at h=h(n+1  )
!          - actual total area  at(1:ngrid) at h=h(n+1  )
!          - actual flow  width wf(1:ngrid) at h=h(n+theta2)
!          - actual flow  area  af(1:ngrid) at h=h(n+theta2)
!          - specially for a pseudo time technique
!            actual total width w2(1:ngrid) at h=h(n+theta2)
!
!     for given interval and weight factor.
!
!     Remark:
!     ------
!     Notice that flow widths and total widths are defined for different
!     water levels
!     Flow widths are defined for water level h=h(n+theta2)
!     Total widths are defined for water level h=h(n+1)
!
   do 40 ibr = 1, nbran
!
!        i1 = global grid point number at node n1
!        i2 = global grid point number at node n2
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
      lslot = int(prslot(1,ibr)) .eq. cslena
!
      if      ( typcr(ibr) .eq. ccrtab ) then
!
!          Arbitrary tabulated cross section for this branch
!
         do 10 i = i1, i2
!
!             wfex and wtex temporarily set to zero
!
            wfex(i) = 0.0
            wtex(i) = 0.0
!
!             Compute index (ilev) and weight factor (wght) w.r.t.
!             tables of widths.
!
            call INDWGH (ngrid  ,i      ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,wght   )
!
!             FM1DIMP2DO: remove debug
!              write(42,*) ilev, h(i)
!
!             Compute actual total width and area
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,wght   ,&
            &wtt    ,att    ,&
            &wt(i)  ,at(i)  ,psltvr )
!
!             Compute actual total extra area
!
            if (arexop(2) .ne. 0) then
               call FLAREX (iter      ,arexop(2)  ,real(h(i)) ,&
               &arex(i,1) ,arex(i,2)  ,arex(i,4)  ,&
               &overlp    ,delA       ,arexcn(i,2),&
               &delW      )
!
!                Linearization
!
               wtex (i) = delW
               at(i)    = at(i) + delA
            endif
!
            hact = theta2*h(i)+(1.-theta2)*h1(i)
!
!              Compute index (ilev) and weight factor (wght) w.r.t.
!              tables of widths.
!
            call INDWGH (ngrid  ,i      ,&
            &maxlev ,nlev   ,hlev   ,&
            &hact   ,ilev   ,wght   )
!
!             FM1DIMP2DO: remove debug
!              write(42,*) ilev, hact
!
!              Compute of actual flow width and area
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &hact   ,ilev   ,wght   ,&
            &wft    ,aft    ,&
            &wf(i)  ,af(i)  ,psltvr )
!
!             FM1DIMP2DO: remove debug
!              write(42,*) af(i)
            dbg1=af(i)
!
!              Compute extra area
!
            if (arexop(1) .ne. 0) then
               call FLAREX (iter      ,arexop(1)  ,real(hact) ,&
               &arex(i,1) ,arex(i,2)  ,arex(i,3)  ,&
               &overlp    ,delA       ,arexcn(i,1),&
               &delW      )
!
!                Linearization
!
               wfex (i) = delW
               af(i)    = af(i) + delA
            endif
!
!              Compute of actual total width at level n+theta2
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &hact   ,ilev   ,wght   ,&
            &wtt    ,att    ,&
            &w2(i)  ,dummy  ,psltvr )

10       continue

      else if ( typcr(ibr) .eq. ccrcir ) then
!
!          Circle used as cross section in this branch
!
         do 20 i = i1, i2
!
!             reflev : hlev(i,1)
!             radius : wft(i,1)
!
!             Compute of actual total width and area
!
            call FLCIRC (h(i)   ,&
            &hlev(i,1) ,wft(i,1),juer  ,&
            &at(i)     ,wt(i)   ,dummy ,ker)

!
!             Compute of actual flow width and area
!
            call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,&
            &hlev(i,1) ,wft(i,1),juer  ,&
            &af(i)     ,wf(i)   ,dummy ,ker)
!
!             Compute of actual total width at level n+theta2
!
            call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,&
            &hlev(i,1) ,wft(i,1),juer  ,&
            &dummy2    ,w2(i)   ,dummy ,ker)

20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!          Sedredge branch
!
         do 30 i = i1, i2
            call FLSEDR (ibr      ,h(i)     ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,at(i)     ,&
            &wt(i)    ,dummy    ,ker       )
            hact = theta2*h(i)+(1.-theta2)*h1(i)
            call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,af(i)     ,&
            &wf(i)    ,dummy    ,ker       )
            call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,dummy2    ,&
            &w2(i)    ,dummy    ,ker       )
30       continue
      endif
40 continue
end

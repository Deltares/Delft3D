subroutine KAAWWP(nbran  ,ngrid  ,branch ,typcr  ,prslot ,&
&h1     ,h      ,maxlev ,nlev   ,hlev   ,&
&wft    ,aft    ,of     ,juer   ,theta2 ,&
&wf     ,af     ,afacc  ,oacc   ,dwfdh  ,&
&overlp ,arex   ,arexcn ,arexop ,ker    ,&
&psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAAWWP (KAlman Areas Widths Wetted Perimeter)
!
! Module description: Calculate the flow area, flow width and wetted
!                     perimeter for an increment to the waterlevel at
!                     t(n+1/2). Also calculate the derivative of the flow
!                     width by numerical differentiation.
!
!                     Three different types of shapes are possible: the
!                     table, the circle and the sedredge cross section.
!                     Arbitrary cross sections are defined in tabular form.
!                     For a given water level h, an interpolation will be
!                     made in order to compute the hydraulic parameters
!                     defined above.
!
!                     If a cross section is defined as a circle, the
!                     hydraulic parameters follow immediately from sub-
!                     routine FLCIRC for a given water level h.
!
!                     Sedredge branches are defined by two bed levels and
!                     two widths. In this case the left channel will be
!                     defined as the main section and the right channel
!                     will be defined as sub section 1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 af(ngrid)         O  Flow area at every grid point at time t(n+1)
! 18 afacc(ngrid)      O  Flow area in every grid point i on time
!                         n+1/2+dh.
! 12 aft               P  -
! 22 arex              P  -
! 23 arexcn            P  -
! 24 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 20 dwfdh(ngrid)      O  Derivative of flow width to waterlevel in every
!                         grid point i on time n+1/2 (dWf/dh).
!  6 h1(ngrid)         I  Water level in every grid point at time t(n).
!  7 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 10 hlev              P  -
! 14 juer              P  -
! 25 ker               P  -
!  8 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  9 nlev              P  -
! 19 oacc              P  -
! 13 of                P  -
! 21 overlp            P  -
!  5 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 26 psltvr            P  -
! 15 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 16 wf(ngrid)         I  Actual flow width at every grid point.
! 11 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flarex  FLow correction for ARea EXtra
! flcirc  FLow CIRCLe cross section
! flinaw  FLow INterpolate Area and Width
! flperi
! flsedr  FLow SEDRedge branch
! indwgh  Compute INDex and WeiGHt factor
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaawwp.pf,v $
! Revision 1.4  1999/03/15  15:51:35  kuipe_j
! tabs removed
!
! Revision 1.3  1997/01/23  08:29:31  kuipe_j
! Make flow module robust
!
! Revision 1.2  1996/04/12  13:04:38  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:15  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   real     overlp, theta2
   real     wft(ngrid,maxlev), aft(ngrid,maxlev), of(ngrid,maxlev),&
   &prslot(3,nbran), wf(ngrid), afacc(ngrid), oacc(ngrid),&
   &af(ngrid), dwfdh(ngrid), arex(ngrid,4), psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h1(ngrid), h(ngrid)
!
!     Declaration of local variables:
!
   integer ilev, ibr, i1, i2, i, iter
   logical lslot
   real    hi, hiacc, dh, wfacc, dela1, dela2, delw,&
   &af1, af2, dum
   double precision wght
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   parameter (dh = 0.001)
   iter = 0
!
!     Computation of
!          - actual flow  width     wf(1:ngrid) at h=h(n+1/2)+dh
!          - actual flow  area      af(1:ngrid) at h=h(n+1/2)+dh
!          - actual wetted perimeter o(1:ngrid) at h=h(n+1/2)+dh
!
!     given interval and weight factor.
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
!           Arbitrary tabulated cross section for this branch
!
         do 10 i = i1, i2
!
!              Compute index (ilev) and weight factor (wght) w.r.t.
!              tables of widths on n+theta2
!
            hi    = theta2*h(i)+(1.-theta2)*h1(i)
            hiacc = hi + dh
!
            call INDWGH (ngrid   ,i      ,&
            &maxlev  ,nlev   ,hlev   ,&
            &dble(hi),ilev   ,wght   )
!
!              Compute actual flow area on n+theta2
!
            call FLINAW (ngrid   ,i      ,lslot  ,&
            &maxlev  ,nlev   ,hlev   ,&
            &dble(hi),ilev   ,wght   ,&
            &wft     ,aft    ,&
            &dum     ,af1    ,psltvr )
!
!              Compute index (ilev) and weight factor (wght) w.r.t.
!              tables of widths on n+theta2+dh
!
            call INDWGH (ngrid       ,i      ,&
            &maxlev      ,nlev   ,hlev   ,&
            &dble(hiacc) ,ilev   ,wght   )
!
!              Compute actual flow width and area on (n+theta2)+dh
!
            call FLINAW (ngrid   ,i      ,lslot  ,&
            &maxlev  ,nlev   ,hlev   ,&
            &dble(hiacc)   ,ilev   ,wght   ,&
            &wft     ,aft    ,&
            &wfacc   ,af2    ,psltvr )

            if (arexop(1) .ne. 0) then
!
!                 Calculate extra area due to summerdikes
!                 At time level n+theta2
!
               call FLAREX (iter      ,arexop(1)  ,hi  ,&
               &arex(i,1) ,arex(i,2)  ,arex(i,3) ,&
               &overlp    ,delA1      ,arexcn(i,1),&
               &delW      )
!
!                 At time level n+theta2+dh
!
               call FLAREX (iter      ,arexop(1)  ,hiacc,&
               &arex(i,1) ,arex(i,2)  ,arex(i,3) ,&
               &overlp    ,delA2      ,arexcn(i,1),&
               &delW      )
!
!                 In case of summerdikes A-flow at n+theta2 must be
!                 recalculated to avoid the effect of relaxation
!                 in A-flow
!
               af   (i) = af1 + delA1
               afacc(i) = af2 + delA2
            else
               afacc(i) = af2
            endif
!
!              Compute of actual wetted perimeter
!
            call FLPERI (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &dble(hiacc)    ,ilev   ,&
            &wft    ,wfacc  ,&
            &of     ,oacc(i),psltvr )
!
            dwfdh(i) = (wfacc - wf(i)) / dh
10       continue

      else if ( typcr(ibr) .eq. ccrcir ) then
!
!           Circle used as cross section in this branch
!
         do 20 i = i1, i2
!
!              reflev : hlev(i,1)
!              radius : wft(i,1)
!
!              Compute of actual flow width and area
!
            hi = theta2*h(i)+(1.-theta2)*h1(i) + dh
!
            call FLCIRC (hi        ,&
            &hlev(i,1) ,wft(i,1) ,juer    ,&
            &afacc(i)  ,wfacc    ,oacc(i) ,ker    )
!
            dwfdh(i) = (wfacc - wf(i)) / dh
20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 30 i = i1, i2
!
            hi = theta2*h(i)+(1.-theta2)*h1(i) + dh
!
            call FLSEDR (ibr       ,hi        ,hlev(i,1),&
            &hlev(i,2) ,wft(i,1)  ,wft(i,2) ,juer      ,&
            &afacc(i)  ,wfacc     ,oacc(i)  ,ker       )
!
            dwfdh(i) = (wfacc - wf(i)) / dh
30       continue
      endif
40 continue
end

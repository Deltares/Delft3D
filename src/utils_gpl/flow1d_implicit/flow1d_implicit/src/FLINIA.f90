subroutine FLINIA(inires ,nbran  ,ngrid  ,branch ,typcr  ,&
&h      ,maxlev ,nlev   ,hlev   ,prslot ,&
&overlp ,arex   ,arexcn ,arexop ,&
&wft    ,aft    ,wtt    ,att    ,of     ,&
&wf     ,af     ,wt     ,at     ,o      ,&
&juer   ,ker    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLINIA (FLow INItialisation Areas and widths)
!
! Module description: Routine FLINIA calculates the initial areas and
!                     widths.
!
!                     The following areas and widths are initially re-
!                     quired:
!
!                     Af, the flow area in case the salt module has been
!                     activated
!                     At on time level n, flow width Wf and total width
!                     Wt.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 af(ngrid)         IO Flow area at every grid point at time t(n+1)
! 16 aft               P  -
! 12 arex              P  -
! 13 arexcn            P  -
! 14 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
! 23 at(ngrid)         IO Actual total area at every grid point.
! 18 att               P  -
!  4 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 h                 P  -
!  9 hlev              P  -
!  1 inires            I  True when no restart info of this module has
!                         been written before.
! 25 juer              P  -
! 26 ker               P  -
!  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  8 nlev              P  -
! 24 o                 P  -
! 19 of                P  -
! 11 overlp            P  -
! 10 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 27 psltvr(7,ngrid)   P  -
!  5 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 20 wf(ngrid)         O  Actual flow width at every grid point.
! 15 wft               P  -
! 22 wt(ngrid)         I  Actual total width at every grid point.
! 17 wtt               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flarei  FLow correction for ARea Extra Initial
! flarex  FLow correction for ARea EXtra
! flcirc  FLow CIRCLe cross section
! flinaw  FLow INterpolate Area and Width
! flperi  FLow PERImeter
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
! $Log: flinia.pf,v $
! Revision 1.7  1999/03/15  15:50:06  kuipe_j
! tabs removed
!
! Revision 1.6  1997/01/23  08:29:08  kuipe_j
! Make flow module robust
!
! Revision 1.5  1995/09/22  10:01:47  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:56  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:31:10  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:51  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   logical  inires
   integer  ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   real     overlp, prslot(3,nbran),&
   &psltvr(7,ngrid),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &wtt(ngrid,maxlev), att(ngrid,maxlev),&
   &of (ngrid,maxlev), arex(ngrid,4),&
   &wf(ngrid), af(ngrid),&
   &wt(ngrid), at(ngrid),&
   &o (ngrid)
   double precision hlev(ngrid,maxlev), h(ngrid)
!
!     Declaration of local variables:
!
   integer iter, ilev, ibr, i1, i2, i
   logical lslot
   real    delA, dummy
   double precision wght
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Computation of
!          - actual total width wt(1:ngrid) at h=h(n)
!          - actual total area  at(1:ngrid) at h=h(n)
!          - actual flow  width wf(1:ngrid) at h=h(n)
!          - actual flow  area  af(1:ngrid) at h=h(n)
!
!     for given interval and weight factor.
!
   iter = 0
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
!              tables of widths.
!
            call INDWGH (ngrid  ,i      ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,wght   )
!
!              Compute of actual total width and area
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,wght   ,&
            &wtt    ,att    ,&
            &wt(i)  ,at(i)  ,psltvr )
!
!              Compute of actual total extra area
!
            if (arexop(2) .ne. 0) then
               if (inires) then
                  call FLAREI (real(h(i))   ,arex(i,1)   ,arex(i,4) ,&
                  &overlp,delA  ,arexcn(i,2) )
               else
                  call FLAREX (iter      ,arexop(2)  , real(h(i)) ,&
                  &arex(i,1) ,arex(i,2)  ,arex(i,4)  ,&
                  &overlp    ,delA       ,arexcn(i,2),&
                  &dummy)
               endif
               at(i) = at(i) + delA
            endif
!
!              Compute actual flow width, area and wetted perimeter
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,wght   ,&
            &wft    ,aft    ,&
            &wf(i)  ,af(i)  ,psltvr )
!
!              Compute actual flow extra area
!
            if (arexop(1) .ne. 0) then
               if (inires) then
                  call FLAREI (real(h(i))  ,arex(i,1)   ,arex(i,3) ,&
                  &overlp,delA  ,arexcn(i,1) )
               else
                  call FLAREX (iter      ,arexop(1)  , real(h(i)) ,&
                  &arex(i,1) ,arex(i,2)  ,arex(i,3)  ,&
                  &overlp    ,delA       ,arexcn(i,1),&
                  &dummy)
               endif
               af(i) = af(i) + delA
            endif
!
!              Compute wetted perimeter
!
            call FLPERI (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h(i)   ,ilev   ,&
            &wft    ,wf(i)  ,&
            &of     ,o(i)   ,psltvr )

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
!              Compute of actual total/flow width and area
!
            call FLCIRC (h(i)   ,&
            &hlev(i,1) ,wft(i,1),juer   ,&
            &at(i)     ,wt(i)   ,o(i)   ,ker    )
!
            af(i) = at(i)
            wf(i) = wt(i)
!
20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 30 i = i1, i2
            call FLSEDR (ibr      ,h(i)     ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,at(i)     ,&
            &wt(i)    ,o(i)     ,ker       )
!
            af(i) = at(i)
            wf(i) = wt(i)
!
30       continue
      endif
40 continue
end

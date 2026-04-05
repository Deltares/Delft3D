subroutine FLAWN1(nbran  ,ngrid  ,branch ,typcr  ,prslot ,&
&h2     ,maxlev ,nlev   ,hlev   ,wft    ,&
&aft    ,overlp ,arex   ,arexcn ,arexop ,&
&of     ,juer   ,wf     ,af     ,o      ,&
&wtt    ,att    ,at     ,ker    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAWN1 (FLow Areas and Widths on time level N+1)
!
! Module description: In subroutine FLAWN1 the flow area, flow width and
!                     the wetted perimeter will be computed on time
!                     level n+1.
!
!                     These variables are needed for usage in other
!                     modules on time level n+1.
!
!                     The flow areas and widths will be computed for
!                     gridpoint and the main section, sub section 1 and
!                     sub section 2 (if available). The shapes of the
!                     cross sections are given as a function (circle) or
!                     in tabular form. The same calculation routines
!                     will be used as in the flow iteration process
!                     except that a water level from time level n+1 will
!                     be passed as parameter.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 af(ngrid)         IO Flow area at every grid point at time t(n+1)
! 11 aft               P  -
! 13 arex              P  -
! 14 arexcn            P  -
! 15 arexop(2)         I  Option to calculate flow(1) and total(2) area.
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
!  6 h2                P  -
!  9 hlev              P  -
! 17 juer              P  -
! 21 ker               P  -
!  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  8 nlev              P  -
! 20 o                 P  -
! 16 of                P  -
! 12 overlp            P  -
!  5 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 22 psltvr            P  -
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 18 wf                P  -
! 10 wft               P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
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
! $Log: flawn1.pf,v $
! Revision 1.8  1999/03/15  15:49:32  kuipe_j
! tabs removed
!
! Revision 1.7  1998/04/10  09:18:23  kuipe_j
! total area recalculated
!
! Revision 1.6  1997/01/23  08:28:57  kuipe_j
! Make flow module robust
!
! Revision 1.5  1995/09/22  10:00:57  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:48  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:30:30  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   real     overlp
   real     arex(ngrid,4),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev), of(ngrid,maxlev),&
   &wtt(ngrid,maxlev), att(ngrid,maxlev), at(ngrid),&
   &prslot(3,nbran), wf(ngrid), af(ngrid), o(ngrid),&
   &psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h2(ngrid)
!
!     Declaration of local variables:
!
   integer ilev, ibr, i1, i2, i, iter
   logical lslot
   real    delA, dummy
   double precision wght
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Computation of
!          - actual flow  width     wf(1:ngrid) at h=h(n+1)
!          - actual flow  area      af(1:ngrid) at h=h(n+1)
!          - actual wetted perimeter o(1:ngrid) at h=h(n+1)
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
!              tables of widths.
!
            call INDWGH (ngrid  ,i      ,&
            &maxlev ,nlev   ,hlev   ,&
            &h2(i)  ,ilev   ,wght   )
!---
!
!             Compute actual total width and area
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h2(i)  ,ilev   ,wght   ,&
            &wtt    ,att    ,&
            &dummy  ,at(i)  ,psltvr )
!
!             Compute actual total extra area
!
            if (arexop(2) .ne. 0) then
               call FLAREX (iter      ,arexop(2)  ,real(h2(i)),&
               &arex(i,1) ,arex(i,2)  ,arex(i,4)  ,&
               &overlp    ,delA       ,arexcn(i,2),&
               &dummy     )
!
!                Linearization
!
               at(i)    = at(i) + delA
            endif
!---
!
!              Compute of actual flow width and area
!
            call FLINAW (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h2(i)  ,ilev   ,wght   ,&
            &wft    ,aft    ,&
            &wf(i)  ,af(i)  ,psltvr )
!
!              Compute extra area
!
            if (arexop(1) .ne. 0) then
               iter = 0
               call FLAREX (iter      ,arexop(1)  ,real(h2(i)),&
               &arex(i,1) ,arex(i,2)  ,arex(i,3)  ,&
               &overlp    ,delA       ,arexcn(i,1),&
               &dummy)
               af(i) = af(i) + delA
            endif
!
!              Compute of actual wetted perimeter
!
            call FLPERI (ngrid  ,i      ,lslot  ,&
            &maxlev ,nlev   ,hlev   ,&
            &h2(i)  ,ilev   ,&
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
!              Compute of actual flow width and area
!
            call FLCIRC (h2(i)     ,&
            &hlev(i,1) ,wft(i,1),juer   ,&
            &af(i)     ,wf(i)   ,o(i)   ,ker    )
20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 30 i = i1, i2
            call FLSEDR (ibr      ,h2(i)    ,hlev(i,1) ,hlev(i,2) ,&
            &wft(i,1) ,wft(i,2) ,juer      ,af(i)     ,&
            &wf(i)    ,o(i)     ,ker       )
30       continue
      endif
40 continue
end

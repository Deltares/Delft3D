subroutine KAAWST(nbran  ,ngrid  ,branch ,typcr  ,prslot ,maxlev ,&
&nlev   ,hlev   ,wft    ,aft    ,h2     ,wf2    ,&
&af2    ,grid   ,overlp ,arex   ,arexcn ,arexop ,&
&juer   ,ker    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAAWST (KAlman Wetted Perimeter)
!
! Module description: Calculate the flow area and flow width and wetted
!                     per imeter on the waterlevel t(n+1). These values
!                     are to be used for structures.
!
!                     Three different types of shapes are possible: the
!                     table, the circle and the sedredge cross section.
!                     For each shape different routines are used.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 af2(ngrid)        IO Flow area in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
! 10 aft               P  -
! 16 arex              P  -
! 17 arexcn            P  -
! 18 arexop(2)         I  Option to calculate flow(1) and total(2) area.
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
! 14 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 11 h2                P  -
!  8 hlev              P  -
! 19 juer              P  -
! 20 ker               P  -
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  7 nlev              P  -
! 15 overlp            P  -
!  5 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 21 psltvr            P  -
!  4 typcr(nbran)      I  Type of cross section used in every branch:
!                         ccrtab (1) : Tabulated cross sections
!                         ccrcir (2) : Circle as cross section
!                         ccrsed (3) : Sedredge cross sections
! 12 wf2(ngrid)        IO Flow width in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
!  9 wft               P  -
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
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaawst.pf,v $
! Revision 1.4  1999/03/15  15:51:34  kuipe_j
! tabs removed
!
! Revision 1.3  1997/01/23  08:29:30  kuipe_j
! Make flow module robust
!
! Revision 1.2  1996/04/12  13:04:37  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:13  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, maxlev, juer, ker,&
   &nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),&
   &grid(ngrid), arexcn(ngrid,2), arexop(2)
   real     overlp
   real     wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &prslot(3,nbran), af2(ngrid), wf2(ngrid),&
   &arex(ngrid,4), psltvr(7,ngrid)
   double precision hlev(ngrid,maxlev), h2(ngrid)
!
!     Declaration of local variables:
!
   integer ilev, ibr, i1, i2, i, j, ij, iter
   logical lslot
   real    rdum, dela, delw
   double precision wght
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Computation of
!          - flow  width     wf2(1:ngrid) at h=h(n+1)
!                  at gridpoints on left-side and right-side
!                  of structures.
!          - flow  area      af2(1:ngrid) at h=h(n+1)
!                  at gridpoints on left-side and right-side
!                  of structures.
!
!     given interval and weight factor.
!
   iter = 0

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
            if ( grid(i) .eq. cstrcl ) then
!
!                 Compute index (ilev) and weight factor (wght) w.r.t.
!                 tables of widths.
!
               do 5 j = 0, 1
!
!                    j=0 : gridpoint at left-side of structure
!                    j=1 : gridpoint at right-side of structure
!
                  ij = i+j
                  call INDWGH (ngrid  ,ij     ,&
                  &maxlev ,nlev   ,hlev   ,&
                  &h2(ij) ,ilev   ,wght   )
!
!                    Compute flow width and area
!
                  call FLINAW (ngrid   ,ij     ,lslot  ,&
                  &maxlev  ,nlev   ,hlev   ,&
                  &h2(ij)  ,ilev   ,wght   ,&
                  &wft     ,aft    ,&
                  &wf2(ij) ,af2(ij),psltvr )
!
!                    Add extra area (and width)
!
                  if (arexop(1) .ne. 0) then
                     call FLAREX (iter      ,arexop(1) ,h2(ij)      ,&
                     &arex(ij,1),arex(ij,2),arex(ij,3)  ,&
                     &overlp    ,delA      ,arexcn(ij,1),&
                     &delW      )
!
                     wf2(ij) = wf2(ij) + delW
                     af2(ij) = af2(ij) + delA
                  endif
5              continue
            endif
10       continue

      else if ( typcr(ibr) .eq. ccrcir ) then
!
!           Circle used as cross section in this branch
!
         do 20 i = i1, i2
            if ( grid(i) .eq. cstrcl ) then
               do 15 j = 0, 1
!
!                    j=0 : gridpoint at left-side of structure
!                    j=1 : gridpoint at right-side of structure
!
!
!                    reflev : hlev(i,1)
!                    radius : wft(i,1)
!
!                    Compute of actual flow width and area
!
                  call FLCIRC (h2(i+j)     ,&
                  &hlev(i+j,1) ,wft(i+j,1) ,juer    ,&
                  &af2(i+j)    ,wf2(i+j)   ,rdum    ,&
                  &ker         )
15             continue
            endif
20       continue

      else if ( typcr(ibr) .eq. ccrsed ) then
!
!           Sedredge branch
!
         do 30 i = i1, i2
            if ( grid(i) .eq. cstrcl ) then
               do 25 j = 0, 1
!
!                    j=0 : gridpoint at left-side of structure
!                    j=1 : gridpoint at right-side of structure
!
!
                  call FLSEDR (ibr         ,h2(i+j)     ,hlev(i+j,1),&
                  &hlev(i+j,2) ,wft(i+j,1)  ,wft(i+j,2) ,&
                  &juer        ,af2(i+j)    ,wf2(i+j)   ,&
                  &rdum        ,ker         )
25             continue
            endif
30       continue
      endif
40 continue
end

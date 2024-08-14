      subroutine KAAWST(nbran  ,ngrid  ,branch ,typcr  ,prslot ,maxlev ,
     +                  nlev   ,hlev   ,wft    ,aft    ,h2     ,wf2    ,
     +                  af2    ,grid   ,overlp ,arex   ,arexcn ,arexop ,
     +                  juer   ,ker    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAAWST (KAlman Wetted Perimeter)
c
c Module description: Calculate the flow area and flow width and wetted
c                     per imeter on the waterlevel t(n+1). These values
c                     are to be used for structures.
c
c                     Three different types of shapes are possible: the
c                     table, the circle and the sedredge cross section.
c                     For each shape different routines are used.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 af2(ngrid)        IO Flow area in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c 10 aft               P  -
c 16 arex              P  -
c 17 arexcn            P  -
c 18 arexop(2)         I  Option to calculate flow(1) and total(2) area.
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
c 14 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 11 h2                P  -
c  8 hlev              P  -
c 19 juer              P  -
c 20 ker               P  -
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  7 nlev              P  -
c 15 overlp            P  -
c  5 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 21 psltvr            P  -
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 12 wf2(ngrid)        IO Flow width in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c  9 wft               P  -
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
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaawst.pf,v $
c Revision 1.4  1999/03/15  15:51:34  kuipe_j
c tabs removed
c
c Revision 1.3  1997/01/23  08:29:30  kuipe_j
c Make flow module robust
c
c Revision 1.2  1996/04/12  13:04:37  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:13  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid),
     +         grid(ngrid), arexcn(ngrid,2), arexop(2)
      real     overlp
      real     wft(ngrid,maxlev), aft(ngrid,maxlev),
     +         prslot(3,nbran), af2(ngrid), wf2(ngrid),
     +         arex(ngrid,4), psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev), h2(ngrid)
c
c     Declaration of local variables:
c
      integer ilev, ibr, i1, i2, i, j, ij, iter
      logical lslot
      real    rdum, dela, delw
      double precision wght
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Computation of
c          - flow  width     wf2(1:ngrid) at h=h(n+1)
c                  at gridpoints on left-side and right-side
c                  of structures.
c          - flow  area      af2(1:ngrid) at h=h(n+1)
c                  at gridpoints on left-side and right-side
c                  of structures.
c
c     given interval and weight factor.
c
      iter = 0

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
               if ( grid(i) .eq. cstrcl ) then
c
c                 Compute index (ilev) and weight factor (wght) w.r.t.
c                 tables of widths.
c
                  do 5 j = 0, 1
c
c                    j=0 : gridpoint at left-side of structure
c                    j=1 : gridpoint at right-side of structure
c
                     ij = i+j
                     call INDWGH (ngrid  ,ij     ,
     +                            maxlev ,nlev   ,hlev   ,
     +                            h2(ij) ,ilev   ,wght   )
c
c                    Compute flow width and area
c
                     call FLINAW (ngrid   ,ij     ,lslot  ,
     +                            maxlev  ,nlev   ,hlev   ,
     +                            h2(ij)  ,ilev   ,wght   ,
     +                            wft     ,aft    ,
     +                            wf2(ij) ,af2(ij),psltvr )
c
c                    Add extra area (and width)
c
                     if (arexop(1) .ne. 0) then
                        call FLAREX (iter      ,arexop(1) ,h2(ij)      ,
     +                               arex(ij,1),arex(ij,2),arex(ij,3)  ,
     +                               overlp    ,delA      ,arexcn(ij,1),
     +                               delW      )
c
                        wf2(ij) = wf2(ij) + delW
                        af2(ij) = af2(ij) + delA
                     endif
    5             continue
               endif
   10       continue

         else if ( typcr(ibr) .eq. ccrcir ) then
c
c           Circle used as cross section in this branch
c
            do 20 i = i1, i2
               if ( grid(i) .eq. cstrcl ) then
                  do 15 j = 0, 1
c
c                    j=0 : gridpoint at left-side of structure
c                    j=1 : gridpoint at right-side of structure
c
c
c                    reflev : hlev(i,1)
c                    radius : wft(i,1)
c
c                    Compute of actual flow width and area
c
                     call FLCIRC (h2(i+j)     ,
     +                            hlev(i+j,1) ,wft(i+j,1) ,juer    ,
     +                            af2(i+j)    ,wf2(i+j)   ,rdum    ,
     +                            ker         )
   15             continue
               endif
   20       continue

         else if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 30 i = i1, i2
               if ( grid(i) .eq. cstrcl ) then
                  do 25 j = 0, 1
c
c                    j=0 : gridpoint at left-side of structure
c                    j=1 : gridpoint at right-side of structure
c
c
                     call FLSEDR (ibr         ,h2(i+j)     ,hlev(i+j,1),
     +                            hlev(i+j,2) ,wft(i+j,1)  ,wft(i+j,2) ,
     +                            juer        ,af2(i+j)    ,wf2(i+j)   ,
     +                            rdum        ,ker         )
   25             continue
               endif
   30       continue
         endif
   40 continue
      end

      subroutine FLWETP(nbran  ,ngrid  ,branch ,typcr  ,h1     ,h      ,
     +                  maxlev ,nlev   ,hlev   ,wft    ,of     ,wf     ,
     +                  prslot ,juer   ,o      ,ker    ,theta2 ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLWETP (FLow Wetted Perimeter)
c
c Module description: In subroutine FLWETP the wetted perimeter will be
c                     computed.
c
c                     This subroutine calculates the wetted perimeter
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  5 h1(ngrid)         I  Water level in every grid point at time t(n).
c  6 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  9 hlev              P  -
c 14 juer              P  -
c 16 ker               P  -
c  7 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  8 nlev              P  -
c 15 o                 P  -
c 11 of                P  -
c 13 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 18 psltvr(7,ngrid)   P  -
c 17 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  4 typcr(nbran)      I  Type of cross section used in every branch:
c                         ccrtab (1) : Tabulated cross sections
c                         ccrcir (2) : Circle as cross section
c                         ccrsed (3) : Sedredge cross sections
c 12 wf                P  -
c 10 wft               P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flcirc  FLow CIRCLe cross section
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
c $Log: flwetp.pf,v $
c Revision 1.6  1997/01/23  08:29:19  kuipe_j
c Make flow module robust
c
c Revision 1.5  1995/09/22  10:02:32  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:11:06  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:31:48  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  ngrid, maxlev, juer, ker,
     +         nbran, branch(4,nbran), typcr(nbran), nlev(ngrid)
      real     wft(ngrid,maxlev), of (ngrid,maxlev),
     +         wf(ngrid), prslot(3,nbran), o (ngrid), theta2,
     +         psltvr(7,ngrid)
      double precision hlev(ngrid,maxlev), h(ngrid), h1(ngrid)
c
c     Declaration of local variables:
c
      logical lslot
      integer ilev, ibr, i1, i2, i
      real    dummy1, dummy2
      double precision wght, hact
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     Computation of
c          - actual perimeter   o (1:ngrid) at h=h(n+1/2)
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
c             hact = (h1(i)+h(i))/2.0
              hact = theta2*h(i)+(1.-theta2)*h1(i)
c
c              Compute index (ilev) and weight factor (wght) w.r.t.
c              tables of widths.
c
              call INDWGH (ngrid  ,i      ,maxlev ,
     +                     nlev   ,hlev   ,
     +                     hact   ,ilev   ,wght   )
c
c              Compute of actual wetted perimeter
c
              call FLPERI (ngrid  ,i      ,lslot  ,
     +                     maxlev ,nlev   ,hlev   ,
     +                     hact   ,ilev   ,
     +                     wft    ,wf(i)  ,
     +                     of     ,o(i)   ,psltvr )
   10      continue

        else if ( typcr(ibr) .eq. ccrcir ) then
c
c          Circle used as cross section in this branch
c
           do 20 i = i1, i2
c
c              reflev : hlev(i,1)
c              radius : wft(i,1)
c
c              Compute of perimeter for circle cross section
c
c             call FLCIRC ((h1(i)+h(i))/2.0 ,
              call FLCIRC ((theta2*h(i)+(1.-theta2)*h1(i)) ,
     +                             hlev(i,1) ,wft(i,1),juer   ,
     +                             dummy1    ,dummy2  ,o(i)   ,ker    )
   20      continue

         else if ( typcr(ibr) .eq. ccrsed ) then
c
c           Sedredge branch
c
            do 30 i = i1, i2
c             hact = (h1(i)+h(i))/2.0
              hact = theta2*h(i)+(1.-theta2)*h1(i)
              call FLSEDR (ibr      ,hact     ,hlev(i,1) ,hlev(i,2) ,
     +                      wft(i,1) ,wft(i,2) ,juer      ,dummy1    ,
     +                      dummy2   ,o(i)     ,ker       )
   30       continue
        endif
   40 continue
c
      end

      subroutine wqmbps (ngrid , nqlat , nsegmt, nsegtb, nexdef, nposeg,
     +                   npntr , dt    , exdef , segmnt, segtab, pntr  ,
     +                   qltpar, qlaggr, qlatgr, qunkno, svol  , svolo ,
     +                   x     , sexfl )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers
c
c Module:             WQMBPS (Water Quality Mass Balance in Parallel
c                             Sections
c
c Module description: Exchange flows are caluculated between the
c                     parallel segments in order to maintain the mass
c                     balance.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 ngrid             I  nr of gridpoints
c  5 nsegmt            I  nr of entries in segmnt (segments + boundaries)
c  6 npntr             I  nr of exchanges
c  7 nsegtb            I  nr of entries in segtab
c  8 nexdef            I  nr of entries in exdef
c 11 exdef(6,nexdef)   O  This table contains the elementary exchange
c                         definitions. Each exchange between segments
c                         can be described by one or more elementary
c                         definitions in this table.
c                         (1,i) = Exchange type:
c                                 cexigp (1) : Exchange in gridpoint.
c                                 cexbgp (2) : Exchange between gpoints.
c                                 cexind (3) : Exchange in a node.
c                                 cexqlt (4) : Exchange from Qlat to seg
c                         - Exchange in a gridpoint (Type 1):
c                         (2,i) = Gridpoint
c                         (3,i) = Section from
c                         (4,i) = Section to
c                         (5,i) = Direction of "from" --> "to":
c                                 cpopnt (+1) : pos branch direction.
c                                 cnepnt (-1) : neg branch direction.
c                         - Exchange between gridpoints (Type 2):
c                         (2,i) = Gridpoint from
c                         (3,i) = Gridpoint to
c                         (4,i) = Section
c                         (5,i) = Length factor (0< length factor <1)
c                         - Exchange in a node (Type 3):
c                         (2,i) = Node number
c                         (3,i) = Gridpoint from
c                         (4,i) = Gridpoint to
c                         (5,i) = Section from
c                         (6,i) = Section to
c                         - Exchange from Qlat stat to segment (Type 4)
c                         (2,i) = Gridpoint
c                         (3,i) = Length factor (0 < factor <= 1).
c                         (4,i) = Lateral station number.
c 13 segmnt(3,nsegmt)  I  Segment definition:
c                         (1,i) = segment number (if > 0)
c                         (2,i) = first entry in segtab for this segment
c                         (3,i) = nr.of entries in segtab for this
c                                 segment
c                         (1,i) = negative boundary number (if < 0)
c                         (2,i) = grdpoint where boundary is situated
c                         (3,i) = should have value 0!
c 14 segtab            I
c 15 segtab(5,nsegtb)  I  This table contains for each segment the en-
c                         closed grid-cells together with the length
c                         factors and section indication.
c                         (1,j) = Gridpoint 1
c                         (2,j) = Gridpoint 2
c                         (3,j) = Length factor Lb
c                         (4,j) = Length factor Le
c                         (5,j) = Section
c                                 cnopar (0) : No parallel sections
c                                 cmainc (1) : Main channel
c                                 csub1  (2) : Sub section 1
c                                 csub2  (3) : Sub section 2
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqslat  Water Quality Segment LATeral flows
c cqlatg  Calculate Q LaTeral per Grid point
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
c $Log: wqmbps.pf,v $
c Revision 1.2  1999/06/15  15:09:07  kuipe_j
c avoid compiler optimization error
c
c Revision 1.1  1999/03/12  12:45:37  kuipe_j
c parallel segments added
c
c
c
c***********************************************************************
c
c     Parameters
c
      integer      ngrid , nqlat, nsegmt, nsegtb, nexdef, nposeg,
     +             npntr , dt   , i
      integer      segmnt(3,nsegmt), pntr(4,npntr)
      real         exdef (6,nexdef), segtab(5,nsegtb),
     +             qltpar(9,nqlat) , qlaggr(nqlat)   ,
     +             qunkno(nposeg)  , qlatgr(ngrid)   ,
     +             svol  (nposeg)  , svolo (nposeg)  ,
     +             sexfl (npntr)   , x     (ngrid)

c     Locals

      integer      iseg  , segno , ifrom , ito   , iex   , istat ,
     +             iptr  ,igr
      real         ex

      include '..\include\sobcon.i'

c     Loop over segments; determine storage flow in segments

      do iseg = 1,nsegmt
        segno = segmnt (1,iseg)
c
        if (segno .gt. 0)
     +     qunkno(segno) = (svol(segno)-svolo(segno))/dt
c       if (segno .gt. 0)
      enddo

c     Loop over exchanges; add all known exchange flows

      do iptr = 1,npntr
         iex = pntr(3,iptr)
         if (nint(exdef(1,iex)).le.3) then
            ifrom = pntr(1,iptr)
            ito   = pntr(2,iptr)
            if (ifrom .gt. 0)
     +         qunkno(ifrom) = qunkno(ifrom) + sexfl(iex)
            if (ito .gt. 0)
     +         qunkno(ito)   = qunkno(ito)   - sexfl(iex)
         endif
      enddo
      do i = 1,nsegmt
        segno = segmnt (1,i)
c       if (segno .gt. 0)
      ENDDO
c     Add lateral discharges, this will be done in 2 steps.
c     - 1 - Distribute the in time aggregated lateral discharges
c           in lateral stations over the grid cells.

      do igr = 1, ngrid
         qlatgr(igr) = 0.
      enddo

      do istat = 1, nqlat
         call CQLATG (ngrid  ,nqlat  ,istat  ,qlaggr(istat) ,qltpar ,
     +                x      ,qlatgr )
      enddo

c     - 2 - Substract the lateral discharge of each grid cell from the
c            discharge of the corresponding segment.

      call wqslat ( nsegmt ,segmnt ,nsegtb ,segtab ,
     +              ngrid  ,nposeg ,qlatgr ,qunkno )

c     Loop over lateral exchanges; Determine the exchange flows.

      do iptr = 1,npntr
         iex = pntr(3,iptr)
         if (nint(exdef(1,iex)).eq.cexbsc) then
            ifrom = nint(exdef(2,iex))
            ito   = nint(exdef(3,iex))
            if (qunkno(ifrom)*qunkno(ito).lt.0.0) then
c              Aux variable introduced to avoid compiler error in Dig 5,
c              opt = 1
               ex         = min(abs(qunkno(ifrom)),abs(qunkno(ito)))
               sexfl(iex) = sign(ex,qunkno(ito))
            endif
         endif
      enddo

      end

subroutine wqmbps (ngrid , nqlat , nsegmt, nsegtb, nexdef, nposeg,&
&npntr , dt    , exdef , segmnt, segtab, pntr  ,&
&qltpar, qlaggr, qlatgr, qunkno, svol  , svolo ,&
&x     , sexfl )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQMBPS (Water Quality Mass Balance in Parallel
!                             Sections
!
! Module description: Exchange flows are caluculated between the
!                     parallel segments in order to maintain the mass
!                     balance.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ngrid             I  nr of gridpoints
!  5 nsegmt            I  nr of entries in segmnt (segments + boundaries)
!  6 npntr             I  nr of exchanges
!  7 nsegtb            I  nr of entries in segtab
!  8 nexdef            I  nr of entries in exdef
! 11 exdef(6,nexdef)   O  This table contains the elementary exchange
!                         definitions. Each exchange between segments
!                         can be described by one or more elementary
!                         definitions in this table.
!                         (1,i) = Exchange type:
!                                 cexigp (1) : Exchange in gridpoint.
!                                 cexbgp (2) : Exchange between gpoints.
!                                 cexind (3) : Exchange in a node.
!                                 cexqlt (4) : Exchange from Qlat to seg
!                         - Exchange in a gridpoint (Type 1):
!                         (2,i) = Gridpoint
!                         (3,i) = Section from
!                         (4,i) = Section to
!                         (5,i) = Direction of "from" --> "to":
!                                 cpopnt (+1) : pos branch direction.
!                                 cnepnt (-1) : neg branch direction.
!                         - Exchange between gridpoints (Type 2):
!                         (2,i) = Gridpoint from
!                         (3,i) = Gridpoint to
!                         (4,i) = Section
!                         (5,i) = Length factor (0< length factor <1)
!                         - Exchange in a node (Type 3):
!                         (2,i) = Node number
!                         (3,i) = Gridpoint from
!                         (4,i) = Gridpoint to
!                         (5,i) = Section from
!                         (6,i) = Section to
!                         - Exchange from Qlat stat to segment (Type 4)
!                         (2,i) = Gridpoint
!                         (3,i) = Length factor (0 < factor <= 1).
!                         (4,i) = Lateral station number.
! 13 segmnt(3,nsegmt)  I  Segment definition:
!                         (1,i) = segment number (if > 0)
!                         (2,i) = first entry in segtab for this segment
!                         (3,i) = nr.of entries in segtab for this
!                                 segment
!                         (1,i) = negative boundary number (if < 0)
!                         (2,i) = grdpoint where boundary is situated
!                         (3,i) = should have value 0!
! 14 segtab            I
! 15 segtab(5,nsegtb)  I  This table contains for each segment the en-
!                         closed grid-cells together with the length
!                         factors and section indication.
!                         (1,j) = Gridpoint 1
!                         (2,j) = Gridpoint 2
!                         (3,j) = Length factor Lb
!                         (4,j) = Length factor Le
!                         (5,j) = Section
!                                 cnopar (0) : No parallel sections
!                                 cmainc (1) : Main channel
!                                 csub1  (2) : Sub section 1
!                                 csub2  (3) : Sub section 2
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqslat  Water Quality Segment LATeral flows
! cqlatg  Calculate Q LaTeral per Grid point
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
! $Log: wqmbps.pf,v $
! Revision 1.2  1999/06/15  15:09:07  kuipe_j
! avoid compiler optimization error
!
! Revision 1.1  1999/03/12  12:45:37  kuipe_j
! parallel segments added
!
!
!
!***********************************************************************
!
!     Parameters
!
   integer      ngrid , nqlat, nsegmt, nsegtb, nexdef, nposeg,&
   &npntr , dt   , i
   integer      segmnt(3,nsegmt), pntr(4,npntr)
   real         exdef (6,nexdef), segtab(5,nsegtb),&
   &qltpar(9,nqlat) , qlaggr(nqlat)   ,&
   &qunkno(nposeg)  , qlatgr(ngrid)   ,&
   &svol  (nposeg)  , svolo (nposeg)  ,&
   &sexfl (npntr)   , x     (ngrid)

!     Locals

   integer      iseg  , segno , ifrom , ito   , iex   , istat ,&
   &iptr  ,igr
   real         ex

   include '..\include\sobcon.i'

!     Loop over segments; determine storage flow in segments

   do iseg = 1,nsegmt
      segno = segmnt (1,iseg)
!
      if (segno .gt. 0)&
      &qunkno(segno) = (svol(segno)-svolo(segno))/dt
!       if (segno .gt. 0)
   enddo

!     Loop over exchanges; add all known exchange flows

   do iptr = 1,npntr
      iex = pntr(3,iptr)
      if (nint(exdef(1,iex)).le.3) then
         ifrom = pntr(1,iptr)
         ito   = pntr(2,iptr)
         if (ifrom .gt. 0)&
         &qunkno(ifrom) = qunkno(ifrom) + sexfl(iex)
         if (ito .gt. 0)&
         &qunkno(ito)   = qunkno(ito)   - sexfl(iex)
      endif
   enddo
   do i = 1,nsegmt
      segno = segmnt (1,i)
!       if (segno .gt. 0)
   ENDDO
!     Add lateral discharges, this will be done in 2 steps.
!     - 1 - Distribute the in time aggregated lateral discharges
!           in lateral stations over the grid cells.

   do igr = 1, ngrid
      qlatgr(igr) = 0.
   enddo

   do istat = 1, nqlat
      call CQLATG (ngrid  ,nqlat  ,istat  ,qlaggr(istat) ,qltpar ,&
      &x      ,qlatgr )
   enddo

!     - 2 - Substract the lateral discharge of each grid cell from the
!            discharge of the corresponding segment.

   call wqslat ( nsegmt ,segmnt ,nsegtb ,segtab ,&
   &ngrid  ,nposeg ,qlatgr ,qunkno )

!     Loop over lateral exchanges; Determine the exchange flows.

   do iptr = 1,npntr
      iex = pntr(3,iptr)
      if (nint(exdef(1,iex)).eq.cexbsc) then
         ifrom = nint(exdef(2,iex))
         ito   = nint(exdef(3,iex))
         if (qunkno(ifrom)*qunkno(ito).lt.0.0) then
!              Aux variable introduced to avoid compiler error in Dig 5,
!              opt = 1
            ex         = min(abs(qunkno(ifrom)),abs(qunkno(ito)))
            sexfl(iex) = sign(ex,qunkno(ito))
         endif
      endif
   enddo

end

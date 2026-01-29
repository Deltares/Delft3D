subroutine wqaux  ( nsegmt ,segmnt ,nsegtb ,segtab ,&
&ngrid  ,nposeg ,psi    ,af     ,&
&afs    ,c      ,qaggr  ,wf     ,&
&wfs    ,x      ,slen   ,schz   ,&
&shor   ,svel   ,swdt   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQAUX (Water Quality AUXiliary files)
!
! Module description: This routine calculates segment functions. These
!                     functions contain:
!
!                     -   Chezy coefficients;
!                     -   Horizontal surfaces;
!                     -   Velocities;
!                     -   Widths.
!
!                     The auxiliary files are calculated in one routine.
!                     ( See also document S-DO-005.2SW )
!
!                     -   Chezy segment function
!
!                         To calculate a Chezy coefficient for a Working
!                         Unit formula (3) is used:
!
!                         A Chezy coefficient for a segment is calcu-
!                         lated by formula (4).
!
!                         Because also the horizontal surfaces and vel-
!                         ocities are calculated the Chezy coefficient
!                         for a segment can be calculated straight for-
!                         ward.
!
!                     -   Horizontal surfaces
!
!                         This file contains horizontal surfaces for
!                         each segment. Optionally a constant can be
!                         given. This is arranged via the User Inter-
!                         face.
!
!                         To calculate the surfaces the averaged widths
!                         in the Working Units gridpoints are required.
!                         (formula 5)
!
!                     -   Widths
!
!                         This file contains widths for each segment.
!                         First the width for each working unit is cal-
!                         culated by formula (6).
!                         When all working unit widths have been calcu-
!                         lated a segment width is calculated by (7).
!
!                         When a segment includes a node with more than
!                         2 working units connected to it the width
!                         cannot be calculated in this way. In this case
!                         the width should be constructed by dividing
!                         the horizontal surfaces by the segment length,
!                         formula (8).
!
!                     -   Velocities
!
!                         This file contains velocities for each seg-
!                         ment. First the velocity for each working unit
!                         is calculated by formula (9).
!
!                         When all velocities have been calculated a
!                         segment velocity is calculated by (10).
!
! NOTE: Input parameter WF is actually WT! (Jos van Gils, September 2001)
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 af                P  -
!  9 afs               P  -
! 10 c                 P  -
!  5 ngrid             I  Number of grid points in network.
!  6 nposeg            I  Number of positive segment numbers.
!  1 nsegmt            I  Number of segments defined.
!  3 nsegtb            I  Number of entries in segtab table.
!  7 psi               I  Space weight factor in Preissmann scheme.
! 11 qaggr             P  -
! 16 schz(nposeg)      IO Chezy coefficients for each segment.
!  2 segmnt(3,nsegmt)  I  Definition of table with segment pointers:
!                         (1,j) = Segment number
!                         (2,j) = Pointer to segtab for segment j.
!                         (3,j) = Number of enclosed gridpoints.
!  4 segtab(5,nsegtb)  I  This table contains for each segment the en-
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
! 17 shor(nposeg)      IO Horizontal surfaces for each segment.
! 15 slen(nposeg)      I  Lengths for each segment.
! 18 svel(nposeg)      IO Velocities for each segment.
! 19 swdt(nposeg)      O  Widths for each segment.
! 12 wf                P  -
! 13 wfs               P  -
! 14 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpar  Water Quality GridPoint ARea
! wqgpch  Water Quality GridPoint CHezy
! wqgpfl  Water Quality GridPoint FLow
! wqgpwd  Water Quality GridPoint WiDth
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
! $Log: wqaux.pf,v $
! Revision 1.4  1999/03/15  15:53:49  kuipe_j
! tabs removed
!
! Revision 1.3  1997/08/29  14:34:42  kuipe_j
! Make velocity positive
!
! Revision 1.2  1995/05/30  07:08:21  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:42  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:25  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer ngrid,&
   &nposeg,&
   &nsegmt,&
   &nsegtb
!
   integer segmnt (3,nsegmt)
!
   real    psi
!
   real    af     (ngrid),&
   &afs    (ngrid,2),&
   &c      (ngrid,4),&
   &qaggr  (ngrid,3),&
   &segtab (5,nsegtb),&
   &schz   (nposeg),&
   &shor   (nposeg),&
   &slen   (nposeg),&
   &svel   (nposeg),&
   &swdt   (nposeg),&
   &wf     (ngrid),&
   &wfs    (ngrid,2),&
   &x      (ngrid)
!
!     Variables
!
   integer igp1, igp2, isecwq, iseg, itab, ngp, ngrd, segno
!
   real    a1, a2, awu, ch1, ch2, cterm1, cterm2, cwu,&
   &lb, le, lemlb, q1, q2, swu, u1, u2, udenum,&
   &uwu, w1, w2, wwu, xemxb
!
!     Loop over segment numbers
!
   do 300 iseg = 1, nsegmt
!
      segno = segmnt(1,iseg)
!
      if (segno .gt. 0) then
!
         schz (segno) = 0
         shor (segno) = 0
         svel (segno) = 0
         swdt (segno) = 0
!
         udenum = 0
!
         itab = segmnt (2,iseg)
         ngrd = segmnt (3,iseg)
!
!           Loop over enclosed grid-cells
!
         do 200 ngp = 1, ngrd
!
!              Fetch definition of working unit enclosed by segment
!
            igp1   = int (segtab (1,itab))
            igp2   = int (segtab (2,itab))
            lb     =      segtab (3,itab)
            le     =      segtab (4,itab)
            isecwq = int (segtab (5,itab))
!
!              In case sec(tion) = 0 the complete grid-cell is enclosed.
!
            call wqgpar ( ngrid, af, afs, igp1, isecwq, a1 )
            call wqgpar ( ngrid, af, afs, igp2, isecwq, a2 )
!
            awu = (1.-psi) * a1 + (psi * a2)
!
            call wqgpfl ( ngrid, qaggr, igp1, isecwq, q1 )
            call wqgpfl ( ngrid, qaggr, igp2, isecwq, q2 )
!
            call wqgpwd ( ngrid, wf, wfs, igp1, isecwq, w1 )
            call wqgpwd ( ngrid, wf, wfs, igp2, isecwq, w2 )
!
            call wqgpch ( ngrid, c, igp1, isecwq, ch1 )
            call wqgpch ( ngrid, c, igp2, isecwq, ch2 )
!
!              Calculate Xe-Xb
!
            xemxb = x (igp2) - x (igp1)
!
!              Calculate Le-Lb
!
            lemlb = le - lb
!
!              Calculate width of working unit i
!
            wwu = (((1.-lb) * w1) + (le*w2)) / (1. - lb + le)
!
!              Calculate horizontal surface working unit i
!
            swu = wwu * xemxb * lemlb
!
!              Add to segment horizontal surface
!
            shor (segno) = shor (segno) + swu
!
!              Calculate velocity of working unit i
!
!              Not sharp enough!!! JvG Sept 2001
!              if (abs(awu) .lt. 1.0E-9) then
            if (abs(awu) .lt. 1.0E-3) then
               uwu = 0.
            else
               uwu = 0.5 * ( q1 + q2 ) / awu
            endif
!
!              Add to segment velocity
!
            svel (segno) = svel (segno) + ( lemlb * xemxb * uwu )
!
!              Update denumerator of velocity
!
            udenum  = udenum + ( lemlb * xemxb )
!
!                            2   2
!              Calculate  ( u / C ) * S for working unit
!
            if (abs(a1) .lt. 1.0E-9) then
               u1 = 0.
            else
               u1  = q1 / a1
            endif
!
            if (abs(a2) .lt. 1.0E-9) then
               u2 = 0.
            else
               u2  = q2 / a2
            endif
!
            if (abs(ch1) .lt. 1.0E-9) then
               cterm1 = 0.
            else
               cterm1 = (u1*u1) / (ch1*ch1) * 0.5 * xemxb * w1
            endif
!
            if (abs(ch2) .lt. 1.0E-9) then
               cterm2 = 0.
            else
               cterm2 = (u2*u2) / (ch2*ch2) * 0.5 * xemxb * w2
            endif
!
            cwu = cterm1 + cterm2
!
            schz (segno) = schz (segno) + (lemlb * cwu)
!
!              Increment itab pointer
!
            itab = itab + 1
!
200      continue
!
!           Calculate u segment
!
!           The velocity sbhould always be positive (van Gils)
         svel(segno) = max(abs(svel(segno)/udenum),1.0E-9)
!
!           Calculate chezy coefficient segment
!
         if (abs(schz(segno)) .gt. 1.0E-9) then
            schz (segno) = sqrt ((shor (segno)*svel (segno)**2) /&
            &schz (segno))
         else
            schz (segno) = 100.
         endif
!
!           Calculate width segment
!
         if (abs(slen(segno)) .lt. 1.0E-9) then
            swdt (segno) = 0.
         else
            swdt (segno) = shor (segno) / slen (segno)
         endif
         shor(segno) = max (shor(segno),0.1)
         swdt(segno) = max (swdt(segno),0.1)
      endif
!
300 continue
!
   return
end

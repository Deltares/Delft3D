      subroutine wqaux  ( nsegmt ,segmnt ,nsegtb ,segtab ,
     +                    ngrid  ,nposeg ,psi    ,af     ,
     +                    afs    ,c      ,qaggr  ,wf     ,
     +                    wfs    ,x      ,slen   ,schz   ,
     +                    shor   ,svel   ,swdt   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQAUX (Water Quality AUXiliary files)
c
c Module description: This routine calculates segment functions. These
c                     functions contain:
c
c                     -   Chezy coefficients;
c                     -   Horizontal surfaces;
c                     -   Velocities;
c                     -   Widths.
c
c                     The auxiliary files are calculated in one routine.
c                     ( See also document S-DO-005.2SW )
c
c                     -   Chezy segment function
c
c                         To calculate a Chezy coefficient for a Working
c                         Unit formula (3) is used:
c
c                         A Chezy coefficient for a segment is calcu-
c                         lated by formula (4).
c
c                         Because also the horizontal surfaces and vel-
c                         ocities are calculated the Chezy coefficient
c                         for a segment can be calculated straight for-
c                         ward.
c
c                     -   Horizontal surfaces
c
c                         This file contains horizontal surfaces for
c                         each segment. Optionally a constant can be
c                         given. This is arranged via the User Inter-
c                         face.
c
c                         To calculate the surfaces the averaged widths
c                         in the Working Units gridpoints are required.
c                         (formula 5)
c
c                     -   Widths
c
c                         This file contains widths for each segment.
c                         First the width for each working unit is cal-
c                         culated by formula (6).
c                         When all working unit widths have been calcu-
c                         lated a segment width is calculated by (7).
c
c                         When a segment includes a node with more than
c                         2 working units connected to it the width
c                         cannot be calculated in this way. In this case
c                         the width should be constructed by dividing
c                         the horizontal surfaces by the segment length,
c                         formula (8).
c
c                     -   Velocities
c
c                         This file contains velocities for each seg-
c                         ment. First the velocity for each working unit
c                         is calculated by formula (9).
c
c                         When all velocities have been calculated a
c                         segment velocity is calculated by (10).
c
c NOTE: Input parameter WF is actually WT! (Jos van Gils, September 2001)
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 af                P  -
c  9 afs               P  -
c 10 c                 P  -
c  5 ngrid             I  Number of grid points in network.
c  6 nposeg            I  Number of positive segment numbers.
c  1 nsegmt            I  Number of segments defined.
c  3 nsegtb            I  Number of entries in segtab table.
c  7 psi               I  Space weight factor in Preissmann scheme.
c 11 qaggr             P  -
c 16 schz(nposeg)      IO Chezy coefficients for each segment.
c  2 segmnt(3,nsegmt)  I  Definition of table with segment pointers:
c                         (1,j) = Segment number
c                         (2,j) = Pointer to segtab for segment j.
c                         (3,j) = Number of enclosed gridpoints.
c  4 segtab(5,nsegtb)  I  This table contains for each segment the en-
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
c 17 shor(nposeg)      IO Horizontal surfaces for each segment.
c 15 slen(nposeg)      I  Lengths for each segment.
c 18 svel(nposeg)      IO Velocities for each segment.
c 19 swdt(nposeg)      O  Widths for each segment.
c 12 wf                P  -
c 13 wfs               P  -
c 14 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c wqgpar  Water Quality GridPoint ARea
c wqgpch  Water Quality GridPoint CHezy
c wqgpfl  Water Quality GridPoint FLow
c wqgpwd  Water Quality GridPoint WiDth
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
c $Log: wqaux.pf,v $
c Revision 1.4  1999/03/15  15:53:49  kuipe_j
c tabs removed
c
c Revision 1.3  1997/08/29  14:34:42  kuipe_j
c Make velocity positive
c
c Revision 1.2  1995/05/30  07:08:21  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:42  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:35:25  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer ngrid,
     +        nposeg,
     +        nsegmt,
     +        nsegtb
c
      integer segmnt (3,nsegmt)
c
      real    psi
c
      real    af     (ngrid),
     +        afs    (ngrid,2),
     +        c      (ngrid,4),
     +        qaggr  (ngrid,3),
     +        segtab (5,nsegtb),
     +        schz   (nposeg),
     +        shor   (nposeg),
     +        slen   (nposeg),
     +        svel   (nposeg),
     +        swdt   (nposeg),
     +        wf     (ngrid),
     +        wfs    (ngrid,2),
     +        x      (ngrid)
c
c     Variables
c
      integer igp1, igp2, isecwq, iseg, itab, ngp, ngrd, segno
c
      real    a1, a2, awu, ch1, ch2, cterm1, cterm2, cwu,
     +        lb, le, lemlb, q1, q2, swu, u1, u2, udenum,
     +        uwu, w1, w2, wwu, xemxb
c
c     Loop over segment numbers
c
      do 300 iseg = 1, nsegmt
c
         segno = segmnt(1,iseg)
c
         if (segno .gt. 0) then
c
            schz (segno) = 0
            shor (segno) = 0
            svel (segno) = 0
            swdt (segno) = 0
c
            udenum = 0
c
            itab = segmnt (2,iseg)
            ngrd = segmnt (3,iseg)
c
c           Loop over enclosed grid-cells
c
            do 200 ngp = 1, ngrd
c
c              Fetch definition of working unit enclosed by segment
c
               igp1   = int (segtab (1,itab))
               igp2   = int (segtab (2,itab))
               lb     =      segtab (3,itab)
               le     =      segtab (4,itab)
               isecwq = int (segtab (5,itab))
c
c              In case sec(tion) = 0 the complete grid-cell is enclosed.
c
               call wqgpar ( ngrid, af, afs, igp1, isecwq, a1 )
               call wqgpar ( ngrid, af, afs, igp2, isecwq, a2 )
c
               awu = (1.-psi) * a1 + (psi * a2)
c
               call wqgpfl ( ngrid, qaggr, igp1, isecwq, q1 )
               call wqgpfl ( ngrid, qaggr, igp2, isecwq, q2 )
c
               call wqgpwd ( ngrid, wf, wfs, igp1, isecwq, w1 )
               call wqgpwd ( ngrid, wf, wfs, igp2, isecwq, w2 )
c
               call wqgpch ( ngrid, c, igp1, isecwq, ch1 )
               call wqgpch ( ngrid, c, igp2, isecwq, ch2 )
c
c              Calculate Xe-Xb
c
               xemxb = x (igp2) - x (igp1)
c
c              Calculate Le-Lb
c
               lemlb = le - lb
c
c              Calculate width of working unit i
c
               wwu = (((1.-lb) * w1) + (le*w2)) / (1. - lb + le)
c
c              Calculate horizontal surface working unit i
c
               swu = wwu * xemxb * lemlb
c
c              Add to segment horizontal surface
c
               shor (segno) = shor (segno) + swu
c
c              Calculate velocity of working unit i
c
c              Not sharp enough!!! JvG Sept 2001
c              if (abs(awu) .lt. 1.0E-9) then
               if (abs(awu) .lt. 1.0E-3) then
                  uwu = 0.
               else
                  uwu = 0.5 * ( q1 + q2 ) / awu
               endif
c
c              Add to segment velocity
c
               svel (segno) = svel (segno) + ( lemlb * xemxb * uwu )
c
c              Update denumerator of velocity
c
               udenum  = udenum + ( lemlb * xemxb )
c
c                            2   2
c              Calculate  ( u / C ) * S for working unit
c
               if (abs(a1) .lt. 1.0E-9) then
                  u1 = 0.
               else
                  u1  = q1 / a1
               endif
c
               if (abs(a2) .lt. 1.0E-9) then
                  u2 = 0.
               else
                  u2  = q2 / a2
               endif
c
               if (abs(ch1) .lt. 1.0E-9) then
                  cterm1 = 0.
               else
                  cterm1 = (u1*u1) / (ch1*ch1) * 0.5 * xemxb * w1
               endif
c
               if (abs(ch2) .lt. 1.0E-9) then
                  cterm2 = 0.
               else
                  cterm2 = (u2*u2) / (ch2*ch2) * 0.5 * xemxb * w2
               endif
c
               cwu = cterm1 + cterm2
c
               schz (segno) = schz (segno) + (lemlb * cwu)
c
c              Increment itab pointer
c
               itab = itab + 1
c
 200        continue
c
c           Calculate u segment
c
c           The velocity sbhould always be positive (van Gils)
            svel(segno) = max(abs(svel(segno)/udenum),1.0E-9)
c
c           Calculate chezy coefficient segment
c
            if (abs(schz(segno)) .gt. 1.0E-9) then
               schz (segno) = sqrt ((shor (segno)*svel (segno)**2) /
     +                               schz (segno))
            else
               schz (segno) = 100.
            endif
c
c           Calculate width segment
c
            if (abs(slen(segno)) .lt. 1.0E-9) then
               swdt (segno) = 0.
            else
               swdt (segno) = shor (segno) / slen (segno)
            endif
            shor(segno) = max (shor(segno),0.1)
            swdt(segno) = max (swdt(segno),0.1)
         endif
c
 300  continue
c
      return
      end

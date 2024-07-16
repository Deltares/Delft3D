      subroutine MOITIP ( igp    ,isec   ,ngrid  ,
     +                    x      ,dtm    ,alphac ,
     +                    celer  ,sedtr  ,
     +                    alphad ,intcel
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOITIP (MOrphology InTegral on Intermediate grid Point)
c
c Module description: Calculate integral on point i+1/2
c
c                     The integral Ii+1/2 is determined according to
c                     the formulas described in the Functional Design
c                     Sediment transport and Morphology (document
c                     S-FO-002.3KV Appendix B).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 alphac            I  Stability factor for bottom scheme (>1)
c  9 alphad            I  Limiter constant
c  7 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c  5 dtm               I  Morphology time step.
c  1 igp               I  Gridpoint number
c 10 intcel            O  Calculated integral value for first or last
c                         cel of a branch
c  2 isec              I  Section number (1 or 2)
c  3 ngrid             I  Number of grid points in network.
c  8 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c  4 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moitip.pf,v $
c Revision 1.3  1998/06/11  11:47:16  kuipe_j
c Estuary special integrated
c
c Revision 1.2  1996/03/08  09:39:11  kuipe_j
c Headers + moptf temporarily = false
c
c Revision 1.1  1996/03/07  10:44:21  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c
c***********************************************************************
c
c     Parameters
c
      integer    igp    ,isec   ,ngrid

      real       alphac ,intcel, alphad

      real       x      (ngrid),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*)

      double     precision  dtm

c
c     Local variables
c
      real       spos1, spos2, spos3, sneg1, sneg2, sneg3,
     +           cpos1, cpos2, cpos3, cneg1, cneg2, cneg3,
     +           dx1, dx2, dx3,
     +           sipos1, sipos2, sineg1, sineg2,
     +           flwpos, flwneg, fuppos, fupneg,
     +           rpos, rneg, ipos, ineg, dtms
c
      logical    epsequ
      external   epsequ
c
c     Include sobek constants
c
      include '..\include\sobcon.i'

      dtms = sngl(dtm)
c
c     First the positive and negative values of Sediment transport
c     and celerities are determined using MIN and MAX functions
c
      cpos1 = MAX(celer(igp-1,isec),0.0)
      cpos2 = MAX(celer(igp,isec),0.0)
      cpos3 = MAX(celer(igp+1,isec),0.0)
      cneg1 = MIN(celer(igp,isec),0.0)
      cneg2 = MIN(celer(igp+1,isec),0.0)
      cneg3 = MIN(celer(igp+2,isec),0.0)
c
c wijziging 18 juni 1997: zie MOITEP.PF
c
c     if ( cpos1 .ge. 0. ) then
      if ( cpos1 .gt. 0. ) then
         spos1 = sedtr(igp-1,isec)
      else
         spos1 = 0.
      endif
      if ( cneg1 .lt. 0. ) then
         sneg1 = sedtr(igp,isec)
      else
         sneg1 = 0.
      endif
c
c wijziging 18 juni 1997: zie MOITEP.PF
c
c     if ( cpos2 .ge. 0. ) then
      if ( cpos2 .gt. 0. ) then
         spos2 = sedtr(igp,isec)
      else
         spos2 = 0.
      endif
      if ( cneg2 .lt. 0. ) then
         sneg2 = sedtr(igp+1,isec)
      else
         sneg2 = 0.
      endif
c
c wijziging 18 juni 1997: zie MOITEP
c
c     if ( cpos3 .ge. 0. ) then
      if ( cpos3 .gt. 0. ) then
         spos3 = sedtr(igp+1,isec)
      else
         spos3 = 0.
      endif
      if ( cneg3 .lt. 0. ) then
         sneg3 = sedtr(igp+2,isec)
      else
         sneg3 = 0.
      endif
c
c     Calculate delta x
c
      dx1 = x(igp) - x(igp-1)
      dx2 = x(igp+1) - x(igp)
      dx3 = x(igp+2) - x(igp+1)
c
c     Calculate sigma and flux, positive and negative
c
      sipos1 = 0.5 * alphac * (cpos2 + cpos1) * dtms / dx1
      sipos2 = 0.5 * alphac * (cpos3 + cpos2) * dtms / dx2
      sineg1 = 0.5 * alphac * (cneg2 + cneg1) * dtms / dx2
      sineg2 = 0.5 * alphac * (cneg3 + cneg2) * dtms / dx3
      flwpos = (1. - sipos2) * (spos3 - spos2)
      flwneg = (1. + sineg1) * (sneg2 - sneg1)
      fuppos = (dx2 / dx1 - sipos1) * (spos2 - spos1)
      fupneg = (dx2 / dx3 + sineg2) * (sneg3 - sneg2)
c
c     Calculate positive attribute of integral
c
      if (epsequ(flwpos, 0. , cdchk)) then
         if (flwpos .lt. 0.) then
            flwpos = flwpos - cdval
         else
            flwpos = flwpos + cdval
         endif
      endif
      rpos = fuppos / flwpos
      if (rpos .le. 0.0) then
         ipos = spos2
      elseif (rpos .ge. alphad) then
         ipos = spos2 + alphad * flwpos / 2.0
      else
         ipos = spos2 + fuppos / 2.0
      endif
c
c     Calculate negative component of integral
c
      if (epsequ(flwneg, 0. , cdchk)) then
         if (flwneg .lt. 0.) then
            flwneg = flwneg - cdval
         else
            flwneg = flwneg + cdval
         endif
      endif
      rneg = fupneg / flwneg
      if (rneg .le. 0.0) then
         ineg = sneg2
      elseif (rneg .ge. alphad) then
c Voorstel Sloff/Borsboom 7-8-1997
c ideetje:         ineg = sneg2 + alphad * flwneg / 2.0
         ineg = sneg2 - alphad * flwneg / 2.0
      else
c Voorstel Sloff/Borsboom 7-8-1997
c ideetje:         ineg = sneg2 + fupneg / 2.0
         ineg = sneg2 - fupneg / 2.0
      endif
c
c     Finally the integral is calculated
c
      intcel = (ineg + ipos) * dtms
      return
      end

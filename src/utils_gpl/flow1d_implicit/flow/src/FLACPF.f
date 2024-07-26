      subroutine FLACPF (nnf    ,pfa    ,mprev  ,mact   ,pf     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLACPF (FLow kalman Average Corr. PAr. Friction)
c
c Module description: The value of the uncertain correction parameter
c                     for bottom friction is determined in a specific
c                     grid point.
c
c                     A grid point can belong to two different groups
c                     so the value will be averaged.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 mact              I  Actual group number for bottom friction.
c  3 mprev             I  Previous group number for bottom friction.
c  1 nnf               I  Number of uncertain bed friction parameters.
c  5 pf                O  Uncertain bed friction parameter.
c  2 pfa(nnf)          I  Uncertain bed friction parameters of all
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flacpf.pf,v $
c Revision 1.3  1999/03/15  15:49:22  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:03:33  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:04  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters:
c
      integer nnf, mprev, mact
      real    pf, pfa(nnf)
c
c
c     Calculate pf for actual gridpoint.
c
      if ( mact .eq. mprev ) then
         if ( mact .eq. 0 ) then
            pf = 1.
         else
            pf = pfa(mact)
         endif
      else if ( mact .eq. 0 ) then
         pf = pfa(mprev)
      else if ( mprev .eq. 0 ) then
         pf = pfa(mact)
      else
         pf = ( pfa(mact) + pfa(mprev) ) / 2.
      endif
c
      end

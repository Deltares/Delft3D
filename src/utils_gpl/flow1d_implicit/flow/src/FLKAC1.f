      subroutine FLKAC1(ngrid  ,nnf    ,c      ,scifri ,pfa    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKAC1 (FLow KAlman Chezy correction 1)
c
c Module description: Correction on total bottom friction due to uncer
c                     tain correction parameter.
c
c                     C = C_cor / SQRT(Pf)
c                     with:
c                     C  : defined in every gridpoint
c                     Pf : defined in every gridcell
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c  1 ngrid             I  Number of grid points in network.
c  2 nnf               I  Number of uncertain bed friction parameters.
c  5 pfa(nnf)          I  Uncertain bed friction parameters of all
c  4 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
c                         process for bed friction (group nr. or correc-
c                         tion parameter nr.) of every normal grid cell,
c                         otherwise zero.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flacpf  FLow kalman Average Corr. PAr. Friction
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flkac1.pf,v $
c Revision 1.3  1999/03/15  15:50:13  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:03:55  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:24  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of function
c
      logical EQUAL
c
c     Declaration of parameters
c
      integer          ngrid, nnf
      integer          scifri(ngrid)
      real             pfa(nnf), c(ngrid)
c
c     Declaration of local variables
c
      integer          i, mprev, mact
      real             pf, pfprev, sqpf
c
      mact   = scifri(1)
      pfprev = pfa(max(mact,1))
c
      do 10 i = 1, ngrid
c
c        Calculate pf for actual gridpoint -i-
c
         mprev = mact
         mact  = scifri(i)
         call FLACPF (nnf    ,pfa    ,mprev  ,mact   ,pf     )
         if ( EQUAL(pf , pfprev) .and. i .ne. 1 ) then
c           - no computing  SQRT(Pf) necessary -
         else
            sqpf = sqrt(pf)
         endif
         pfprev = pf
c
         c(i) = c(i) / sqpf
c
   10 continue
c
      end

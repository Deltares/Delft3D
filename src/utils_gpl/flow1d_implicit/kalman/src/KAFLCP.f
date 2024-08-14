      subroutine KAFLCP(ngrid  ,nnf    ,scifri ,pfa    ,c      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAFLCP (Kalman Flow Chezy correction Parameter)
c
c Module description: Remove correction on total bottom friction due to
c                     uncertain correction parameter.
c
c                     C = C_cor * SQRT(Pf)
c                     with:
c                     C  : defined in every gridpoint
c                     Pf : defined in every gridcell
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c  1 ngrid             I  Number of grid points in network.
c  2 nnf               I  Number of uncertain bed friction parameters.
c  4 pfa(nnf)          I  Uncertain bed friction parameters of all
c  3 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
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
c $Log: kaflcp.pf,v $
c Revision 1.3  1999/03/15  15:51:45  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:53  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:30  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Function: Remove correction on total bottom friction.
c
c     Description: C_cor. = C * SQRT(Pf)
c
c                  C  : defined in every gridpoint
c                  Pf : defined in every gridcell
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
         c(i) = c(i) * sqpf
c
   10 continue
c
      end

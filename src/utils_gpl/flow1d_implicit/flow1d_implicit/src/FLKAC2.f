      subroutine FLKAC2(ngrid  ,nnf    ,scifri ,pfa    ,asubsc ,c      ,
     +                  cs     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLKAC2 (FLow Kalman Chezy correction 2)
c
c Module description: Correction of bottom friction per section due to
c                     uncertain corection parameter.
c                     Correction will be done for the total bottom
c                     friction and for the bottom friction of each
c                     section.
c
c                     C_cor. = C / SQRT(Pf)
c                     with:
c                     C  : defined in every gridpoint
c                     Pf : defined in every gridcell
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 asubsc(ngrid)     I  Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c  6 c(ngrid)          IO Actual Chezy coefficient for total channel in
c                         every grid point.
c  7 cs(ngrid,3)       IO Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
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
c $Log: flkac2.pf,v $
c Revision 1.3  1999/03/15  15:50:15  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:03:57  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:26  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Function: Correction of bottom friction per section.
c
c     Description: C_cor. = C / SQRT(Pf)
c
c                  C  : defined in every gridpoint
c                  Pf : defined in every gridcell
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
      real             pfa(nnf), asubsc(ngrid)
      real             c(ngrid), cs(ngrid,3)
c
c     Declaration of local variables
c
      integer          i, j, mprev, mact
      real             pf, pfprev, sqpf
c
      mact   = scifri(1)
      pfprev = pfa(max(mact,1))
c
      do 20 i = 1, ngrid
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
c        Correct C-total
c
         c(i) = c(i) / sqpf
c
         do 10 j=1, int(asubsc(i))+ 1 ,1
c
c           Correct C for main channel and sub sections
c
            cs(i,j) = cs(i,j) / sqpf
   10    continue
c
   20 continue
c
      end

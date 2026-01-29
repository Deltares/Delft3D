subroutine FLKAC1(ngrid  ,nnf    ,c      ,scifri ,pfa    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKAC1 (FLow KAlman Chezy correction 1)
!
! Module description: Correction on total bottom friction due to uncer
!                     tain correction parameter.
!
!                     C = C_cor / SQRT(Pf)
!                     with:
!                     C  : defined in every gridpoint
!                     Pf : defined in every gridcell
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
!  1 ngrid             I  Number of grid points in network.
!  2 nnf               I  Number of uncertain bed friction parameters.
!  5 pfa(nnf)          I  Uncertain bed friction parameters of all
!  4 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
!                         process for bed friction (group nr. or correc-
!                         tion parameter nr.) of every normal grid cell,
!                         otherwise zero.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flacpf  FLow kalman Average Corr. PAr. Friction
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flkac1.pf,v $
! Revision 1.3  1999/03/15  15:50:13  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:03:55  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:24  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of function
!
   logical EQUAL
!
!     Declaration of parameters
!
   integer          ngrid, nnf
   integer          scifri(ngrid)
   real             pfa(nnf), c(ngrid)
!
!     Declaration of local variables
!
   integer          i, mprev, mact
   real             pf, pfprev, sqpf
!
   mact   = scifri(1)
   pfprev = pfa(max(mact,1))
!
   do 10 i = 1, ngrid
!
!        Calculate pf for actual gridpoint -i-
!
      mprev = mact
      mact  = scifri(i)
      call FLACPF (nnf    ,pfa    ,mprev  ,mact   ,pf     )
      if ( EQUAL(pf , pfprev) .and. i .ne. 1 ) then
!           - no computing  SQRT(Pf) necessary -
      else
         sqpf = sqrt(pf)
      endif
      pfprev = pf
!
      c(i) = c(i) / sqpf
!
10 continue
!
end

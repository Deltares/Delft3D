subroutine KAFLCP(ngrid  ,nnf    ,scifri ,pfa    ,c      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFLCP (Kalman Flow Chezy correction Parameter)
!
! Module description: Remove correction on total bottom friction due to
!                     uncertain correction parameter.
!
!                     C = C_cor * SQRT(Pf)
!                     with:
!                     C  : defined in every gridpoint
!                     Pf : defined in every gridcell
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
!  1 ngrid             I  Number of grid points in network.
!  2 nnf               I  Number of uncertain bed friction parameters.
!  4 pfa(nnf)          I  Uncertain bed friction parameters of all
!  3 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
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
! $Log: kaflcp.pf,v $
! Revision 1.3  1999/03/15  15:51:45  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:53  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:30  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Function: Remove correction on total bottom friction.
!
!     Description: C_cor. = C * SQRT(Pf)
!
!                  C  : defined in every gridpoint
!                  Pf : defined in every gridcell
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
      c(i) = c(i) * sqpf
!
10 continue
!
end

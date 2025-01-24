subroutine FLKAC2(ngrid  ,nnf    ,scifri ,pfa    ,asubsc ,c      ,&
&cs     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKAC2 (FLow Kalman Chezy correction 2)
!
! Module description: Correction of bottom friction per section due to
!                     uncertain corection parameter.
!                     Correction will be done for the total bottom
!                     friction and for the bottom friction of each
!                     section.
!
!                     C_cor. = C / SQRT(Pf)
!                     with:
!                     C  : defined in every gridpoint
!                     Pf : defined in every gridcell
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 asubsc(ngrid)     I  Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!  6 c(ngrid)          IO Actual Chezy coefficient for total channel in
!                         every grid point.
!  7 cs(ngrid,3)       IO Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
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
! $Log: flkac2.pf,v $
! Revision 1.3  1999/03/15  15:50:15  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:03:57  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:26  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Function: Correction of bottom friction per section.
!
!     Description: C_cor. = C / SQRT(Pf)
!
!                  C  : defined in every gridpoint
!                  Pf : defined in every gridcell
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
   real             pfa(nnf), asubsc(ngrid)
   real             c(ngrid), cs(ngrid,3)
!
!     Declaration of local variables
!
   integer          i, j, mprev, mact
   real             pf, pfprev, sqpf
!
   mact   = scifri(1)
   pfprev = pfa(max(mact,1))
!
   do 20 i = 1, ngrid
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
!        Correct C-total
!
      c(i) = c(i) / sqpf
!
      do 10 j=1, int(asubsc(i))+ 1 ,1
!
!           Correct C for main channel and sub sections
!
         cs(i,j) = cs(i,j) / sqpf
10    continue
!
20 continue
!
end

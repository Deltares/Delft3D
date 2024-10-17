subroutine FLACPF (nnf    ,pfa    ,mprev  ,mact   ,pf     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLACPF (FLow kalman Average Corr. PAr. Friction)
!
! Module description: The value of the uncertain correction parameter
!                     for bottom friction is determined in a specific
!                     grid point.
!
!                     A grid point can belong to two different groups
!                     so the value will be averaged.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 mact              I  Actual group number for bottom friction.
!  3 mprev             I  Previous group number for bottom friction.
!  1 nnf               I  Number of uncertain bed friction parameters.
!  5 pf                O  Uncertain bed friction parameter.
!  2 pfa(nnf)          I  Uncertain bed friction parameters of all
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flacpf.pf,v $
! Revision 1.3  1999/03/15  15:49:22  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:03:33  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:04  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters:
!
   integer nnf, mprev, mact
   real    pf, pfa(nnf)
!
!
!     Calculate pf for actual gridpoint.
!
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
!
end

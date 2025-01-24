subroutine gsrory (d90   ,hrad  ,dunehe ,dunele ,chezy)
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsrory.F,v $
! Revision 1.2  1995/09/27  10:12:47  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Graded Sediment ROughness predictor according to van RIJn
!

!
!     Declaration of parameters
!
   real       d90   ,hrad  ,dunehe ,dunele ,chezy
!
!     Declaration of local parameters
!
   real       ks
!
   ks    = 3. * d90 + 1.1 * dunehe * (1. - exp(-25.*dunehe/dunele))
   chezy = 18. * log10 (12. * hrad / ks)

end

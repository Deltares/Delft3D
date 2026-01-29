subroutine gsdunl (lenopt ,depth  ,duncof ,dunele )

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdunl.F,v $
! Revision 1.2  1995/09/27  10:12:25  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment Calculation of DUNe Lenght

!
!     Declaration of parameters
!
   real       duncof(*)
   integer    lenopt
   real       depth  ,dunele
!
!     Declaration of local parameters
!
   real       factor (2)
!                        Yalin  Van Rijn
   data       factor /5.5   ,7.3     /

   if (lenopt .le. 2) then
      dunele = factor(lenopt) * depth
   else
      dunele = duncof(3)
   endif

end

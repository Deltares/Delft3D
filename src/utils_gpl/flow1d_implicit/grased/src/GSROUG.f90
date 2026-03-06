subroutine gsroug (rouopt ,d90   ,hrad  ,dunehe ,dunele ,chezy)
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsroug.F,v $
! Revision 1.2  1995/09/27  10:12:49  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Graded Sediment ROUGhness predictor
!

!
!     Declaration of parameters
!
   integer    rouopt
   real       d90   ,hrad  ,dunehe ,dunele ,chezy
!
!     Declaration of constants
!
!                Roughness option
!                Van Rijn   White, Paris and Bettess
   integer    rouryn    ,rouwpb
   parameter  (rouryn=1 ,rouwpb=2)

   if (rouopt .eq. rouryn) then
      call gsrory (d90   ,hrad  ,dunehe ,dunele ,chezy)
   else if (rouopt .eq. rouwpb) then
!        White, Paris and Bettess
   endif

end

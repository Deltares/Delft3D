subroutine gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,&
&chezy  ,velo   ,depth  ,frou2  ,duncof ,trforb,&
&duncon ,sedexp ,dunehe )

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsdunh.F,v $
! Revision 1.2  1995/09/27  10:12:23  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment Calculation of DUNe Height

!
!     Declaration of parameters
!
   integer    heiopt
   real       g      ,relden ,kinvis ,dmed   ,chezy   ,&
   &velo   ,depth  ,frou2  ,sedexp ,dunehe
   real       trforb(*)      ,duncof(*)      ,duncon(*)
   logical    initra
!
!     Declaration of constants
!
!                Dune height option
!                Gill      Van Rijn
   integer    dhgill   ,dhvryn
   parameter (dhgill=1 ,dhvryn=2)

   if (heiopt .eq. dhgill) then
      call gsdhgi (initra  ,g    ,relden ,kinvis ,dmed  ,&
      &chezy   ,velo ,depth  ,frou2  ,duncof(1)      ,&
      &duncof(2)     ,trforb ,duncon(1)     ,sedexp  ,&
      &dunehe  )
   else if (heiopt .eq. dhvryn) then
!        Van Rijn
   endif

end

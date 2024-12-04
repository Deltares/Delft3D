subroutine safilt (nbran  ,ngrid ,branch ,&
&grid   ,x      ,af    ,disgr  ,&
&filc   ,csd2  ,csa2   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SAFILT (SAlt FILTer)
!
! Module description: Filter concentration field in case negative values
!                     occur.
!
!                     This subroutine tests if negative concentration
!                     values occur in the calculated concentrations
!                     field. If this is the case subroutine SAFORF is
!                     called to ensure a positive
!                     concentration on each gridpoint on the network.
!                     If the concentrations are adapted c's (A*D*dc/dx)
!                     will be recalculated on adapted grid points.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!    af(ngrid)         I  Flow area at every grid point at time t(n+1)
!    branch            P  -
!    csa2              P  -
!    csd2(ngrid)       IO Diffusion (c s) in every grid point at time
!                         t(n+1).
!    disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
!    grid              P  -
!
!    nbran             I  Number of branches.
!    ngrid             I  Number of grid points in network.
!    x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! sadcdx  SAlt DC/DX calculation
! saforf  SAlt FORester Filter
! satneg  SAlt Test NEGative concentrations
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: safilt.pf,v $
! Revision 1.7  1999/03/15  15:53:22  kuipe_j
! tabs removed
!
! Revision 1.6  1995/10/18  09:00:21  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/09/22  10:03:17  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:37:17  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:09  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:02  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:44  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    nbran  ,ngrid
   integer    branch(4,nbran)
   integer    grid  (ngrid)
   real       x     (ngrid)   ,af    (ngrid)  ,disgr(ngrid),&
   &filc  (ngrid)   ,csd2  (ngrid)  ,csa2 (ngrid)
!
!     Declaration of local variables
!
   integer    igr
   real       ceps
   logical    filter
!
   ceps   = -1.e-7
!
   call satneg (ngrid ,ceps ,csa2 ,filter)
!
   if (filter) then
!
!        Eenvoudige beveiliging tegen negatieve concentraties
!
      call saforf (nbran ,ngrid ,ceps ,branch ,csa2  ,filc)

!        Calculate A*D*dc/dx when the concentrations are adapted.
!        Calculate dc/dx first.
!                                                                 dc/dx
      call sadcfi (nbran  ,ngrid  ,branch ,grid   ,csa2   ,x  ,csd2,&
      &filc   )
!
      do igr = 1,ngrid

         if (filc(igr).gt.0.) then
!                                             dc/dx
            csd2(igr) = af(igr)*disgr(igr)*csd2(igr)
         endif
      enddo
   endif
!
end

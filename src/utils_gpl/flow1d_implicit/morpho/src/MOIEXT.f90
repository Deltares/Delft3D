subroutine MOIEXT ( igpbou ,dir    ,isec   ,ngrid  ,&
&dtm    ,x      ,celer  ,sedtr  ,iextra )


!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         J. Kuipers
!
! Module:             MOGREP (MOrphology Integeral EXTra)
!
! Module description: Calculate extra integral on point n-1/2
!                     for outflowing part of branch.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 31 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 29 dtm               I  Morphology time step.
!  1 igpbou            I  Calculated integral value on boundary
!  3 isec              I  Section number (1 or 2)
!  4 ngrid             I  Number of grid points in network.
! 32 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moiext.pf,v $
! Revision 1.2  1998/06/18  13:26:42  kuipe_j
! sign change
!
! Revision 1.1  1998/06/12  07:59:00  kuipe_j
! Estuary special integrated
!
!
!***********************************************************************
!
!     Parameters
!
   integer    igpbou ,dir, isec ,ngrid

   real       iextra

   real       x      (ngrid),&
   &celer  (ngrid,*),&
   &sedtr  (ngrid,*)

   double     precision  dtm
!
!     Local variables
!
   real       sigma, dtms, sboun
!
   dtms   = sngl(dtm)
!
   sigma  = celer(igpbou,isec)*dtms/&
   &(x(igpbou+dir)-x(igpbou))*real(dir)
   sboun  = sedtr(igpbou,isec)
   iextra = (sboun + sigma * (sboun - sedtr(igpbou+dir,isec))*&
   &0.5 * real(dir)) * dtms
   return
end

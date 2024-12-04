subroutine FLKAPA(istep  ,ngrid  ,nnf    ,nstru  ,nnmu   ,nbran  ,&
&c      ,scifri ,pfa    ,strpar ,scimu  ,pmua   ,&
&branch ,wfrict ,tauwi  ,pw     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLKAPA (FLow KAlman correction PArameter)
!
! Module description: Correction of bottom friction, contraction
!                     coefficient of general structure and wind friction
!                     due to uncertain correction parameters.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 branch            P  -
!  7 c                 P  -
!  1 istep             I  Current time step number (t(n+1)).
!  6 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  3 nnf               I  Number of uncertain bed friction parameters.
!  5 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  4 nstru             I  Number of structures.
!  9 pfa               P  -
! 12 pmua              P  -
! 16 pw                P  -
!  8 scifri            P  -
! 11 scimu             P  -
! 10 strpar            P  -
! 15 tauwi             P  -
! 14 wfrict            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flkac1  FLow KAlman Chezy correction 1
! flkasp  FLow KAlman Structure correction Parameter
! flkawp  FLow KAlman Wind correction Parameter
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flkapa.pf,v $
! Revision 1.3  1999/03/15  15:50:18  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:01  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:23:31  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer   istep, ngrid, nnf, nstru, nnmu, nbran
   integer   branch(4,nbran), wfrict(3,nbran)
   integer   scifri(ngrid), scimu(nstru)
   real      pfa(nnf), c(ngrid), strpar(dmstrpar,*),&
   &pmua(nnmu)
   real      pw(1), tauwi(ngrid)
!
   call FLKAC1(ngrid  ,nnf    ,c      ,scifri ,pfa    )
!
   if ( istep .eq. 1 ) then
      call FLKASP(nstru  ,nnmu   ,strpar ,scimu  ,pmua   )
   endif
!
   call FLKAWP(nbran  ,ngrid  ,branch ,wfrict ,tauwi  ,pw     )
!
end

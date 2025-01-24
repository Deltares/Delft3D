subroutine FLGA03 (strpar, istru , nstru , af    , wf    ,&
&h     , ngrid , teken , q     , iup   ,&
&idn   , npier , wn    , le    , cg    ,&
&leg   , ksii  , kp    , ka    , lw    ,&
&li    , lo    , ksa   , ksi   , kso   ,&
&wp    , alpha , beta  , bot   , qin   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLGA03 (FLow Get Arguments for structure type 03)
!
! Module description: Unpack the array strpar for a sluice with bottom
!                     hinged gate.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flga03.pf,v $
! Revision 1.2  1998/06/08  12:35:24  kuipe_j
! log added
!
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
! declare arguments
   integer teken , istru , nstru , ngrid , iup   , idn   ,&
   &npier
   real    strpar(dmstrpar,nstru)
   real    af(ngrid), wf(ngrid)
   double precision  q(ngrid), h(ngrid)
   real    wn    , le    , cg    , leg   , ksii  , kp    ,&
   &ka    , lw    , li    , lo    , ksa   , ksi   ,&
   &kso   , wp    , alpha , beta  , bot   , qin
!
! unpack array
   wn    = strpar(1,istru)
   le    = strpar(2,istru)
   cg    = strpar(3,istru)
   leg   = strpar(4,istru)
   ksii  = strpar(5,istru)
   npier = int(strpar(6,istru))
   kp    = strpar(7,istru)
   ka    = strpar(8,istru)
   lw    = strpar(9,istru)
   li    = strpar(10,istru)
   lo    = strpar(11,istru)
   ksa   = strpar(12,istru)
   ksi   = strpar(13,istru)
   kso   = strpar(14,istru)
   wp    = strpar(15,istru)
   alpha = strpar(16,istru)
   beta  = strpar(17,istru)
!
   if (teken .gt. 0) then
      bot = h(iup) - af(iup) / wf(iup)
      qin = q(iup)
   else
      bot = h(idn) - af(idn) / wf(idn)
      qin = q(idn)
   endif
!
   return
end

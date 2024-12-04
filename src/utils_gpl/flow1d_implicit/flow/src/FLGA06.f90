subroutine FLGA06 (strpar, istru , nstru , af    , wf    ,&
&h     , ngrid , q     , iup   , idn   ,&
&teken , nculv , wi    , di    , wo    ,&
&do    , lg    , alpha , go    , gm    ,&
&wg    , li    , lo    , ksa   ,&
&ksi   , kso   , ksipos, lw    , beta  ,&
&bot   , qin   , lpress)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLGA06 (FLow Get Arguments for structure type 06)
!
! Module description: Unpack the array strpar for a culvert with
!                     pressure flow.
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
! $Log: flga06.pf,v $
! Revision 1.2  1998/06/08  12:35:27  kuipe_j
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
   integer istru , nstru , iup   , idn   , ngrid , teken ,&
   &nculv
   real    strpar(dmstrpar,nstru)
   real    wi    , di    , wo    , do    , lg    , alpha ,&
   &go    , gm    , wg    , li    , lo    ,&
   &ksa   , ksi   , kso   , ksipos, lw    , beta  ,&
   &bot   , qin   , af(ngrid), wf(ngrid)
   double precision h(ngrid), q(ngrid)
   logical lpress
!
! unpack array
   wi     = strpar(1,istru)
   di     = strpar(2,istru)
   wo     = strpar(3,istru)
   do     = strpar(4,istru)
   lg     = strpar(5,istru)
   alpha  = strpar(6,istru)
   go     = strpar(7,istru)
   gm     = strpar(8,istru)
   wg     = strpar(9,istru)
!     ksineg = strpar(10,istru)
   li     = strpar(11,istru)
   lo     = strpar(12,istru)
   ksa    = strpar(13,istru)
   ksi    = strpar(14,istru)
   kso    = strpar(15,istru)
   ksipos = strpar(16,istru)
   lw     = strpar(17,istru)
   nculv  = INT(strpar(18,istru))
   beta   = strpar(19,istru)
   lpress = INT(strpar(20,istru)) .eq. 1
!
   if (teken .gt. 0) then
      bot   = h(iup) - af(iup) / wf(iup)
      qin   = q(iup)
   else
      bot   = h(idn) - af(idn) / wf(idn)
      qin   = q(idn)
   endif
!
   return
end

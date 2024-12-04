subroutine FLGA08 (strpar, istru , nstru , af    , wf    ,&
&h     , ngrid , q     , iup   , idn   ,&
&teken , npier , wn    , kp    , ka    ,&
&le    , go    , gu    , lw    ,&
&li    , lo    , c2    , c3    , wp    ,&
&ksa   , ks    , alpha , ksii  , cga   ,&
&beta  , bot   , qin   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLGA08 (FLow Get Arguments for structure type 08)
!
! Module description: Unpack the array strpar for a sluice with over/
!                     underflow gate.
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
! $Log: flga08.pf,v $
! Revision 1.2  1998/06/08  12:35:30  kuipe_j
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
   &npier
   real    strpar(dmstrpar,nstru)
   real    wn    , kp    , ka    , le    , go    ,&
   &gu    , lw    , li    , lo    , c2    , c3    ,&
   &wp    , ksa   , ks    , alpha , ksii  , cga   ,&
   &beta  , bot   ,&
   &af(ngrid), wf(ngrid)
   double precision h(ngrid), q(ngrid),qin
!
! unpack array
   wn    = strpar(1,istru)
   npier = INT(strpar(2,istru))
   kp    = strpar(3,istru)
   ka    = strpar(4,istru)
   le    = strpar(5,istru)
   go    = strpar(6,istru)
   gu    = strpar(7,istru)
   lw    = strpar(8,istru)
   li    = strpar(9,istru)
   lo    = strpar(10,istru)
   c2    = strpar(11,istru)
   c3    = strpar(12,istru)
   wp    = strpar(13,istru)
   ksa   = strpar(14,istru)
   ks    = strpar(15,istru)
   alpha = strpar(16,istru)
   ksii  = strpar(17,istru)
   cga   = INT(strpar(18,istru))
   beta  = strpar(19,istru)
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

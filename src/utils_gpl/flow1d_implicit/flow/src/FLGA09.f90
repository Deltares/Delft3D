subroutine FLGA09 (strpar, istru , nstru , q     , ngrid ,&
&qin   , iup   , idn   , teken , wn    ,&
&wp    , le    , shapco)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLGA08 (FLow Get Arguments for structure type 09)
!
! Module description: Unpack the array strpar for bridge piers
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
! $Log: flga09.pf,v $
! Revision 1.2  1998/06/08  12:35:31  kuipe_j
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
   &itype
   real    strpar(dmstrpar,nstru)
   real    wn    , wp    , le    , shapco, qin
   double precision  q(ngrid)
   logical  equal
   external equal
!
! unpack array
   wn     = strpar(1,istru)
   wp     = strpar(2,istru)
   le     = strpar(3,istru)
   shapco = strpar(4,istru)
   itype  = INT(strpar(5,istru))
!
   if (EQUAL(shapco,0.)) then
      if (itype .eq. 1) shapco = 1.25
      if (itype .eq. 2) shapco = 0.9
      if (itype .eq. 3) shapco = 1.05
   endif

   if (teken .gt. 0) then
      qin   = q(iup)
   else
      qin   = q(idn)
   endif
!
   return
end

subroutine FLGA10 (strpar, le    , appwid, gapwid, gaplen,&
&ks    , c1    , c3    , ccr   , applen,&
&itype , q     , ngrid , qin   , iup   ,&
&idn   , teken , istru , nstru )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Parse New Model Database (V2.0)
!
! Programmer:         P.R. Evans
!
! Module:             FLow Get Arguments structure type 10 (Abutments)
!
! Module description: Unpack array strpar for structure type 10
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
! $Log: flga10.pf,v $
! Revision 1.2  1998/06/08  12:35:33  kuipe_j
! log added
!
!
!
!***********************************************************************
!
   implicit none
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
! declare arguments
   integer istru , nstru
   real    strpar(dmstrpar,nstru)
   real    le    , appwid, gapwid, gaplen, ks    ,&
   &c1    , c3    , ccr   , applen, qin
   integer itype , iup   , idn   , ngrid , teken
   double precision  q(ngrid)
!
! unpack array
   le     = strpar(1,istru)
   appwid = strpar(2,istru)
   gapwid = strpar(3,istru)
   gaplen = strpar(4,istru)
   ks     = strpar(5,istru)
   c1     = strpar(6,istru)
   c3     = strpar(7,istru)
   ccr    = strpar(8,istru)
   applen = strpar(9,istru)
   itype  = INT(strpar(10,istru))
!
   if (teken .gt. 0) then
      qin   = q(iup)
   else
      qin   = q(idn)
   endif
!
   return
end

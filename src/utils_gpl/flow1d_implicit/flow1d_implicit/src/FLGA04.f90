subroutine FLGA04 (strpar, istru , nstru , af    , wf    ,&
&h     , ngrid , teken , width , heidia,&
&cullen, le    , z     , alpha , ks    ,&
&ksa   , rloc  , theta , beta  , bot   ,&
&qin   , wloc  , cc    , q     , iup   ,&
&idn   , applen, rle   )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLGA04 (FLow Get Arguments for structure type 04)
!
! Module description: Unpack the array strpar for a culvert
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
! $Log: flga04.pf,v $
! Revision 1.2  1998/06/08  12:35:26  kuipe_j
! log added
!
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
! declare arguments
   integer istru , nstru , iup   , idn   , ngrid , teken
   real    width , heidia, cullen, le    , z     , alpha ,&
   &ks    , ksa   , rloc  , theta , wloc  , bot   ,&
   &qin   , beta  , applen, rle
   real    strpar(dmstrpar,nstru), cc(6), af(ngrid), wf(ngrid)
   double precision h(ngrid), q(ngrid)
!
! unpack array
   width  = strpar(1,istru)
   heidia = strpar(2,istru)
   cullen = strpar(3,istru)
   le     = strpar(4,istru)
   alpha  = strpar(6,istru)
   ks     = strpar(7,istru)
   ksa    = ks
   rloc   = 0.15
   theta  = 30.
   beta   = 1.0
   applen = 5 * width
!
   if (teken .gt. 0) then
      bot   = h(iup) - af(iup) / wf(iup)
      qin   = q(iup)
      rle   = strpar(4,istru)
      z     = strpar(5,istru)
      wloc  = strpar(8,istru)
      cc(1) = strpar(9,istru)
      cc(2) = strpar(10,istru)
      cc(3) = strpar(11,istru)
      cc(4) = strpar(12,istru)
      cc(5) = strpar(13,istru)
      cc(6) = strpar(14,istru)
   else
      bot   = h(idn) - af(idn) / wf(idn)
      qin   = q(idn)
      rle   = strpar(4,istru) - strpar(5,istru)
      z     = -strpar(5,istru)
      wloc  = strpar(15,istru)
      cc(1) = strpar(16,istru)
      cc(2) = strpar(17,istru)
      cc(3) = strpar(18,istru)
      cc(4) = strpar(19,istru)
      cc(5) = strpar(20,istru)
      cc(6) = strpar(21,istru)
   endif
!
   return
end

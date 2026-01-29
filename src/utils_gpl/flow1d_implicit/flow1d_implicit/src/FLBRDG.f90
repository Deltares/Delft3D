subroutine FLBRDG (g     , il    , ir    , ngrid , istru ,&
&nstru , strpar, strclo, h     , h1    ,&
&q     , q2    , af    , rho   ,&
&strhis, asde  , csde  , esde  )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLBRDG (FLow structure BRiDGe piers)
!
! Module description: In subroutine FLBRDG the ABCDE coefficients are
!                     computed for bridge piers.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga09  unpacks the array strpar for bridge piers
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flbrdg.pf,v $
! Revision 1.2  1998/06/08  12:35:10  kuipe_j
! log added
!
!
!
!***********************************************************************

!
! declare include files
   include '../include/sobdim.i'
!
! declare arguments
   integer il   , ir    , ngrid , istru , nstru
   real    g    , asde  , csde  , esde
   real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   real af(ngrid), rho(ngrid)
   logical strclo(nstru)

!
! declare local variables
   integer teken , iup   , idn
   real    crest , hunp1 , hun   , hdn   , uu    ,&
   &hdnp1, ud    , dh    , qa    , qdhu  , qdhd
   real    wn    , wp    , le    , shapco, qin
   real    rteken
   logical strsta
!
! declare functions
   real FLQH09

   crest = strpar(3,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
!
   teken = INT(rteken)
   call FLGA09 (strpar, istru , nstru , q     , ngrid ,&
   &qin   , iup   , idn   , teken , wn    ,&
   &wp    , le    , shapco)
!
   dh = 0.001
!
   strsta = .true.
   qa = rteken * FLQH09 (g, hunp1 , real(hdnp1) , qin   , wn    ,&
   &wp    , le    , shapco, teken , strsta,&
   &strclo, istru , nstru )
!
!      strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH09 (g     , hunp1+dh,&
      &real(hdnp1) , qin   , wn    ,&
      &wp    , le    , shapco, teken , strsta,&
      &strclo, istru , nstru )
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH09 (g     , hunp1 , real(hdnp1)-dh,&
      &qin   , wn    ,&
      &wp    , le    , shapco, teken , strsta,&
      &strclo, istru , nstru )
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH09 (g     , hunp1 , real(hdnp1-dh),&
      &qin   , wn    ,&
      &wp    , le    , shapco, teken , strsta,&
      &strclo, istru , nstru )
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH09 (g     , hunp1+dh,&
      &real(hdnp1) , qin   , wn    ,&
      &wp    , le    , shapco, teken , strsta,&
      &strclo, istru , nstru )
      qdhd = (-qdhd - qa) / dh
   endif
!
   asde = qdhu
   csde = qdhd
!
!      if (iter.ne.1) then
!         relst1 = 1.0 - relstr
!         qa   = relstr*qa + relst1*strhis(4,istru)
!      endif
   strhis(4,istru) = qa
!
   if (teken .gt. 0) then
      esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
   else
      esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
   endif

!      if (teken .gt. 0) then
!        esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
!      else
!        esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
!      endif

!
!       if (qa .le. 0.) then
!         asde = 0.
!         csde = 0.
!         esde = 0.
!       endif

end

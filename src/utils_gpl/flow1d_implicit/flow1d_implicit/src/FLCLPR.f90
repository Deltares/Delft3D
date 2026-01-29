subroutine FLCLPR (g     , il    , ir    , ngrid , istru ,&
&nstru , strpar, strclo, h     , h1    ,&
&q     , q2    , af    , wf    , rho   ,&
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
! Module:             FLCLPR (FLow structure CuLvert with PRessure flow)
!
! Module description: In subroutine FLCLPR the ABCDE coefficients are
!                     computed for a culvert with pressure flow
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga06  unpacks the array strpar for a culvert with pressure flow
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flclpr.pf,v $
! Revision 1.2  1998/06/08  12:35:11  kuipe_j
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
   real    af(ngrid), wf(ngrid), rho(ngrid)
   logical strclo(nstru)

!
! declare local variables
   integer teken , iup   , idn   , nculv
   real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,&
   &ud    , dh    , qa    , qdhu  , qdhd
   real    wi    , di    , wo    ,&
   &do    , lg    , alpha , go    , gm    ,&
   &wg    , li    , lo    , ksa   ,&
   &ksi   , kso   , ksipos, lw    , beta  ,&
   &bot   , qin
   logical strsta
   logical lpress
   real    rteken
!
! declare functions
   real FLQH06


   crest = strpar(5,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
   teken = INT(rteken)
!
   call FLGA06 (strpar, istru , nstru , af    , wf    ,&
   &h     , ngrid , q     , iup   , idn   ,&
   &teken , nculv , wi    , di    , wo    ,&
   &do    , lg    , alpha , go    , gm    ,&
   &wg    , li    , lo    , ksa   ,&
   &ksi   , kso   , ksipos, lw    , beta  ,&
   &bot   , qin   , lpress)
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQH06 (istru , nstru , strsta, strclo, g     ,&
   &hunp1 , hdnp1 , uu    , qin   , nculv ,&
   &wi    , di    , wo    , do    , lg    ,&
   &alpha , go    , gm    , wg    ,&
   &li    , lo    , ksa   , ksi   , kso   ,&
   &ksipos, lw    , beta  , bot   , lpress)
!
   strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH06 (istru , nstru , strsta, strclo, g     ,&
      &hunp1+dh,&
      &hdnp1 , uu    , qin   , nculv ,&
      &wi    , di    , wo    , do    , lg    ,&
      &alpha , go    , gm    , wg    ,&
      &li    , lo    , ksa   , ksi   , kso   ,&
      &ksipos, lw    , beta  , bot   , lpress)
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH06 (istru , nstru , strsta, strclo, g     ,&
      &hunp1 , hdnp1-dh,&
      &uu    , qin   , nculv ,&
      &wi    , di    , wo    , do    , lg    ,&
      &alpha , go    , gm    , wg    ,&
      &li    , lo    , ksa   , ksi   , kso   ,&
      &ksipos, lw    , beta  , bot   , lpress)
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH06 (istru , nstru , strsta, strclo, g     ,&
      &hunp1 , hdnp1-dh,&
      &uu    , qin   , nculv ,&
      &wi    , di    , wo    , do    , lg    ,&
      &alpha , go    , gm    , wg    ,&
      &li    , lo    , ksa   , ksi   , kso   ,&
      &ksipos, lw    , beta  , bot   , lpress)
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH06 (istru , nstru , strsta, strclo, g     ,&
      &hunp1+dh,&
      &hdnp1 , uu    , qin   , nculv ,&
      &wi    , di    , wo    , do    , lg    ,&
      &alpha , go    , gm    , wg    ,&
      &li    , lo    , ksa   , ksi   , kso   ,&
      &ksipos, lw    , beta  , bot   , lpress)
      qdhd = (-qdhd - qa) / dh
   endif
!
   asde = qdhu
   csde = qdhd
!
   if (teken .gt. 0) then

      esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde

   else

      esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde

   endif

!

end

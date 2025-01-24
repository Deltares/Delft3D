subroutine FLSLBO (g     , il    , ir    , ngrid , istru ,&
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
! Module:             FLSLBO (FLow structure SLuice with BOttom hinged
!                     gate
!
! Module description: In subroutine FLSLBO the ABCDE coefficients are
!                     computed for a sluice with bottom hinged gate.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga03  unpacks the array strpar for a sluice with bottom hinged gate.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flslbo.pf,v $
! Revision 1.2  1998/06/08  12:35:47  kuipe_j
! log added
!
!
!
!***********************************************************************

!
! declare include files
   include '..\include\sobdim.i'
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
   integer teken , iup   , idn   , npier
   real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,&
   &ud    , dh    , qa    , qdhu  , qdhd
   real    wn    , le    , cg    , leg   , ksii  , kp    ,&
   &ka    , lw    , li    , lo    , ksa   , ksi   ,&
   &kso   , wp    , alpha , beta  , bot   , qin
   logical strsta
   real    rteken
!
! declare functions
   real FLQH03


   crest = strpar(4,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
   teken = INT(rteken)
!
   call FLGA03 (strpar, istru , nstru , af    , wf    ,&
   &h     , ngrid , teken , q     , iup   ,&
   &idn   , npier , wn    , le    , cg    ,&
   &leg   , ksii  , kp    , ka    , lw    ,&
   &li    , lo    , ksa   , ksi   , kso   ,&
   &wp    , alpha , beta  , bot   , qin   )
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQH03 (teken , strsta, strclo, istru , nstru ,&
   &wn    , le    , cg    , leg   , ksii  ,&
   &npier , kp    , ka    , lw    , li    ,&
   &lo    , ksa   , ksi   , kso   , wp    ,&
   &alpha , beta  , qin   , bot   , g     ,&
   &hunp1 , hdnp1 , uu    )
!
   strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH03 (teken , strsta, strclo, istru , nstru ,&
      &wn    , le    , cg    , leg   , ksii  ,&
      &npier , kp    , ka    , lw    , li    ,&
      &lo    , ksa   , ksi   , kso   , wp    ,&
      &alpha , beta  , qin   , bot   , g     ,&
      &hunp1+dh, hdnp1 , uu    )
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH03 (teken , strsta, strclo, istru , nstru ,&
      &wn    , le    , cg    , leg   , ksii  ,&
      &npier , kp    , ka    , lw    , li    ,&
      &lo    , ksa   , ksi   , kso   , wp    ,&
      &alpha , beta  , qin   , bot   , g     ,&
      &hunp1 , hdnp1-dh, uu    )
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH03 (teken , strsta, strclo, istru , nstru ,&
      &wn    , le    , cg    , leg   , ksii  ,&
      &npier , kp    , ka    , lw    , li    ,&
      &lo    , ksa   , ksi   , kso   , wp    ,&
      &alpha , beta  , qin   , bot   , g     ,&
      &hunp1 , hdnp1-dh, uu    )
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH03 (teken , strsta, strclo, istru , nstru ,&
      &wn    , le    , cg    , leg   , ksii  ,&
      &npier , kp    , ka    , lw    , li    ,&
      &lo    , ksa   , ksi   , kso   , wp    ,&
      &alpha , beta  , qin   , bot   , g     ,&
      &hunp1+dh, hdnp1 , uu    )
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

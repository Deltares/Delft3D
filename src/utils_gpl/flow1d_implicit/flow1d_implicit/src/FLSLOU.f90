subroutine FLSLOU (g     , il    , ir    , ngrid , istru ,&
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
! Module:             FLSLOU (FLow structure SLuice with Overflow/
!                             Underflow date.
!
! Module description: In subroutine FLSLOU the ABCDE coefficients are
!                     computed for a sluice with overflow/ underflow
!                     gate
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga08  unpacks the array strpar for an open flume
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flslou.pf,v $
! Revision 1.2  1998/06/08  12:35:48  kuipe_j
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
   real    af(ngrid), wf(ngrid), rho(ngrid)
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   logical strclo(nstru)

!
! declare local variables
   integer teken , iup   , idn   , npier
   real    crest , hunp1 , hun   , hdn   , uu    ,&
   &ud    , dh    , qa    , qdhu  , qdhd
   real    wn    , kp    , ka    , le    , go    , gu    ,&
   &lw    , li    , lo    , c2    , c3    , wp    ,&
   &ksa   , ks    , alpha , ksii  , cga   , beta  ,&
   &bot   , hdnp1
   double precision qin
   logical strsta
   real    rteken
!
! declare functions
   real FLQH08


   crest = strpar(5,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
   teken = INT(rteken)
!
   call FLGA08 (strpar, istru , nstru , af    , wf    ,&
   &h     , ngrid , q     , iup   , idn   ,&
   &teken , npier , wn    , kp    , ka    ,&
   &le    , go    , gu    , lw    ,&
   &li    , lo    , c2    , c3    , wp    ,&
   &ksa   , ks    , alpha , ksii  , cga   ,&
   &beta  , bot   , qin   )
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQH08 (teken , npier , istru , nstru , g     ,&
   &qin   , hunp1 , hdnp1 , uu    ,&
   &wn    , kp    , ka    , le    , go    ,&
   &gu    , lw    , li    , lo    , c2    ,&
   &c3    , wp    , ksa   , ks    , alpha ,&
   &ksii  , cga   , beta  , strsta, strclo,&
   &bot   )
!
   strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH08 (teken , npier , istru , nstru , g     ,&
      &qin   , hunp1+dh,&
      &hdnp1 , uu    ,&
      &wn    , kp    , ka    , le    , go    ,&
      &gu    , lw    , li    , lo    , c2    ,&
      &c3    , wp    , ksa   , ks    , alpha ,&
      &ksii  , cga   , beta  , strsta, strclo,&
      &bot   )
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH08 (teken , npier , istru , nstru , g     ,&
      &qin   , hunp1 , hdnp1-dh,&
      &uu    ,&
      &wn    , kp    , ka    , le    , go    ,&
      &gu    , lw    , li    , lo    , c2    ,&
      &c3    , wp    , ksa   , ks    , alpha ,&
      &ksii  , cga   , beta  , strsta, strclo,&
      &bot   )
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH08 (teken , npier , istru , nstru , g     ,&
      &qin   , hunp1 , hdnp1-dh,&
      &uu    ,&
      &wn    , kp    , ka    , le    , go    ,&
      &gu    , lw    , li    , lo    , c2    ,&
      &c3    , wp    , ksa   , ks    , alpha ,&
      &ksii  , cga   , beta  , strsta, strclo,&
      &bot   )
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH08 (teken , npier , istru , nstru , g     ,&
      &qin   , hunp1+dh,&
      &hdnp1 , uu    ,&
      &wn    , kp    , ka    , le    , go    ,&
      &gu    , lw    , li    , lo    , c2    ,&
      &c3    , wp    , ksa   , ks    , alpha ,&
      &ksii  , cga   , beta  , strsta, strclo,&
      &bot   )
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

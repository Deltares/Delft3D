subroutine FLFLUM (g     , il    , ir    , ngrid , istru ,&
&nstru , strpar, strclo, h     , h1    ,&
&q     , q2    , af    , rho   ,&
&strhis, asde  , csde  , esde  , hlev  ,&
&maxlev)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLFLUM (FLow structure open FLUMe)
!
! Module description: In subroutine FLFLUM the ABCDE coefficients are
!                     computed for an open flume.
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga02  unpacks the array strpar for an open flume
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flflum.pf,v $
! Revision 1.2  1998/06/08  12:35:16  kuipe_j
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
   integer il   , ir    , ngrid , istru , nstru, maxlev
   real    g    , asde  , csde  , esde, hdnp1
   real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
   double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   real    af(ngrid), rho(ngrid)
   double precision hlev(ngrid,maxlev)
   logical strclo(nstru)

!
! declare local variables
   integer teken , iup   , idn
   real    crest , hunp1 , hun   , hdn   , uu    ,&
   &ud    , dh    , qa    , qdhu  , qdhd
   real    le    , z     , length, alpha , lw    , ksa   ,&
   &ks    , beta  , bot   , qin   , rloc  , wloc  ,&
   &theta , cc(3) , width , rteken
   logical strsta
   logical lbvin , lbvout
!
! declare functions
   real FLQH02


   crest = strpar(2,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken )
   teken = INT(rteken)
!
   call FLGA02 (strpar, istru , nstru , hlev  , maxlev,&
   &ngrid , teken , width , le    , z     ,&
   &length, alpha , lw    , ksa   , ks    ,&
   &beta  , lbvin , lbvout, bot   , qin   ,&
   &rloc  , wloc  , theta , cc    , q     ,&
   &iup   , idn   )

!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQH02 (g     , istru , nstru , strsta, strclo,&
   &hunp1 , hdnp1 , uu    , width , le    ,&
   &z     , length, alpha , lw    , ksa   ,&
   &ks    , rloc  , wloc  , theta , cc    ,&
   &beta  , bot   , qin   , teken , lbvin ,&
   &lbvout)
!
!      strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH02 (g     , istru , nstru , strsta, strclo,&
      &hunp1+dh,&
      &hdnp1 , uu    , width , le    ,&
      &z     , length, alpha , lw    , ksa   ,&
      &ks    , rloc  , wloc  , theta , cc    ,&
      &beta  , bot   , qin   , teken , lbvin ,&
      &lbvout)
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH02 (g     , istru , nstru , strsta, strclo,&
      &hunp1 , hdnp1-dh,&
      &uu    , width , le    ,&
      &z     , length, alpha , lw    , ksa   ,&
      &ks    , rloc  , wloc  , theta , cc    ,&
      &beta  , bot   , qin   , teken , lbvin ,&
      &lbvout)
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH02 (g     , istru , nstru , strsta, strclo,&
      &hunp1 , hdnp1-dh,&
      &uu    , width , le    ,&
      &z     , length, alpha , lw    , ksa   ,&
      &ks    , rloc  , wloc  , theta , cc    ,&
      &beta  , bot   , qin   , teken , lbvin ,&
      &lbvout)
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH02 (g     , istru , nstru , strsta, strclo,&
      &hunp1+dh,&
      &hdnp1 , uu    , width , le    ,&
      &z     , length, alpha , lw    , ksa   ,&
      &ks    , rloc  , wloc  , theta , cc    ,&
      &beta  , bot   , qin   , teken , lbvin ,&
      &lbvout)
      qdhd = (-qdhd - qa) / dh
   endif
!
   asde = qdhu
   csde = qdhd
!
   strhis(4,istru) = qa
   if (teken .gt. 0) then

      esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde

   else

      esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde

   endif

!

end

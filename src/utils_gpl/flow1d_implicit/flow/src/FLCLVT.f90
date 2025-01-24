subroutine FLCLVT (g     , il    , ir    , ngrid , istru ,&
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
! Module:             FLCLVT (FLow structure CuLVerT)
!
! Module description: In subroutine FLCLVT the ABCDE coefficients are
!                     computed for a culvert
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga04  unpacks the array strpar for a culvert
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flclvt.pf,v $
! Revision 1.2  1998/06/08  12:35:12  kuipe_j
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
   integer teken , iup   , idn
   real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,&
   &ud    , qa    , qdhu  , qdhd  , dh
   real         width , heidia, cullen, le    , z     ,&
   &alpha , ks    , ksa   , rloc  , theta ,&
   &beta  , bot   , wloc  ,&
   &qin   , applen, rle
   real         cc(6)
   logical strsta
   real    rteken
!
! declare functions
   real FLQH04


   crest = MAX(strpar(4,istru),strpar(4,istru)+strpar(5,istru))
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
   teken = INT(rteken)
!
   call FLGA04 (strpar, istru , nstru , af    , wf    ,&
   &h     , ngrid , teken , width , heidia,&
   &cullen, le    , z     , alpha , ks    ,&
   &ksa   , rloc  , theta , beta  , bot   ,&
   &qin   , wloc  , cc    , q     , iup   ,&
   &idn   , applen, rle   )
!
   dh = 0.001
!
   strsta = .true.
   qa = teken * FLQH04 (teken , width , le    , z     , cullen,&
   &heidia, applen, ksa   , ks    , rloc  ,&
   &wloc  , theta , alpha , beta  , hunp1 ,&
   &hdnp1 , uu    , qin   , g     , cc    ,&
   &bot   , strsta, strclo, istru , nstru ,&
   &rle   )
!
   strhis(4,istru) = qa
   strsta = .false.
!
   if (teken .gt. 0) then
      qdhu = FLQH04 (teken , width , le    , z     , cullen,&
      &heidia, applen, ksa   , ks    , rloc  ,&
      &wloc  , theta , alpha , beta  , hunp1+dh,&
      &hdnp1 , uu    , qin   , g     , cc    ,&
      &bot   , strsta, strclo, istru , nstru ,&
      &rle   )
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH04 (teken , width , le    , z     , cullen,&
      &heidia, applen, ksa   , ks    , rloc  ,&
      &wloc  , theta , alpha , beta  , hunp1 ,&
      &hdnp1-dh,&
      &uu    , qin   , g     , cc    ,&
      &bot   , strsta, strclo, istru , nstru ,&
      &rle   )
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH04 (teken , width , le    , z     , cullen,&
      &heidia, applen, ksa   , ks    , rloc  ,&
      &wloc  , theta , alpha , beta  , hunp1 ,&
      &hdnp1-dh,&
      &uu    , qin   , g     , cc    ,&
      &bot   , strsta, strclo, istru , nstru ,&
      &rle   )
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH04 (teken , width , le    , z     , cullen,&
      &heidia, applen, ksa   , ks    , rloc  ,&
      &wloc  , theta , alpha , beta  , hunp1+dh,&
      &hdnp1 , uu    , qin   , g     , cc    ,&
      &bot   , strsta, strclo, istru , nstru ,&
      &rle   )
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

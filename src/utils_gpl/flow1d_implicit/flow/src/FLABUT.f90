subroutine FLABUT (g     , il    , ir    , ngrid , istru ,&
&nstru , strpar, h     , h1    ,&
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
! Module:             FLABUT (FLow structure ABUTments)
!
! Module description: In subroutine FLABUT the ABCDE coefficients are
!                     computed for abutments
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  flow conditions near structure
! flga10  unpacks the array strpar for abutments
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flabut.pf,v $
! Revision 1.2  1998/06/08  12:35:01  kuipe_j
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
   double precision  h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
   real    af(ngrid), rho(ngrid)

!
! declare local variables
   integer teken , iup   , idn
   real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,&
   &ud    , dh    , qa    , qdhu  , qdhd
   integer itype
   real    le    , appwid, gapwid, gaplen, ks    , c1    ,&
   &c3    , ccr   , applen, qin   , rteken
!
! declare functions
   real FLQH10


   crest = strpar(1,istru)
!
   call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,&
   &q     , q2    , af    , rho   , crest , hunp1  ,&
   &hdnp1 , hun   , hdn   , uu    , ud    , iup    ,&
   &idn   , rteken)
   teken = INT(rteken)
!
   call FLGA10 (strpar, le    , appwid, gapwid, gaplen,&
   &ks    , c1    , c3    , ccr   , applen,&
   &itype , q     , ngrid , qin   , iup   ,&
   &idn   , teken , istru , nstru )
!
   dh = 0.001
!
   qa = teken * FLQH10 (g     , hunp1 , hdnp1 , uu    , le    ,&
   &appwid, gapwid, gaplen, ks    , c1    ,&
   &c3    , applen, itype , qin   )
!
!      strhis(4,istru) = qa
!
   if (teken .gt. 0) then
      qdhu = FLQH10 (g     , hunp1+dh,&
      &hdnp1 , uu    , le    ,&
      &appwid, gapwid, gaplen, ks    , c1    ,&
      &c3    , applen, itype , qin   )
      qdhu = (qdhu - qa) / dh
   else
      qdhu = FLQH10 (g     , hunp1 , hdnp1-dh,&
      &uu    , le    ,&
      &appwid, gapwid, gaplen, ks    , c1    ,&
      &c3    , applen, itype , qin   )
      qdhu = (qdhu + qa) / dh
   endif
!
   if (teken .gt. 0) then
      qdhd = FLQH10 (g     , hunp1 , hdnp1-dh,&
      &uu    , le    ,&
      &appwid, gapwid, gaplen, ks    , c1    ,&
      &c3    , applen, itype , qin   )
      qdhd = (qdhd - qa) / dh
   else
      qdhd = FLQH10 (g     , hunp1+dh,&
      &hdnp1 , uu    , le    ,&
      &appwid, gapwid, gaplen, ks    , c1    ,&
      &c3    , applen, itype , qin   )
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

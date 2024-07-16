      subroutine FLBRDG (g     , il    , ir    , ngrid , istru ,
     &                   nstru , strpar, strclo, h     , h1    ,
     &                   q     , q2    , af    , rho   ,
     &                   strhis, asde  , csde  , esde  ) 
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLBRDG (FLow structure BRiDGe piers)
c
c Module description: In subroutine FLBRDG the ABCDE coefficients are
c                     computed for bridge piers.
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga09  unpacks the array strpar for bridge piers 
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flbrdg.pf,v $
c Revision 1.2  1998/06/08  12:35:10  kuipe_j
c log added
c
c
c
c***********************************************************************

c
c declare include files
      include '..\include\sobdim.i'
c
c declare arguments
      integer il   , ir    , ngrid , istru , nstru
      real    g    , asde  , csde  , esde
      real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real af(ngrid), rho(ngrid)
      logical strclo(nstru)

c
c declare local variables
      integer teken , iup   , idn
      real    crest , hunp1 , hun   , hdn   , uu    ,
     &        hdnp1, ud    , dh    , qa    , qdhu  , qdhd 
      real    wn    , wp    , le    , shapco, qin
      real    rteken
      logical strsta
c
c declare functions
      real FLQH09

      crest = strpar(3,istru) 
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
c
      teken = INT(rteken)
      call FLGA09 (strpar, istru , nstru , q     , ngrid ,
     &             qin   , iup   , idn   , teken , wn    ,
     &             wp    , le    , shapco)
c
      dh = 0.001
c
      strsta = .true.
      qa = rteken * FLQH09 (g, hunp1 , sngl(hdnp1) , qin   , wn    , 
     &                      wp    , le    , shapco, teken , strsta, 
     &                      strclo, istru , nstru )
c
c      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH09 (g     , hunp1+dh,
     &                 sngl(hdnp1) , qin   , wn    , 
     &                 wp    , le    , shapco, teken , strsta, 
     &                 strclo, istru , nstru )
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH09 (g     , hunp1 , sngl(hdnp1)-dh,
     &                                         qin   , wn    , 
     &                 wp    , le    , shapco, teken , strsta, 
     &                 strclo, istru , nstru )
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH09 (g     , hunp1 , sngl(hdnp1-dh),
     &                                         qin   , wn    , 
     &                 wp    , le    , shapco, teken , strsta, 
     &                 strclo, istru , nstru )
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH09 (g     , hunp1+dh,
     &                 sngl(hdnp1) , qin   , wn    , 
     &                 wp    , le    , shapco, teken , strsta, 
     &                 strclo, istru , nstru )
        qdhd = (-qdhd - qa) / dh
      endif
c
      asde = qdhu
      csde = qdhd
c
c      if (iter.ne.1) then
c         relst1 = 1.0 - relstr
c         qa   = relstr*qa + relst1*strhis(4,istru)
c      endif
      strhis(4,istru) = qa
c
      if (teken .gt. 0) then
         esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
      else
         esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
      endif

c      if (teken .gt. 0) then
c        esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde
c      else
c        esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde
c      endif

c
c       if (qa .le. 0.) then
c         asde = 0.
c         csde = 0.
c         esde = 0.
c       endif

      end

      subroutine FLFLUM (g     , il    , ir    , ngrid , istru ,
     &                   nstru , strpar, strclo, h     , h1    ,
     &                   q     , q2    , af    , rho   ,
     &                   strhis, asde  , csde  , esde  , hlev  ,
     &                   maxlev)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLFLUM (FLow structure open FLUMe)
c
c Module description: In subroutine FLFLUM the ABCDE coefficients are
c                     computed for an open flume.
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga02  unpacks the array strpar for an open flume
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flflum.pf,v $
c Revision 1.2  1998/06/08  12:35:16  kuipe_j
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
      integer il   , ir    , ngrid , istru , nstru, maxlev
      real    g    , asde  , csde  , esde, hdnp1
      real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real    af(ngrid), rho(ngrid)
      double precision hlev(ngrid,maxlev)
      logical strclo(nstru)

c
c declare local variables
      integer teken , iup   , idn
      real    crest , hunp1 , hun   , hdn   , uu    ,
     &        ud    , dh    , qa    , qdhu  , qdhd 
      real    le    , z     , length, alpha , lw    , ksa   ,
     &        ks    , beta  , bot   , qin   , rloc  , wloc  ,
     &        theta , cc(3) , width , rteken
      logical strsta
      logical lbvin , lbvout
c
c declare functions
      real FLQH02


      crest = strpar(2,istru)
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken )
      teken = INT(rteken)
c
      call FLGA02 (strpar, istru , nstru , hlev  , maxlev,
     &             ngrid , teken , width , le    , z     ,
     &             length, alpha , lw    , ksa   , ks    ,
     &             beta  , lbvin , lbvout, bot   , qin   ,
     &             rloc  , wloc  , theta , cc    , q     ,
     &             iup   , idn   )

c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQH02 (g     , istru , nstru , strsta, strclo,
     &                     hunp1 , hdnp1 , uu    , width , le    ,
     &                     z     , length, alpha , lw    , ksa   ,
     &                     ks    , rloc  , wloc  , theta , cc    ,
     &                     beta  , bot   , qin   , teken , lbvin ,
     &                     lbvout)
c
c      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH02 (g     , istru , nstru , strsta, strclo,
     &                 hunp1+dh, 
     &                         hdnp1 , uu    , width , le    ,
     &                 z     , length, alpha , lw    , ksa   ,
     &                 ks    , rloc  , wloc  , theta , cc    ,
     &                 beta  , bot   , qin   , teken , lbvin ,
     &                 lbvout)
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH02 (g     , istru , nstru , strsta, strclo,
     &                 hunp1 , hdnp1-dh,
     &                                 uu    , width , le    ,
     &                 z     , length, alpha , lw    , ksa   ,
     &                 ks    , rloc  , wloc  , theta , cc    ,
     &                 beta  , bot   , qin   , teken , lbvin ,
     &                 lbvout)
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH02 (g     , istru , nstru , strsta, strclo,
     &                 hunp1 , hdnp1-dh,
     &                                 uu    , width , le    ,
     &                 z     , length, alpha , lw    , ksa   ,
     &                 ks    , rloc  , wloc  , theta , cc    ,
     &                 beta  , bot   , qin   , teken , lbvin ,
     &                 lbvout)
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH02 (g     , istru , nstru , strsta, strclo,
     &                 hunp1+dh,
     &                         hdnp1 , uu    , width , le    ,
     &                 z     , length, alpha , lw    , ksa   ,
     &                 ks    , rloc  , wloc  , theta , cc    ,
     &                 beta  , bot   , qin   , teken , lbvin ,
     &                 lbvout)
        qdhd = (-qdhd - qa) / dh
      endif
c
      asde = qdhu
      csde = qdhd
c
      strhis(4,istru) = qa
      if (teken .gt. 0) then

        esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde

      else

        esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde

      endif

c

      end

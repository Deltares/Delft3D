      subroutine FLSLUN (g     , il    , ir    , ngrid , istru ,
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
c Module:             FLSLUN (FLow structure SLuice with UNderflow gate)
c
c Module description: In subroutine FLSLUN the ABCDE coefficients are
c                     computed for a sluice with underflow gate
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga07  unpacks the array strpar for a sluice with underflow gate
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flslun.pf,v $
c Revision 1.2  1998/06/08  12:35:50  kuipe_j
c log added
c
c
c
c***********************************************************************

c
c declare include files
      include '../include/sobdim.i'
c
c declare arguments
      integer il   , ir    , ngrid , istru , nstru , maxlev 
      real    g    , asde  , csde  , esde
      real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
      double precision  h(ngrid), h1(ngrid),
     +                  q(ngrid), q2(ngrid)
      real af(ngrid), rho(ngrid)
      logical strclo(nstru)
      double precision hlev(ngrid,maxlev)

c
c declare local variables
      integer teken , iup   , idn
      real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,
     &        ud    , qa    , qdhu  , qdhd  , dh   
      integer npier
      real         wn    , kp    , ka    , level , hgate ,
     &             lw    , li    , lo    , c2    , c3    ,
     &             ksa   , ks    , ksii  , wp    , alpha ,
     &             qin   , bot   , beta  
      logical strsta
      real    rteken
c
c declare functions
      real FLQH07


      crest = strpar(2,istru)
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
      teken = INT(rteken)
c
      call FLGA07 (strpar, istru , nstru , ngrid , npier ,
     &             q     , wn    , kp    , ka    , level ,
     &             hgate , lw    , li    , lo    , c2    ,
     &             c3    , ksa   , ks    , ksii  , wp    ,
     &             alpha , qin   , bot   , hlev  , maxlev,
     &             beta  , iup   , idn   , teken )

c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQH07 (g     , istru , nstru , strsta, strclo,
     &                     qin   , hunp1 , hdnp1 , alpha , beta  ,
     &                     uu    , teken , bot   , wn    , npier ,
     &                     kp    , ka    , level , hgate , lw    ,
     &                     li    , lo    , c2    , c3    , ksa   ,
     &                     ks    , ksii  , wp    )

c
      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH07 (g     , istru , nstru , strsta, strclo,
     &                 qin   , hunp1+dh,
     &                                 hdnp1 , alpha , beta  ,
     &                 uu    , teken , bot   , wn    , npier ,
     &                 kp    , ka    , level , hgate , lw    ,
     &                 li    , lo    , c2    , c3    , ksa   ,
     &                 ks    , ksii  , wp    )
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH07 (g     , istru , nstru , strsta, strclo,
     &                 qin   , hunp1 , hdnp1-dh,
     &                                         alpha , beta  ,
     &                 uu    , teken , bot   , wn    , npier ,
     &                 kp    , ka    , level , hgate , lw    ,
     &                 li    , lo    , c2    , c3    , ksa   ,
     &                 ks    , ksii  , wp    )
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH07 (g     , istru , nstru , strsta, strclo,
     &                 qin   , hunp1 , hdnp1-dh,
     &                                         alpha , beta  ,
     &                 uu    , teken , bot   , wn    , npier ,
     &                 kp    , ka    , level , hgate , lw    ,
     &                 li    , lo    , c2    , c3    , ksa   ,
     &                 ks    , ksii  , wp    )
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH07 (g     , istru , nstru , strsta, strclo,
     &                 qin   , hunp1+dh,
     &                                 hdnp1 , alpha , beta  ,
     &                 uu    , teken , bot   , wn    , npier ,
     &                 kp    , ka    , level , hgate , lw    ,
     &                 li    , lo    , c2    , c3    , ksa   ,
     &                 ks    , ksii  , wp    )
        qdhd = (-qdhd - qa) / dh
      endif
c
      asde = qdhu
      csde = qdhd
c
      if (teken .gt. 0) then

        esde = -qa + (hunp1-hun) * asde + (hdnp1-hdn) * csde

      else

        esde = -qa + (hdnp1-hdn) * asde + (hunp1-hun) * csde

      endif

c

      end

      subroutine FLSLOU (g     , il    , ir    , ngrid , istru ,
     &                   nstru , strpar, strclo, h     , h1    ,
     &                   q     , q2    , af    , wf    , rho   ,
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
c Module:             FLSLOU (FLow structure SLuice with Overflow/
c                             Underflow date.
c
c Module description: In subroutine FLSLOU the ABCDE coefficients are
c                     computed for a sluice with overflow/ underflow
c                     gate
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga08  unpacks the array strpar for an open flume
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flslou.pf,v $
c Revision 1.2  1998/06/08  12:35:48  kuipe_j
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
      real    af(ngrid), wf(ngrid), rho(ngrid)
      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      logical strclo(nstru)

c
c declare local variables
      integer teken , iup   , idn   , npier
      real    crest , hunp1 , hun   , hdn   , uu    ,
     &        ud    , dh    , qa    , qdhu  , qdhd 
      real    wn    , kp    , ka    , le    , go    , gu    ,
     &        lw    , li    , lo    , c2    , c3    , wp    ,
     &        ksa   , ks    , alpha , ksii  , cga   , beta  ,
     &        bot   , hdnp1
      double precision qin 
      logical strsta
      real    rteken
c
c declare functions
      real FLQH08


      crest = strpar(5,istru)
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
      teken = INT(rteken)
c
      call FLGA08 (strpar, istru , nstru , af    , wf    ,
     &             h     , ngrid , q     , iup   , idn   ,
     &             teken , npier , wn    , kp    , ka    ,
     &             le    , go    , gu    , lw    ,
     &             li    , lo    , c2    , c3    , wp    ,
     &             ksa   , ks    , alpha , ksii  , cga   ,
     &             beta  , bot   , qin   )
c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQH08 (teken , npier , istru , nstru , g     ,
     &                     qin   , hunp1 , hdnp1 , uu    ,
     &                     wn    , kp    , ka    , le    , go    ,
     &                     gu    , lw    , li    , lo    , c2    ,
     &                     c3    , wp    , ksa   , ks    , alpha ,
     &                     ksii  , cga   , beta  , strsta, strclo,
     &                     bot   )
c
      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH08 (teken , npier , istru , nstru , g     ,
     &                 qin   , hunp1+dh,
     &                                 hdnp1 , uu    ,
     &                 wn    , kp    , ka    , le    , go    ,
     &                 gu    , lw    , li    , lo    , c2    ,
     &                 c3    , wp    , ksa   , ks    , alpha ,
     &                 ksii  , cga   , beta  , strsta, strclo,
     &                 bot   )
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH08 (teken , npier , istru , nstru , g     ,
     &                 qin   , hunp1 , hdnp1-dh,
     &                                         uu    , 
     &                 wn    , kp    , ka    , le    , go    ,
     &                 gu    , lw    , li    , lo    , c2    ,
     &                 c3    , wp    , ksa   , ks    , alpha ,
     &                 ksii  , cga   , beta  , strsta, strclo,
     &                 bot   )
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH08 (teken , npier , istru , nstru , g     ,
     &                 qin   , hunp1 , hdnp1-dh,
     &                                         uu    ,
     &                 wn    , kp    , ka    , le    , go    ,
     &                 gu    , lw    , li    , lo    , c2    ,
     &                 c3    , wp    , ksa   , ks    , alpha ,
     &                 ksii  , cga   , beta  , strsta, strclo,
     &                 bot   )
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH08 (teken , npier , istru , nstru , g     ,
     &                 qin   , hunp1+dh,
     &                                 hdnp1 , uu    ,
     &                 wn    , kp    , ka    , le    , go    ,
     &                 gu    , lw    , li    , lo    , c2    ,
     &                 c3    , wp    , ksa   , ks    , alpha ,
     &                 ksii  , cga   , beta  , strsta, strclo,
     &                 bot   )
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

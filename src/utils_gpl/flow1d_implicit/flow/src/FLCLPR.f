      subroutine FLCLPR (g     , il    , ir    , ngrid , istru ,
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
c Module:             FLCLPR (FLow structure CuLvert with PRessure flow)
c
c Module description: In subroutine FLCLPR the ABCDE coefficients are
c                     computed for a culvert with pressure flow
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga06  unpacks the array strpar for a culvert with pressure flow
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flclpr.pf,v $
c Revision 1.2  1998/06/08  12:35:11  kuipe_j
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
      real    af(ngrid), wf(ngrid), rho(ngrid)
      logical strclo(nstru)

c
c declare local variables
      integer teken , iup   , idn   , nculv
      real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,
     &        ud    , dh    , qa    , qdhu  , qdhd 
      real    wi    , di    , wo    , 
     &        do    , lg    , alpha , go    , gm    ,
     &        wg    , li    , lo    , ksa   ,
     &        ksi   , kso   , ksipos, lw    , beta  ,
     &        bot   , qin 
      logical strsta
      logical lpress
      real    rteken
c
c declare functions
      real FLQH06


      crest = strpar(5,istru)
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
      teken = INT(rteken)
c
      call FLGA06 (strpar, istru , nstru , af    , wf    ,
     &             h     , ngrid , q     , iup   , idn   ,
     &             teken , nculv , wi    , di    , wo    ,
     &             do    , lg    , alpha , go    , gm    ,
     &             wg    , li    , lo    , ksa   ,
     &             ksi   , kso   , ksipos, lw    , beta  ,
     &             bot   , qin   , lpress)
c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQH06 (istru , nstru , strsta, strclo, g     ,
     &                     hunp1 , hdnp1 , uu    , qin   , nculv ,
     &                     wi    , di    , wo    , do    , lg    ,
     &                     alpha , go    , gm    , wg    , 
     &                     li    , lo    , ksa   , ksi   , kso   ,
     &                     ksipos, lw    , beta  , bot   , lpress)
c
      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH06 (istru , nstru , strsta, strclo, g     ,
     &                 hunp1+dh,
     &                         hdnp1 , uu    , qin   , nculv ,
     &                 wi    , di    , wo    , do    , lg    ,
     &                 alpha , go    , gm    , wg    , 
     &                 li    , lo    , ksa   , ksi   , kso   ,
     &                 ksipos, lw    , beta  , bot   , lpress)
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH06 (istru , nstru , strsta, strclo, g     ,
     &                 hunp1 , hdnp1-dh,
     &                                 uu    , qin   , nculv ,
     &                 wi    , di    , wo    , do    , lg    ,
     &                 alpha , go    , gm    , wg    , 
     &                 li    , lo    , ksa   , ksi   , kso   ,
     &                 ksipos, lw    , beta  , bot   , lpress)
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH06 (istru , nstru , strsta, strclo, g     ,
     &                 hunp1 , hdnp1-dh,
     &                                 uu    , qin   , nculv ,
     &                 wi    , di    , wo    , do    , lg    ,
     &                 alpha , go    , gm    , wg    , 
     &                 li    , lo    , ksa   , ksi   , kso   ,
     &                 ksipos, lw    , beta  , bot   , lpress)
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH06 (istru , nstru , strsta, strclo, g     ,
     &                 hunp1+dh,
     &                         hdnp1 , uu    , qin   , nculv ,
     &                 wi    , di    , wo    , do    , lg    ,
     &                 alpha , go    , gm    , wg    , 
     &                 li    , lo    , ksa   , ksi   , kso   ,
     &                 ksipos, lw    , beta  , bot   , lpress)
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

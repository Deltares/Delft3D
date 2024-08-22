      subroutine FLCLVT (g     , il    , ir    , ngrid , istru ,
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
c Module:             FLCLVT (FLow structure CuLVerT) 
c
c Module description: In subroutine FLCLVT the ABCDE coefficients are
c                     computed for a culvert 
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga04  unpacks the array strpar for a culvert 
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flclvt.pf,v $
c Revision 1.2  1998/06/08  12:35:12  kuipe_j
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
      integer il   , ir    , ngrid , istru , nstru 
      real    g    , asde  , csde  , esde
      real    strpar(dmstrpar,nstru), strhis(dmstrh,nstru)
      double precision h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real    af(ngrid), wf(ngrid), rho(ngrid)
      logical strclo(nstru)

c
c declare local variables
      integer teken , iup   , idn
      real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,
     &        ud    , qa    , qdhu  , qdhd  , dh   
      real         width , heidia, cullen, le    , z     ,
     &             alpha , ks    , ksa   , rloc  , theta ,
     &             beta  , bot   , wloc  , 
     &             qin   , applen, rle
      real         cc(6)
      logical strsta
      real    rteken
c
c declare functions
      real FLQH04


      crest = MAX(strpar(4,istru),strpar(4,istru)+strpar(5,istru))
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
      teken = INT(rteken)
c
      call FLGA04 (strpar, istru , nstru , af    , wf    ,
     &             h     , ngrid , teken , width , heidia,
     &             cullen, le    , z     , alpha , ks    ,
     &             ksa   , rloc  , theta , beta  , bot   ,
     &             qin   , wloc  , cc    , q     , iup   ,
     &             idn   , applen, rle   )
c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQH04 (teken , width , le    , z     , cullen,
     &                     heidia, applen, ksa   , ks    , rloc  ,
     &                     wloc  , theta , alpha , beta  , hunp1 ,
     &                     hdnp1 , uu    , qin   , g     , cc    ,
     &                     bot   , strsta, strclo, istru , nstru ,
     &                     rle   )
c
      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
        qdhu = FLQH04 (teken , width , le    , z     , cullen,
     &                 heidia, applen, ksa   , ks    , rloc  ,
     &                 wloc  , theta , alpha , beta  , hunp1+dh,
     &                 hdnp1 , uu    , qin   , g     , cc    ,
     &                 bot   , strsta, strclo, istru , nstru ,
     &                 rle   )
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH04 (teken , width , le    , z     , cullen,
     &                 heidia, applen, ksa   , ks    , rloc  ,
     &                 wloc  , theta , alpha , beta  , hunp1 ,
     &                 hdnp1-dh,
     &                         uu    , qin   , g     , cc    ,
     &                 bot   , strsta, strclo, istru , nstru ,
     &                 rle   )
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH04 (teken , width , le    , z     , cullen,
     &                 heidia, applen, ksa   , ks    , rloc  ,
     &                 wloc  , theta , alpha , beta  , hunp1 ,
     &                 hdnp1-dh,
     &                         uu    , qin   , g     , cc    ,
     &                 bot   , strsta, strclo, istru , nstru ,
     &                 rle   )
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH04 (teken , width , le    , z     , cullen,
     &                 heidia, applen, ksa   , ks    , rloc  ,
     &                 wloc  , theta , alpha , beta  , hunp1+dh,
     &                 hdnp1 , uu    , qin   , g     , cc    ,
     &                 bot   , strsta, strclo, istru , nstru ,
     &                 rle   )
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

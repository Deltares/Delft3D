      subroutine FLABUT (g     , il    , ir    , ngrid , istru ,
     &                   nstru , strpar, h     , h1    ,
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
c Module:             FLABUT (FLow structure ABUTments)
c
c Module description: In subroutine FLABUT the ABCDE coefficients are
c                     computed for abutments     
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  flow conditions near structure
c flga10  unpacks the array strpar for abutments 
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flabut.pf,v $
c Revision 1.2  1998/06/08  12:35:01  kuipe_j
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
      double precision  h(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real    af(ngrid), rho(ngrid)

c
c declare local variables
      integer teken , iup   , idn
      real    crest , hunp1 , hdnp1 , hun   , hdn   , uu    ,
     &        ud    , dh    , qa    , qdhu  , qdhd 
      integer itype
      real    le    , appwid, gapwid, gaplen, ks    , c1    ,
     &        c3    , ccr   , applen, qin   , rteken 
c
c declare functions
      real FLQH10


      crest = strpar(1,istru) 
c
      call FLUPDO(g     , il    , ir    , ngrid , h     , h1     ,
     &            q     , q2    , af    , rho   , crest , hunp1  ,
     &            hdnp1 , hun   , hdn   , uu    , ud    , iup    ,
     &            idn   , rteken)
      teken = INT(rteken)
c
      call FLGA10 (strpar, le    , appwid, gapwid, gaplen,
     &             ks    , c1    , c3    , ccr   , applen,
     &             itype , q     , ngrid , qin   , iup   ,
     &             idn   , teken , istru , nstru )
c
      dh = 0.001
c
      qa = teken * FLQH10 (g     , hunp1 , hdnp1 , uu    , le    , 
     &                     appwid, gapwid, gaplen, ks    , c1    , 
     &                     c3    , applen, itype , qin   )
c
c      strhis(4,istru) = qa
c
      if (teken .gt. 0) then
        qdhu = FLQH10 (g     , hunp1+dh,
     &                                 hdnp1 , uu    , le    , 
     &                 appwid, gapwid, gaplen, ks    , c1    , 
     &                 c3    , applen, itype , qin   )
        qdhu = (qdhu - qa) / dh
      else
        qdhu = FLQH10 (g     , hunp1 , hdnp1-dh,
     &                                         uu    , le    , 
     &                 appwid, gapwid, gaplen, ks    , c1    , 
     &                 c3    , applen, itype , qin   )
        qdhu = (qdhu + qa) / dh
      endif
c
      if (teken .gt. 0) then
        qdhd = FLQH10 (g     , hunp1 , hdnp1-dh,
     &                                         uu    , le    , 
     &                 appwid, gapwid, gaplen, ks    , c1    , 
     &                 c3    , applen, itype , qin   )
        qdhd = (qdhd - qa) / dh
      else
        qdhd = FLQH10 (g     , hunp1+dh,
     &                                 hdnp1 , uu    , le    , 
     &                 appwid, gapwid, gaplen, ks    , c1    , 
     &                 c3    , applen, itype , qin   )
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

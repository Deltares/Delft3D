
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          SOBEK
c
c Programmer:         J.Kuipers
c
c Module:             error message administration
c
c-----------------------------------------------------------------------
c      
      integer    nmes  ,nmesmx, jumes, nmestm
      integer    alerperr, nrerperr, neragenstr
      parameter (alerperr=10)
      parameter (neragenstr=1)
      common    /mescom/nmes ,nmesmx, jumes, nmestm,nrerperr(alerperr)
      save      /mescom/


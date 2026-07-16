
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          SOBEK
!
! Programmer:         J.Kuipers
!
! Module:             error message administration
!
!-----------------------------------------------------------------------
!
integer    nmes  ,nmesmx, jumes, nmestm
integer    alerperr, nrerperr, neragenstr
parameter (alerperr=10)
parameter (neragenstr=1)
common    /mescom/nmes ,nmesmx, jumes, nmestm,nrerperr(alerperr)
save      /mescom/


module m_initsedtra

private 

public initsedtra

contains

subroutine initsedtra(sedtra, sedpar, trapar, morpar, morlyr, rhow, ag, vicmol, nmlb, nmub, nmmax, lsed, lsedtot)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: - Initialize the arrays of sedtra_type data structure.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: sedtra_type, sedpar_type, trapar_type, morpar_type
    use bedcomposition_module, only: getfrac, bedcomp_data
    !
    implicit none
    !
    ! Function/routine arguments
    !
    type (sedtra_type)                                       :: sedtra
    type (sedpar_type)                              , target :: sedpar
    type (trapar_type)                 , intent(in)          :: trapar
    type (morpar_type)                 , intent(in) , target :: morpar
    type (bedcomp_data)                , intent(in)          :: morlyr
    real(fp)                           , intent(in)          :: rhow
    real(fp)                           , intent(in)          :: ag
    real(fp)                           , intent(in)          :: vicmol
    integer                            , intent(in)          :: nmlb
    integer                            , intent(in)          :: nmub
    integer                            , intent(in)          :: nmmax
    integer                            , intent(in)          :: lsed
    integer                            , intent(in)          :: lsedtot
    !
    ! Local variables
    !
    logical                                , pointer :: anymud
    !
    integer              , dimension(:)    , pointer :: iform
    integer                                , pointer :: ihidexp
    integer              , dimension(:)    , pointer :: nseddia
    integer                                , pointer :: nxx
    integer              , dimension(:)    , pointer :: sedtyp
    !
    real(fp)                               , pointer :: asklhe
    real(fp)                               , pointer :: factcr
    real(fp)             , dimension(:)    , pointer :: dg
    real(fp)             , dimension(:)    , pointer :: dgsd
    real(fp)             , dimension(:)    , pointer :: dm
    real(fp)             , dimension(:)    , pointer :: dstar
    real(fp)             , dimension(:,:)  , pointer :: dxx
    real(fp)             , dimension(:,:)  , pointer :: frac
    real(fp)             , dimension(:,:)  , pointer :: hidexp
    real(fp)             , dimension(:)    , pointer :: logsedsig
    real(fp)             , dimension(:,:,:), pointer :: logseddia
    real(fp)             , dimension(:)    , pointer :: mudcnt
    real(fp)             , dimension(:)    , pointer :: mudfrac
    real(fp)             , dimension(:)    , pointer :: sandfrac
    real(fp)                               , pointer :: mwwjhe
    real(fp)             , dimension(:)    , pointer :: rhosol
    real(fp)             , dimension(:)    , pointer :: sedd50
    real(fp)             , dimension(:)    , pointer :: sedd50fld
    real(fp)             , dimension(:)    , pointer :: taucr
    real(fp)             , dimension(:)    , pointer :: tetacr
    real(fp)             , dimension(:)    , pointer :: xx
    !
    integer                                          :: ll
    real(fp)                                         :: drho
!
!! executable statements -------------------------------------------------------
!
    iform     => trapar%iform
    ihidexp   => morpar%ihidexp
    nseddia   => sedpar%nseddia
    nxx       => morpar%nxx
    !
    asklhe    => morpar%asklhe
    frac      => sedtra%frac
    mudfrac   => sedtra%mudfrac
    sandfrac  => sedtra%sandfrac
    anymud    => sedpar%anymud
    logsedsig => sedpar%logsedsig
    logseddia => sedpar%logseddia
    mudcnt    => sedpar%mudcnt
    mwwjhe    => morpar%mwwjhe
    rhosol    => sedpar%rhosol
    sedtyp    => sedpar%sedtyp
    sedd50    => sedpar%sedd50
    sedd50fld => sedpar%sedd50fld
    xx        => morpar%xx
    factcr    => morpar%factcr
    !
    dg        => sedtra%dg
    dgsd      => sedtra%dgsd
    dm        => sedtra%dm
    dstar     => sedpar%dstar
    dxx       => sedtra%dxx
    hidexp    => sedtra%hidexp
    taucr     => sedpar%taucr
    tetacr    => sedpar%tetacr
    !
    ! Calculation of dimensionless grain size and critical shear stress
    ! Only for uniform sedd50
    ! For space varying sedd50:
    ! - this is done every time step, for every nm in erosed
    ! - the do-loop below should not be executed since sedd50==-999
    !
    if (lsedtot/=1 .or. sedpar%flsdia==' ') then
       do ll = 1, lsedtot
          if (sedd50(ll) > 0.0_fp) then
              drho      = (sedpar%rhosol(ll)-rhow) / rhow
              dstar(ll) = sedd50(ll) * (drho*ag/vicmol**2)**0.3333_fp
              if (dstar(ll) < 1.0_fp) then
                 if (iform(ll) == -2) then
                    tetacr(ll) = 0.115_fp / (dstar(ll)**0.5_fp)
                 else
                    tetacr(ll) = 0.24_fp / dstar(ll)
                 endif
              elseif (dstar(ll) <= 4.0_fp) then
                 if (iform(ll) == -2) then
                    tetacr(ll) = 0.115_fp / (dstar(ll)**0.5_fp)
                 else
                    tetacr(ll) = 0.24_fp / dstar(ll)
                 endif
              elseif (dstar(ll)>4.0_fp .and. dstar(ll)<=10.0_fp) then
                 tetacr(ll) = 0.14_fp  / (dstar(ll)**0.64_fp)
              elseif (dstar(ll)>10.0_fp .and. dstar(ll)<=20.0_fp) then
                 tetacr(ll) = 0.04_fp  / (dstar(ll)**0.1_fp)
              elseif (dstar(ll)>20.0_fp .and. dstar(ll)<=150.0_fp) then
                 tetacr(ll) = 0.013_fp * (dstar(ll)**0.29_fp)
              else
                 tetacr(ll) = 0.055_fp
              endif
              taucr(ll) = factcr * (rhosol(ll)-rhow) * ag * sedd50(ll) * tetacr(ll)
           else
              dstar(ll)  = 0.0_fp
              tetacr(ll) = 0.0_fp
              taucr(ll)  = 0.0_fp
           endif
       enddo
    endif
    !
    ! Initialise fractions
    !
    call getfrac(morlyr   ,frac     ,anymud   ,mudcnt    , &
               & mudfrac  ,nmlb     ,nmub     )
    !
    ! Calculate arithmetic mean sediment diameter Dm
    ! Calculate geometric mean sediment diameter Dg
    ! Calculate percentiles Dxx
    !
    call compdiam(frac      ,sedd50    ,sedd50    ,sedtyp    ,lsedtot   , &
                & logsedsig ,nseddia   ,logseddia ,nmmax     ,nmlb      , &
                & nmub      ,xx        ,nxx       ,sedpar%max_mud_sedtyp, sedpar%min_dxx_sedtyp, &
                & sedd50fld ,dm        ,dg        ,dxx       ,dgsd      )
    !
    ! Determine hiding & exposure factors
    !
    if (lsedtot > 1) then
       call comphidexp(frac     ,dm        ,nmmax     ,lsedtot   , &
                     & sedd50   ,hidexp    ,ihidexp   ,asklhe    , &
                     & mwwjhe   ,nmlb      ,nmub      )
    else
       hidexp = 1.0
    endif
    !
    call compsandfrac(frac, sedd50, nmmax, lsedtot, sedtyp, &
                    & sedpar%max_mud_sedtyp, sandfrac, sedd50fld, &
                    & nmlb, nmub)  
end subroutine initsedtra

end module m_initsedtra
subroutine erosilt(thick    ,kmax      ,ws        ,lundia   , &
                 & thick0   ,thick1    ,fixfac    ,srcmax   , &
                 & frac     ,oldmudfrac,flmd2l    ,iform    , &
                 & npar     ,par       ,numintpar ,numrealpar, &
                 & numstrpar,dllfunc  ,dllhandle ,intpar    , &
                 & realpar  ,strpar   ,iflufflyr ,mflufftot , &
                 & fracf    ,maxslope ,wetslope  , & ! output:
                 & error    ,wstau     ,sinktot   ,sourse   , &
                 & sourf    )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!    Function: Computes sediment fluxes for cohesive sediment fractions
!              at the bed and the fluff layer. Internally implemented
!              are the Partheniades-Krone formulations, optional external
!              lib may be used.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module
    use morphology_data_module, only: RP_TAUB
    use message_module, only: write_error
    use iso_c_binding, only: c_char
    !
    implicit none
    !
    integer                             , intent(in)    :: iform
    integer                             , intent(in)    :: iflufflyr
    integer                             , intent(in)    :: numintpar
    integer                             , intent(in)    :: numrealpar
    integer                             , intent(in)    :: numstrpar
    integer                             , intent(in)    :: kmax
    integer                                             :: lundia     !> handle of diagnostics file
    integer                             , intent(in)    :: npar
    integer       , dimension(numintpar), intent(inout) :: intpar
    integer(pntrsize)                   , intent(in)    :: dllhandle
    !
    real(fp)                            , intent(in)    :: fixfac
    real(fp)                            , intent(in)    :: frac
    real(fp)                            , intent(in)    :: fracf
    real(fp)                            , intent(in)    :: maxslope
    real(fp)                            , intent(in)    :: mflufftot
    real(fp)     , dimension(npar)      , intent(inout) :: par
    real(fp)                            , intent(out)   :: sinktot
    real(fp)                            , intent(out)   :: sourf
    real(fp)                            , intent(out)   :: sourse
    real(fp)                            , intent(in)    :: srcmax
    real(fp)       , dimension(kmax)    , intent(in)    :: thick
    real(fp)                            , intent(in)    :: thick0
    real(fp)                            , intent(in)    :: thick1
    real(fp)                            , intent(in)    :: wetslope
    real(fp)       , dimension(0:kmax)  , intent(in)    :: ws
    real(fp)                            , intent(out)   :: wstau
    !
    real(hp)     , dimension(numrealpar), intent(inout) :: realpar
    !
    logical                             , intent(out)   :: error
    logical                             , intent(in)    :: flmd2l
    logical                             , intent(in)    :: oldmudfrac
    character(256), dimension(numstrpar), intent(inout) :: strpar
    character(256)                      , intent(in)    :: dllfunc
!
! Local variables
!
    real(fp) :: betaslope     !> coefficient in bed slope effect on critical shear stress for bed erosion (-)
    real(fp) :: sour          !> entrainment flux from bed (kg/m2/s)
    real(fp) :: sour_fluff    !> entrainment flux from fluff layer (kg/m2/s)
    real(fp) :: sink          !> dimensionless sedimentation factor (-)
    real(fp) :: taub          !> (wave enhanced) bed shear stress (N/m2)
    real(fp) :: taum          !> dimensionless excess bed shear stress for erosion (-)
    real(fp) :: entr          !> entrainment flux in case of two-layer fluid mud (kg/m2/s)
    real(fp) :: taucrmin      !> minimim critical shear stress for erosion during bed slope effect computation (N/m2)
    real(fp) :: tcrdep        !> critical shear stress for deposition (N/m2)
    real(fp) :: tcrero        !> critical shear stress for erosion from the bed (N/m2)
    real(fp) :: eropar        !> erosion rate parameter (kg/m2/s)
    real(fp) :: tcrflf        !> critical shear stress for erosion from the fluff layer (N/m2)
    real(fp) :: parfl0        !> zero-order erosion rate parameter for the fluff layer (m*s)
    real(fp) :: parfl1        !> first-order erosion rate parameter for the fluff layer (m*s/kg)
    real(fp) :: depeff        !> coefficient determining mud sedimentation (to fluff layer or bed) (-)
    real(fp) :: powern        !> exponent in the erosion rate formulation (-)
    !
    ! Interface to dll is in High precision!
    !
    real(hp)                    :: sink_dll       !> dimensionless deposition flux computed by user dll (-)
    real(hp)                    :: sour_dll       !> entrainment flux computed by user dll (kg/m2/s)
    integer(pntrsize)           :: ierror_ptr
    integer(pntrsize), external :: perf_function_erosilt
    character(1024)             :: errmsg         !> error message from this routine
    character(256)              :: message        !> error message from user dll
    character(kind=c_char)      :: message_c(257) !> C-version of "message", including C_NULL_CHAR
                                                  ! Calling perf_function_erosilt with "message" caused problems
                                                  ! Solved by using "message_c"
    integer                     :: i              !> loop index
!
!! executable statements ------------------
!
    error  = .false.
    !
    ! Calculate total (possibly wave enhanced) roughness
    !
    taub   = real(realpar(RP_TAUB), fp)
    !
    ! Bed transport following Partheniades and Krone
    ! but in case of fluid mud, source term is determined by
    ! fluid mud part (sourmu). Information is passed via entr()
    ! maximum erosion is sediment available at bed (ignores sediment
    ! settling during the current morphological timestep)
    ! In case of fluid mud the maximum erosion is determined in sourmu
    ! of the fluid mud module. So ignore this check when fluid mud.
    ! Also, taum is not required in the formulation since whether or not
    ! and how much entrainment occurs is entirely handled by the sourmu
    ! routine.
    !
    ! calculation both for mud and floc
    !
    if (flmd2l) then
       entr   = par(11)
       tcrdep = par(12)
       !
       ! maximum erosion is sediment available at bed
       ! (ignores sediment settling during the current morphological timestep)
       !
       sour = entr
       if (tcrdep > 0.0_fp) then
          sink = max(0.0_fp , 1.0_fp-taub/tcrdep)
       else
          sink = 0.0_fp
       endif
       sour_fluff = 0.0_fp
    else
       if (iform == -3) then
          eropar = par(11)
          tcrdep = par(12)
          tcrero = par(13)
          tcrflf = par(14)
          parfl0 = par(15)
          parfl1 = par(16)
          depeff = par(17)
          powern = par(18)
          !
          ! Default Partheniades-Krone formula
          !
          if (maxslope>wetslope) then
             !
             ! Maximum bed slope in surrounding velocity points exceeds WetSlope
             ! Decrease critical shear stress for erosion
             !
             taucrmin  = 0.1_fp
             betaslope = 2.0_fp
             tcrero = ((taucrmin - tcrero)/(betaslope*wetslope - wetslope))*taub + tcrero - ((taucrmin - tcrero)/(betaslope*wetslope - wetslope))*wetslope
             tcrero = max(tcrero, taucrmin)
          endif
          !
          taum = max(0.0_fp, taub/tcrero - 1.0_fp)
          sour = eropar * taum**powern
          !
          ! Erosion from fluff layer
          !
          if (iflufflyr>0) then
            taum       = max(0.0_fp, taub - tcrflf)
            sour_fluff = min(mflufftot*parfl1,parfl0)*taum
          else
            sour_fluff = 0.0_fp
          endif
          !
          if (comparereal(depeff,-1.0_fp)==0) then
             if (tcrdep > 0.0_fp) then
                sink = max(0.0_fp , 1.0_fp-taub/tcrdep)
             else
                sink = 0.0_fp
             endif
          else
             sink = max(0.0_fp,min(depeff,1.0_fp))
          endif
       elseif (iform == 21) then
          !
          ! Initialisation of output variables of user defined transport formulae
          !
          sink_dll    = 0.0_hp
          sour_dll    = 0.0_hp
          message     = ' '
          do i=1,256
             message_c(i) = message(i:i)
          enddo
          message_c(257) = C_NULL_CHAR
          !
          ! psem/vsem is used to be sure this works fine in DD calculations
          !
          call psemlun
          ierror_ptr = 0
          ierror_ptr = perf_function_erosilt(dllhandle       , dllfunc           , &
                                             intpar          , numintpar         , &
                                             realpar         , numrealpar        , &
                                             strpar          , numstrpar         , &
                                             sink_dll        , sour_dll          , &
                                             message_c)
          message = transfer(message_c(1:256), message)
          call vsemlun
          if (ierror_ptr /= 0) then
             errmsg = 'Cannot find function "'//trim(dllfunc)//'" in dynamic library.'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          if (message /= ' ') then
             errmsg = 'Message from user defined erosion/deposition formulae '//trim(dllfunc)//' :'
             call write_error(errmsg, unit=lundia)
             write (lundia,'(a,a  )') '          ', trim(message)
             write (lundia,'(a    )') ' '
             error = .true.
             return
          endif
          !
          ! Output parameters
          !
          sour       = real(sour_dll,fp)
          sink       = real(sink_dll,fp)
          sour_fluff = 0.0_fp
       else
          write (errmsg,'(a,i0,a)') 'Invalid transport formula ',iform,' for mud fraction.'
          call write_error(errmsg, unit=lundia)
       endif
       !
       if (oldmudfrac) then
          sour       = fixfac * sour
          sour_fluff = 0.0_fp
       else
          sour       = fixfac * frac  * sour
          sour_fluff =          fracf * sour_fluff
       endif
    endif
    !
    wstau         = ws(kmax) * sink ! used for flmd2l
    !
    sour    = min(sour, srcmax)
    !
    sourf   = sour_fluff / thick0
    sourse  = sour       / thick0
    sinktot = wstau      / thick1
end subroutine erosilt

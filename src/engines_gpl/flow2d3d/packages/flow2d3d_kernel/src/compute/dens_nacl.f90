subroutine dens_nacl(temp, salt, rhonacl, rhods, rhodt)
!!--copyright-------------------------------------------------------------------
! Copyright (c) 2006-2024, WL | Delft Hydraulics. All rights reserved.
!!--disclaimer------------------------------------------------------------------
! This code is part of the Delft3D software system. WL|Delft Hydraulics has
! developed c.q. manufactured this code to its best ability and according to the
! state of the art. Nevertheless, there is no express or implied warranty as to
! this software whether tangible or intangible. In particular, there is no
! express or implied warranty as to the fitness for a particular purpose of this
! software, whether tangible or intangible. The intellectual property rights
! related to this software code remain with WL|Delft Hydraulics at all times.
! For details on the licensing agreement, we refer to the Delft3D software
! license and any modifications to this license, if applicable. These documents
! are available upon request.
!!--version information---------------------------------------------------------
! $Author$
! $Date$
! $Revision$
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!
! Method used: Equation of state following Millero/Delft Hydraulics
!              Valid for NaCl solutions
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)     :: salt
    real(fp), intent(in)     :: temp
    real(fp), intent(out)    :: rhonacl
    real(fp), intent(out)    :: rhods
    real(fp), intent(out)    :: rhodt
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    rhonacl  = 999.904_fp                  + 4.8292E-2_fp * temp           - &
     &         7.2312E-3_fp * temp**2      + 2.9963E-5_fp * temp**3        + &
     &         7.6427E-1_fp * salt         - 3.1490E-3_fp * salt * temp    + &
     &         3.1273E-5_fp * salt * temp**2

    rhods    = 7.6427E-1_fp                - 3.1490E-3_fp * temp           + &
     &         2*3.1273E-5_fp * salt * temp

    rhodt    = 4.8292E-2_fp                - 2_fp*7.2312E-3_fp * temp      + &
     &         3_fp*2.9963E-5_fp * temp**2 - 3.1490E-3_fp * salt           + &
     &         2_fp*3.1273E-5_fp * salt * temp
end subroutine dens_nacl

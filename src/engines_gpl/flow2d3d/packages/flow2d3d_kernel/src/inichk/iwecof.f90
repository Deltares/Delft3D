subroutine iwecof(kmxt      ,gdp       )
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
!    Function: Sets parameters for subprogram TAYLOR.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp) , pointer :: ricrit
    real(fp) , pointer :: bvmin
    real(fp) , pointer :: accur
    real(fp) , pointer :: epsuko
    real(fp) , pointer :: argsnh
    real(fp) , pointer :: argexp
    real(fp) , pointer :: epsbv2
    real(fp) , pointer :: alfaz
    real(fp) , pointer :: xmu0
    real(fp) , pointer :: clu
    real(fp) , pointer :: clw
    real(fp) , pointer :: ckw
    real(fp) , pointer :: viscof
    real(fp) , pointer :: tol
    integer  , pointer :: nrange
    integer  , pointer :: ninc
!
! Global variables
!
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
!
!
! Local variables
!
    real(fp)                       :: xmax
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ricrit  => gdp%gdiwearr%ricrit
    bvmin   => gdp%gdiwearr%bvmin
    accur   => gdp%gdiwearr%accur
    epsuko  => gdp%gdiwearr%epsuko
    argsnh  => gdp%gdiwearr%argsnh
    argexp  => gdp%gdiwearr%argexp
    epsbv2  => gdp%gdiwearr%epsbv2
    alfaz   => gdp%gdiwearr%alfaz
    xmu0    => gdp%gdiwearr%xmu0
    clu     => gdp%gdiwearr%clu
    clw     => gdp%gdiwearr%clw
    ckw     => gdp%gdiwearr%ckw
    viscof  => gdp%gdiwearr%viscof
    tol     => gdp%gdiwearr%tol
    nrange  => gdp%gdiwearr%nrange
    ninc    => gdp%gdiwearr%ninc
    !
    xmax = 3.4E38
    accur = xmax/(kmxt**3)
    !
    argsnh = 5.
    argexp = 100.
    epsbv2 = 0.05
    epsuko = 1.E-10
    ricrit = 0.25
    bvmin = 1.E-6
    tol = 1.E-10
    !
    ! IWE coefficients, ref. Ch. 4, Ph.D. Thesis Uittenbogaard (1995).
    !
    alfaz = 0.5
    xmu0 = 7.5
    !
    ! Angle range (NRANGE) and increment/bracket (NINC) for searching
    ! lee-wave roots by subroutine BRKBED:
    !
    ninc = 5
    nrange = 180 + ninc
    !
    ! Turbulence coefficients:
    ! For longitudinal horizontal and longitudinal vertical
    ! integral length scale are derived from (Kaimal, 1973), see also
    ! the Veriparse report (Uittenbogaard, 1994, p. 3-37):
    !
    clu = 0.88
    clw = 0.064
    ckw = 0.46
    viscof = 1.E-6
end subroutine iwecof

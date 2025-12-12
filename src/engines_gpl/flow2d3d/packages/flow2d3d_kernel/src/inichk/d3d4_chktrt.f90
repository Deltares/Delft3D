module m_d3d_chktrt
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
!-------------------------------------------------------------------------------
    
implicit none 

private

public :: d3d4_chktrt

contains 
    
    
subroutine d3d4_chktrt(lundia    ,error     ,kcu, kcv, gdp)

use globaldata
    use trachytopes_data_module, only: trachy_type
    use grid_dimens_module, only: griddimtype
    use m_trtrou, only: chktrt
    
!
! Global variables
!
    type(globdat),target :: gdp
    integer                                                                   :: lundia
    logical                                                     ,intent(out)  :: error
    !integer                                                                   :: ddbval
    !character(256)                                              , intent(in)  :: flnmD50
    !character(256)                                              , intent(in)  :: flnmD90
    !type (griddimtype), target , intent(in)  :: griddim
    !type(trachy_type), target :: gdtrachy
    !logical                                                     , intent(in)  :: lfbedfrmrou
    !logical                                                                   :: sedim
    
!
! Local variables
!
    integer :: jdir, m, n, nm
    !integer                    , pointer :: mmax
    !integer                    , pointer :: nmax
    integer                    , pointer :: nodir
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)           , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    
    !nmlb           => griddim%nmlb   
    !nmub           => griddim%nmub   
    !nmax           => griddim%nmax
    !mmax           => griddim%mmax
    !n_m_to_nm      => griddim%n_m_to_nm
    nodir          => gdp%gdtrachy%gen%nodir
     !   kcu         => gdp%gdr_i_ch%kcu
    !kcv         => gdp%gdr_i_ch%kcv
    
    !nmaxddb = nmax + 2*ddbval
    
    !nmmax = nmaxddb*(mmax + 2*ddbval)

    
!in FM:
    ! `call rdtrt`
    ! copy `kcu` `kcv` data
    ! `call chktrt`
    do jdir = 1, nodir
       do m = gdp%d%mlb, gdp%d%mub
          do n = gdp%d%nlb, gdp%d%nub
             call n_and_m_to_nm(n, m, nm, gdp)
             !n_m_to_nm
             if (jdir==1) then
                gdp%gdtrachy%dir(jdir)%kcu_trt(nm) = kcu(n,m) 
                else
                gdp%gdtrachy%dir(jdir)%kcu_trt(nm) = kcv(n,m) 
                endif
          enddo
       enddo
       enddo
    !n_m_to_nm      => griddim%n_m_to_nm
    !kcu_trt = 1
    !do jdir = 1, nodir
    !do nm = 1, nmmax
    !  gdtrachy%dir(jdir)%kcu_trt(nm) = kcu_trt(nm) ! Copy here to be able to pass on to chktrt. TODO: choose which kcu_trt should remain (or both).
    !end do
    !end do
  
    
    !call chktrt(lundia    ,error     ,griddim   , & 
    !         & gdtrachy  ,flnmD50   ,flnmD90   ,lfbedfrmrou, sedim, ddbval)
    call chktrt(lundia    ,error     ,gdp%griddim   , & 
                 & gdp%gdtrachy  ,gdp%gdbedformpar%flnmD50   ,gdp%gdbedformpar%flnmD90   ,gdp%gdbedformpar%lfbedfrmrou, gdp%gdprocs%sedim, gdp%d%ddbound)
   
end subroutine d3d4_chktrt
            
end module m_d3d_chktrt
                
                
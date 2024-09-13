subroutine gdp_dump(gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: gdp_dump.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/gdp_dump.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
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
!
! Global variables
!
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    write (*, *) 'GDP % gdaddress'
    write (*, *) gdp%gdaddress
    !      write(*,*) 'GDP % gdcheckc'
    !      write(*,*) GDP % gdcheckc
    !      write(*,*) 'GDP % gdchecki'
    !      write(*,*) GDP % gdchecki
    !      write(*,*) 'GDP % gdcoup'
    !      write(*,*) GDP % gdcoup
    write (*, *) 'GDP % gddatusr'
    write (*, *) gdp%gddatusr
    write (*, *) 'GDP % D'
    write (*, *) gdp%d
    ! write (*, *) 'GDP % gdveg3d'
    ! write (*, *) gdp%gdveg3d
    write (*, *) 'GDP % gdexttim'
    write (*, *) gdp%gdexttim
    write (*, *) 'GDP % gdfmtbcc'
    write (*, *) gdp%gdfmtbcc
    write (*, *) 'GDP % gdfmtbct'
    write (*, *) gdp%gdfmtbct
    write (*, *) 'GDP % gdfmtdis'
    write (*, *) gdp%gdfmtdis
    !write (*, *) 'GDP % gdheat'
    !write (*, *) gdp%gdheat
    write (*, *) 'GDP % gdinout'
    write (*, *) gdp%gdinout
    write (*, *) 'GDP % gdinttim'
    write (*, *) gdp%gdinttim
    write (*, *) 'GDP % gdiwearr'
    write (*, *) gdp%gdiwearr
    write (*, *) 'GDP % gdiwepar'
    write (*, *) gdp%gdiwepar
    write (*, *) 'GDP % gdkeywtd'
    write (*, *) gdp%gdkeywtd
    write (*, *) 'GDP % gdluntmp'
    write (*, *) gdp%gdluntmp
    write (*, *) 'GDP % gdnumeco'
    write (*, *) gdp%gdnumeco
    write (*, *) 'GDP % gdnfl'
    write (*, *) gdp%gdnfl
    write (*, *) 'GDP % gdphysco'
    write (*, *) gdp%gdphysco
    write (*, *) 'GDP % gdpointrs'
    write (*, *) gdp%gdpointrs
    write (*, *) 'GDP % gdprocs'
    write (*, *) gdp%gdprocs
    write (*, *) 'GDP % gdr_i_ch'
    write (*, *) gdp%gdr_i_ch
    write (*, *) 'GDP % gdrdpara'
    write (*, *) gdp%gdrdpara
    write (*, *) 'GDP % gdtfzeta'
    write (*, *) gdp%gdtfzeta
    write (*, *) 'GDP % gdtmpfil'
    write (*, *) gdp%gdtmpfil
    write (*, *) 'GDP % gdturcoe'
    write (*, *) gdp%gdturcoe
    write (*, *) 'GDP % gdusrpar'
    write (*, *) gdp%gdusrpar
    write (*, *) 'GDP % gdzmodel'
    write (*, *) gdp%gdzmodel
    write (*, *) 'GDP % gdnonhyd'
    write (*, *) gdp%gdnonhyd
    !
    write (*, *) 'GDP % gdf0isf1'
    write (*, *) gdp%gdf0isf1
    ! write (*, *) 'GDP % gdincbc'
    ! write (*, *) gdp%gdincbc
    write (*, *) 'GDP % gdincbcc'
    write (*, *) gdp%gdincbcc
    write (*, *) 'GDP % gdinibcc'
    write (*, *) gdp%gdinibcc
    write (*, *) 'GDP % gdinibct'
    write (*, *) gdp%gdinibct%timscl
    write (*, *) gdp%gdinibct%tseriesfile%htype
    write (*, *) 'GDP % gdinidis'
    write (*, *) gdp%gdinidis
    write (*, *) 'GDP % gdpostpr'
    ! write (*, *) gdp%gdpostpr
    write (*, *) 'GDP % gdtricom'
    write (*, *) gdp%gdtricom
    write (*, *) 'GDP % gdu_ppr'
    write (*, *) gdp%gdu_ppr
    write (*, *) 'GDP % gdupdbcc'
    write (*, *) gdp%gdupdbcc
    write (*, *) 'GDP % gdupdbct'
    write (*, *) gdp%gdupdbct
    write (*, *) 'GDP % gdupddis'
    write (*, *) gdp%gdupddis
    write (*, *) 'GDP % gdwrirst'
    write (*, *) gdp%gdwrirst
    write (*, *) 'GDP % gdwrline'
    write (*, *) gdp%gdwrline
    write (*, *) 'GDP % gdz_initcg'
    write (*, *) gdp%gdz_initcg
    !
    write (*, *) 'GDP % dd'
    write (*, *) gdp%dd
    !
    write (*, *) 'GDP % runid'
    write (*, *) gdp%runid
    !
end subroutine gdp_dump

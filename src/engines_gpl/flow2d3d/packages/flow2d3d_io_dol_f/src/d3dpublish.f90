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
!
! Utility module for DOL / Delft OnLine
!
! It contains some global data and some functions

module D3DPublish
use D3DOnline

implicit none

private    !All viriables and routines are private, unless declared public

public publishGDP
public publishUtils
public setRunningFlag
public setEndTimeStep
public setEndFlag
public timeStepPrint
public printArray

logical :: first       =  .FALSE.
integer :: runningFlag =  0       !0=Flow not running (waiting)
                                  !1=Flow is iterating time steps
integer :: currentStep =  0
integer :: endTimeStep =  0
integer :: endFlag     =  0       !0 = Flow simulation has not finished yet
                                  !1 = Flow has finished
integer :: timeStepInt = -1
!=============================================================================
contains

    subroutine publishGDP(olv_handle, gdp, runid, zmodel)
        
        use precision
        use globaldata
        use d3d_olv_class
        use flow2d3d_version_module
        
        implicit none

        !-----  Global data  ------!

        type(globdat), target :: gdp ! Declares the gdp and has a USE statement
        include 'fsm.i'
        include 'tri-dyn.igd'
        include 'delftonline.i'

        !-----  Parameters  ------!

        type(olvhandle) :: olv_handle
        logical         :: zmodel
        character(*)    :: runid

        !-----  Local data  ------!
            
        type(olv_handle_t), pointer  :: olv
        integer                      :: dimens(3)     ! 3D array to contain various D3D array dimensions
        integer                      :: dimens_k(1)   ! 1D array to contain kmax array dimension
        integer                      :: j
        integer                      :: k
        integer(pntrsize)            :: indx
        integer                      :: realDouble
        integer                      :: size_const
        integer                      :: r_offset
        integer                      :: ch_offset
        character(20)                :: name          ! character length MUST be 20 according namcon!!!
        character(120)               :: cident
        

        if (isNull_olv(olv_handle)) return

        !-----  Executable statements  ------!
        olv => olv_handle%fields
        
        !----- Set the precision for reals

        if (fp /= prec) then
            return
            !
            ! This causes the Main Development Stream version to abort on single precision mode
            ! ToDo: What DO you want?
            !
            !write (*,*) 'Delft3D Online Abort: Mixed precision between FLOW and BODSED/DPS'
            !call d3stop (1, gdp)    ! ToDo: A proper way to terminate 
        endif

        if (fp == hp) then
            realDouble = DOL_DOUBLE
        else
            realDouble = DOL_REAL
        endif

        !-----  Set the run title

        call getfullversionstring_flow2d3d(cident)
        call FLOWOL_SetDescription (cident, "Delft3D-FLOW")

        !----- Size of the array dimensions
        !
        dimens(1)   = gdp%d%nub - gdp%d%nlb + 1
        dimens(2)   = gdp%d%mub - gdp%d%mlb + 1
        dimens(3)   = gdp%d%kmax
        dimens_k(1) = gdp%d%kmax

        call FLOWOL_ArrayShape ("nm"  , 2, dimens)
        call FLOWOL_ArrayShape ("nmk" , 3, dimens)
        call FLOWOL_ArrayShape ("kmax", 1, dimens_k)

        ! For some arrays dimens(3)=kmax+1; So, the third dimension varies, quantity is defined at interfaces.
        !
        dimens(3) = gdp%d%kmax+1
        call FLOWOL_ArrayShape ("nmk1", 3, dimens)
        !
        ! Do not use dimens(3) below (is it kmax or kmax+1?) but use gdp%d%kmax
        ! For safety:
        dimens(3) = -999

        !----- Description parameters D3DOnline_Publish (full description and declaration in esm_alloc_real.f90):
        ! A      internal name
        ! B      user name (description)
        ! C      units as string
        ! D      code "definedon"
        ! E   array shape, defined earlier or empty, which means a scalair
        ! F   type of the variable or array, an integer (enumeration), See file "delftonline.i"
        ! G   the scalair or the array (first array position)
        ! H   integer (enumeration). For OLV it is only DOL_OUT
        !    
        ! parameter codes     A               B                        C    D   E       F            G                      H
        !
        !The following parameters, always need to be published (descriptions can be found in esm_alloc_real.f90)!
        call FLOWOL_Publish( 'nlb'  , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%nlb            , DOL_OUT)
        call FLOWOL_Publish( 'nub'  , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%nub            , DOL_OUT)
        call FLOWOL_Publish( 'mlb'  , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%mlb            , DOL_OUT)
        call FLOWOL_Publish( 'mub'  , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%mub            , DOL_OUT)
        call FLOWOL_Publish( 'nmax' , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%nmax           , DOL_OUT)
        call FLOWOL_Publish( 'mmax' , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%mmax           , DOL_OUT)
        call FLOWOL_Publish( 'kmax' , ''                             , '' , '', ''    , DOL_INTEGER, gdp%d%kmax           , DOL_OUT)
        call FLOWOL_Publish( 'thick', 'Rel. layer thickness'         , '' , '', 'kmax', realDouble , r(gdp%gdr_i_ch%thick), DOL_OUT)      
        call FLOWOL_Publish( 'xcor' , 'Grid X coordinates'           , 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%xcor) , DOL_OUT)
        call FLOWOL_Publish( 'ycor' , 'Grid Y coordinates'           , 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%ycor) , DOL_OUT)
        call FLOWOL_Publish( 'gvz'  , 'Zeta-Grid cell distance (eta)', 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%gvz)  , DOL_OUT)
        call FLOWOL_Publish( 'guz'  , 'Zeta-Grid cell distance (ksi)', 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%guz)  , DOL_OUT)
        call FLOWOL_Publish( 'xz'   , 'Zeta-Grid X coord.'           , 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%xz)   , DOL_OUT)
        call FLOWOL_Publish( 'yz'   , 'Zeta-Grid Y coord.'           , 'm', '', 'nm'  , realDouble , r(gdp%gdr_i_ch%yz)   , DOL_OUT)
        call FLOWOL_Publish( 'kcs'  , 'KCS mask array'               , '' , '', 'nm'  , DOL_INTEGER, i(gdp%gdr_i_ch%kcs)  , DOL_OUT)
        call FLOWOL_Publish( 'kfs'  , 'KFS mask array'               , '' , '', 'nm'  , DOL_INTEGER, i(gdp%gdr_i_ch%kfs)  , DOL_OUT)
        call FLOWOL_Publish( 'kfu'  , 'KFU mask array'               , '' , '', 'nm'  , DOL_INTEGER, i(gdp%gdr_i_ch%kfu)  , DOL_OUT)
        call FLOWOL_Publish( 'kfv'  , 'KFV mask array'               , '' , '', 'nm'  , DOL_INTEGER, i(gdp%gdr_i_ch%kfv)  , DOL_OUT)
        call FLOWOL_Publish( 'alfas', 'Cell orientations'            , '' , '', 'nm'  , realDouble , r(gdp%gdr_i_ch%alfas), DOL_OUT)

        !The following variables/parameters are optional
        call FLOWOL_Publish( 's1'   , 'Water level'                  , '' , '', 'nm'  , realDouble , r(gdp%gdr_i_ch%s1)   , DOL_OUT)  
        call FLOWOL_Publish( 'dp'   , 'Depth'                        , '' , '', 'nm'  , realDouble , r(gdp%gdr_i_ch%dpd)  , DOL_OUT)  
        call FLOWOL_Publish( 'dps'  , 'Bedlevel'                     , '' , '', 'nm'  , DOL_DOUBLE , d(gdp%gdr_i_ch%dps)  , DOL_OUT)
        call FLOWOL_Publish( 'u1'   , 'Velocity ksi component'       , '' , '', 'nmk' , realDouble , r(gdp%gdr_i_ch%u1)   , DOL_OUT)
        call FLOWOL_Publish( 'v1'   , 'Velocity eta component'       , '' , '', 'nmk' , realDouble , r(gdp%gdr_i_ch%v1)   , DOL_OUT)  

        !call FLOWOL_Publish( 'rho',  'Density',              '',    '', 'nm',   realDouble,  r(rho),   DOL_INOUT)

        ! The following variables are only necessary when the z-model is used. It has to be done in this manner,
        ! otherwise things will go wrong in the client (test if zmodel exists).
        if ( zmodel ) then 
            call FLOWOL_Publish('zbot'  , ''               , '', '', ''   , realDouble , gdp%gdzmodel%zbot    , DOL_OUT)
            call FLOWOL_Publish('ztop'  , ''               , '', '', ''   , realDouble , gdp%gdzmodel%ztop    , DOL_OUT)
            call FLOWOL_Publish('zmodel', ''               , '', '', ''   , DOL_LOGICAL, zmodel               , DOL_OUT)
            call FLOWOL_Publish('kfsz1' , 'KFSZ mask array', '', '', 'nmk', DOL_INTEGER, i(gdp%gdr_i_ch%kfsz1), DOL_OUT)
        endif
    

        ! Publish so called "transportable quantities"
        ! The array character(20), dimension(lstsci + ltur) :: namcom 
        ! contains the names of these "transportable quantities" AND the turbulence quantities.
        ! Variable  lstsci  is the number of quanties and including  Sal., temp. en spiraalstroming,
        ! which has the names 'Salinity', 'Temperature', 'Secondary flow', 'Turbulent energy' and
        ! 'Energy dissipation '
        ! Variable ltur is the number of turbulence quantities.
        ! lmax = lstsci + ltur
        !
        ! The "transportable quantities" are in array r(r1) as first postion and 
        ! the turbulence quantities are in array r(rtur1) as first position.
        ! 

        ! Character array namcon is part of the large memory pool array "ch" and indicated
        ! by namcon.
        ! Character array namcon is defined as character(len=20).
        ! So here you must calculate the right offset in the large array "ch"
        ! To pass the right character length to routine FLOWOL_Publish the
        ! local character variable name is used.
        !
        ! Publish the "transportable quantities" by calculating their relative position:
        !
        size_const = dimens(1) * dimens(2) * gdp%d%kmax   !number of gridpoints per quantity

        ! Do an allocate of the same array size as ch from esm/fsm
        ! character length is 20 for both arrays
        !

        do j = 1, gdp%d%lstsci
            ch_offset = (j-1) * 20
            r_offset  = (j-1) * size_const
            indx = gdp%gdr_i_ch%namcon+ch_offset
            do k = 1, 20
                name(k:k) = ch(indx)(k:k)
            enddo
            call FLOWOL_Publish(name, name, '', '', 'nmk', realDouble, r(gdp%gdr_i_ch%r1+r_offset), DOL_OUT)  
        enddo

        ! Publish the "turbulence quantities" by calculating their relative position:
        !
        size_const = dimens(1) * dimens(2) * (gdp%d%kmax+1)     !number of gridpoints per quantity
        do j = 1, gdp%d%ltur
            ch_offset = (j-1 + gdp%d%lstsci) * 20    
            r_offset  = (j-1)                * size_const
            indx = gdp%gdr_i_ch%namcon+ch_offset
            do k = 1, 20
                name(k:k) = ch(indx)(k:k)
            enddo
            call FLOWOL_Publish(name, name, '', '', 'nm', realDouble, r(gdp%gdr_i_ch%rtur1+r_offset), DOL_OUT)
        enddo


    end subroutine publishGDP
!
!---------------------------------------------------------------------------------------

    subroutine publishUtils(olv_handle)
        use d3d_olv_class
        
        include 'delftonline.i'
        type(olvhandle)             :: olv_handle
        type(olv_handle_t), pointer :: olv

        if (isNull_olv(olv_handle)) return
                
        olv => olv_handle%fields
        
        call FLOWOL_Publish( 'runningFlag', '', '', '', '', DOL_INTEGER, olv%runningFlag,  DOL_OUT)
        call FLOWOL_Publish( 'currentStep', '', '', '', '', DOL_INTEGER, olv%currentStep,  DOL_OUT)
        call FLOWOL_Publish( 'endTimeStep', '', '', '', '', DOL_INTEGER, olv%endTimeStep,  DOL_OUT)
        call FLOWOL_Publish( 'endFlag'    , '', '', '', '', DOL_INTEGER, olv%endFlag    ,  DOL_OUT)
        call FLOWOL_Publish( 'timeStepInt', '', '', '', '', DOL_INTEGER, olv%timeStepInt,  DOL_INOUT)

    end subroutine publishUtils
!---------------------------------------------------------------------------------------

    subroutine setRunningFlag(olv_handle, value, nst)
        use d3d_olv_class
        
        integer :: value
        integer :: nst

        type(olvhandle) :: olv_handle
        type(olv_handle_t), pointer :: olv

        if (isNull_olv(olv_handle)) return

        olv => olv_handle%fields
        
        olv%runningFlag = value
        olv%currentStep = nst

    end subroutine setRunningFlag
!---------------------------------------------------------------------------------------

    subroutine setEndTimeStep(olv_handle, itstop)
        use d3d_olv_class
        
        integer :: itstop
        
        type(olvhandle) :: olv_handle
        type(olv_handle_t), pointer :: olv

        if (isNull_olv(olv_handle)) return

        olv => olv_handle%fields

        olv%endTimeStep = itstop

    end subroutine setEndTimeStep
!---------------------------------------------------------------------------------------

    subroutine setEndFlag(olv_handle, value)
        use d3d_olv_class
        integer :: value

        type(olvhandle) :: olv_handle
        type(olv_handle_t), pointer :: olv

        if (isNull_olv(olv_handle)) return

        olv => olv_handle%fields

        olv%endFlag = value

    end subroutine

!---------------------------------------------------------------------------------------

    subroutine timeStepPrint(olv_handle)
        use d3d_olv_class

        type(olvhandle) :: olv_handle
        type(olv_handle_t), pointer :: olv

        olv => olv_handle%fields

        write(*,*) 'Current Timestep =' , olv%timeStepInt + 1

    end subroutine

!---------------------------------------------------------------------------------------
    
    subroutine printArray(nlb, nub, mlb, mub, value, name)
        
        integer :: nlb, nub, mlb, mub
        integer, dimension( nlb:nub, mlb:mub ) :: value
        character(len=*) :: name

        integer :: m, n

        write(*,*) name
        do m = mlb, mub
            write(*, *) 'm=' , m
            do n = nlb, nub
                write(*,'(i4)') value(n,m)
            enddo
        enddo

    end subroutine printArray
!---------------------------------------------------------------------------------------

end module D3DPublish

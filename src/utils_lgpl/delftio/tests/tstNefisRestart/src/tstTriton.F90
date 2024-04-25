!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!*********************
!* TEST TRITON RESTART
!*********************

!
! Store restart file
!

subroutine test_triton_store(fileName)

    use M_TritonRestart

    implicit none

    ! arguments
    character(Len=*), intent(IN)  :: fileName  ! restart file name

    ! sizes
    integer, parameter :: numM1 = 5  ! dimensions of some 2D-arrays
    integer, parameter :: numN1 = 3

    integer, parameter :: numM2 = 4  ! dimensions of some other 2D-arrays
    integer, parameter :: numN2 = 7  ! Other dimensions will cause and error; needs to be handled in the future

    integer, parameter :: size1D_A = 12  ! dimension of 1D-arrays
    integer, parameter :: size1D_B = 8   ! dimension of some other 1D-arrays

    ! 2D arrays
    double precision, &
        dimension(numM1,numN1) :: depth, depth_aux, q    ! some 2D-arrays
    double precision, &
        dimension(numM2,numN2) :: r, prop_dir, age_cur   ! some more 2D-arrays

    ! 1D arrays
    double precision, &
        dimension(size1D_A)    :: a, b, c  ! some 1D-arrays
    double precision, &
        dimension(size1D_B)    :: d, e     ! some other 1D-arrays

    ! time step
    double precision           :: timeCounter = 12.D+00

    !
    ! INITIALIZE
    !
    depth     = 1.D+00
    depth_aux = 2.D+00
    q         = 3.D+00
    r         = 4.D+00
    prop_dir  = 5.D+00
    age_cur   = 6.D+00
 
    a = 11.0D+0
    b = 12.0D+0
    c = 13.0D+0
    d = 14.0D+0
    e = 15.0D+0

    !
    ! STORE DATA
    !
    if ( TritonRestartOpen(fileName, 'w', timeCounter ) ) then

        call TritonRestartStore('depth'    , depth    )
        call TritonRestartStore('depth_aux', depth_aux)
        call TritonRestartStore('q'        , q        )
        call TritonRestartStore('r'        , r        )
        call TritonRestartStore('prop_dir' , prop_dir )
        call TritonRestartStore('age_cur'  , age_cur  )

        call TritonRestartStore('a', a )
        call TritonRestartStore('b', b )
        call TritonRestartStore('c', c )
        call TritonRestartStore('d', d )
        call TritonRestartStore('e', e )

        call TritonRestartClose()

    endif

end subroutine test_triton_store


subroutine test_triton_retrieve(fileName)

    use M_TritonRestart

    implicit none

    ! arguments
    character(Len=*), intent(IN)  :: fileName    ! restart file name

    ! resultFile
    integer                       :: resLun      ! handle to result file
    character(Len=DioMaxStreamLen):: resFileName = 'TESTTriton-res.txt'

    ! sizes
    integer, parameter :: numM1 = 5     ! dimensions of some 2D-arrays
    integer, parameter :: numN1 = 3

    integer, parameter :: numM2 = 4     ! dimensions of some other 2D-arrays
    integer, parameter :: numN2 = 7

    integer, parameter :: numMWrong = 4 ! wrong dimensions when getting prop_dir
    integer, parameter :: numNWrong = 8

    integer, parameter :: size1D_A = 12  ! dimension of 1D-arrays
    integer, parameter :: size1D_B = 8   ! dimension of some other 1D-arrays

    ! 2D arrays
    double precision, &
        dimension(numM1,numN1) :: depth, depth_aux, q ! some arrays
    double precision, &
        dimension(numM2,numN2) :: r, age_cur          ! some more arrays
    double precision, &
        dimension(numMWrong,numNWrong) :: prop_dir    ! wrong size array

    ! 1D arrays
    double precision, &
        dimension(size1D_A)    :: a, b, c  ! some 1D-arrays
    double precision, &
        dimension(size1D_B)    :: d, e     ! some other 1D-arrays

    ! time step
    double precision   :: timeCounter = 0.D+00 ! Time counter in restart File

    !
    ! INITIALIZE
    !
    
    open(newunit=resLun, file=resFileName)
  
    depth     = 0.D+00
    depth_aux = 0.D+00
    q         = 0.D+00
    r         = 0.D+00
    prop_dir  = 0.D+00
    age_cur   = 0.D+00
 
    !
    ! LOAD DATA
    !

    if ( TritonRestartOpen(fileName, 'r', timeCounter ) ) then

        write(resLun,*) 'TIME COUNTER: ', timeCounter

        if ( TritonRestartLoad('depth', depth ) ) then
            write (resLun, *) 'DEPTH'
            write (resLun, *) depth
        endif

        if ( TritonRestartLoad('depth_aux', depth_aux ) ) then
            write (resLun, *) 'DEPTH_AUX'
            write (resLun, *) depth_aux
        endif

        if ( TritonRestartLoad('q' , q ) ) then
            write (resLun, *) 'Q'
            write (resLun, *) q
        endif

        if ( TritonRestartLoad('r' , r ) ) then
            write (resLun, *) 'R'
            write (resLun, *) r
        endif

        if ( TritonRestartLoad('prop_dir' , prop_dir ) ) then
            write (resLun, *) 'PROP_DIR'
            write (resLun, *) prop_dir
        endif

        if ( TritonRestartLoad('age_cur' , age_cur ) ) then
            write (resLun, *) 'AGE_CUR'
            write (resLun, *) age_cur
        endif

        if ( TritonRestartLoad('a' , a ) ) then
            write (resLun, *) 'a'
            write (resLun, *) a
        endif

        if ( TritonRestartLoad('b' , b ) ) then
            write (resLun, *) 'b'
            write (resLun, *) b
        endif

        if ( TritonRestartLoad('c' , c ) ) then
            write (resLun, *) 'c'
            write (resLun, *) c
        endif

        if ( TritonRestartLoad('d' , d ) ) then
            write (resLun, *) 'd'
            write (resLun, *) d
        endif

        if ( TritonRestartLoad('e' , e ) ) then
            write (resLun, *) 'e'
            write (resLun, *) e
        endif

        call TritonRestartClose()

    endif

    close(resLun)

end subroutine test_triton_retrieve


program test_triton

    character(Len=100) :: fileName = 'TESTtritonRestart'  ! restart file name

    call test_triton_store   (fileName)
    call test_triton_retrieve(fileName)

end program test_triton

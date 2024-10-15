subroutine rdhyvd(error     ,nrrec     ,mdfrec    ,filedy    ,fmtedy    , &
                & tkemod    ,xlo       ,vicouv    ,dicouv    ,vicoww    , &
                & dicoww    ,mmax      ,nmax      ,nmaxus    ,kmax      , &
                & lstsci    ,vicuv     ,dicuv     ,gdp       )
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
!    Function: - Reads the following records from the MD-file:
!                (if Htur2d is true:)
!                   Htural,Hturnd,Hturst,Hturlp,Hturrt,
!                   Hturdm,Hturdm
!                (else:)
!                FILEDY, FMTEDY, VICOUV & DICOUV
!              - If FILEDY = non-blank then reads the VICUV and
!                DICUV (optional) arrays from the specified
!                attribute file, otherwise
!              - Reads VICOUV and DICOUV (optional) values and
!                fills the related arrays with these uniform
!                values
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer  , pointer :: lunmd
    integer  , pointer :: lundia
    integer  , pointer :: lunscr
    integer  , pointer :: itis
    integer  , pointer :: nd
    real(fp) , pointer :: alpha
    real(fp) , pointer :: sigmat
    real(fp) , pointer :: flp
    real(fp) , pointer :: reltim
    real(fp) , pointer :: gamma
    real(fp) , pointer :: iinf
    real(fp) , pointer :: dicmol
    logical  , pointer :: elder
    logical  , pointer :: htur2d
!
! Global variables
!
    integer                                                                     , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                   :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                                   :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                   :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                   :: nrrec  !!  Pointer to the record number in the
                                                                                                        !!  MD-file
    logical                                                                                   :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                                                  :: dicouv !!  Horizontal Diffusion coeff. [m2/s]
    real(fp)                                                                                  :: dicoww !  Description and declaration in tricom.igs
    real(fp)                                                                                  :: vicouv !!  Horizontal eddy visc. coefficient
                                                                                                        !!  [m2/s] (in density point)
    real(fp)                                                                                  :: vicoww !  Description and declaration in tricom.igs
    real(fp)                                                                                  :: xlo    !  Description and declaration in turcoe.igs
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)              :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 2)              :: vicuv  !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                              :: filedy !!  File name for the eddy viscosities
    character(*)                                                                              :: mdfrec !!  Standard rec. length in MD-file (300)
    character(12)                                                                             :: tkemod !  Description and declaration in tricom.igs
    character(2)                                                                , intent(out) :: fmtedy !!  File format for the eddy viscosities
!
! Local variables
!
    integer                     :: idef
    integer                     :: k      ! Help var. 
    integer                     :: kbg    ! denotes the k-index of vicuv/dicuv containing the background values
    integer                     :: lenc   ! Help var. (length of var. cvar to be looked for in the MD-file) 
    integer                     :: lkw
    integer                     :: m      ! Help var. 
    integer                     :: n      ! Help var. 
    integer                     :: nlook  ! Help var.: nr. of data to look for in the MD-file 
    integer                     :: ntrec  ! Help. var to keep track of NRREC 
    integer      , dimension(1) :: ival
    logical                     :: defaul ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                     :: found  ! FOUND=TRUE if KEYW in the MD-file was found 
    logical                     :: lerror ! Flag=TRUE if an error is encountered 
    logical                     :: newkw  ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    real(fp)                    :: rdef   ! Help var. containing default va- lue(s) for real variable 
    real(fp)     , dimension(1) :: rval   ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    character(1)                :: cdef
    character(1)                :: chulp
    character(11)               :: fmtdef ! Default file format (usually=blank) 
    character(11)               :: fmttmp ! Help variable for file format 
    character(12)               :: fildef ! Default file name (usually = blank) 
    character(6)                :: keyw   ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
!
!! executable statements -------------------------------------------------------
!
    htur2d     => gdp%gdprocs%htur2d
    nd         => gdp%gdhtur2d%nd
    alpha      => gdp%gdhtur2d%alpha
    sigmat     => gdp%gdhtur2d%sigmat
    flp        => gdp%gdhtur2d%flp
    reltim     => gdp%gdhtur2d%reltim
    gamma      => gdp%gdhtur2d%gamma
    iinf       => gdp%gdhtur2d%iinf
    dicmol     => gdp%gdhtur2d%dicmol
    elder      => gdp%gdhtur2d%elder
    itis       => gdp%gdrdpara%itis
    lunmd      => gdp%gdinout%lunmd
    lundia     => gdp%gdinout%lundia
    lunscr     => gdp%gdinout%lunscr
    !
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    fildef = ' '
    fmtdef = 'FRformatted'
    fmttmp = ' '
    nlook  = 1
    kbg    = kmax + 1
    !
    ! initialize parameters that are to be read
    !
    alpha  = 5.0_fp/3.0_fp
    nd     = 2
    sigmat = 0.7_fp
    flp    = 1.0_fp/3.0_fp
    reltim = -999.0_fp
    dicmol = 0.0
    elder  = .true.
    filedy = ' '
    fmtedy = 'FR'
    vicouv = 0.0_fp
    dicouv = 0.0_fp
    !
    ! default value for vicoww=dicoww= 0.0 
    ! since the introduction of vicmol/dicmol
    !
    vicoww = 0.0_fp
    dicoww = 0.0_fp
    xlo    = 0.0_fp
    !
    ! HLES horizontal viscosity calculation?
    !
    if (htur2d) then
       write (lunscr, *) '*** HLES subgrid viscosity model activated'
       write (lundia, *) '*** HLES subgrid viscosity model activated'
       call prterr(lundia, 'U190', 'Background viscosity/diffusivity is added to the HLES contribution (since version 3.50.09.01)')
       !
       ! Read HLES alpha value
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Htural', alpha)
       !
       ! Read HLES nd value
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturnd', nd)
       !
       ! Read HLES sigmat value
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturst', sigmat)
       !
       ! Read HLES flp value
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturlp', flp)
       !
       ! Read HLES reltim value
       ! No default value!!
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturrt', reltim)
       if (reltim <= 0.0_fp .and. comparereal(reltim,-1.0_fp) /= 0) then
          call prterr(lundia, 'U021', 'Required parameter Hturrt not specified or incorrect.')
          call d3stop(1, gdp)
       endif
       !
       ! Read HLES dicmol value
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturdm', dicmol)
       !
       ! Read HLES elder flag
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Hturel', elder)
       !
       ! Derived parameters
       !
       iinf = 0.844_fp
       gamma = iinf*sqrt((1.0_fp - alpha**(-2.0_fp))/(2.0_fp*real(nd,fp)))
    endif
    !
    ! locate 'Filedy' record for non-uniform viscosity and
    ! diffusivity coefficients in extra input file
    !
    filedy = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'Filedy', filedy)
    if (filedy /= ' ') then
       !
       ! eddy viscosity and diffusifity values in file
       ! locate 'Fmtedy' record for format definition of input file
       !
       fmtedy = 'FR'
       call prop_get(gdp%mdfile_ptr, '*', 'Fmtedy', fmtedy)
       fmttmp = fmtedy
       call filfmt(lundia    ,'Fmtedy'  ,fmttmp    ,lerror    ,gdp       )
       call edyfil(lundia    ,error     ,filedy    ,fmttmp    ,nmax      , &
                 & mmax      ,nmaxus    ,kmax      ,lstsci    ,vicuv     , &
                 & dicuv     ,gdp       )
    else
       !
       ! No eddy- viscosity and diffusivity file
       ! 'Vicouv': uniform horizontal eddy-viscosity
       !
       call prop_get(gdp%mdfile_ptr,'*','Vicouv',vicouv)
       if (lstsci > 0) then
          !
          ! 'dicouv': uniform horizontal eddy-diffusivity
          !
          call prop_get(gdp%mdfile_ptr,'*','Dicouv',dicouv)
       endif
       !
       ! write per nmaxus mmax vicouv in vicuv array
       !
       do m = 1, mmax
          do n = 1, nmaxus
             vicuv(n, m, kbg) = vicouv
          enddo
       enddo
       !
       ! write per nmaxus mmax dicouv in dicuv array
       ! if lstsci = 0 then dicouv = 0 (ok)
       !
       do m = 1, mmax
          do n = 1, nmaxus
             dicuv(n, m, kbg) = dicouv
          enddo
       enddo
    endif
    !
    ! copy vicuv and dicuv for all layers
    !
    do k = 1, kmax
       do m = 1, mmax
          do n = 1, nmaxus
             vicuv(n, m, k) = vicuv(n, m, kbg)
             dicuv(n, m, k) = dicuv(n, m, kbg)
          enddo
       enddo
    enddo
    !
    ! locate 'Tkemod' record for the specification of the turb. model
    ! default value allowed = 'Algebraic ' (version 2.03 upwards)
    !
    if (kmax > 1) then
       tkemod = ' '
       call prop_get(gdp%mdfile_ptr,'*','Tkemod',tkemod)
       if (tkemod == ' ') then
          tkemod = 'Algebraic   '
       endif
       !
       ! Read uniform verical eddy-viscosity
       ! only if kmax > 1
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Vicoww', vicoww)
       !
       ! Read uniform vertical eddy-diffusivity
       ! only if kmax > 1
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Dicoww', dicoww)
       !
       ! Read xlo parameter
       !
       call prop_get(gdp%mdfile_ptr, '*', 'Xlo', xlo)
    endif
end subroutine rdhyvd

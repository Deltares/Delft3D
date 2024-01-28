module read_grids
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !
    use precision_basics


contains
!
!
!==============================================================================
subroutine get_gri(filnam    ,xz        ,yz        ,guu       ,gvv       , &
                &  alfas     ,kcs       ,covered   ,mmax      ,nmax      , &
                &  kmax      ,xymiss    ,layer_model)
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx  = 17
    integer, parameter :: nelmx2 =  4
    integer, parameter :: nelmx3 =  3
!
! Global variables
!
    integer                         , intent(out) :: mmax
    integer                         , intent(out) :: nmax
    integer                         , intent(out) :: kmax
    integer, dimension(:,:), pointer              :: kcs
    integer, dimension(:,:), pointer              :: covered !  0: target point is not covered by source grid
                                                             !  1: target point is covered by   valid points of source grid
                                                             ! -1: target point is covered by invalid points of source grid
    real(kind=hp)                   , intent(out) :: xymiss
    real   , dimension(:,:), pointer              :: alfas
    real   , dimension(:,:), pointer              :: guu
    real   , dimension(:,:), pointer              :: gvv
    real(kind=hp)   , dimension(:,:), pointer     :: xz
    real(kind=hp)   , dimension(:,:), pointer     :: yz
    character(*)                    , intent(in)  :: filnam
    character(*)                    , intent(out) :: layer_model
!
! Local variables
!
    integer, dimension(:,:), allocatable                 :: ibuff
    real,    dimension(:,:), allocatable                 :: rbuff

    integer                         :: celidt
    integer                         :: error
    integer                         :: ielem
    integer                         :: ierr
    integer                         :: m
    integer                         :: n
    integer, dimension(1)           :: ival
    integer, dimension(6, nelmx)    :: elmdms
    integer, dimension(nelmx)       :: nbytsg
    logical                         :: wrswch
    character(16), dimension(1)     :: cval
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grpnam
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(64), dimension(nelmx) :: elmdes

    integer, dimension(6, nelmx2)   :: elmdm2
    integer, dimension(nelmx2)      :: nbyts2
    character(10), dimension(nelmx2):: elmun2
    character(16)                   :: grpna2
    character(16), dimension(nelmx2):: elmnm2
    character(16), dimension(nelmx2):: elmqt2
    character(16), dimension(nelmx2):: elmtp2
    character(64), dimension(nelmx2):: elmde2

    integer, dimension(6, nelmx3)   :: elmdm3
    integer, dimension(nelmx3)      :: nbyts3
    character(10), dimension(nelmx3):: elmun3
    character(16)                   :: grpna3
    character(16), dimension(nelmx3):: elmnm3
    character(16), dimension(nelmx3):: elmqt3
    character(16), dimension(nelmx3):: elmtp3
    character(64), dimension(nelmx3):: elmde3
!
    !     Define data structure; element dimensions are required only
    !     in write-mode.
    !
    data grpnam/'GRID'/
    data elmnms/'MMAX', 'NMAX', 'XORI', 'YORI', 'ALFORI', 'XCOR', &
              & 'YCOR', 'GUU',  'GVV',  'GUV',  'GVU',    'GSQS', 'GSQD', &
              & 'ALFAS','KMAX', 'THICK', 'LAYER_MODEL'/
    data elmtps/2*'INTEGER', 12*'REAL','INTEGER','REAL', 'CHARACTER'/
    data nbytsg/16*4,16/
    data grpna2/'TEMPOUT'/
    data elmnm2/'XWAT','YWAT','CODB','CODW'/
    data elmtp2/2*'REAL',2*'INTEGER'/
    data nbyts2/nelmx2*4/
    data grpna3/'KENMCNST'/
    data elmnm3/'KCU', 'KCV', 'KCS'/
    data elmqt3/3*' '/
    data elmun3/3*'[   -   ]'/
    data elmtp3/3*'INTEGER'/
    data nbyts3/3*4/
!
!! executable statements -------------------------------------------------------
!
    ! missing value is currently not available in the com-file
    ! default is zero
    !
    xymiss = 0.0_hp
    !
    celidt = 1
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,15        ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,17        ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    
    wrswch = .false.
    ielem  = 1     ! MMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    mmax   = ival(1)

    ielem  = 2     ! NMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    nmax   = ival(1)

    ielem  = 15    ! KMAX
    call putgti(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,ival(1)   )
    kmax   = ival(1)

    ielem = 17     ! LAYER_MODEL
    call putgtc(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,cval      )
    layer_model = cval(1)

    call filldm(elmdms    ,3         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,9         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,10        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,11        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,12        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,13        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,14        ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,16        ,1         ,kmax      ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,1         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm2    ,4         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,1         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,2         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    call filldm(elmdm3    ,3         ,2         ,nmax      ,mmax      , &
              & 0         ,0         ,0         )
    !
    !
    ! Allocate arrays
    allocate (rbuff (nmax,mmax))    ! Note com-file dimensions are nmax,mmax
    allocate (ibuff (nmax,mmax))    ! Note com-file dimensions are nmax,mmax
    allocate (guu   (mmax,nmax))
    allocate (gvv   (mmax,nmax))
    allocate (alfas (mmax,nmax))
    allocate (xz    (mmax,nmax))
    allocate (yz    (mmax,nmax))
    allocate (kcs   (mmax,nmax))
    allocate (covered(mmax,nmax))
    covered = 0

    ielem = 8      ! GUU
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          guu(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 9      ! GVV
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          gvv(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 14     ! ALFAS
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          alfas(m, n) = rbuff(n, m)
       enddo
    enddo

    ielem = 1     ! XZ
    call putgtr(filnam    ,grpna2    ,nelmx2    ,elmnm2    ,elmdm2    , &
              & elmqt2    ,elmun2    ,elmde2    ,elmtp2    ,nbyts2    , &
              & elmnm2(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          xz(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 2     ! YZ
    call putgtr(filnam    ,grpna2    ,nelmx2    ,elmnm2    ,elmdm2    , &
              & elmqt2    ,elmun2    ,elmde2    ,elmtp2    ,nbyts2    , &
              & elmnm2(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    do m = 1, mmax
       do n = 1, nmax
          yz(m, n) = rbuff(n, m)
       enddo
    enddo
    ielem = 3     ! KCS
    call putgti(filnam    ,grpna3    ,nelmx3    ,elmnm3    ,elmdm3    , &
              & elmqt3    ,elmun3    ,elmde3    ,elmtp3    ,nbyts3    , &
              & elmnm3(ielem)        ,celidt    ,wrswch    ,error     ,ibuff     )
    do m = 1, mmax
       do n = 1, nmax
          kcs(m, n) = ibuff(n, m)
       enddo
    enddo

    deallocate (ibuff, stat=ierr)
    deallocate (rbuff, stat=ierr)

end subroutine get_gri
!
!
!==============================================================================
subroutine readgriddims(filnam, mmax, nmax)
    implicit none
!
! Global variables
!
    character(*), intent(in)  :: filnam
    integer     , intent(out) :: mmax
    integer     , intent(out) :: nmax
!
! Local variables
!
    integer           :: irgf
    character(256)    :: rec
!
!! executable statements -------------------------------------------------------
!
    open (newunit=irgf, file = filnam, form = 'formatted', status = 'old')
    read (irgf, '(a)', end = 7777, err = 8888) rec
10  continue
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*' &
           & .or. index(rec,'Coordinate System') >= 1 &
           & .or. index(rec,'Missing Value')     >= 1) goto 10
    read(rec,*,err=8888)  mmax, nmax
    goto 9999
 7777 continue
 8888 continue
    write(*,'(2a)') '*** ERROR: Unable to read dimensions in file ',trim(filnam)
    close(irgf)
    call wavestop(1, 'Unable to read dimensions in file '//trim(filnam))
 9999 continue
    close(irgf)
end subroutine readgriddims
!
!
!==============================================================================
subroutine replacecoordinates(filnam, mmax, nmax, xb, yb)
    implicit none
!
! Global variables
!
    character(*), intent(in)  :: filnam
    integer     , intent(in) :: mmax
    integer     , intent(in) :: nmax
    real(hp), dimension(mmax,nmax), intent(out) :: xb
    real(hp), dimension(mmax,nmax), intent(out) :: yb
!
! Local variables
!
    integer           :: i
    integer           :: irgf
    integer           :: j
    character(256)    :: rec
!
!! executable statements -------------------------------------------------------
!
    open (newunit=irgf, file = filnam, form = 'formatted', status = 'old')
    do j=1,nmax
       do i=1,mmax
          read(irgf,*) xb(i,j), yb(i,j)
       enddo
    enddo
    close(irgf)
end subroutine replacecoordinates
!
!
!==============================================================================
subroutine read_grd(filnam    ,xb     ,yb   ,codb ,covered, mmax  ,nmax ,sferic ,xymiss)
    use geometry_module, only: clockwise
    implicit none
!
! Global variables
!
    character(232)                   , intent(in)  :: filnam
    integer                          , intent(out) :: mmax
    integer                          , intent(out) :: nmax
    real(hp)                         , intent(out) :: xymiss
    integer , dimension(:,:), pointer              :: codb
    integer , dimension(:,:), pointer              :: covered
    real(hp), dimension(:,:), pointer              :: xb
    real(hp), dimension(:,:), pointer              :: yb
    logical                                        :: sferic
!
!
! Local variables
!
    real(hp), dimension(:,:,:),allocatable :: xy
    real(hp), dimension(4)                 :: xcell
    real(hp), dimension(4)                 :: ycell
    integer                                :: etamax
    integer                                :: i
    integer                                :: ierr
    integer                                :: irgf
    integer                                :: j
    integer                                :: k
    integer                                :: ksimax
    integer                                :: npareg
    integer                                :: pos
    logical                                :: kw_found
    character(10)                          :: dum
    character(256)                         :: rec
!
!! executable statements -------------------------------------------------------
!
    ! Default value for missing value: zero
    !
    xymiss = 0.0_hp
    sferic = .false.
    open (newunit=irgf, file = filnam, form = 'formatted', status = 'old')
    !
    ! Copied from file rdrgf
    !
    ! Read file, check for end of file or error in file:
    ! - The first line always contains comments
    !   sferic is true when the first line contains the keyword Spherical
    ! - Skip comment lines (starting with a '*'), while trying to read the
    !   following keywords: 'Coordinate System'
    !                       'Missing Value'
    !   If 'Coordinate System' is present, it overrules the sferic-specification
    !   in the first line!
    ! - The next line contains the dimensions mc and nc
    !   Parameter npart may also be on this line, but it is neglected
    ! - Read the next line containing three zero's
    !   xori, yori and alfori are not used anymore
    ! - Read x coordinates
    ! - Read y coordinates
    !
    read (irgf, '(a)', end = 7777, err = 8888) rec
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then
       sferic = .true.
    endif
10  continue
       kw_found = .false.
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*') goto 10
       !
       pos = index(rec,'Coordinate System')
       if (pos >= 1) then
          kw_found = .true.
          if (index(rec(pos+1:),'spherical') >= 1 .or. &
            & index(rec(pos+1:),'Spherical') >= 1 .or. &
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then
             sferic = .true.
          else
             sferic = .false.
          endif
       endif
       !
       pos = index(rec,'Missing Value')
       if (pos >= 1) then
          kw_found = .true.
          pos      = index(rec,'=') + 1
          read(rec(pos:),*,err=8888) xymiss
       endif
    if (kw_found) goto 10
    !
    if (sferic) then
       write (*, *)
       write (*, '(a)') 'Coordinate System: Spherical'
       write (*, *)
    endif
    read(rec,*,err=8888)  ksimax, etamax

    mmax = ksimax
    nmax = etamax

    allocate (xy    (2,mmax,nmax))
    allocate (xb      (mmax,nmax))
    allocate (yb      (mmax,nmax))
    allocate (codb    (mmax,nmax))
    allocate (covered (mmax,nmax))

    xy      = 0.0_hp
    xb      = 0.0_hp
    yb      = 0.0_hp
    codb    = 0
    covered = 0
    !
    ! read three zero's
    !
    read(irgf,'(a)',end = 7777,err=8888) rec
    !
    ! read X and Y coordinates
    ! read unformatted: The number of digits of xcor may vary
    !
    do k = 1, 2
       do j = 1, etamax
          read (irgf, *, end = 7777, err = 8888) dum,dum,(xy(k, i, j),i=1, ksimax)
       enddo
    enddo
    close (irgf)
    goto 9999
    !
    ! test for reading error : label 7777 end of file
    !                                8888 error while reading
    !
 7777 continue
 8888 continue
    goto 999
    !
 9999 continue
    !
    npareg = 0
    !
    if (npareg<=1) then
       !
       ! grid consists of one rectangle
       !
       do j = 1, etamax
          do i = 1, ksimax
             codb(i, j) = +1
          enddo
       enddo
    else
       !
       ! grid consists of more than one rectangle
       !
    endif
    !
    !     correction of code in bottom points
    !
    do i = 1, ksimax
       do j = 1, etamax
          if (abs(xy(1, i, j))<1.0e-6_hp .and. abs(xy(2, i, j))<1.0e-6_hp) then
             codb(i,j) = 0
          endif
       enddo
    enddo
    !
    !
    ! x- and y-coordinates in bottom points
    !
    do i = 1, ksimax
       do j = 1, etamax
          xb(i, j) = xy(1, i, j)
          yb(i, j) = xy(2, i, j)
       enddo
    enddo
    !
    ! test grid orientation based on first "grid cell"
    !
    do i = 2, ksimax
       do j = 2, etamax
          if (codb(i-1,j-1)/=0 .and. codb(i-1,j)/=0 .and. codb(i,j-1)/=0 .and. codb(i,j)/=0) then
             xcell(1) = xb(i-1,j-1)
             xcell(2) = xb(i  ,j-1)
             xcell(3) = xb(i  ,j)
             xcell(4) = xb(i-1,j)
             ycell(1) = yb(i-1,j-1)
             ycell(2) = yb(i  ,j-1)
             ycell(3) = yb(i  ,j)
             ycell(4) = yb(i-1,j)
             if (      xcell(1) == xcell(2) &
                 .and. xcell(1) == xcell(3) &
                 .and. xcell(1) == xcell(4) &
                 .and. ycell(1) == ycell(2) &
                 .and. ycell(1) == ycell(3) &
                 .and. ycell(1) == ycell(4) ) then
                ! subroutine clockwise will not work.
                ! This might happen if lowest indexed cells are removed (-999)
                ! Try another point
             else
                if (clockwise(xcell,ycell)) then
                   write (*, '(a)') '*** ERROR: Grid orientation incorrect: Delft3D requires (M,N) to form a counter-clockwise coordinate system.'
                   goto 999
                else
                   goto 1000
                endif
             endif
          endif
       enddo
    enddo
    goto 1000 ! shouldn't come here
    !
  999 continue
   write(*,'(2a)') '*** ERROR: reading GRD file ', trim(filnam)
   call wavestop(1, 'reading GRD file '//trim(filnam))
 1000 continue
    deallocate (xy, stat=ierr)
end subroutine read_grd
!
!
!==============================================================================
subroutine read_netcdf_grd(i_grid, filename, xcc, ycc, codb, covered, mmax, nmax, kmax, &
                         & sferic, xymiss, bndx, bndy, numenclpts, numenclparts, numenclptsppart, &
                         & filename_tmp, flowLinkConnectivity)
    use netcdf
    use dwaves_version_module
    implicit none
!
! Parameters
!
    integer,parameter :: nh = 1
!
! Global variables
!
    integer                          , intent(in)  :: i_grid
    character(*)                     , intent(in)  :: filename
    integer                          , intent(out) :: mmax
    integer                          , intent(out) :: nmax
    integer                          , intent(out) :: kmax
    integer                          , intent(out) :: numenclpts
    integer                          , intent(out) :: numenclparts
    real(hp)                         , intent(out) :: xymiss
    integer , dimension(:,:), pointer              :: codb
    integer , dimension(:,:), pointer              :: covered
    integer , dimension(:),   pointer              :: numenclptsppart
    real(hp), dimension(:,:), pointer              :: xcc
    real(hp), dimension(:,:), pointer              :: ycc
    real(hp), dimension(:),   pointer              :: bndx
    real(hp), dimension(:),   pointer              :: bndy    
    logical                                        :: sferic
    character(*)                                   :: filename_tmp
    logical                          , intent(in)  :: flowLinkConnectivity
!
! Local variables
!
    byte    , dimension(:)  , allocatable  :: nelemconn
    integer                                :: commonnodes
    integer                                :: etamax
    integer                                :: i
    integer                                :: idfile
    integer                                :: iddim_e
    integer                                :: iddim_enclsp
    integer                                :: iddim_numencpts
    integer                                :: iddim_numencparts
    integer                                :: iddim_laydim
    integer                                :: iddim_n
    integer                                :: iddim_mmax
    integer                                :: iddim_ndx
    integer                                :: iddim_nelm
    integer                                :: iddim_nemax
    integer                                :: iddim_nflowlink
    integer                                :: iddim_corners
    integer                                :: iddim_rank
    integer                                :: idvar_coords
    integer                                :: idvar_econn
    integer                                :: idvar_en
    integer                                :: idvar_flowlink
    integer                                :: idvar_griddims
    integer                                :: idvar_mask
    integer                                :: idvar_neconn
    integer                                :: idvar_nx
    integer                                :: idvar_ny
    integer                                :: idvar_x
    integer                                :: idvar_y
    integer                                :: idvar_encx
    integer                                :: idvar_ency
    integer                                :: idvar_encptsppt
    integer                                :: ierror
    integer                                :: ik
    integer                                :: irgf
    integer                                :: j
    integer                                :: jatri
    integer                                :: jk
    integer                                :: k
    integer                                :: ksimax
    integer                                :: maxelem
    integer                                :: necurnodes
    integer                                :: nemax
    integer                                :: nemaxout
    integer                                :: nelm
    integer                                :: nflowlink
    integer                                :: nnodes
    integer                                :: npareg
    integer, external                      :: nc_def_var
    integer                                :: numedge
    integer                                :: numencl
    integer                                :: pos
    integer                                :: elt
    integer                                :: lc
    integer                                :: hc
    integer                                :: eltlink2
    integer                                :: neighblow
    integer                                :: neighbhigh
    integer                                :: neighneigh
    integer , dimension(:,:), allocatable  :: elemtonode
    integer , dimension(:,:), allocatable  :: elemconn
    integer , dimension(:,:), allocatable  :: elemconntmp
    integer , dimension(:,:), allocatable  :: edgeindx
    integer , dimension(:,:), allocatable  :: flowlink
    integer , dimension(:,:), allocatable  :: triedge
    integer , dimension(:,:), allocatable  :: linkadmin
    real(hp)                               :: xh(nh)
    real(hp)                               :: yh(nh)
    real(hp)                               :: trisize
    real(hp), dimension(:)  , allocatable  :: xnode
    real(hp), dimension(:)  , allocatable  :: ynode
    real(hp), dimension(:)  , allocatable  :: mask_area
    real(hp), dimension(:)  , allocatable  :: nelmslice
    real(hp), dimension(:,:), allocatable  :: grid_corner
    logical                                :: eltlink2found
    logical                                :: kw_found
    logical                                :: regulargrid
    character(10)                          :: dum
    character(NF90_MAX_NAME)               :: string
    character(8)                           :: cdate
    character(10)                          :: ctime
    character(5)                           :: czone
    character(256)                         :: full_version
!
!! executable statements -------------------------------------------------------
!
    ! Default value for missing value: zero
    !
    nmax   = 1   ! Unstructured grid: use only mmax to count the elements
    xymiss = 0.0_hp
    !
    ierror = nf90_open(filename, NF90_NOWRITE, idfile); call nc_check_err(ierror, "opening file", filename)
    if (ierror /= 0) then
       ! First catch in case the com.nc file does not exist
       call wavestop(1, 'Com-file does not exist')
    endif
    !
    ierror = nf90_inq_dimid(idfile, 'nNetElem'        , iddim_e        ); call nc_check_err(ierror, "inq_dimid nNetElem", filename)
    ierror = nf90_inq_dimid(idfile, 'nNetNode'        , iddim_n        ); call nc_check_err(ierror, "inq_dimid nNetNode", filename)
    ierror = nf90_inq_dimid(idfile, 'nNetElemMaxNode' , iddim_nemax    ); call nc_check_err(ierror, "inq_dimid nNetElemMaxNode", filename)
    ierror = nf90_inq_dimid(idfile, 'nFlowElemWithBnd', iddim_ndx      ); call nc_check_err(ierror, "inq_dimid nFlowElemWithBnd", filename)
    if (flowLinkConnectivity) then
       ierror = nf90_inq_dimid(idfile, 'nFlowLink'       , iddim_nflowlink); call nc_check_err(ierror, "inq_dimid nFlowLink", filename)
    else
       ierror = nf90_inq_dimid(idfile, 'nNetLink'        , iddim_nflowlink); call nc_check_err(ierror, "inq_dimid nNetLink", filename)
    endif
    ierror = nf90_inq_dimid(idfile, 'laydim'          , iddim_laydim   ) ! no nc_check_err call: error is handled here
    if (ierror /= nf90_noerr) then
       kmax = 1
    else
       ierror = nf90_inquire_dimension(idfile, iddim_laydim, string, kmax); call nc_check_err(ierror, "inq_dim laydim", filename)
    endif
    ierror = nf90_inquire_dimension(idfile, iddim_e        , string, nelm     ); call nc_check_err(ierror, "inq_dim nNetElem", filename)
    ierror = nf90_inquire_dimension(idfile, iddim_n        , string, nnodes   ); call nc_check_err(ierror, "inq_dim nNetNode", filename)
    ierror = nf90_inquire_dimension(idfile, iddim_nflowlink, string, nflowlink); call nc_check_err(ierror, "inq_dim nFlowLink", filename)
    ierror = nf90_inquire_dimension(idfile, iddim_nemax    , string, nemax    ); call nc_check_err(ierror, "inq_dim nNetElemMaxNode", filename)
    if (nemax/=3 .and. nemax/=4) then
       write(*,'(a,i0,a)') "ERROR nNetElemMaxNode = ", nemax, ". Expecting 3 or 4."
       call wavestop(1, 'nNetElemMaxNode should be 3 or 4')
    endif
    ierror = nf90_inquire_dimension(idfile, iddim_ndx, string, mmax); call nc_check_err(ierror, "inq_dim ndx", filename)
    ierror = nf90_inq_varid(idfile, 'NetNode_x'   , idvar_nx      ); call nc_check_err(ierror, "inq_varid nx", filename)
    ierror = nf90_inq_varid(idfile, 'NetNode_y'   , idvar_ny      ); call nc_check_err(ierror, "inq_varid ny", filename)
    ierror = nf90_inq_varid(idfile, 'NetElemNode' , idvar_en      ); call nc_check_err(ierror, "inq_varid en", filename)
    string = ' '
    ierror = nf90_get_att(idfile, idvar_nx,  'standard_name', string); call nc_check_err(ierror, "get_att NetNode_x standard_name", filename)
    if (string == 'longitude') then
       sferic = .true.
    else
       sferic = .false.
    endif
    ierror = nf90_inq_varid(idfile, 'FlowElem_xcc', idvar_x       ); call nc_check_err(ierror, "inq_varid x", filename)
    ierror = nf90_inq_varid(idfile, 'FlowElem_ycc', idvar_y       ); call nc_check_err(ierror, "inq_varid y", filename)
    if (.not.sferic .and. flowLinkConnectivity) then
          ierror = nf90_inq_varid(idfile, 'FlowLink'   , idvar_flowlink); call nc_check_err(ierror, "inq_varid FlowLink", filename)
    endif
    !
    ierror = nf90_inq_dimid(idfile, 'nmesh2d_EnclosurePoints', iddim_numencpts       ); call nc_check_err(ierror, "inq_dimid nmesh2d_EnclosurePoints", filename)
    ierror = nf90_inquire_dimension(idfile, iddim_numencpts, string, numenclpts)      ; call nc_check_err(ierror, "inq_dim numencpts", filename)
    !
    ierror = nf90_inq_dimid(idfile, 'nmesh2d_EnclosureParts', iddim_numencparts       ); call nc_check_err(ierror, "inq_dimid nmesh2d_EnclosureParts", filename)
    ierror = nf90_inquire_dimension(idfile, iddim_numencparts, string, numenclparts)   ; call nc_check_err(ierror, "inq_dim numencparts", filename)
    !
    ierror = nf90_inq_varid(idfile, 'mesh2d_enc_x', idvar_encx       ); call nc_check_err(ierror, "inq_varid encx", filename)
    ierror = nf90_inq_varid(idfile, 'mesh2d_enc_y', idvar_ency       ); call nc_check_err(ierror, "inq_varid ency", filename)
    !
    ierror = nf90_inq_varid(idfile, 'mesh2d_enc_part_node_count', idvar_encptsppt  ); call nc_check_err(ierror, "inq_varid encptsppt", filename)
    !
    ! Allocate arrays
    ! xcc,ycc: +4 needed by subroutine tricall
    !
    allocate (xcc            (mmax+4,nmax)     , STAT=ierror)
    allocate (ycc            (mmax+4,nmax)     , STAT=ierror)
    allocate (bndx           (numenclpts)      , STAT=ierror)
    allocate (bndy           (numenclpts)      , STAT=ierror)
    allocate (numenclptsppart(numenclparts)    , STAT=ierror)
    allocate (codb           (mmax,nmax)       , STAT=ierror)
    allocate (covered        (mmax,nmax)       , STAT=ierror)
    allocate (xnode          (nnodes)          , STAT=ierror)
    allocate (ynode          (nnodes)          , STAT=ierror)
    allocate (elemtonode     (nemax,nelm)      , STAT=ierror)
    allocate (nelmslice      (nelm)            , STAT=ierror)
    if (sferic) then
       allocate (grid_corner(nemax,nelm), STAT=ierror)
       allocate (mask_area  (nelm)      , STAT=ierror)
    else
       ! Always use hexahedra
       !
       nemaxout = 4
       !
       ! Structured grid: maxelem= mmax is big enough
       ! Triangular grid: No idea what is a correct value
       ! Using same value as in DFlowFM
       !
       maxelem = mmax*6 +20
       !
       ! grid_corner: First  dim: x+y=2
       !              Second dim: number of xcc,ycc points
       allocate (grid_corner(2,mmax)          , STAT=ierror)
       allocate ( elemconn  (nemaxout,maxelem), STAT=ierror)
       allocate (nelemconn  (maxelem)         , STAT=ierror)
       allocate (flowlink   (2,nflowlink)     , STAT=ierror)
       ! Dummy arrays for calling tricall:
       allocate (edgeindx   (2,1)             , STAT=ierror)
       allocate (triedge    (3,1)             , STAT=ierror)
    endif
    if (ierror /= 0) then
       write(*,'(a)') "ERROR allocating in read_netcdf_grd"
       call wavestop(1, "ERROR allocating in read_netcdf_grd")
    endif
    codb    = 1
    covered = 0
    ierror = nf90_get_var(idfile, idvar_x        , xcc            , start=(/ 1 /)   , count=(/ mmax /)            ); call nc_check_err(ierror, "get_var x", filename)
    ierror = nf90_get_var(idfile, idvar_y        , ycc            , start=(/ 1 /)   , count=(/ mmax /)            ); call nc_check_err(ierror, "get_var y", filename)
    ierror = nf90_get_var(idfile, idvar_nx       , xnode          , start=(/ 1 /)   , count=(/ nnodes /)          ); call nc_check_err(ierror, "get_var xnode", filename)
    ierror = nf90_get_var(idfile, idvar_ny       , ynode          , start=(/ 1 /)   , count=(/ nnodes /)          ); call nc_check_err(ierror, "get_var ynode", filename)
    ierror = nf90_get_var(idfile, idvar_encx     , bndx           , start=(/ 1 /)   , count=(/ numenclpts /)      ); call nc_check_err(ierror, "get_var bndx", filename)
    ierror = nf90_get_var(idfile, idvar_ency     , bndy           , start=(/ 1 /)   , count=(/ numenclpts /)      ); call nc_check_err(ierror, "get_var bndy", filename)
    ierror = nf90_get_var(idfile, idvar_encptsppt, numenclptsppart, start=(/ 1 /)   , count=(/ numenclparts /)    ); call nc_check_err(ierror, "get_var encptsppt", filename)
    ierror = nf90_get_var(idfile, idvar_en       , elemtonode     , start=(/ 1, 1 /), count=(/ nemax, nelm /)     ); call nc_check_err(ierror, "get_var netelemnode", filename)
    if (.not.sferic .and. flowLinkConnectivity) then
       ierror = nf90_get_var(idfile, idvar_flowlink, flowlink  , start=(/ 1, 1 /), count=(/ 2, nflowlink /)); call nc_check_err(ierror, "get_var flowlink", filename)
    endif
    ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)
    !
    if (.not. sferic) then
       ! Is this a regular grid?
       if (nemax == 4) then
          regulargrid = .true.
          do i = 1, nelm
             if (elemtonode(4,i) <= 0) then
                regulargrid = .false.
                exit
             endif
          enddo
       else
          regulargrid = .false.
       endif
       !
       if (regulargrid) then
          ! Construct a regular grid with xcc,ycc as corner points and elemconn as connections between the elements
          ! Use the DFlowFM flowlink administration
          ! Pseudo code:
          !     Count the number of links per element
          !     do while true
          !         if an element has 1 link: remove from administration
          !         if an element has 2 links:
          !             find neighbour elements
          !             find (other) element being the neighbour of both neighbours
          !             add to elemconn: elt,neighbourLow, neighneigh, neighbourHigh
          !             update administration
          !         if no element with 2 links found: exit
          !     enddo
          !
          maxelem = 0
          allocate(linkadmin(mmax,5), STAT=ierror)
          !
          ! linkadmin(elt, 1): index of first neighbour (zero when no neighbours)
          ! linkadmin(elt, 2): index of second neighbour (zero when less than 2 neighbours)
          ! linkadmin(elt, 3): index of third neighbour (zero when less than 3 neighbours)
          ! linkadmin(elt, 4): index of fourth neighbour (zero when less than 4 neighbours)
          ! linkadmin(elt, 5): number of neighbours
          !
          linkadmin = 0
          if (flowLinkConnectivity) then
             !
             ! Fill linkadmin with info from flowlink
             ! TODO: boundary links and thindams are NOT INCLUDED in flowlink!
             !
             do i=1, nflowlink
                ! When flowlink contains netlinks, it might refer to cells outside the flow domain
                !
                if (flowlink(1,i)>mmax .or. flowlink(2,i) > mmax) cycle
                elt                             = flowlink(1,i)
                linkadmin(elt,5)                = linkadmin(elt,5) + 1
                linkadmin(elt,linkadmin(elt,5)) = flowlink(2,i)
                elt                             = flowlink(2,i)
                linkadmin(elt,5)                = linkadmin(elt,5) + 1
                linkadmin(elt,linkadmin(elt,5)) = flowlink(1,i)
             enddo
          else
             !
             ! Fill linkadmin with info from elemtonode:
             ! If elemtonode(:,i) and elemtonode(:,j) have two nodes in common then elem i is connected to elem j
             ! elemtonode has dimension (4, nelm)
             ! xcc/ycc have dimension mmax
             ! Assumption: mmax <= nelm
             !
             do i=1, nelm
                do j=i+1, nelm
                   commonnodes = 0
                   do ik=1, 4
                      do jk=1, 4
                         if (elemtonode(ik,i) == elemtonode(jk,j)) then
                            commonnodes = commonnodes + 1
                            exit
                         endif
                      enddo
                   enddo
                   if (commonnodes > 2) then
                      write(*,*) "ERROR: more than 2 nodes in common???"
                      call wavestop(1, "ERROR: more than 2 nodes in common???")
                   endif
                   if (commonnodes == 2) then
                      linkadmin(i,5)              = linkadmin(i,5) + 1
                      linkadmin(i,linkadmin(i,5)) = j
                      linkadmin(j,5)              = linkadmin(j,5) + 1
                      linkadmin(j,linkadmin(j,5)) = i
                   endif
                enddo
             enddo
          endif
          !
          ! Use linkadmin to fill array elemconn
          ! If an element is added to elemconn, the related data has to be removed from linkadmin
          !
          do while (.true.)
             ! Search for an elt linked to exactly 2 other elts (and clean the "linked to 1" elts)
             eltlink2 = 0
             do elt=1, mmax
                if (linkadmin(elt,5) == 1) then
                   neighblow        = linkadmin(elt,1)
                   ! remove elt from linkadmin
                   linkadmin(elt,:) = 0
                   ! also update in linkadmin the element where elt is linked to
                   eltlink2found = .false.
                   do i=1,3
                      if (.not.eltlink2found) then
                         eltlink2found = linkadmin(neighblow,i) == elt
                      endif
                      if (eltlink2found) then
                         linkadmin(neighblow,i) = linkadmin(neighblow,i+1)
                      endif
                   enddo
                   linkadmin(neighblow,4) = 0
                   linkadmin(neighblow,5) = linkadmin(neighblow,5) - 1
                endif
                if (linkadmin(elt,5) == 2) then
                   ! element found with exactly 2 links. Exit do loop and handle this point
                   eltlink2 = elt
                   exit
                endif
             enddo
             if (eltlink2 == 0) exit ! finished
             !
             ! Get the 2 neighbouring point indexes
             ! The order is important! Addition to elemconn must be in counter clock wise order
             ! Assumption: The following order is always correct: eltlink2, neighblow, neighneigh, neighbhigh
             !
             if (linkadmin(elt,1) < linkadmin(elt,2)) then
                neighblow = linkadmin(elt,1)
                neighbhigh = linkadmin(elt,2)
             else
                neighblow = linkadmin(elt,2)
                neighbhigh = linkadmin(elt,1)
             endif
             !
             ! Find element (other than eltlink2) being a neighbour of both neighblow and neighbhigh
             neighneigh = 0
             do i=1,4
                if (linkadmin(neighblow,i) == eltlink2) cycle
                do j=1,4
                   if (linkadmin(neighbhigh,j) == eltlink2) cycle
                   if (linkadmin(neighbhigh,j) == linkadmin(neighblow,i)) then
                      neighneigh = linkadmin(neighbhigh,j)
                      exit
                   endif
                enddo
                if (neighneigh /= 0) exit
             enddo
             !
             ! neighneigh == 0: no problem, but no element found to add to elemconn
             !
             if (neighneigh /= 0) then
                maxelem = maxelem + 1
                elemconn(1,maxelem) = eltlink2
                elemconn(2,maxelem) = neighblow
                elemconn(3,maxelem) = neighneigh
                elemconn(4,maxelem) = neighbhigh
             endif
             !
             ! update administration:
             !
             ! element eltlink2:
             linkadmin(eltlink2,:) = 0
             !
             ! element neighblow:
             eltlink2found = .false.
             do i=1,3
                if (.not.eltlink2found) then
                   eltlink2found = linkadmin(neighblow,i) == eltlink2
                endif
                if (eltlink2found) then
                   linkadmin(neighblow,i) = linkadmin(neighblow,i+1)
                endif
             enddo
             linkadmin(neighblow,4) = 0
             linkadmin(neighblow,5) = linkadmin(neighblow,5) - 1
             !
             ! element neighhigh:
             eltlink2found = .false.
             do i=1,3
                if (.not.eltlink2found) then
                   eltlink2found = linkadmin(neighbhigh,i) == eltlink2
                endif
                if (eltlink2found) then
                   linkadmin(neighbhigh,i) = linkadmin(neighbhigh,i+1)
                endif
             enddo
             linkadmin(neighbhigh,4) = 0
             linkadmin(neighbhigh,5) = linkadmin(neighbhigh,5) - 1
             !
             ! element neighneigh does not need to be updated
          enddo
          deallocate(linkadmin, STAT=ierror)
       else
          ! Not a regular grid (containing at least 1 triangle)
          ! Use Delauney algorithm to cover the area spanned by xcc,ycc with triangles
          !
          ! maxelem: is the current allocated size of elemconn, will be changed into the actual size of elemconn
          !
          jatri   = 1 ! yes, create triangles, do not create points
          numedge = 0
          !
          ! When passing through "elemconn(:3,:)", this array is copied to the stack
          ! This will cause a crash on big arrays
          ! So allocate a temporary array
          !
          allocate(elemconntmp(3,maxelem), STAT=ierror)
          !
          ! Indices of the triangles are added to elemconn(:3,:)
          ! Not used: edgeindx, numedge, triedge, xh, yh, nh, trisize
          !
          call tricall(jatri, xcc, ycc, nelm, elemconntmp, maxelem, &
                     & edgeindx, numedge, triedge, xh, yh, nh, trisize)
          !
          ! Turn the triangles in elemconn into quadrilaterals (rectangles):
          ! point4 == point3
          !
          do i = 1, maxelem
             elemconn(1,i) = elemconntmp(1,i)
             elemconn(2,i) = elemconntmp(2,i)
             elemconn(3,i) = elemconntmp(3,i)
             elemconn(4,i) = elemconntmp(3,i)
          enddo
          deallocate(elemconntmp, STAT=ierror)
       endif
    endif
    !
    ! Check that there is at least one element
    !
    if (sferic) then
       if (nelm == 0) then
          write(*,*) "ERROR: nelm=0: Not able to create 'shifted' elements (using centre points as corner points)"
          call wavestop(1, "ERROR: nelm=0")
       endif
    else
       if (maxelem == 0) then
          write(*,*) "ERROR: maxelem=0: Not able to create 'shifted' elements (using centre points as corner points)"
          call wavestop(1, "ERROR: maxelem=0")
       endif
    endif
    !
    ! Write the flow grid to a temporary NetCDF file in SCRIP format. Needed by ESMF_RegridWeightGen
    ! See http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/node3.html
    !
    call getfullversionstring_dwaves(full_version)
    call date_and_time(cdate, ctime, czone)
    !
    ! define name of output file
    !
    write(filename_tmp,'(a,i4.4,a)') "TMP_ESMF_RegridWeightGen_flow_source_", i_grid, ".nc"
    !
    ! create file
    !
    ierror = nf90_create(filename_tmp, 0, idfile); call nc_check_err(ierror, "creating file", filename_tmp)
    !
    ! global attributes
    !
    ierror = nf90_put_att(idfile, nf90_global,  'institution', trim(company)); call nc_check_err(ierror, "put_att global institution", filename_tmp)
    ierror = nf90_put_att(idfile, nf90_global,  'references', trim(company_url)); call nc_check_err(ierror, "put_att global references", filename_tmp)
    ierror = nf90_put_att(idfile, nf90_global,  'source', trim(full_version)); call nc_check_err(ierror, "put_att global source", filename_tmp)
    ierror = nf90_put_att(idfile, nf90_global,  'history', &
           'Created on '//cdate(1:4)//'-'//cdate(5:6)//'-'//cdate(7:8)//'T'//ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//czone(1:5)// &
           ', '//trim(product_name)); call nc_check_err(ierror, "put_att global history", filename_tmp)
    if (.not.sferic) then
       ierror = nf90_put_att(idfile, nf90_global,  'gridType', 'unstructured'); call nc_check_err(ierror, "put_att global gridType", filename_tmp)
       ierror = nf90_put_att(idfile, nf90_global,  'version', '0.9'); call nc_check_err(ierror, "put_att global version", filename_tmp)
    endif
    !
    ! dimensions
    !
    if (sferic) then
       ! SCRIP format
       ierror = nf90_def_dim(idfile, 'grid_size', nelm, iddim_nelm); call nc_check_err(ierror, "def_dim grid_size", filename_tmp)
       ierror = nf90_def_dim(idfile, 'grid_corners', nemax, iddim_corners); call nc_check_err(ierror, "def_dim grid_corners", filename_tmp)
       ierror = nf90_def_dim(idfile, 'grid_rank', 1, iddim_rank); call nc_check_err(ierror, "def_dim grid_rank", filename_tmp)
    else
       ! ESMFgrid format
       ierror = nf90_def_dim(idfile, 'elementCount', maxelem, iddim_nelm); call nc_check_err(ierror, "def_dim elementCount", filename_tmp)
       ierror = nf90_def_dim(idfile, 'nodeCount', mmax, iddim_corners); call nc_check_err(ierror, "def_dim nodeCount", filename_tmp)
       ierror = nf90_def_dim(idfile, 'maxNodePElement', nemaxout, iddim_nemax); call nc_check_err(ierror, "def_dim maxNodePElement", filename_tmp)
       ierror = nf90_def_dim(idfile, 'coordDim', 2, iddim_rank); call nc_check_err(ierror, "def_dim coordDim", filename_tmp)
    endif
    !
    ! define vars
    !
    if (sferic) then
       idvar_griddims = nc_def_var(idfile, 'grid_dims'       , nf90_int   , 1, (/iddim_rank/), '', '', '', .false., filename_tmp)
       idvar_y        = nc_def_var(idfile, 'grid_center_lat' , nf90_double, 1, (/iddim_nelm/), '', '', "degrees", .false., filename_tmp)
       idvar_x        = nc_def_var(idfile, 'grid_center_lon' , nf90_double, 1, (/iddim_nelm/), '', '', "degrees", .false., filename_tmp)
       idvar_nx       = nc_def_var(idfile, 'grid_corner_lon' , nf90_double, 2, (/iddim_corners,iddim_nelm/), '', '', "degrees", .false., filename_tmp)
       ! ierror         = nf90_def_var_fill(idfile, idvar_nx,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename_tmp))
       idvar_ny       = nc_def_var(idfile, 'grid_corner_lat' , nf90_double, 2, (/iddim_corners,iddim_nelm/), '', '', "degrees", .false., filename_tmp)
       ! ierror         = nf90_def_var_fill(idfile, idvar_ny,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename_tmp))
       idvar_mask     = nc_def_var(idfile, 'grid_imask' , nf90_double, 1, (/iddim_nelm/), '', '', '', .false., filename_tmp)
       ! ierror         = nf90_def_var_fill(idfile, idvar_mask,  0, -9999); call nc_check_err(ierror, "put_att _FillValue", trim(filename_tmp))
    else
       idvar_coords   = nc_def_var(idfile, 'nodeCoords'    , nf90_double, 2, (/iddim_rank,iddim_corners/), '', '', "meters", .false., filename_tmp)
       idvar_eConn    = nc_def_var(idfile, 'elementConn'   , nf90_int   , 2, (/iddim_nemax,iddim_nelm/)  , '', 'Node Indices that define the element connectivity', '', .false., filename_tmp)
       ierror         = nf90_put_att(idfile, idvar_eConn,  '_FillValue', -1); call nc_check_err(ierror, "put_att elementConn fillVal", filename_tmp)
       idvar_neConn   = nc_def_var(idfile, 'numElementConn', nf90_int   , 1, (/iddim_nelm/)              , '', 'Number of nodes per element', '', .false., filename_tmp)
    endif
    !
    ierror = nf90_enddef(idfile); call nc_check_err(ierror, "enddef", filename_tmp)
    !
    ! put vars
    !
    if (sferic) then
       !
       ! ESMF sferic:
       ! Write xcc,ycc to source.nc
       !
       ierror = nf90_put_var(idfile, idvar_griddims, (/nelm/), start=(/1/), count=(/1/))   ; call nc_check_err(ierror, "put_var griddims", filename_tmp)
       ! An array slice cannot be passed to netcdf C-library (risk of stack overflow), so use placeholder.
       nelmslice = xcc(:nelm,1)
       ierror    = nf90_put_var(idfile, idvar_x       , nelmslice, start=(/1/), count=(/nelm/)); call nc_check_err(ierror, "put_var x", filename_tmp)
       nelmslice = ycc(:nelm,1)
       ierror    = nf90_put_var(idfile, idvar_y       , nelmslice, start=(/1/), count=(/nelm/)); call nc_check_err(ierror, "put_var y", filename_tmp)
       do i=1, nemax
          do j=1, nelm
             if (i>3 .and. elemtonode(i,j)<0) then
                ! Cells with different number of corners (e.g triangles and rectangles)
                ! Add redundant grid points
                grid_corner(i,j) = grid_corner(i-1,j)
             else
                grid_corner(i,j) = xnode(elemtonode(i,j))
             endif
          enddo
       enddo
       ierror = nf90_put_var(idfile, idvar_nx  , grid_corner  , start=(/1,1/), count=(/nemax,nelm/)); call nc_check_err(ierror, "put_var corner_lon", filename_tmp)
       do i=1, nemax
          do j=1, nelm
             if (i>3 .and. elemtonode(i,j)<0) then
                ! Cells with different number of corners (e.g triangles and rectangles)
                ! Add redundant grid points
                grid_corner(i,j) = grid_corner(i-1,j)
             else
                grid_corner(i,j) = ynode(elemtonode(i,j))
             endif
          enddo
       enddo
       ierror    = nf90_put_var(idfile, idvar_ny  , grid_corner  , start=(/1,1/), count=(/nemax,nelm/)); call nc_check_err(ierror, "put_var corner_lat", filename_tmp)
       mask_area = 1.0_hp
       ierror    = nf90_put_var(idfile, idvar_mask, mask_area  , start=(/1/), count=(/nelm/)); call nc_check_err(ierror, "put_var imask", filename_tmp)
    else
       !
       ! ESMF Cartesian:
       ! Write hexahedron(cube) around xcc,ycc to source.nc
       !
       do i=1, mmax
          grid_corner(1,i     ) = xcc(i,1)
          grid_corner(2,i     ) = ycc(i,1)
       enddo
       ierror = nf90_put_var(idfile, idvar_coords, grid_corner, start=(/1,1/), count=(/2,mmax/));   call nc_check_err(ierror, "put_var nodeCoords", filename_tmp)
       !       
       ierror = nf90_put_var(idfile, idvar_eConn , elemconn , start=(/1,1/), count=(/nemaxout,maxelem/)); call nc_check_err(ierror, "put_var elementConn", filename_tmp)
       !
       do i=1, maxelem
          nelemconn(i) = nemaxout
       enddo
       ierror = nf90_put_var(idfile, idvar_neConn , nelemconn , start=(/1,1/), count=(/maxelem/)); call nc_check_err(ierror, "put_var numElementConn", filename_tmp)
    endif
    ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename_tmp)
    !
    deallocate (xnode      , STAT=ierror)
    deallocate (ynode      , STAT=ierror)
    deallocate (elemtonode , STAT=ierror)
    deallocate (grid_corner, STAT=ierror)
    deallocate (nelmslice  , STAT=ierror)
    if (sferic) then
       deallocate (mask_area  , STAT=ierror)
    else
       deallocate ( elemconn, STAT=ierror)
       deallocate (nelemconn, STAT=ierror)
       deallocate (flowlink, STAT=ierror)
       deallocate (EDGEINDX , STAT=ierror)
       deallocate (TRIEDGE  , STAT=ierror)
    endif
end subroutine read_netcdf_grd
!
!
!==============================================================================
subroutine readregulargrid(filnam, sferic_exp, xorigin, yorigin, alpha, &
                          & mmax, nmax, dx, dy)
    use geometry_module, only: clockwise
    implicit none
!
! Global variables
!
    character(256), intent(in)                        :: filnam
    logical, intent(in)                               :: sferic_exp
    integer, intent(out)                              :: mmax
    integer, intent(out)                              :: nmax
    real   , intent(out)                              :: xorigin
    real   , intent(out)                              :: yorigin
    real   , intent(out)                              :: alpha
    real   , intent(out)                              :: dx
    real   , intent(out)                              :: dy
!
! Local variables
!
    integer                        :: i
    integer                        :: ierr
    integer                        :: irgf
    integer                        :: j
    integer                        :: k
    integer                        :: pos
    real                           :: dxx
    real                           :: dxy
    real                           :: dyx
    real                           :: dyy
    real                           :: pi
    real                           :: x1
    real                           :: x2
    real, dimension(4)             :: xcell
    real                           :: y1
    real                           :: y2
    real, dimension(4)             :: ycell
    real, dimension(:,:,:),allocatable :: xy
    logical                        :: kw_found
    logical                        :: sferic_read
    character(10)                  :: dum
    character(256)                 :: rec
!
!! executable statements -------------------------------------------------------
!
    sferic_read = .false.
    open (newunit=irgf, file = filnam, form = 'formatted', status = 'old')
    !
    ! Copied from file rdrgf
    !
    ! Read file, check for end of file or error in file:
    ! - The first line always contains comments
    !   sferic is true when the first line contains the keyword Spherical
    ! - Skip comment lines (starting with a '*'), while trying to read the
    !   following keywords: 'Coordinate System'
    !                       'Missing Value'
    !   If 'Coordinate System' is present, it overrules the sferic-specification
    !   in the first line!
    ! - The next line contains the dimensions mc and nc
    !   Parameter npart may also be on this line, but it is neglected
    ! - Read the next line containing three zero's
    !   xori, yori and alfori are not used anymore
    ! - Read x coordinates
    ! - Read y coordinates
    !
    read (irgf, '(a)', end = 7777, err = 8888) rec
    if (index(rec, 'Spherical')>=1 .or. index(rec, 'SPHERICAL')>=1) then
       sferic_read = .true.
    endif
10  continue
       kw_found = .false.
       read(irgf,'(a)',end = 7777,err=8888) rec
       if (rec(1:1) == '*') goto 10
       !
       pos = index(rec,'Coordinate System')
       if (pos >= 1) then
          kw_found = .true.
          if (index(rec(pos+1:),'spherical') >= 1 .or. &
            & index(rec(pos+1:),'Spherical') >= 1 .or. &
            & index(rec(pos+1:),'SPHERICAL') >= 1       ) then
             sferic_read = .true.
          else
             sferic_read = .false.
          endif
       endif
       !
       pos = index(rec,'Missing Value')
       if (pos >= 1) then
          kw_found = .true.
       endif
    if (kw_found) goto 10
    !
    if (      sferic_read .and. .not. sferic_exp) then
       write (*, '(3a)') '*** ERROR: file ',trim(filnam),' contains Spherical coordinates'
       write (*, '(11x,3a)') 'Expecting Cartesian coordinates'
       close (irgf)
       call wavestop(1, 'Spherical coordinates found while expecting Cartesian coordinates in file'//trim(filnam))
    endif
    if (.not. sferic_read .and.       sferic_exp) then
       write (*, '(3a)') '*** ERROR: file ',trim(filnam),' contains Cartesian coordinates'
       write (*, '(11x,3a)') 'Expecting Spherical coordinates'
       close (irgf)
       call wavestop(1, 'Cartesian coordinates found while expecting Spherical coordinates in file'//trim(filnam))
    endif
    read(rec,*,err=8888)  mmax, nmax
    !
    ! poles? no, fences!
    !
    allocate (xy (2,mmax,nmax))
    xy   = 0.
    !
    ! read three zero's
    !
    read(irgf,'(a)',end = 7777,err=8888) rec
    !
    ! read X and Y coordinates
    ! read unformatted: The number of digits of xcor may vary
    !
    do k = 1, 2
       do j = 1, nmax
          read (irgf, *, end = 7777, err = 8888) dum,dum,(xy(k, i, j),i=1, mmax)
       enddo
    enddo
    close (irgf)
    goto 9999
    !
    ! test for reading error : label 7777 end of file
    !                                8888 error while reading
    !
 7777 continue
 8888 continue
    goto 999
    !
9999 continue
    !
    xcell(1) = xy(1,1,1)
    xcell(2) = xy(1,2,1)
    xcell(3) = xy(1,2,2)
    xcell(4) = xy(1,1,2)
    ycell(1) = xy(2,1,1)
    ycell(2) = xy(2,2,1)
    ycell(3) = xy(2,2,2)
    ycell(4) = xy(2,1,2)
    if (clockwise(xcell,ycell)) then
       write (*, '(a)') '*** ERROR: Grid orientation incorrect: Delft3D requires (M,N) to form a counter-clockwise coordinate system.'
       goto 999
    endif
    !
    pi   = 4.*atan(1.)
    alpha = atan2(xy(2,mmax,1)-xy(2,1,1),xy(1,mmax,1)-xy(1,1,1))*180./pi
    xorigin   = xy(1,1,1)
    yorigin   = xy(2,1,1)
    x1   = xy(1,mmax,1)
    y1   = xy(2,mmax,1)
    !
    dxx  = (x1 - xorigin)/(mmax - 1)
    dyx  = (y1 - yorigin)/(mmax - 1)
    dx   = sqrt(dxx * dxx + dyx * dyx)
    !
    x2   = xy(1,mmax,nmax)
    y2   = xy(2,mmax,nmax)
    dxy  = (x2 - x1)/(nmax - 1)
    dyy  = (y2 - y1)/(nmax - 1)
    dy   = sqrt(dyy * dyy + dxy * dxy)
    !
    goto 1000
  999 continue
    write(*,'(2a)') '*** ERROR: reading GRD file ', trim(filnam)
    deallocate (xy, stat=ierr)
    call wavestop(1, '*** ERROR: reading GRD file '//trim(filnam))
 1000 continue
    deallocate (xy, stat=ierr)
end subroutine readregulargrid
!
!
!==============================================================================
subroutine write_swan_grid (x,y,mmax,nmax,inest,fname)
   implicit none
   !
   integer                       , intent(in)  :: mmax
   integer                       , intent(in)  :: nmax
   real(hp), dimension(mmax,nmax), intent(in)  :: x
   real(hp), dimension(mmax,nmax), intent(in)  :: y
   character(37)                               :: fname
   integer                       , intent(in)  :: inest
   !
   integer                        :: m
   integer                        :: n
   integer                        :: ugrd
   !
   fname       = ' '
   fname(1:13) = 'TMP_grid2swan'
   write (fname(14:15),'(I2.2)') inest
   open  (newunit = ugrd, file = fname(1:15), form ='formatted')
   write (ugrd,'(A)') 'x-coordinates'
   do n = 1,nmax
      write (ugrd,'(6(E25.17,1X))') (x(m,n),m=1,mmax)
   enddo
   write (ugrd,'(A)') 'y-coordinates'
   do n = 1,nmax
      write (ugrd,'(6(E25.17,1X))') (y(m,n),m=1,mmax)
   enddo
   close (ugrd)
end subroutine write_swan_grid
end module read_grids

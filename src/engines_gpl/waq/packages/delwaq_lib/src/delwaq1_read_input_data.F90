!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.


!>\file
!>                    delwaq1_read_user_data
!     SUBROUTINES CALLED :
!                          DLWQ01, reads block 1 of user data
!                          DLWQ02, reads block 2 of user data
!                          DLWQ03, reads block 3 of user data
!                          DLWQ04, reads block 4 of user data
!                          DLWQ05, reads block 5 of user data
!                          DLWQ06, reads block 6 of user data
!                          DLWQ07, reads block 7 of user data
!                          DLWQ08, reads block 8 of user data
!                          DLWQ09, reads block 9 of user data
!                          DLWQS1, reads block 10 , statistical definition
!                          DLWQP1, proces pre-processor
!                          SPACE , computes space needed

subroutine delwaq1_read_input_data()
    use m_delwaq1_data

    implicit none


    cchar   = ' '
    ilun    = 0
    ilun(1) = lun  (26)
    lch (1) = lchar(26)
    lunut   = lun(29)
    call dlwq01 ( lun     , psynam  , nosys   , notot   , nomult  , &
                  multp   , iwidth  , otime   , isfact  , refday  , &
                  vrsion  , ioutpt  , ierr    , iwar    )

    if ( ierr .ne. 0 ) then
        write ( lunrep , '(A)') " ERROR: reading system names"
        ierr = ierr + 1
        return
    endif
    allocate(syname(notot+nomult),stat=ierr_alloc)
    allocate(imultp( 2 ,  nomult),stat=ierr_alloc)
    if ( ierr_alloc .ne. 0 ) then
        write ( lunrep , '(A,I6)') " ERROR: allocating memory for system names:",ierr_alloc
        ierr = ierr + 1
        return
    endif
    syname = psynam
    imultp = multp
    if ( associated(psynam) ) deallocate(psynam)
    if ( associated(multp) )  deallocate(multp )
    deltim = otime
    car(1) = ' '
    k = 2
    icmak = cmax   - 1

    nullify(nsegdmp)
    nullify(isegdmp)
    nullify(nexcraai)
    nullify(iexcraai)
    nullify(ioptraai)
    call dlwq02 ( lun     , lchar   , filtype , nrftot  , nlines  , &
                  npoins  , dtflg1  , dtflg2  , nodump  , iopt    , &
                  noint   , iwidth  , dtflg3  , ndmpar  , ntdmps  , &
                  noraai  , ntraaq  , nosys   , notot   , nototp  , &
                  vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai, &
                  iexcraai, ioptraai, ierr    , iwar    )

    if ( mod(intopt,16) .gt. 7 ) then
        ibflag = 1
    else
        ibflag = 0
    endif

    call dlwq03 ( lun     , lchar   , filtype , nrftot  , nrharm  , &
                  ivflag  , dtflg1  , iwidth  , dtflg3  , vrsion  , &
                  ioutpt  , gridps  , syname  , ierr    , iwar    , &
                  has_hydfile       , nexch   )


    if ( .not. associated(nsegdmp)  ) allocate(nsegdmp(1))
    if ( .not. associated(isegdmp)  ) allocate(isegdmp(1))
    if ( .not. associated(nexcraai) ) allocate(nexcraai(1))
    if ( .not. associated(iexcraai) ) allocate(iexcraai(1))
    if ( .not. associated(ioptraai) ) allocate(ioptraai(1))
    call dlwq04 ( lun     , lchar   , filtype , nrftot  , nrharm  , &
                  ilflag  , dtflg1  , iwidth  , intsrt  , dtflg3  , &
                  vrsion  , ioutpt  , nsegdmp , isegdmp , nexcraai, &
                  iexcraai, ioptraai, gridps  , ierr    , iwar    , &
                  has_hydfile       , nexch   )
    if ( associated(nsegdmp)  ) deallocate(nsegdmp)
    if ( associated(isegdmp)  ) deallocate(isegdmp)
    if ( associated(nexcraai) ) deallocate(nexcraai)
    if ( associated(iexcraai) ) deallocate(iexcraai)
    if ( associated(ioptraai) ) deallocate(ioptraai)

    deltim = otime
    call dlwq05 ( lun    , lchar  , filtype, car(k) , iar    , &
                  rar    , nrftot , nrharm , nobnd  , nosys  , &
                  notot  , nobtyp , rmax   , imax   , dtflg1 , &
                  iwidth , intsrt , ierr   , dtflg3 , syname , &
                  icmak  , vrsion , ioutpt , iwar   )

    deltim = otime

    nosss = noseg + nseg2     ! increase with bottom segments
    call dlwq06 ( lun    , lchar  , filtype, icmak  , car(k) , &
                  imax   , iar    , rmax   , rar    , notot  , &
                  nosss  , syname , nowst  , nowtyp , nrftot , &
                  nrharm , dtflg1 , dtflg3 , iwidth , vrsion , &
                  ioutpt , chkpar , ierr   , iwar   )

    novec = 50
    inpfil%dtflg1 = dtflg1
    inpfil%dtflg2 = dtflg2
    inpfil%dtflg3 = dtflg3
    inpfil%itfact = itfact
    inpfil%vrsion = vrsion

    nrharm(10) = 0
    deltim     = otime
    call dlwq07 ( lun    , lchar  , filtype, inpfil   , syname , &
                  iwidth , ioutpt , gridps , constants, chkpar , &
                  ierr   , iwar   )

    !
    !     Finish and close system file ( DLWQ09 can re-read it )
    !
    write ( lun(2) ) ( nrftot(i) , i = 1,noitem )
    write ( lun(2) ) ( nrharm(i) , i = 1,noitem )
    close ( lun(2) )

    call dlwq08 ( lun    , lchar  , filtype, nosss  , notot  , &
                  syname , iwidth , vrsion , ioutpt , inpfil , &
                  gridps , ierr   , iwar   )

    call dlwq09 ( lun    , lchar  , filtype, car    , iar    , &
                  icmak  , iimax  , iwidth , ibflag , vrsion , &
                  ioutpt , ioutps , outputs, ierr   , iwar   )

    call dlwqs1 ( lunrep       , npos         , &
                  cchar        , vrsion       , &
                  ilun         , lch          , &
                  lstack       , ioutpt       , &
                  dtflg1       , dtflg3       , &
                  statprocesdef, allitems     , &
                  noinfo       , iwar         , &
                  ierr         )
    write ( lunrep,'(//'' Messages presented in this .lst file:'')')
    write ( lunrep,'( /'' Number of WARNINGS            :'',I6)') iwar
    write ( lunrep,'(  '' Number of ERRORS during input :'',I6)') ierr
    write ( lunrep,'(  '' '')')

    call dlwqp1 ( lun          , lchar        , &
                  statprocesdef, allitems     , &
                  ioutps       , outputs      , &
                  nomult       , imultp       , &
                  constants    , noinfo       , &
                  refday       ,  &
                  iwar         , ierr         )

    deallocate(syname)
    deallocate(imultp)


end subroutine delwaq1_read_input_data

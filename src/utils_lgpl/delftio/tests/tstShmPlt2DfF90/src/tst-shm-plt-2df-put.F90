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
program putDatasets

    ! modules

    use Dio_Shm_Tst

    ! PLT datasets and their data

    type(DioPltType) , dimension(nPltSets) :: pltSet
    type(dsData)     , dimension(nPltSets), target :: pltData


    ! 2DField datasets and their data

    type(Dio2DFType) , dimension(n2DFSets) :: f2dSet
    type(dsData)     , dimension(n2DFSets), target :: f2dData

    ! time / dataset loops

    integer               :: t     ! time step counter

    integer               :: plt   ! PLT dataset counter
    integer               :: f2d   ! 2DField dataset counter

    type(dsData), pointer :: dsDat ! pointer to current dataset in loop

    ! temporarily store for values that will be disturbed before puttin

    real             :: rTempVal
    double precision :: dTempVal
    integer          :: iTempVal

!
!   Create Data to be put
!

    call DioInit('dioconfig_shm.ini')

    ! #pars/locs in PLT sets, #M/N in 2DField datasets, var. type

    call InitPLTData(pltData)
    call Init2DFData(f2dData)
    
    ! Initialise values

    do plt = 1, nPltsets
        call initValues(pltData(plt))
    enddo
    do f2d = 1, n2DFSets
        call initValues(f2dData(f2d))
    enddo

!
!   Create PLT datasets, disturb par./loc. names
!
    do plt = 1, nPltSets
        dsDat => pltData(plt)
        if (plt .eq. 2 ) dsDat % pars(1) = 'WRONG PAR'
        if (plt .eq. 3 ) dsDat % locs(1) = 'WRONG LOC'
        write(*,*) 'Creating OUT PLT dataset ', dsDat % Name
        pltSet(plt) = DioPltDefine(dsDat % Name, dsDat % varType, &
                        dsDat % pars, dsDat % locs)
    enddo

!
!   Create 2DF datasets
!
    do f2d = 1, n2DFSets
        dsDat => f2dData(f2d)
        write(*,*) 'Creating OUT 2DF dataset ', dsDat % Name
        f2dSet(f2d) = Dio2DFDefine(dsDat % Name, dsDat % varType,  &
                                    dsDat % numM, dsDat % numN )
    enddo

    write (*, *) 'OUT Datasets Created'

!   Put data for each timestop

    do t = 1, NTIMES

        ! Disturb some of the PLT values before putting them,
        ! Put PLT dataset values.

        do plt = 1, nPltSets
            dsDat => pltData(plt)
            call incrementValues(dsDat)

            select case ( dsDat % varType )

                case (Dio_Plt_Real)

                    if (t .eq. 7 ) then
                        rTempVal = dsDat % rValues(1,1)
                        dsDat % rValues(1,1) = 0.0
                    endif

                    call DioPltPut(pltSet(plt), dsDat % rValues)

                    if (t .eq. 7 ) dsDat % rValues(1,1) = rTempVal

                case (Dio_Plt_Double)

                    if (t .eq. NTIMES ) then
                        dTempVal = dsDat % dValues(1,1)
                        dsDat % dValues(1,1) = 0.0D+00
                    endif

                    call DioPltPut(pltSet(plt), dsDat % dValues)

                    if (t .eq. NTIMES ) dsDat % dValues(1,1) = dTempVal

                case (Dio_Plt_Integer)

                    if (t .eq. 12) then
                        iTempVal = dsDat % iValues(1,1)
                        dsDat % iValues(1,1) = 0
                    endif

                    call DioPltPut(pltSet(plt), dsDat % iValues)

                    if (t .eq. 12) dsDat % iValues(1,1) = iTempVal

            end select

        enddo

        ! Disturb some of the 2DF values before putting them,
        ! Put 2DF dataset values.

        do f2d = 1, n2DFSets
            dsDat => f2dData(f2d)
            call incrementValues(dsDat)

            select case ( dsDat % varType )
                case (Dio_Plt_Real)

                    if (t .eq. 11 ) then
                        rTempVal = dsDat % rValues(1,1)
                        dsDat % rValues(1,1) = 0.0
                    endif

                    call Dio2DFPut(f2dSet(f2d), dsDat % rValues)

                    if (t .eq. 11 ) dsDat % rValues(1,1) = rTempVal

                case (Dio_Plt_Double)

                    if (t .eq. NTIMES ) then
                        dTempVal = dsDat % dValues(1,1)
                        dsDat % dValues(1,1) = 0.0D+00
                    endif

                    call Dio2DFPut(f2dSet(f2d), dsDat % dValues)

                    if (t .eq. NTIMES ) dsDat % dValues(1,1) = dTempVal

                case (Dio_Plt_Integer)

                    if (t .eq. 9) then
                        iTempVal = dsDat % iValues(1,1)
                        dsDat % iValues(1,1) = 0
                    endif

                    call Dio2DFPut(f2dSet(f2d), dsDat % iValues)

                    if (t .eq. 9) dsDat % iValues(1,1) = iTempVal

            end select

        enddo

        if (mod(t,100).eq.0) write (*, *) 'Data has been put for Step: ', t

    enddo

!   cleanup

    do plt = 1, nPltSets
        call DioPltDestroy(pltSet(plt))
        call DestroyData(pltData(plt))
        write (*, *) 'Have destroyed OUT PLT dataset', trim(pltData(plt) % name)
    enddo

    do f2d = 1, n2DFSets
        call Dio2DFDestroy(f2dSet(f2d))
        call DestroyData(f2dData(f2d))
        write (*, *) 'Have destroyed OUT 2DF dataset', trim(f2dData(plt) % name)
    enddo
end


subroutine prterr(lundia, msgno, filtxt)
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
!    Function: This routine prints a message to output device.
!              Errfil is no longer needed.
! Method used:
!
! Empty Message : G051
! Empty Warning : U190 and Z013
! Empty Error   : P004 and U021
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    use string_module
    !
    implicit none
    !
!
! Global variables
!
    integer     , intent(in)  :: lundia !  Description and declaration in inout.igs
    character(*), intent(in)  :: filtxt !!  String to insert into error message
    character(4), intent(in)  :: msgno  !!  Message nr. to look for in 'errfil'
!
! Local variables
!
    integer        :: ilen       ! Help var. containing the actual length of filtxt 
    integer        :: ip         ! Help var. 
    integer        :: msglen     ! Help var.; length of MSG 
    integer        :: oldlen     ! Help var.; length of MSGOLD 
    character(300) :: filtxtcopy ! copy of FILTXT (needed in remove_leading_spaces call) 
    character(300) :: msg        ! Message output to LUNOUT 300 = 256 + a bit (field, =, ##, etc.) 
    character(300) :: msgold     ! Help var. to store MSG 300 = 256 + a bit (field, =, ##, etc.) 
!
!! executable statements -------------------------------------------------------
!
    ! initialization
    !
    filtxtcopy = ' '
    !
    ! error messages in User Interface (lundia = 0)
    !
    if (lundia==0) goto 9999
    messagenumber:select case (msgno)
    case ('D001')
       msg = '*** ERROR Conflict of run-time parameters. Contact Deltares'
    case ('D002')
       msg = '*** ERROR DT * Tunit not an integer multiple of Tscale'
    case ('D003')
       msg = '*** ERROR Incorrect time frame found after ' //                   &
            & 'skipping the initialisation part'
    case ('D004')
       msg = '*** ERROR Unable to run a cyclical simulation in' //              &
            & ' this standalone run'
    case ('D005')
       msg = '*** WARNING Cyclical simulation but COM-file writing ' //         &
            & 'interval equal to zero'
    case ('D006')
       msg = '*** ERROR FLOW simulation date incompatible with' //              &
            & ' the Delft3D input !!'
    case ('D007')
       msg = '*** ERROR Waves activated but no COM-file present !!'
    case ('F001')
       msg = '*** ERROR Variable # not known'
    case ('F002')
       msg = '*** ERROR Temperature variable not available in ' //              &
            & 'the simulation'
    case ('F003')
       msg = '*** ERROR Salinity variable not available in the' // ' simulation'
    case ('F004')
       msg = '*** ERROR Constituent number # does not exist'
    case ('F005')
       msg = '*** ERROR Start time for Fourier analysis lies ' //               &
            & 'outside simulation time frame'
    case ('F006')
       msg = '*** ERROR Stop time for Fourier analysis lies ' //                &
            & 'outside simulation time frame'
    case ('F007')
       msg = '*** ERROR Incorrect layer number specified'
    case ('F008')
       msg = '*** WARNING Number of cycles <> 0 for function "#"'
    case ('G003')
       msg = '*** ERROR Start current program using the ' //                    &
            & 'Delft3D-FLOW shell'
    case ('G004')
       msg = '*** ERROR File: # not found'
    case ('G005')
       msg = '*** ERROR Model does not fit in memory. Contact Deltares'
    case ('G006')
       msg = '*** ERROR Premature EOF in file: #'
    case ('G007')
       msg = '*** ERROR Read error from file: #'
    case ('G008')
       msg = '*** ERROR License file: not found/invalid/' //                    &
            & 'corrupt. Contact Deltares'
    case ('G015')
       msg = '*** ERROR Number of open boundaries too large. ' //               &
            & 'Contact Deltares'
    case ('G016')
       msg = '*** ERROR Number of layers too large. Contact Deltares'
    case ('G020')
       msg = '*** ERROR Not enough memory to define # array. ' //               &
            & 'Contact Deltares'
    case ('G021')
       msg = '*** ERROR Pointer for # array of wrong type. Contact Deltares'
    case ('G022')
       msg = '*** ERROR Pointer name of # array too long. Contact Deltares'
    case ('G023')
       msg = '*** ERROR Inconsistent length requested for array #.'
    case ('G029')
       msg = '*** ERROR Use of "#" add-on unauthorised. ' //                    &
            & 'Contact Deltares'
    case ('G033')
       msg = '*** WARNING Skipping empty line in file #'
    case ('G040')
       msg = '*** WARNING FLOW-WAQ interface needs at least ' //                &
            & '2 times in COM-file'
    case ('G050')
       msg = '*** ERROR Number of discharge locations too ' //                  &
            & 'large. Contact Deltares'
    case ('G051')
       msg = '*** MESSAGE #'
    case ('G106')
       msg = '*** ERROR Old, no longer supported, version of the input MDF-file !!'
    case ('G107')
       msg = '*** WARNING No version of the input MDF-file ' //                  &
            & 'found, version 3.00 presumed'
    case ('G907')
       msg = '*** ERROR RUNID for "#" FLOW-file too large'
    case ('G920')
       msg = '*** ERROR Unexpected error from FMM (#). Contact Deltares'
    case ('J001')
       msg = '*** ERROR Trachytopes: #'
    case ('J004')
       msg = '*** ERROR Trachytopes: Number of trachytope ' //                  &
           & 'definitions in file > array size.'
    case ('J005')
       msg = '*** ERROR Trachytopes: Unknown roughness code in  file #'
    case ('J006')
       msg = '*** ERROR Trachytopes: Number of area entries ' //                &
           & 'in file # > array size.'
    case ('J007')
       msg = '*** ERROR Trachytopes: Out of array size while ' //               &
           & 'expanding trachytope data.'
    case ('J008')
       msg = '*** ERROR Trachytopes: Trachytope number <= 0: #'
    case ('J009')
       msg = '*** ERROR Trachytopes: Trachytope roughness description <= 0: #'
    case ('J010')
       msg = '*** ERROR Trachytopes: Too many NM-Errors in #-direction.'
    case ('J011')
       msg = '*** ERROR Trachytopes: Too many definition errors in #-direction.'
    case ('J012')
       msg = '*** ERROR Trachytopes: Trachytope number # defined more than once.'
    case ('J013')
       msg = '*** ERROR Trachytopes: Negative value(s) ' //                     &
           & 'roughness calibration in #-direction.'
    case ('J014')
       msg = '*** ERROR Trachytopes: Wrong number of parameters defined, File: #'
    case ('J015')
       msg = '*** WARNING Trachytopes: Struiksma, ' //                          &
           & 'Theta-grain <= 0 for cell (#); set to 0.001'
    case ('J016')
       msg = '*** WARNING Trachytopes: Struiksma, ' //                          &
           & 'Theta-grain >= 1 for cell (#); set to 0.999'
    case ('J020')
       msg = '*** ERROR In synchronisation with RTC Routine: #.'
    case ('P004')
       msg = '*** ERROR #'
    case ('P101')
       msg = '*** ERROR The specified file # does not exist'
    case ('S014')
       msg = '*** WARNING The file # for restart already exists, written over'
    case ('S027')
       msg = '*** ERROR  Conflict with existing COM-file (version #). Contact Deltares'
    case ('S044')
       msg = '*** WARNING # is not multiple of the simulation time step'
    case ('S100')
       msg = '*** WARNING concentration in # discharge < 0. Set to zero.'
    case ('S101')
       msg = '*** WARNING discharge #; cannot drain at cell with volume zero.'
    case ('S102')
       msg = '*** WARNING discharge #; cannot extract from dry cell.'
    case ('S103')
       msg = '*** WARNING discharge intake point # is dry.'
    case ('S200')
       msg = '*** ERROR Non existing BC at begin row, ROW: #'
    case ('S201')
       msg = '*** ERROR Non existing BC at end   row, ROW: #'
    case ('S205')
       msg = '*** WARNING No convergence in UZD at tstep: #'
    case ('S206')
       msg = '*** WARNING No convergence in DIFU for resp. constituent nr. & tstep: #'
    case ('S208')
       msg = '*** WARNING Sinks set to zero at tstep and (dry) discharge point nr. #'
    case ('S220')
       msg = '*** ERROR Tide Generating Forces file not found. Re-run program from the shell'
    case ('U001')
       msg = '*** ERROR Invalid selection / Select a lower level item.'
    case ('U002')
       msg = '*** ERROR Invalid format'
    case ('U003')
       msg = '*** ERROR Undefined grid dimensions - goto domain/grid/dimension menu'
    case ('U004')
       msg = '*** ERROR Layer thickness is equal to or less than zero for k = #'
    case ('U005')
       msg = '*** WARNING Iteration parameter is set to #'
    case ('U006')
       msg = '*** ERROR Sum of layer thickness must equal 100 %'
    case ('U007')
       msg = '*** ERROR Invalid #.'
    case ('U008')
       msg = '*** ERROR You have to enter a value'
    case ('U009')
       msg = '*** ERROR Invalid co-ordinate'
    case ('U010')
       msg = '*** ERROR Only type "U" or "V" allowed for thin dams'
    case ('U011')
       msg = '*** ERROR Only data type "H", "T" or "A" allowed'
    case ('U012')
       msg = '*** ERROR Start time must be less than or equal to the stop time'
    case ('U013')
       msg = '*** ERROR Consecutive times must increase'
    case ('U014')
       msg = '*** ERROR Direction of lines must be a multiple of 45 degrees'
    case ('U015')
       msg = '*** ERROR Error while opening file #'
    case ('U016')
       msg = '*** ERROR Error while writing file #'
    case ('U017')
       msg = '*** WARNING Enter at least one name'
    case ('U018')
       msg = '*** WARNING The product of the entered values may not exceed #'
    case ('U019')
       msg = '*** ERROR Time interval must be a multiple of #'
    case ('U020')
       msg = '*** ERROR Either x or y co-ordinates must be identical'
    case ('U021')
       msg = '*** ERROR #'
    case ('U022')
       msg = '*** ERROR Invalid values found. Try appropriate input or hit F1 for help'
    case ('U023')
       msg = '*** ERROR No time steps provided; choose times option first'
    case ('U024')
       msg = '*** ERROR Sources or sinks missing - goto domain/special/discharge menu'
    case ('U025')
       msg = '*** ERROR This form consists of # pages'
    case ('U026')
       msg = '*** WARNING File # already exists; overwrite ?'
    case ('U027')
       msg = '*** ERROR Time must be later than or equal to #'
    case ('U029')
       msg = '*** ERROR Lines formed by the co-ordinates must be multiples of 45 degrees'
    case ('U030')
       msg = '*** ERROR Error in program # or program not found'
    case ('U031')
       msg = '*** ERROR Enter Yes or No !'
    case ('U032')
       msg = '*** ERROR # is not yet implemented'
    case ('U033')
       msg = '*** ERROR # have not been selected'
    case ('U034')
       msg = '*** ERROR Open Boundary not of the type: ' //                     &
            & '# - see boundary definition'
    case ('U035')
       msg = '*** ERROR I/O ERROR while reading input file'
    case ('U036')
       msg = '*** ERROR # - insufficient number of values or record is corrupt'
    case ('U037')
       msg = '*** ERROR # - Insufficient number of records'
    case ('U038')
       msg = '*** ERROR Record # not found'
    case ('U039')
       msg = '*** ERROR Boundary type H(armonic)/A(stronomical ' //             &
            & 'tide) must precede type T(ime series)'
    case ('U040')
       msg = '*** ERROR Time step must be multiple of #'
    case ('U041')
       msg = '*** ERROR # Simulation start time'
    case ('U042')
       msg = '*** ERROR # Simulation stop time'
    case ('U043')
       msg = '*** ERROR # Simulation period'
    case ('U044')
       msg = '*** ERROR # is/are not multiple of the simulation time step'
    case ('U045')
       msg = '*** ERROR # is not multiple of the time interval'
    case ('U046')
       msg = '*** WARNING File will not be created - #'
    case ('U047')
       msg = '*** ERROR Only boundary type Z, C, Q, R, T or N are allowed'
    case ('U048')
       msg = '*** ERROR 45 degree open boundary only allowed ' //               &
            & 'for water elevation type'
    case ('U049')
       msg = '*** ERROR Frequency component # deg/hr has a negative amplitude'
    case ('U050')
       msg = '*** ERROR 3D application and # not allowed !!'
    case ('U051')
       msg = '*** ERROR Invalid Heat module option (only 0 to 5 allowed)'
    case ('U052')
       msg = '*** ERROR # unspecified. Reloading of file may cause error !!'
    case ('U053')
       msg = '*** WARNING # does not exist !!'
    case ('U054')
       msg = '*** ERROR 3D turbulence model only available ' //                 &
            & 'if nr. of layers > 1'
    case ('U055')
       msg = '*** ERROR Return time at # must be multiple of integration period'
    case ('U056')
       msg = '*** WARNING Return time at # is > simulation stop time'
    case ('U057')
       msg = '*** ERROR Return time at # must be >= 0.'
    case ('U059')
       msg = '*** ERROR Blank label/identifier not accepted'
    case ('U060')
       msg = '*** ERROR Consecutive times in # must increase'
    case ('U061')
       msg = '*** ERROR Invalid Heat module parameters CD/Stanton/Dalton option (only 0 or 1 allowed)'
    case ('U062')
       msg = '*** ERROR Consecutive times on file # should increase'
    case ('U063')
       msg = '*** ERROR Vertical profiles only available if ' //                &
            & 'the number of layers is > 1 !'
    case ('U064')
       msg = '*** ERROR Vertical profiles only available if ' //                &
            & 'boundary type = Q,R or C'
    case ('U065')
       msg = '*** ERROR Open Boundary type A(stronomic) may ' //                &
            & 'not be mixed with H(armonic) type'
    case ('U066')
       msg = '*** WARNING Vertical profile name # not allowed; reset to Uniform'
    case ('U067')
       msg = '*** ERROR Tide Generating Forces only in ' //                     &
            & 'combination with Spherical co-ordinate'
    case ('U068')
       msg = '*** ERROR Inconsistent co-ordinates #; hit F1 for help'
    case ('U069')
       msg = '*** ERROR Only allowed in 3D model with ' //                      &
            & 'Constant turbulence closure model'
    case ('U070')
       msg = '*** WARNING Smoothing of boundary condition will be cancelled'
    case ('U071')
       msg = '*** ERROR Specify user-defined process first'
    case ('U072')
       msg = '*** ERROR # not requested in user-defined process'
    case ('U075')
       msg = '*** WARNING Return time at # < 0.'
    case ('U076')
       msg = '*** WARNING Bottom return time at # < 0.'
    case ('U077')
       msg = '*** ERROR QH-relation only valid for Z-type boundary'
    case ('U078')
       msg = '*** ERROR QH-relation boundary must precede Time-series boundary'
    case ('U079')
       msg = '*** ERROR Reading QH-relation during updating'
    case ('U080')
       msg = '*** ERROR Time Varying Data file # should be unformatted'
    case ('U081')
       msg = '*** ERROR Time Varying Data file # should be direct access'
    case ('U100')
       msg = '*** MESSAGE No # record found in MDF-file, item will be skipped'
    case ('U101')
       msg = '*** ERROR Coupled records # in MDF-file not consistent ?!'
    case ('U110')
       msg = '*** WARNING You have an old version of the MDF-file (#)'
    case ('U111')
       msg = '*** WARNING Pre-processor out of date. MDF-file is more recent (#)'
    case ('U112')
       msg = '*** WARNING This MDF-file (version number 1.xx) ' //               &
            & 'is not supported anymore !!'
    case ('U122')
       msg = '*** ERROR Number of boundary openings in # exceeds maximum'
    case ('U130')
       msg = '*** WARNING Grid enclosure points exceeds maximum, # skipped'
    case ('U131')
       msg = '*** WARNING Dry point sections exceeds maximum, # skipped'
    case ('U132')
       msg = '*** WARNING Thin dams exceeds maximum, # skipped'
    case ('U135')
       msg = '*** ERROR Coriolis values in attribute file and ' //              &
            & 'SFERIC not allowed'
    case ('U136')
       msg = '*** WARNING Latitude of your model is 0 degrees,' //              &
            & ' model located at equator'
    case ('U140')
       msg = '*** ERROR Improper location of discharge #, default is used'
    case ('U145')
       msg = '*** ERROR Internal inconsistency in open bnd. ' //                &
            & 'def. Contact Deltares'
    case ('U150')
       msg = '*** ERROR No Boundary Definition name, default is used'
    case ('U151')
       msg = '*** ERROR Improper location of open boundary #, default is used'
    case ('U152')
       msg = '*** WARNING Nr. of data for T-boundaries (hydro)' //              &
            & ' exceeds maximum, last skipped'
    case ('U153')
       msg = '*** WARNING Nr. of data for T-boundaries ' //                     &
            & '(processes) exceeds max., last skipped'
    case ('U154')
       msg = '*** WARNING Nr. of time varying data for ' //                     &
            & 'discharges exceeds max., last skipped'
    case ('U157')
       msg = '*** WARNING Nr. of time varying heat model data ' //              &
            & 'exceeds maximum, last skipped'
    case ('U158')
       msg = '*** ERROR Free formatted file # with quotes not implemented (yet)'
    case ('U160')
       msg = '*** ERROR Name of constituent already used (#)'
    case ('U161')
       msg = '*** ERROR Free formatted file # with quotes not implemented (yet)'
    case ('U162')
       msg = '*** ERROR Decay rate constituent "#" should be > 0.'
    case ('U163')
       msg = '*** WARNING Evaporation in continuity equation taken into account'
    case ('U164')
       msg = '*** WARNING Heat excess model does not contain evaporation option'
    case ('U165')
       msg = '*** WARNING Number of drogues exceeds maximum, last skipped'
    case ('U166')
       msg = '*** ERROR Constituent name Sediment is reserved for sediment process'
    case ('U167')
       msg = '*** ERROR Number of sediment is inconsistent ' //                 &
            & 'with data in attribute file.'
    case ('U170')
       msg = '*** ERROR Monitoring station name already used (#)'
    case ('U171')
       msg = '*** ERROR Cross-section name already used (#)'
    case ('U172')
       msg = '*** ERROR Drogue name already used (#)'
    case ('U173')
       msg = '*** ERROR Discharge location name already used (#)'
    case ('U174')
       msg = '*** ERROR Open boundary name already used (#)'
    case ('U180')
       msg = '*** WARNING No point defined for diagnostic output IWE. No output'
    case ('U181')
       msg = '*** ERROR User-defined wave length (#) for IWE should be <> 0.'
    case ('U182')
       msg = '*** ERROR Time increment for IWE should be defined (<> 0.)'
    case ('U183')
       msg = '*** WARNING Due to IWE values for DICOWW and ' //                 &
            & 'VICOWW are re-defined (0.)'
    case ('U184')
       msg = '*** WARNING Longitude of your model is 0 degrees,' //              &
            & ' using solar radiation computed at Greenwich meridian'
    case ('U190')
       msg = '*** WARNING #'
    case ('U958')
       msg = '*** WARNING Nr. of time varying rain/evap data ' //               &
            & 'exceeds maximum, last skipped'
    case ('V001')
       msg = '*** ERROR Grid dimension contains unrealistic values'
    case ('V002')
       msg = '*** ERROR Number of frequencies must be at least 1'
    case ('V003')
       msg = '*** ERROR # - insufficient number of values or record is corrupt'
    case ('V004')
       msg = '*** WARNING Type of boundary nr. # is set to default value "H"'
    case ('V005')
       msg = '*** ERROR Restart file ID must differ from current Run ID'
    case ('V006')
       msg = '*** ERROR Only S(econds), M(inutes), H(ours), D(ays) ' //         &
           & 'and W(eeks) as time unit have been implemented'
    case ('V007')
       msg = '*** ERROR Simulation date is not according to the convention'
    case ('V008')
       msg = '*** WARNING # outside simulation period; will be reset'
    case ('V009')
       msg = '*** WARNING # outside period for writing file; will be reset'
    case ('V010')
       msg = '*** WARNING stations or cross-sections defined, but HIS interval is 0 ?!'
    case ('V011')
       msg = '*** WARNING no stations and cross-sections ' //                   &
            & 'defined; HIS-file not created !!'
    case ('V012')
       msg = '*** ERROR No name defined for discharge source'
    case ('V013')
       msg = '*** ERROR No name defined for monitoring station'
    case ('V014')
       msg = '*** ERROR No name defined for monitoring cross-section'
    case ('V015')
       msg = '*** ERROR No name defined for constituent'
    case ('V016')
       msg = '*** ERROR Curvi-linear coefficient # = 0, while point is active'
    case ('V017')
       msg = '*** WARNING # outside simulation period; not calculated'
    case ('V018')
       msg = '*** WARNING # times corrupt (tfdro > tldro); not calculated'
    case ('V019')
       msg = '*** WARNING # lies on inactive point; not calculated'
    case ('V020')
       msg = '*** ERROR Nr. of polygons too large. Contact Deltares'
    case ('V021')
       msg = '*** ERROR Line # violates the rules for the grid enclosure'
    case ('V022')
       msg = '*** ERROR Grid enclosure polygon nr. # is not closed'
    case ('V023')
       msg = '*** ERROR Computational grid enclosure line # ' //                &
            & 'intersects another line'
    case ('V024')
       msg = '*** ERROR Negative or zero grid spacing not allowed'
    case ('V025')
       msg = '*** ERROR Direction of boundaries must be a multiple of 45 degrees'
    case ('V026')
       msg = '*** ERROR Nr. of opening points too large. Contact Deltares'
    case ('V027')
       msg = '*** ERROR Internal (IROCOL) array overwrite. Contact Deltares'
    case ('V028')
       msg = '*** WARNING A default computational grid enclosure will be generated'
    case ('V029')
       msg = '*** ERROR Mismatch of grid dimensions in the #'
    case ('V030')
       msg = '*** ERROR Boundary point # lies inside the computational domain'
    case ('V031')
       msg = '*** ERROR Boundary point # lies outside the computational domain'
    case ('V032')
       msg = '*** ERROR Boundary point # lies on the vertex of' //              &
            & ' the grid enclosure'
    case ('V033')
       msg = '*** ERROR Inconsistent opening type for boundary point #'
    case ('V034')
       msg = '*** ERROR No name defined for drogue'
    case ('V035')
       msg = '*** ERROR Dry points must lie on a line with an ' //              &
            & 'angle multiple of 45 degrees'
    case ('V036')
       msg = '*** WARNING Dry point # lies on an inactive point'
    case ('V037')
       msg = '*** ERROR Dry point # lies on a boundary point'
    case ('V038')
       msg = '*** ERROR Thin dams must lie on a line with an ' //               &
            & 'angle multiple of 45 degrees'
    case ('V039')
       msg = '*** WARNING Thin dam # lies on an inactive point'
    case ('V040')
       msg = '*** ERROR Thin dams at co-ordinates # lie ' //                    &
            & 'outside computational domain'
    case ('V041')
       msg = '*** WARNING # Simulation start time'
    case ('V045')
       msg = '*** ERROR Undefined bottom friction method detected'
    case ('V046')
       msg = '*** ERROR Bottom friction method Z not allowed ' //               &
            & 'for 2D computation.'
    case ('V047')
       msg = '*** WARNING Undefined 3D turbulence closure ' //                  &
            & 'model, Algebraic is assumed'
    case ('V048')
       msg = '*** WARNING Undefined bed-stress option for wave, FR88 is assumed.'
    case ('V049')
       msg = '*** MESSAGE Using default transport solver #'
    case ('V349')
       msg = '*** MESSAGE Transport solver # method is specified'
    case ('V050')
       msg = '*** WARNING Station # lies outside the computational domain'
    case ('V051')
       msg = '*** ERROR Discharge # lies outside the computational domain'
    case ('V052')
       msg = '*** ERROR Layer number of discharge # outside definition (KMAX)'
    case ('V053')
       msg = '*** ERROR X nor Y co-ordinate identical in cross-section #'
    case ('V054')
       msg = '*** WARNING (Part of) Cross-section # lies on ' //                &
            & 'inactive points'
    case ('V055')
       msg = '*** ERROR Cross-section # constitutes only a single point'
    case ('V056')
       msg = '*** WARNING No stations or cross-sections ' //                    &
            & 'defined; his print not created !!'
    case ('V057')
       msg = '*** WARNING Discharge # located near a structure'
    case ('V058')
       msg = '*** WARNING Weir crest below bottom for (M,N) = #'
    case ('V059')
       msg = '*** WARNING Floating structure # located near a structure'
    case ('V060')
       msg = '*** WARNING Negative values for constituents ' //                 &
            & 'encountered in #'
    case ('V061')
       msg = '*** ERROR Zero or negative value found for #'
    case ('V062')
       msg = '*** ERROR Wind stress coefficient is not a ' //                   &
            & 'linear function, check input'
    case ('V063')
       msg = '*** ERROR Negative value found for #'
    case ('V064')
       msg = '*** WARNING Comm. stop time outside (#) simulation period; will be reset'
    case ('V070')
       msg = '*** ERROR Invalid excess temperature model flag ' //              &
            & '(only 0 to 5 allowed)'
    case ('V071')
       msg = '*** WARNING Iteration parameter is reset to a ' //                &
            & 'minimum value of #'
    case ('V073')
       msg = '*** WARNING Drying and flooding criterion exceeds 1.0 m.'
    case ('V074')
       msg = '*** WARNING Drying and flooding criterion should' //              &
            & ' be >= .02 m. Will be reset'
    case ('V075')
       msg = '*** WARNING Cutcell GUV, problem at M,N: #'
    case ('V076')
       msg = '*** WARNING Cutcell GVU, problem at M,N: #'
    case ('V078')
       msg = '*** MESSAGE Using default momentum solver #'
    case ('V079')
       msg = '*** MESSAGE Momentum solver # method is specified'
    case ('V080')
       msg = '*** ERROR Initial date # in combination with ' //                 &
            & 'ASCON not allowed.'
    case ('V081')
       msg = '*** ERROR Requested component # not allowed for ' //              &
            & 'Tide Generating Forces'
    case ('V082')
       msg = '*** ERROR Cannot apply stair case points: #; it is' //            &
            & ' not a 45 degree line'
    case ('V083')
       msg = '*** ERROR Stair case point # is defined at dry' //                &
            & ' point or open boundary'
    case ('V084')
       msg = '*** ERROR Cannot apply stair case point #; it does' //            &
            & ' not satisfy the rule'
    case ('V085')
       msg = '*** ERROR Cannot apply cut-cell points #; it does ' //             &
            & 'not satisfy the rule'
    case ('V086')
       msg = '*** ERROR Cut-cell point # is defined at dry point' //            &
            & ' or open boundary'
    case ('V087')
       msg = '*** ERROR Cut-cell point # is not adjacent to dry point'
    case ('V088')
       msg = '*** ERROR Corner Cut-cell not allowed at M,N: #'
    case ('V089')
       msg = '*** WARNING Cut-cell area equals 0 at M,N: #'
    case ('V091')
       msg = '*** ERROR Missing value (or the keyword): # in file'
    case ('V093')
       msg = '*** ERROR Only ITDATE as reference date is accepted'
    case ('V094')
       msg = '*** ERROR Missing input for # (file not in sequential order?)'
    case ('V095')
       msg = '*** ERROR Vertical profile # not allowed'
    case ('V096')
       msg = '*** ERROR Parameter name # in (BCT, BCC or DIS) ' //              &
            & 'file not expected'
    case ('V097')
       msg = '*** ERROR Missing parameter name in file'
    case ('V098')
       msg = '*** WARNING Time dependent data for # reset (read time < TSTOP)'
    case ('V099')
       msg = '*** ERROR Location name # not expected'
    case ('V200')
       msg = '*** WARNING User-Defined Functions #'
    case ('V201')
       msg = '*** ERROR All User-Defined Functions skipped, ' //                &
            & 'due to inconstistencies !!'
    case ('V202')
       msg = '*** ERROR No authorisation to use # as ' //                       &
            & 'User-Defined Function !!'
    case ('V205')
       msg = '*** ERROR Combination 2D-weirs and 3D model ' //                  &
            & '(kmax > 1) not allowed'
    case ('V211')
       msg = '*** ERROR Number of User-Defined Processes ' //                   &
            & 'exceeds the maximum ( # )'
    case ('V212')
       msg = '*** ERROR No name specified for User-Defined Process: #'
    case ('V213')
       msg = '*** ERROR The User-Defined Process ( # ) already declared'
    case ('V214')
       msg = '*** ERROR Inconsistent nr. of parameters for ' //                 &
            & 'User-Defined Process: #'
    case ('V215')
       msg = '*** ERROR Number of User-Defined files/const. ' //                &
            & 'exceeds the maximum ( # )'
    case ('V220')
       msg = '*** ERROR No name specified for User-Defined Files'
    case ('V221')
       msg = '*** ERROR Pre-defined User-Defined # constants not in MDF-file'
    case ('V222')
       msg = '*** ERROR top z-coordinate of the gate must be > than the ' //    &
            & 'bottom z-coordinate'
    case ('V230')
       msg = '*** ERROR Secondary Flow coefficient conflict #'
    case ('V231')
       msg = '*** ERROR Direction of # must be a multiple of 45 degrees'
    case ('V232')
       msg = '*** ERROR # lies outside the computational domain'
    case ('V233')
       msg = '*** ERROR Unknown value for direction of #'
    case ('V234')
       msg = '*** ERROR Wall roughness value for # should be > 0.0'
    case ('V235')
       msg = '*** ERROR Structure # defined'
    case ('V236')
       msg = '*** WARNING Structure # defined'
    case ('V239')
       msg = '*** ERROR 2D spillways are only allowed for ' //                  &
            & '2D models (kmax = 1)'
    case ('V240')
       msg = '*** ERROR Combination of Diagnostic mode and ' //                 &
            & 'Spiral Motion not allowed.'
    case ('V241')
       msg = '*** WARNING Diagnostic mode time > stop time ' //                 &
            & 'simulation; no Diagnostic mode'
    case ('V242')
       msg = '*** WARNING Diagnostic mode time <= start time ' //               &
            & 'simulation; Only Diagnostic mode'
    case ('V243')
       msg = '*** ERROR Wind factor for Particles outside ' //                  &
            & 'range, no extra wind !!'
    case ('V247')
       msg = '*** WARNING Undefined 2D turbulence model, Constant is assumed'
    case ('V248')
       msg = '*** ERROR # specified in file for 2D-turb. BC is' //              &
            & ' not a boundary point'
    case ('V249')
       msg = '*** WARNING # lies outside the computational domain'
    case ('M001')
       msg = '*** ERROR Multi domain and # is not available'
    case ('M002')
       msg = '*** WARNING Combination of multi domain and # ' //                &
            & 'has not yet been tested'
    case ('Z003')
       msg = '*** ERROR Z-coordinate at the top layer must be ' //              &
            & 'above the bottom layer'
    case ('Z006')
       msg = '*** WARNING In Z-model DPUOPT must be either MIN' //              &
            & ' or UPW; DPUOPT reset to MIN'
    case ('Z007')
       msg = '*** MESSAGE DPSOPT found in MDF-file: #'
    case ('Z008')
       msg = '*** MESSAGE DPUOPT found in MDF-file: #'
    case ('Z009')
       msg = '*** WARNING If Momsol=FLOOD, DPUOPT must be MIN;' //              &
            & ' DPUOPT reset to MIN'
    case ('Z011')
       msg = '*** ERROR Z-model and # is not available'
    case ('Z012')
       msg = '*** WARNING Combination of Z-model and # has ' //                 &
            & 'not yet been tested'
    case ('Z013')
       msg = '*** WARNING #'
    case ('Z014')
       msg = '*** WARNING Non-hydrostatic domain set to default(1:MMAX, 1:NMAX)'
    case ('Z015')
       msg = '*** ERROR Non-hydrostatic domain specified' //                    &
            & ' outside computaion domain'
    case ('Z016')
       msg = '*** WARNING Non-hydrostatic domain covers only 1 grid line!'
    case ('Z018')
       msg = '*** WARNING Iteration parameter for Non-' //                      &
            & 'hydrostatic approach # set to: 50 (default)'
    case ('Z019')
       msg = '*** WARNING Iteration norm for Non-hydrostatic' //                &
            & ' approach set to: L2-norm (default)'
    case ('Z020')
       msg = '*** WARNING Iteration stop criterion for Non-' //                 &
            & 'hydrostatic approac hset to: 1e-2 (default)'
    case ('Z021')
       msg = '*** WARNING No CG Convergence for Non-hydrostatic' //             &
            & ' approach at tstep: #'
    case ('Z022')
       msg = '*** MESSAGE Iteration parameter for Non-hydrostatic approach = #'
    case default
       msg = '*** ERROR message list incomplete ! ' // msgno
    end select messagenumber
    !
    ! insert the string
    !
    msgold = msg
    msg = ' '
    ip = index(msgold, '#')
    call remove_leading_spaces(msgold    ,oldlen    )
    !
    ! filtxt can be a constant string.
    ! call remove_leading_spaces with a writable copy
    !
    ilen = min(len(filtxt), len(filtxtcopy))
    filtxtcopy(:ilen) = filtxt(:ilen)
    call remove_leading_spaces(filtxtcopy,ilen      )
    ilen = min(ilen, len(msg) - oldlen)
    if (ip/=0 .and. ilen/=0) then
       msglen = oldlen + ilen
       msg(:msglen) = msgold(:ip - 1) // filtxtcopy(:ilen)                      &
                    & // msgold(ip + 1:oldlen)
    elseif (ip/=0 .and. ilen==0) then
       msglen = oldlen
       msg(:msglen) = msgold(:ip - 1) // msgold(ip + 1:oldlen)
    else
       msglen = oldlen
       msg(:msglen) = msgold(:oldlen)
    endif
    !
    ! write message to output and close the message file
    !
    write (lundia, '(a)') msg(:msglen)
 9999 continue
end subroutine prterr

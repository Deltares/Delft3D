subroutine esm_alloc_int(lundia, error, zmodel, gdp)
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
! Determines memory requirements for the INTEGER ARRAY.
! In this subroutine the start indices of all integer arrays are calculated by
! using the memory management function MKIPNT.
! The start adress of an array can be found by using the function GTIPNT.
! Function MKIPNT will when errors occure call an errorroutine (ERRPNT).
! The function MKIPNT will return with value 1 or for memory already
! declared with correct length with value -1.
! Because the Delft3D-FLOW module can be changed to use static
! array declaration the error messages will stay at the end of the routine.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: nmax
    integer , pointer :: mmax
    integer , pointer :: nlb
    integer , pointer :: nub
    integer , pointer :: mlb
    integer , pointer :: mub
    integer , pointer :: kmax
    integer , pointer :: nmaxd
    integer , pointer :: mmaxd
    integer , pointer :: lsts
    integer , pointer :: lstsc
    integer , pointer :: lstsci
    integer , pointer :: lsed
    integer , pointer :: ltur
    integer , pointer :: nlcest
    integer , pointer :: nostat
    integer , pointer :: nto
    integer , pointer :: kc
    integer , pointer :: nopest
    integer , pointer :: nsrc
    integer , pointer :: nofou
    integer , pointer :: ndro
    integer , pointer :: nsluv
    integer , pointer :: nipntr
!
! Global variables
!
    integer              :: lundia ! Description and declaration in inout.igs
    logical, intent(out) :: error  ! TRUE if an error is encountered
    logical, intent(in)  :: zmodel ! Description and declaration in procs.igs
!
! Local variables
!
    integer           :: ddb
    integer           :: ierr    ! Errorflag 
    integer           :: kfacz   ! Multiple factor; 0 if ZMODEL=TRUE 1 if ZMODEL=FALSE for vertical grid arrays 
    integer           :: mmaxdb
    integer           :: mmaxddb
    integer           :: nmaxdb
    integer           :: nmaxddb
    integer, external :: mkipnt
    character(6)      :: pntnam  ! Pointername 
!
!! executable statements -------------------------------------------------------
!
    nipntr    => gdp%gdpointrs%nipntr
    nmax      => gdp%d%nmax
    mmax      => gdp%d%mmax
    nlb       => gdp%d%nlb
    nub       => gdp%d%nub
    mlb       => gdp%d%mlb
    mub       => gdp%d%mub
    kmax      => gdp%d%kmax
    nmaxd     => gdp%d%nmaxd
    mmaxd     => gdp%d%mmaxd
    lsts      => gdp%d%lsts
    lstsc     => gdp%d%lstsc
    lstsci    => gdp%d%lstsci
    lsed      => gdp%d%lsed
    ltur      => gdp%d%ltur
    nlcest    => gdp%d%nlcest
    nostat    => gdp%d%nostat
    nto       => gdp%d%nto
    kc        => gdp%d%kc
    nopest    => gdp%d%nopest
    nsrc      => gdp%d%nsrc
    nofou     => gdp%d%nofou
    ndro      => gdp%d%ndro
    nsluv     => gdp%d%nsluv
    !
    ! initialize array boundaries
    !
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    mmaxddb = gdp%d%mub - gdp%d%mlb + 1
    !
    kfacz = 0
    if (zmodel) then
       kfacz = 1
    endif
    !
    ! arrays for: discharge sources
    !
    pntnam = 'MNKSRC'        !  Discharge:
                             !     MNK indices of inlet; total column when K=0
                             !     MNK indices of outlet; total column when K=0
                             !     single integer specifying the type:
                             !         0 : Ordinary discharge (fixed position)
                             !         1 : Walking discharge (inlet is start position, outlet is time dependent current position)
                             !         2 : Power station type P (inlet and outlet are coupled, temperature and/or constituents may be changed, discharge specified)
                             !         3 : Culvert
                             !         4 : Culvert; special type 'E' (one-way) for Borgerhout
                             !         5 : Culvert; special type 'D' (two-way) for Borgerhout
                             !         6 : Power station type Q (inlet and outlet are coupled, temperature and/or constituents may be changed, heat dump specified)
                             !         7 : Culvert in user defined dll
                             !         8 : two-way culvert with density sensitivity
    ierr = mkipnt(pntnam, 7*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'itdis'         !  Times and pointers in direct access files
                             !  of the time-dependent data for discharge time series:
                             !  1 = previous time
                             !  2 = next time
                             !  3 = start record
                             !  4 = number of time records
                             !  5 = last read record
    ierr = mkipnt(pntnam, 5*nsrc, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: time varying and fourier openings
    !
    pntnam = 'MNBND'         !  Coordinates of the open boundary sections
                             !  MNBND(1,K)=M index of the begin pnt.
                             !  MNBND(2,K)=N index of the begin pnt.
                             !  MNBND(3,K)=M index of the end   pnt.
                             !  MNBND(4,K)=N index of the end   pnt.
                             !  MNBND(5,K)=lowest  open Z index (default = 0: all layers are open)
                             !  MNBND(6,K)=highest open Z index
                             !  MNBND(7,K)=code denoting the position of the open boundary, related to the complete grid:
                             !             1 : left   of grid (low m side)
                             !             2 : bottom of grid (low n side)
                             !             3 : right  of grid (high m side)
                             !             4 : top    of grid (high n side)
                             !        K = 1,.....,NOPEN
    ierr = mkipnt(pntnam, 7*nto, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NOB'           !  Adm. array for open boundary points
                             !  NOB(1,I): M COORD. of the bnd. pnt.
                             !  NOB(2,I): N COORD. of the bnd. pnt.
                             !  NOB(3,I): Bnd. Type=1 - closed
                             !                      2 - Water level
                             !                      3 - Current
                             !                      4 - Unused
                             !                      5 - Discharge
                             !                      6 - Riemann
                             !                      7 - Total disch.
                             !                      8 - Neumann
                             !  NOB(4,I): 1 begin of the open bnd.
                             !            2 end   of the open bnd.
                             !  NOB(5,I): Index to the ROW nr. in array IROCOL/IRC
                             !  NOB(6,I): 1 begin of the open bnd.
                             !            2 end   of the open bnd.
                             !  NOB(7,I): Index to the COLUMN nr. in array IROCOL/IRC
                             !  NOB(8,I): pointer to sequence nr. of opening section
                             !        I : 1,.....,NROB
    ierr = mkipnt(pntnam, 8*nopest, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kstp'          !  First layer number with concentration
                             !  value of the bottom
                             !  k: 1      .. kstp;  r = r-surface
                             !  k: kstp+1 .. kmax;  r = r-bottom
    ierr = mkipnt(pntnam, nopest*lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'itbct'         !  Times and pointers in direct access files
                             !  of the time-dependent data for open boundaries (hydrodynamic)
                             !  1 = previous time
                             !  2 = next time
                             !  3 = start record
                             !  4 = number of time records
                             !  5 = last read record
                             !  for QH relations used as
                             !  1 = flag (-1:Q<Qmin,
                             !             0:Q in range
                             !             1:Q>Qmax)
                             !  2 = unused
                             !  3 = start record
                             !  4 = number of time records
                             !  5 = number of record stored in first
                             !      two rows of HYDRBC: HYDRBC(1/2,:
    ierr = mkipnt(pntnam, 5*nto, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'itbcc'         !  Times and pointers in direct access files
                             !  of the time-dependent data for concentrations
                             !  at open boundary time series
                             !  1 = previous time
                             !  2 = next time
                             !  3 = start record
                             !  4 = number of time records
                             !  5 = last read record
    ierr = mkipnt(pntnam, 5*nto*lstsc, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: computational grid table
    !
    pntnam = 'IROCOL'        !  Pointer table with bound. coord. and
                             !  bound. types (comp. cols. and rows)
    ierr = mkipnt(pntnam, 5*nlcest, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: mask arrays (permanent)
    !
    pntnam = 'kcu'           !  Mask array for the u-velocity point (time INdependent)
                             !  =0 dry      point
                             !  =1 active   point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kcv'           !  Mask array for the v-velocity point (time INdependent)
                             !  =0 dry      point
                             !  =1 active   point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kcs'           !  Mask array for the zeta points (time INdependent)
                             !  =-1 active   point belonging to other partition (ghost point)
                             !  = 0 inactive point
                             !  = 1 active   point
                             !  = 2 open boundary point
                             !  = 3 Domain Decomposition boundary point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: mask arrays (temporary)
    !
    pntnam = 'kfu'           !  Mask array for the u-velocity point (time dependent)
                             !  =0 dry      point
                             !  =1 active   point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfv'           !  Mask array for the v-velocity point (time dependent)
                             !  =0 dry      point
                             !  =1 active   point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfs'           !  Mask array for the zeta points (time dependent)
                             !  =0 dry      point
                             !  =1 active   point
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: mask arrays (special points)
    !
    pntnam = 'kspu'          !  Mask array for drying and flooding
                             !  upwind when special points are used for U points
                             !  KSPU(NM,0) =  1 Discharge location
                             !             =  2 Floating structure
                             !             =  3 Local weir
                             !             =  4 Gate
                             !             =  5 Rigid sheet
                             !             =  6 Porous plate
                             !             =  7 Bridge
                             !             =  8 Barrier
                             !             =  9 2D Weir
                             !             = 10 Fixed Gate (CDW)
                             !  For type 1-3,5-8 the negative equivalence implice no upwind
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp) ! (nmaxddb  ,mmaxddb,0:kmax)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kspv'          !  Mask array for drying and flooding
                             !  upwind when special points are used for V points
                             !  KSPV(NM,0) = 1 Discharge location
                             !             = 2 Floating structure
                             !             = 3 Local weir
                             !             = 4 Gate
                             !             = 5 Rigid sheet
                             !             = 6 Porous plate
                             !             = 7 Bridge
                             !             = 8 Barrier
                             !             = 9 2D Weir
                             !  For type 1-3,5-8 the negative equivalence implice no upwind
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb*(kmax + 1), gdp)  ! (nmaxddb  ,mmaxddb,0:kmax)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kadu'          !  Mask array for adv. term adjustment for structures in U-points
                             !  = 1 no structure (HYD)
                             !  = 1 no gate (TRA)
                             !  = 0 structure
                             !  = 0 gate (KSPU(NM,0)*KSPU(NM,K)=4)
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kadv'          !  Mask array for adv. term adjustment for structures in V-points
                             !  = 1 no structure (HYD)
                             !  = 1 no gate (TRA)
                             !  = 0 structure
                             !  = 0 gate (KSPV(NM,0)*KSPV(NM,K)=4)
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb*kmax, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: cut cell approach
    !
    pntnam = 'kcscut'        !  no description (yet)
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kcu45'         !  no description (yet)
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kcv45'         !  no description (yet)
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: diffusivity
    !
    pntnam = 'idifu'         !  Work space, Identification if numerical diffusive flux is added
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for post processing MAP file & online graphics
    !
    pntnam = 'ibuff'         !  Help array for writing NEFIS files
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: drogues;
    !
    pntnam = 'mndro'         !  M(1)N(2)-Coordinate of drogue tracks starting point
    ierr = mkipnt(pntnam, 2*ndro, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'itdro'         !  1: Timestep number at which a drogue will be released
                             !     (relative to itdate definition and dt)
                             !  2: Timestep number at which a drogue is stopped
    ierr = mkipnt(pntnam, 2*ndro, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for: barriers;
    !
    pntnam = 'mnbar'         !  Coordinates and type of the barrier
    ierr = mkipnt(pntnam, 5*nsluv, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for plot mask arrays in the vertical direction (temporary)
    !
    pntnam = 'kzu'           !  no description (yet)
    ierr = mkipnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kzv'           !  no description (yet)
    ierr = mkipnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kzs'           !  no description (yet)
    ierr = mkipnt(pntnam, max(nmax, mmax)*(kmax + 2), gdp)
    if (ierr <= -9) goto 9999
    !
    ! KFS mask array for plotting in monitoring stations (time-dependent)
    !
    pntnam = 'zkfs'           !  KFS in monitoring stations 
                              !  Non-active (0) or active (1) zeta point (time-dependent)
    ierr = mkipnt(pntnam, nostat, gdp)
    if (ierr <= -9) goto 9999
    !
    ! work arrays
    !
    pntnam = 'iwrk1'         !  Internal work array
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'iwrk2'         !  Internal work array
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'iwrk3'         !  Internal work array
    ierr = mkipnt(pntnam, nmaxddb*mmaxddb, gdp)
    if (ierr <= -9) goto 9999
    !
    ! arrays for fixed layer approach
    !
    pntnam = 'kfsz0'         !  3D Mask array for cells for for each layer in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfuz0'         !  3D Mask array for U-cell interfaces for for each layer in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvz0'         !  3D Mask array for V-cell interfaces for for each layer in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfsz1'         !  3D Mask array for cells for for each layer in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfuz1'         !  3D Mask array for U-cell interfaces for for each layer in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvz1'         !  3D Mask array for V-cell interfaces for for each layer in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kmax*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfsmin'        !  Index of the 1st active (bed) layer in a water level point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfsmax'        !  Index of the last active (surface level) layer in a water level point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfumin'        !  Index of the first active (bed) layer in a U-velocity point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfumax'        !  Index of the last active (surface level) layer in a U-velocity point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvmin'        !  Index of the first active (bed) layer in a V-velocity point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvmax'        !  Index of the last active (surface level) layer in a V-velocity point in a z-model
                             !  For the geometry at the new time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfsmx0'        !  Index of the last active (surface level) layer in a water level point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfumx0'        !  Index of the last active (surface level) layer in a U-velocity point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvmx0'        !  Index of the last active (surface level) layer in a V-velocity point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfsmn0'        !  Index of the first active (bottom level) layer in a water level point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfumn0'        !  Index of the first active (bottom level) layer in a U-velocity point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kfvmn0'        !  Index of the first active (bottom level) layer in a V-velocity point in a z-model
                             !  For the geometry at the current/old time step
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'kcshyd'        !  no description (yet)
    ierr = mkipnt(pntnam, max(1, nmaxddb*mmaxddb*kfacz), gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'izmodl'        !  no description (yet)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'iroll'        !  1: Roller functionality used
                            !  0: No roller functionality (default)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    ! The following list of scalar integers have to be placed in shared memory,
    ! because then the mapper can read them.
    ! BE CAREFUL:
    !    These integers are allocated TWICE inside FLOW:
    !    in esm_alloc_int.f90 as part of the shared memory block, allocated via esm/fsm, and
    !    in *.igs-files as part of the GDP structure (e.g. nmax) or
    !    locally on a high level (e.g. it01 in trisim.f90)
    !
    !    FLOW uses the instance in the GDP-structure (or the
    !    local definition respectively) and place a copy of these parameters in
    !    the shared memory block. The mapper uses this copy and is assumed not to
    !    change the values.
    !
    ! TO DO: Clean implementation:
    !    These parameters should be allocated in shared memory only (since the
    !    mapper needs them). The GDP-structure and local allocation variants
    !    must be replaced by pointers to the shared memory instance.
    !
    pntnam = 'NMAX'          !  Number of gridpoints in the y-dir. (always odd)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NMAXUS'        !  Number of grid points actually specified by the user.
                             !  If NMAXUS = even then NMAX = NMAXUS + 1
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'MMAX'          !  Number of gridpoints in the x-dir.
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NOROW'         !  Number of comp. rows  in IROCOL-array
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NOCOL'         !  Number of comp. cols. in IROCOL-array
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NOROCO'        !  Number of Computational rows & cols
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NTO'           !  Total number of open boundary sections
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NROB'          !  Number of open boundary points
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NFLTYP'        !  Integer representation for DRYFLC
                             !  =0 NO drying and flooding
                             !  =1 MEAN procedure
                             !  =2 MAX  procedure
                             !  =3 MIN  procedure
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'KMAX'          !  Number of layers in the z-dir.
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'NSRC'          !  Total number of discharges (source or sink,
                             !  including all automatically added 'artificial' discharge
                             !  points used to model bubble screens
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ITLEN'         !  Lenght of the tide cycle
                             !  FLOW stand alone and no waves: ITLEN = 0
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'IT01'          !  Reference date in yymmdd
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'IT02'          !  Reference time in hhmmss
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'lstsci'        !  Number of Constituents (Salinity, Temperature, Sediment,
                             !  Conservative Constituents and Secondary Flow)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ltur'          !  Flag for 3D turbulence model, also denoting
                             !  the number of turbulent energy constituents
                             !     0 = Algebraic model
                             !     1 = k-l       model
                             !     2 = k-eps     model
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'lsed'          !  Number of sediment constituents
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'lsedtt'        !  Totoal number of sediment fractions, i.e. number
                             !  of suspended sediments (sediment constituents)
                             !  and number of bed load fractions.
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'ddb'           !  no description (yet)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'mmaxdb'        !  = mmaxddb (putnam only accepts 6 characters)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    pntnam = 'nmaxdb'        !  = nmaxddb (putnam only accepts 6 characters)
    ierr = mkipnt(pntnam, 1, gdp)
    if (ierr <= -9) goto 9999
    !
    !
    !
    ! Test if pointer declaration outside declaration in POINTRS.INC
    ! Only useful when using static array declaration include files
    !
    if (ierr== - 3) then
       error = .true.
       call prterr(lundia    ,'G005'    ,' '       )
       write (lundia, *) '         Parameter MXIPNT to small, add ',            &
                       & nipntr - mxipnt
    endif
    !
    ! Test exit code which are not allowed (in theory not possible)
    !
 9999 continue
    if (ierr <= -9) then
       error = .true.
       call prterr(lundia    ,'G920'    ,'esm_alloc_int'   )
    endif
end subroutine esm_alloc_int

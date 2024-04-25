subroutine gdp_dealloc(gdp)
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use morphology_data_module
    use bedcomposition_module
    use message_module
    use flow_tables
    !use ec_module
    use globaldata
    use dfparall
    use dredge_data_module, only: clrdredge
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
    integer :: i
    integer :: istat
    !
    type(dfparalltype) , pointer :: partp
    !
    integer :: localnofou
    logical :: localrhum_file
    logical :: localtair_file
    logical :: localclou_file
    logical :: localprcp_file
    logical :: localswrf_file
    logical :: localculvert
    logical :: localdrogue
    logical :: locallftrto
    logical :: localveg3d
    logical :: localbubble
    logical :: localscour
    logical :: success
!
!! executable statements -------------------------------------------------------
!
    !
    ! copy a number of logical flags to local parameters, because their gdp entries
    ! will be deallocated before they are used to deallocate their related structures
    !
    localnofou       = gdp%d%nofou
    localrhum_file   = gdp%gdheat%rhum_file
    localtair_file   = gdp%gdheat%tair_file
    localclou_file   = gdp%gdheat%clou_file
    localprcp_file   = gdp%gdheat%prcp_file
    localswrf_file   = gdp%gdheat%swrf_file
    localculvert     = gdp%gdprocs%culvert
    localdrogue      = gdp%gdprocs%drogue
    locallftrto      = gdp%gdprocs%lftrto
    localveg3d       = gdp%gdprocs%veg3d
    localbubble      = gdp%gdprocs%bubble
    localscour       = gdp%gdscour%scour
    !
    call iofiles_dealloc(gdp)
    !
    call clradv2d(istat, gdp)
    deallocate (gdp%gdadv2d  , STAT = istat)
    deallocate (gdp%gdaddress, STAT = istat)
    if (localbubble) then
       if (associated(gdp%gdbubble%cpdis)) deallocate (gdp%gdbubble%cpdis, STAT = istat)
       if (associated(gdp%gdbubble%hsink)) deallocate (gdp%gdbubble%hsink, STAT = istat)
       if (associated(gdp%gdbubble%hsour)) deallocate (gdp%gdbubble%hsour, STAT = istat)
       if (associated(gdp%gdbubble%xlbub)) deallocate (gdp%gdbubble%xlbub, STAT = istat)
       if (associated(gdp%gdbubble%zbubl)) deallocate (gdp%gdbubble%zbubl, STAT = istat)
       if (associated(gdp%gdbubble%zvelo)) deallocate (gdp%gdbubble%zvelo, STAT = istat)
       if (associated(gdp%gdbubble%flbub)) deallocate (gdp%gdbubble%flbub, STAT = istat)
    endif
    deallocate (gdp%gdbubble, STAT = istat)
    deallocate (gdp%gdconst , STAT = istat)
    deallocate (gdp%gdconstd, STAT = istat)
    deallocate (gdp%gdcoup  , STAT = istat)
    deallocate (gdp%gddatusr, STAT = istat)

    if (associated(gdp%gddischarge%capacity)) deallocate (gdp%gddischarge%capacity, STAT = istat)
    deallocate (gdp%gddischarge, STAT = istat)
    deallocate (gdp%d          , STAT = istat)
    deallocate (gdp%griddim    , STAT = istat)
    if (localveg3d) then
       do i=1,gdp%gdveg3d%nveg
          if (associated(gdp%gdveg3d%vegs(i)%dia   )) deallocate (gdp%gdveg3d%vegs(i)%dia   , STAT = istat)
          if (associated(gdp%gdveg3d%vegs(i)%nstem )) deallocate (gdp%gdveg3d%vegs(i)%nstem , STAT = istat)
          if (associated(gdp%gdveg3d%vegs(i)%cdcoef)) deallocate (gdp%gdveg3d%vegs(i)%cdcoef, STAT = istat)
          if (associated(gdp%gdveg3d%vegs(i)%rho   )) deallocate (gdp%gdveg3d%vegs(i)%rho   , STAT = istat)
          if (associated(gdp%gdveg3d%vegs(i)%z     )) deallocate (gdp%gdveg3d%vegs(i)%z     , STAT = istat)
       enddo
       if (associated(gdp%gdveg3d%vegs     )) deallocate (gdp%gdveg3d%vegs     , STAT = istat)
       if (associated(gdp%gdveg3d%planttype)) deallocate (gdp%gdveg3d%planttype, STAT = istat)
       if (associated(gdp%gdveg3d%nplants  )) deallocate (gdp%gdveg3d%nplants  , STAT = istat)
    endif
    deallocate (gdp%gdveg3d , STAT = istat)
    deallocate (gdp%gdexttim, STAT = istat)
    deallocate (gdp%gdfmtbcc, STAT = istat)
    deallocate (gdp%gdfmtbct, STAT = istat)
    deallocate (gdp%gdfmtdis, STAT = istat)
    if (localnofou > 0) then
       if (associated(gdp%gdfourier%fconno       )) deallocate (gdp%gdfourier%fconno       , STAT = istat)
       if (associated(gdp%gdfourier%flayno       )) deallocate (gdp%gdfourier%flayno       , STAT = istat)
       if (associated(gdp%gdfourier%fnumcy       )) deallocate (gdp%gdfourier%fnumcy       , STAT = istat)
       if (associated(gdp%gdfourier%ftmsto       )) deallocate (gdp%gdfourier%ftmsto       , STAT = istat)
       if (associated(gdp%gdfourier%ftmstr       )) deallocate (gdp%gdfourier%ftmstr       , STAT = istat)
       if (associated(gdp%gdfourier%ifoupt       )) deallocate (gdp%gdfourier%ifoupt       , STAT = istat)
       if (associated(gdp%gdfourier%iofset       )) deallocate (gdp%gdfourier%iofset       , STAT = istat)
       if (associated(gdp%gdfourier%foumask      )) deallocate (gdp%gdfourier%foumask      , STAT = istat)
       if (associated(gdp%gdfourier%fknfac       )) deallocate (gdp%gdfourier%fknfac       , STAT = istat)
       if (associated(gdp%gdfourier%foucomp      )) deallocate (gdp%gdfourier%foucomp      , STAT = istat)
       if (associated(gdp%gdfourier%foufas       )) deallocate (gdp%gdfourier%foufas       , STAT = istat)
       if (associated(gdp%gdfourier%fousma       )) deallocate (gdp%gdfourier%fousma       , STAT = istat)
       if (associated(gdp%gdfourier%fousmb       )) deallocate (gdp%gdfourier%fousmb       , STAT = istat)
       if (associated(gdp%gdfourier%fouvec       )) deallocate (gdp%gdfourier%fouvec       , STAT = istat)
       if (associated(gdp%gdfourier%fv0pu        )) deallocate (gdp%gdfourier%fv0pu        , STAT = istat)
       if (associated(gdp%gdfourier%fouelp       )) deallocate (gdp%gdfourier%fouelp       , STAT = istat)
       if (associated(gdp%gdfourier%founam       )) deallocate (gdp%gdfourier%founam       , STAT = istat)
       if (associated(gdp%gdfourier%fouvarnam    )) deallocate (gdp%gdfourier%fouvarnam    , STAT = istat)
       if (associated(gdp%gdfourier%fouvarnamlong)) deallocate (gdp%gdfourier%fouvarnamlong, STAT = istat)
       if (associated(gdp%gdfourier%fouvarunit   )) deallocate (gdp%gdfourier%fouvarunit   , STAT = istat)
       if (associated(gdp%gdfourier%foutyp       )) deallocate (gdp%gdfourier%foutyp       , STAT = istat)
       !
       if (associated(gdp%gdpostpr%kfst0)) deallocate (gdp%gdpostpr%kfst0, STAT = istat)
    endif
    deallocate (gdp%gdfourier, STAT = istat)
    if (associated(gdp%gdheat%secchi)) deallocate (gdp%gdheat%secchi, STAT = istat)
    if (localrhum_file) then
       if (associated(gdp%gdheat%rhumarr)) deallocate (gdp%gdheat%rhumarr, STAT = istat)
    endif
    if (localtair_file) then
       if (associated(gdp%gdheat%tairarr)) deallocate (gdp%gdheat%tairarr, STAT = istat)
    endif
    if (localclou_file) then
       if (associated(gdp%gdheat%clouarr)) deallocate (gdp%gdheat%clouarr, STAT = istat)
    endif
    if (localswrf_file) then
       if (associated(gdp%gdheat%swrfarr)) deallocate (gdp%gdheat%swrfarr, STAT = istat)
    endif
    if (associated(gdp%gdheat%flbcktemp)) deallocate (gdp%gdheat%flbcktemp, STAT = istat)
    deallocate (gdp%gdheat    , STAT = istat)
    deallocate (gdp%gdhtur2d  , STAT = istat)
    deallocate (gdp%gdinout   , STAT = istat)
    deallocate (gdp%gdinttim  , STAT = istat)
    deallocate (gdp%gdiwearr  , STAT = istat)
    deallocate (gdp%gdiwepar  , STAT = istat)
    deallocate (gdp%gdkeywtd  , STAT = istat)
    deallocate (gdp%gdluntmp  , STAT = istat)
    call clearstack (gdp%messages)
    deallocate (gdp%messages  , STAT = istat)
    deallocate (gdp%gdmudcoe  , STAT = istat)
    deallocate (gdp%gdnfl     , STAT = istat)
    deallocate (gdp%gdnumeco  , STAT = istat)
    deallocate (gdp%gdphysco  , STAT = istat)
    deallocate (gdp%gdpointrs , STAT = istat)
    deallocate (gdp%gdprocs   , STAT = istat)
    deallocate (gdp%gdprognm  , STAT = istat)
    deallocate (gdp%gdr_i_ch  , STAT = istat)
    deallocate (gdp%gdrdpara  , STAT = istat)
    deallocate (gdp%gdrivpro  , STAT = istat)
    deallocate (gdp%gdsobek   , STAT = istat)
    if (associated(gdp%gdstations%sta_orgline)) deallocate(gdp%gdstations%sta_orgline, STAT = istat)
    if (associated(gdp%gdstations%tra_orgline)) deallocate(gdp%gdstations%tra_orgline, STAT = istat)
    if (associated(gdp%gdstations%stat_type  )) deallocate(gdp%gdstations%stat_type  , STAT = istat)
    if (associated(gdp%gdstations%stat_drogue)) deallocate(gdp%gdstations%stat_drogue, STAT = istat)
    if (associated(gdp%gdstations%stat_table )) deallocate(gdp%gdstations%stat_table , STAT = istat)
    if (associated(gdp%gdstations%stat_par   )) deallocate(gdp%gdstations%stat_par   , STAT = istat)
    if (associated(gdp%gdstations%stat_tabidx)) deallocate(gdp%gdstations%stat_tabidx, STAT = istat)
    if (associated(gdp%gdstations%mnit       )) deallocate(gdp%gdstations%mnit       , STAT = istat)
    if (associated(gdp%gdstations%mnstat     )) deallocate(gdp%gdstations%mnstat     , STAT = istat)
    if (associated(gdp%gdstations%xystat     )) deallocate(gdp%gdstations%xystat     , STAT = istat)
    if (associated(gdp%gdstations%namst      )) deallocate(gdp%gdstations%namst      , STAT = istat)
    if (associated(gdp%gdstations%namtra     )) deallocate(gdp%gdstations%namtra     , STAT = istat)
    call cleartable(gdp%gdstations%moving_stat_file)
    deallocate (gdp%gdstations, STAT = istat)
    deallocate (gdp%gdtfzeta  , STAT = istat)
    deallocate (gdp%gdtmpfil  , STAT = istat)
    call clrtrachy(istat, gdp)
    deallocate (gdp%gdtrachy, STAT = istat)
    deallocate (gdp%gdturcoe, STAT = istat)
    deallocate (gdp%gdusrpar, STAT = istat)
    deallocate (gdp%gdzmodel, STAT = istat)
    deallocate (gdp%gdnonhyd, STAT = istat)
    !
    call clrbedformpar(istat, gdp)
    deallocate (gdp%gdbedformpar, STAT = istat)
    deallocate (gdp%gdbetaro    , STAT = istat)
    if (associated(gdp%gdbcdat%pindex))          deallocate (gdp%gdbcdat%pindex         , STAT = istat)
    if (associated(gdp%gdbcdat%bct_order))       deallocate (gdp%gdbcdat%bct_order      , STAT = istat)
    if (associated(gdp%gdbcdat%ext_bnd))         deallocate (gdp%gdbcdat%ext_bnd        , STAT = istat)
    if (associated(gdp%gdbcdat%compnames))       deallocate (gdp%gdbcdat%compnames      , STAT = istat)
    if (associated(gdp%gdbcdat%dist_pivot_part)) deallocate (gdp%gdbcdat%dist_pivot_part, STAT = istat)
    if (associated(gdp%gdbcdat%hydrbcf))         deallocate (gdp%gdbcdat%hydrbcf        , STAT = istat)
    deallocate (gdp%gdbcdat, STAT = istat)
    if (localdrogue) then
        if (associated(gdp%gdcline%inc))  deallocate (gdp%gdcline%inc , STAT = istat)
        if (associated(gdp%gdcline%ud))   deallocate (gdp%gdcline%ud  , STAT = istat)
        if (associated(gdp%gdcline%xd))   deallocate (gdp%gdcline%xd  , STAT = istat)
        if (associated(gdp%gdcline%rdep)) deallocate (gdp%gdcline%rdep, STAT = istat)
    endif
    deallocate (gdp%gdcline, STAT = istat)
    if (localculvert) then
        if (associated(gdp%gdculver%arcul))   deallocate (gdp%gdculver%arcul  , STAT = istat)
        if (associated(gdp%gdculver%calfa))   deallocate (gdp%gdculver%calfa  , STAT = istat)
        if (associated(gdp%gdculver%clcul))   deallocate (gdp%gdculver%clcul  , STAT = istat)
        if (associated(gdp%gdculver%cleng))   deallocate (gdp%gdculver%cleng  , STAT = istat)
        if (associated(gdp%gdculver%closs1))  deallocate (gdp%gdculver%closs1 , STAT = istat)
        if (associated(gdp%gdculver%closs2))  deallocate (gdp%gdculver%closs2 , STAT = istat)
        if (associated(gdp%gdculver%closs3))  deallocate (gdp%gdculver%closs3 , STAT = istat)
        if (associated(gdp%gdculver%cmann))   deallocate (gdp%gdculver%cmann  , STAT = istat)
        if (associated(gdp%gdculver%htcul))   deallocate (gdp%gdculver%htcul  , STAT = istat)
        if (associated(gdp%gdculver%numrel1)) deallocate (gdp%gdculver%numrel1, STAT = istat)
        if (associated(gdp%gdculver%numrel2)) deallocate (gdp%gdculver%numrel2, STAT = istat)
        if (associated(gdp%gdculver%numrel3)) deallocate (gdp%gdculver%numrel3, STAT = istat)
        if (associated(gdp%gdculver%poscul))  deallocate (gdp%gdculver%poscul , STAT = istat)
        if (associated(gdp%gdculver%wetar1))  deallocate (gdp%gdculver%wetar1 , STAT = istat)
        if (associated(gdp%gdculver%wetar2))  deallocate (gdp%gdculver%wetar2 , STAT = istat)
        if (associated(gdp%gdculver%wetar3))  deallocate (gdp%gdculver%wetar3 , STAT = istat)
        if (associated(gdp%gdculver%wtcul))   deallocate (gdp%gdculver%wtcul  , STAT = istat)
        if (associated(gdp%gdculver%dll_name    )) deallocate (gdp%gdculver%dll_name    , STAT = istat)
        if (associated(gdp%gdculver%dll_function)) deallocate (gdp%gdculver%dll_function, STAT = istat)
        if (associated(gdp%gdculver%dll_handle  )) deallocate (gdp%gdculver%dll_handle  , STAT = istat)
    endif
    deallocate (gdp%gdculver, STAT = istat)
    call clrflwpar(istat, gdp)
    deallocate (gdp%gdflwpar, STAT = istat)
    !
    call clrsedtra(istat, gdp%gderosed)
    call clrsedpar(istat, gdp%gdsedpar)
    call clrmorpar(istat, gdp%gdmorpar)
    call clrtrapar(istat, gdp%gdtrapar)
    istat = clrmorlyr(gdp%gdmorlyr)
    deallocate (gdp%gdmorlyr , STAT = istat)
    call clrdredge(istat, gdp%gddredge)
    deallocate (gdp%gddredge , STAT = istat)
    deallocate (gdp%gdtrapar , STAT = istat)
    deallocate (gdp%gderosed , STAT = istat)
    if (associated(gdp%gdsdu%sdu_t0))     deallocate (gdp%gdsdu%sdu_t0    , STAT = istat)
    if (associated(gdp%gdsdu%sdu_tp))     deallocate (gdp%gdsdu%sdu_tp    , STAT = istat)
    if (associated(gdp%gdsdu%sdu_tn))     deallocate (gdp%gdsdu%sdu_tn    , STAT = istat)
    deallocate (gdp%gdsdu    , STAT = istat)    
    deallocate (gdp%gdsedpar , STAT = istat)
    deallocate (gdp%gdmorpar , STAT = istat)
    call clrmassbal(istat, gdp)
    deallocate (gdp%gdmassbal, STAT = istat)
    deallocate (gdp%gdf0isf1 , STAT = istat)
    if (associated(gdp%gdincbc%cwidth))     deallocate (gdp%gdincbc%cwidth    , STAT = istat)
    if (associated(gdp%gdincbc%zavg))       deallocate (gdp%gdincbc%zavg      , STAT = istat)
    deallocate (gdp%gdincbc  , STAT = istat)
    deallocate (gdp%gdincbcc , STAT = istat)
    deallocate (gdp%gdincwav , STAT = istat)
    deallocate (gdp%gdinibcc , STAT = istat)
    call cleartable(gdp%gdinibct%tseriesfile)
    deallocate (gdp%gdinibct , STAT = istat)
    deallocate (gdp%gdinidis , STAT = istat)
    if (associated(gdp%gdpostpr%smlay))     deallocate (gdp%gdpostpr%smlay    , STAT = istat)
    if (associated(gdp%gdpostpr%shlay))     deallocate (gdp%gdpostpr%shlay    , STAT = istat)
    deallocate (gdp%gdpostpr , STAT = istat)
    deallocate (gdp%gdrestart, STAT = istat)
    if (gdp%gdrtc%rtcmod /= noRTC) then
       if (associated(gdp%gdrtc%mnrtcsta))    deallocate (gdp%gdrtc%mnrtcsta   , STAT = istat)
       if (associated(gdp%gdrtc%mnrtcsta_gl)) deallocate (gdp%gdrtc%mnrtcsta_gl, STAT = istat)
       if (associated(gdp%gdrtc%inodertcsta)) deallocate (gdp%gdrtc%inodertcsta, STAT = istat)
       if (associated(gdp%gdrtc%namrtcsta))   deallocate (gdp%gdrtc%namrtcsta  , STAT = istat)
       if (associated(gdp%gdrtc%r0rtcsta))    deallocate (gdp%gdrtc%r0rtcsta   , STAT = istat)
       if (associated(gdp%gdrtc%s1rtcsta))    deallocate (gdp%gdrtc%s1rtcsta   , STAT = istat)
       if (associated(gdp%gdrtc%zrtcsta))     deallocate (gdp%gdrtc%zrtcsta    , STAT = istat)
    endif
    deallocate (gdp%gdrtc, STAT = istat)
    if (localscour) then
       if (associated(gdp%gdscour%nmapp))     deallocate (gdp%gdscour%nmapp    , STAT = istat)
       if (associated(gdp%gdscour%nmref))     deallocate (gdp%gdscour%nmref    , STAT = istat)
       if (associated(gdp%gdscour%factor))    deallocate (gdp%gdscour%factor   , STAT = istat)
       if (associated(gdp%gdscour%tauv))      deallocate (gdp%gdscour%tauv     , STAT = istat)
       if (associated(gdp%gdscour%depchange)) deallocate (gdp%gdscour%depchange, STAT = istat)
    endif
    deallocate (gdp%gdscour   , STAT = istat)
    deallocate (gdp%gdsnel    , STAT = istat)
    deallocate (gdp%gdtimers  , STAT = istat)
    deallocate (gdp%gdtricom  , STAT = istat)
    if (associated(gdp%gdtrisol%ustokes))     deallocate (gdp%gdtrisol%ustokes , STAT = istat)
    if (associated(gdp%gdtrisol%vstokes))     deallocate (gdp%gdtrisol%vstokes , STAT = istat)
    deallocate (gdp%gdtrisol  , STAT = istat)
    deallocate (gdp%gdu_ppr   , STAT = istat)
    deallocate (gdp%gdupdbcc  , STAT = istat)
    deallocate (gdp%gdupdbct  , STAT = istat)
    deallocate (gdp%gdupddis  , STAT = istat)
 !   call clrwaqpar(istat, gdp)
    deallocate (gdp%gdwaqpar  , STAT = istat)
    deallocate (gdp%gdwrirst  , STAT = istat)
    deallocate (gdp%gdwrline  , STAT = istat)
    deallocate (gdp%gdz_initcg, STAT = istat)
    !
    deallocate (gdp%dd      , STAT = istat)
    !
    deallocate (gdp%runid   , STAT = istat)
    !
    if (parll) then
        do i = 1,2
            if (i==1) then
                partp => gdp%gdparall
            else
                partp => gdp%iopartit
            endif
            !
            if (associated(partp%iblkad))       deallocate (partp%iblkad      , STAT = istat)
            if (associated(partp%iweig ))       deallocate (partp%iweig       , STAT = istat)
            if (associated(partp%order_tra ))   deallocate (partp%order_tra   , STAT = istat)
            if (associated(partp%order_sta ))   deallocate (partp%order_sta   , STAT = istat)
            if (associated(partp%mnit_global )) deallocate (partp%mnit_global , STAT = istat)
            if (associated(partp%iarrc       )) deallocate (partp%iarrc       , STAT = istat)
            if (associated(partp%nf          )) deallocate (partp%nf          , STAT = istat)
            if (associated(partp%nl          )) deallocate (partp%nl          , STAT = istat)
            if (associated(partp%mf          )) deallocate (partp%mf          , STAT = istat)
            if (associated(partp%ml          )) deallocate (partp%ml          , STAT = istat)
       enddo
    endif
    deallocate (gdp%gdparall , STAT = istat)
    deallocate (gdp%iopartit , STAT = istat)
    !
    !success = free(gdp%gd_ECHandle)
    deallocate (gdp%uniqueid , STAT = istat)
    deallocate (gdp%arch     , STAT = istat)
    deallocate (gdp%errorcode, STAT = istat)
    !
    call tree_destroy(gdp%input_tree)
    !
    call sbuff_dealloc
end subroutine gdp_dealloc

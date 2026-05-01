module m_source_sink
   use precision, only: dp

   implicit none(type, external)

   ! Source/sink counters.
   integer :: num_source_sink !< [-] number of source/sinks in the model. {former:numsrc}
   integer :: num_source_sink_oldfile !< [-] number of source/sinks in old extforce file. {former:numsrc_old}
   integer :: num_source_sink_for_nearfield !< [-] number of source/sinks added for near field. {former:numsrc_nf}
   integer :: max_source_sink_polyline_points !< [-] maximum number of points in source_sink_x, source_sink_y over all sources/sinks. Used for array dimensions. {former:msrc}

   ! Source/sink identification and geometry. All except _indices are read from the extforce file.
   character(len=255), dimension(:), allocatable :: source_sink_name !< [-] Name of the source/sink. {size=(num_source_sink), former:srcname}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_x !< [m] x-coordinates of source/sink. {size=(num_source_sink,max_source_sink_polyline_points), former:xsrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_y !< [m] y-coordinates of source/sink. {size=(num_source_sink,max_source_sink_polyline_points), former:ysrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_z_bottom !< [m] z-level of bottom sink (1) and source (2). {size=(2,num_source_sink), former:zsrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_z_top !< [m] z-level of top sink (1) and source (2). {size=(2,num_source_sink), former:zsrc2}
   integer, dimension(:,:), allocatable :: source_sink_indices !< [-] Index array of source/sinks. 1 = nodenr sink, 2 = kbot sink, 3 = ktop sink, 4 = nodenr source, 5 = kbot source, 6 = ktop source. {size=(6,num_source_sink), former:ksrc}

   ! Momentum variables. _area is specified in the extforce file.
   real(kind=dp), dimension(:), allocatable :: source_sink_area !< [m2] area of source/sink. If zero, source/sink transport no momentum. {size=(num_source_sink), former:arsrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_discharge_cosine !< [-] Cosine of discharge on sink side (1) and source side (2). {size=(2,num_source_sink), former:cssrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_discharge_sine !< [-] Sine of discharge on sink side (1) and source side (2). {size=(2,num_source_sink), former:snsrc}

   ! Discharge variables. _all_discharges is read out from the *.tim/*.bc file using the EC module, while _water_discharge and _constituents are used for internal computations.
   real(kind=dp), dimension(:,:), allocatable, target :: source_sink_all_discharges !< [m3/s,ppt,degC,kg/m3] Source/sink water discharge (1) and constituent concentrations of discharge (2:). {size=(numconst+1,num_source_sink), former:qstss}
   real(kind=dp), dimension(:), target, allocatable :: source_sink_water_discharge !< [m3/s] Water discharge of source/sink. {size=(num_source_sink), former:qsrc}
   real(kind=dp), dimension(:,:), allocatable :: source_sink_constituents !< [ppt,degC,kg/m3] Constituents of source/sink discharges. {size=(numconst,num_source_sink), former:ccsrc}

   ! Miscellaneous variables.
   real(kind=dp), dimension(:,:), allocatable :: source_sink_reduction !< [-] Source/sink reduction array for partitioned models. {size=(2*(numconst+1),num_source_sink), former:srsn}   
   integer, dimension(:), allocatable :: source_sink_max_xy_points !< [-] Maximum number of points per source.sink in source_sink_x, source_sink_y. {size=(num_source_sink), former:nxsrc}
   integer, dimension(:), allocatable :: source_sink_extraction_warning !< [-] Issue a warning message if the extraction flux exceeds the cell volume (0 = no message, 1 = sink extraction too large, 2 = source extraction too large). {size=(num_source_sink), former:jamess}
   logical :: source_sink_add_k_to_turkin !< [-] Add k of sources to turkin (.false. = no, .true. = yes). {former:addksources}

   ! Cumulative volume and discharge variables. Used in output and for waq coupling.
   real(kind=dp), dimension(:), target, allocatable :: source_sink_cumulative_volume !< [m3] Cumulative volume at each source/sink from Tstart to now. {size=(num_source_sink), former:vsrccum}
   real(kind=dp), dimension(:), target, allocatable :: source_sink_cumulative_volume_previous !< [m3] Cumulative volume at each source/sink from Tstart to the previous His-output time. {size=(num_source_sink), former:vsrccum_pre}
   real(kind=dp), dimension(:), target, allocatable :: source_sink_average_discharge_previous !< [m3/s] Average discharge in the past his-interval at each source/sink. {size=(num_source_sink), former:qsrcavg}
   integer, dimension(:), allocatable :: source_sink_waq_index !< [-] Index array to map source/sink to waq source/sink arrays. {size=(num_source_sink), former:ksrcwaq}
   real(kind=dp), dimension(:), allocatable :: source_sink_cumulative_discharge_waq !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep. {size=(num_source_sink), former:qsrcwaq}
   real(kind=dp), dimension(:), allocatable :: source_sink_cumulative_discharge_waq_previous !< [m3/s] Cumulative discharge at each source/sink within current waq-timestep at the beginning of the time step before possible reduction. {size=(num_source_sink), former:qsrcwaq0}

end module m_source_sink

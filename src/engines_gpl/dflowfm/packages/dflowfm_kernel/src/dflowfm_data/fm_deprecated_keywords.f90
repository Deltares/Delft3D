!> All FM specific deprecated keyword definitions
module fm_deprecated_keywords
   use m_deprecation

   implicit none

   type(deprecated_keyword_set), target :: deprecated_mdu_keywords, deprecated_ext_keywords

contains

!> subroutine that initialises all deprecated keyword sets
   subroutine default_fm_deprecated_keywords()

      if (allocated(deprecated_mdu_keywords%deprecated_keyword_list)) then
         deallocate (deprecated_mdu_keywords%deprecated_keyword_list, deprecated_ext_keywords%deprecated_keyword_list)
      end if
      allocate (deprecated_mdu_keywords%deprecated_keyword_list(100), deprecated_ext_keywords%deprecated_keyword_list(100))

      deprecated_mdu_keywords%additional_information = 'Check Section A.4 in the User Manual for information on how to update this input file.'
      deprecated_ext_keywords%additional_information = 'Check Section C.5 in the User Manual for information on how to update this input file.'
      deprecated_mdu_keywords%count = 0
      deprecated_ext_keywords%count = 0

      call add_deprecated_keyword(deprecated_mdu_keywords, 'processes', 'dtmassbalance', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'processes', 'wriwaqbot3doutput', OBSOLETE, 'Remove it or use [Output] wrihis_wqbot3d and wrimap_wqbot3d instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'bathymetryfile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'bedlevelfile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'botlevuni', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'botlevtype', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'ithindykescheme', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'manholefile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'geometry', 'nooptimizedpolygon', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'numerics', 'hkad', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'numerics', 'ithindykescheme', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'numerics', 'thindykecontraction', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'numerics', 'transportmethod', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'numerics', 'transporttimestepping', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'output', 'writebalancefile', OBSOLETE)

   end subroutine default_fm_deprecated_keywords

end module fm_deprecated_keywords

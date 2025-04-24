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

      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'dtMassBalance', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'wriWaqBot3dOutput', OBSOLETE, 'Remove it or use [Output] wriHis_wqBot3d and wriMap_wqBot3d instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'bathymetryFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'bedLevelFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'botLevUni', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'botLevType', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'iThinDykeScheme', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'manholeFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'noOptimizedPolygon', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'hkad', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'iThinDykeScheme', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'thinDykeContraction', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'transportMethod', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'transportTimeStepping', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'barocZLayBed', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'orgBarocKeywords', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'barocTerm', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'barocTimeInt', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'jaDrhoDz', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'writeBalanceFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'type', DEPRECATED, 'Use [Lateral] locationType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'flow', DEPRECATED, 'Use [Lateral] discharge instead.')

   end subroutine default_fm_deprecated_keywords

end module fm_deprecated_keywords

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

      deprecated_mdu_keywords%additional_information = 'Check the User Manual appendix about the Master Definition file for information on how to update this input file.'
      deprecated_ext_keywords%additional_information = 'Check the User Manual appendix about the external forcings file for information on how to update this input file.'
      deprecated_mdu_keywords%count = 0
      deprecated_ext_keywords%count = 0

      ! Adding DEPRECATED MDU keywords
      call add_deprecated_keyword(deprecated_mdu_keywords, 'General', 'AutoStart', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'OrgFloorlevtoplaydef', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'circumcenter', DEPRECATED, 'Use circumcenterMethod instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'Vertadvtypsal', DEPRECATED, 'Use verticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'Vertadvtyptem', DEPRECATED, 'Use verticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'ThetaVertical', DEPRECATED, 'Use VerticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'dtMassBalance', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'type', DEPRECATED, 'Use [Lateral] locationType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'flow', DEPRECATED, 'Use [Lateral] discharge instead.')

      ! Adding OBSOLETE MDU keywords
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
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'FacLaxTurb', OBSOLETE, 'Use [Numerics] turbulenceTimeIntegrationFactor instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'FacLaxTurbHor', OBSOLETE, 'Use [Numerics] turbulenceTimeIntegrationMethod instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'FacLaxTurbVer', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'epsTKE', OBSOLETE, 'Use [Physics] TKEMin instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'epsEPS', OBSOLETE, 'Use [Physics] EPSMin (k-epsilon turbulence model) or [Physics] TAUmin (k-tau turbulence model) instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'Allowcoolingbelowzero', OBSOLETE, &
                                  'Consider using MDU-keyword salinityDependentFreezingPoint to allow cooling below zero degrees Celsius.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'RhoairRhowater', OBSOLETE, &
                                  'This keyword is replaced with rhoWaterInWindStress in the [Wind] block in the MDU-file.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'wriWaqBot3dOutput', OBSOLETE, 'Remove it or use [Output] wriHis_wqBot3d and wriMap_wqBot3d instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'writeBalanceFile', OBSOLETE)

   end subroutine default_fm_deprecated_keywords

end module fm_deprecated_keywords

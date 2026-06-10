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
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'sigmaGrowthFactor', DEPRECATED, 'Use zLayerGrowthFactor instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'Keepzlayeringatbed', DEPRECATED, 'Use [numerics] keepZLayeringAtBed instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'circumcenterMethod', DEPRECATED, 'Once the keyword is removed/becomes obsolete, the "allNetlinksLoop" method will be used.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'circumcenterTolerance', DEPRECATED, 'Once the keyword is removed/becomes obsolete, a fixed tolerance will be used.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'maxItVerticalForesterSal', DEPRECATED, 'Use maxItVerticalForester instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'maxItVerticalForesterTem', DEPRECATED, 'Use maxItVerticalForester instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'Vertadvtypsal', DEPRECATED, 'Use verticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'Vertadvtyptem', DEPRECATED, 'Use verticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'Jadelvappos', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'SecchiDepth2', DEPRECATED, 'Use SecchiDepthNonPenetrative instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'SecchiDepth2Fraction', DEPRECATED, 'Use SecchiDepthNonPenetrativeFraction instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'ThetaVertical', DEPRECATED, 'Use VerticalAdvectionType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'dtMassBalance', DEPRECATED)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'type', DEPRECATED, 'Use [Lateral] locationType instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Lateral', 'flow', DEPRECATED, 'Use [Lateral] discharge instead.')

      ! Adding OBSOLETE MDU keywords
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Model', 'mduFormatVersion', OBSOLETE, 'Use [General] fileVersion (with version >= 1.07) instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'bathymetryFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'bedLevelFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'botLevUni', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'botLevType', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'iThinDykeScheme', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'manholeFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'noOptimizedPolygon', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'circumcenter', OBSOLETE, 'Use [Geometry] circumcenterMethod instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', 'thinDykeFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Geometry', '1dNetworkFile', OBSOLETE, 'Use individual keywords such as [Geometry] StructureFile, CrossDefFile, etc. instead.')
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
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'jaOrgSethu', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'CFLWaveFrac', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'jaEmbed1d', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'sobekdfm_umin', OBSOLETE, 'Use [Numerics] lateral_fixedweir_umin instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'sobekdfm_umin_method', OBSOLETE, 'Use [Numerics] lateral_fixedweir_umin_method instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'sobekdfm_minimal_1d2d_embankment', OBSOLETE, 'Use [Numerics] lateral_fixedweir_1d2d_embankment instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'relax', OBSOLETE, 'Use [Numerics] lateral_fixedweir_relax instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'wridia_viscosity_diffusivity_limit', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Numerics', 'qhrelax', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'allowCoolingBelowZero', OBSOLETE, &
                                  'Consider using MDU-keyword salinityDependentFreezingPoint to allow cooling below zero degrees Celsius.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'RhoairRhowater', OBSOLETE, &
                                  'This keyword is replaced with rhoWaterInWindStress in the [Wind] block in the MDU-file.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'effectSpiral', OBSOLETE, 'Use Espir contained in MorFile instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Physics', 'stericCorrection', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Sedtrails', 'allowCoolingBelowZero', OBSOLETE, &
                                  'Consider using MDU-keyword salinityDependentFreezingPoint to allow cooling below zero degrees Celsius.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Wind', 'gapres', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Waves', 'waveNikuradse', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Time', 'autoTimeStepDiff', OBSOLETE, 'Use [Time] autoTimeStepVisc instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Trachytopes', 'trtdt', OBSOLETE, 'Use [Trachytopes] dtTrt instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'writeBalanceFile', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 's1incInterval', OBSOLETE, 'Use [Output] classMapFile instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'waqFileBase', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'snapshotdir', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'heatFluxesOnOutput', OBSOLETE, 'Use [Output] wrihis_heatfluxes and wrimap_heatfluxes instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'wrimap_input_dt', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'wrihis_heatflux', OBSOLETE, 'Renamed to [Output] wrihis_heatfluxes.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Output', 'wrishp_enc', OBSOLETE)
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'wriWaqBot3dOutput', OBSOLETE, 'Remove it or use [Output] wriHis_wqBot3d and wriMap_wqBot3d instead.')
      call add_deprecated_keyword(deprecated_mdu_keywords, 'Processes', 'processFluxIntegration', OBSOLETE)

   end subroutine default_fm_deprecated_keywords

end module fm_deprecated_keywords

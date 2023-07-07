# Set directories
set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/../src")

# Configuration below based on original package's Makefile.am/proj.vcxproj,
# only for generic parts and libproj library and only for Windows.

# Gather source files
set(include_HEADERS proj_api.h projects.h geodesic.h
                    org_proj4_Projections.h org_proj4_PJ.h)

set(libproj_la_SOURCES pj_list.h
    PJ_aeqd.c PJ_gnom.c PJ_laea.c PJ_mod_ster.c 
    PJ_nsper.c PJ_nzmg.c PJ_ortho.c PJ_stere.c PJ_sterea.c 
    PJ_aea.c PJ_bipc.c PJ_bonne.c PJ_eqdc.c PJ_isea.c 
    PJ_imw_p.c PJ_krovak.c PJ_lcc.c PJ_poly.c 
    PJ_rpoly.c PJ_sconics.c proj_rouss.c 
    PJ_cass.c PJ_cc.c PJ_cea.c PJ_eqc.c 
    PJ_gall.c PJ_labrd.c PJ_lsat.c PJ_misrsom.c PJ_merc.c 
    PJ_mill.c PJ_ocea.c PJ_omerc.c PJ_somerc.c 
    PJ_tcc.c PJ_tcea.c PJ_times.c PJ_tmerc.c 
    PJ_airy.c PJ_aitoff.c PJ_august.c PJ_bacon.c 
    PJ_chamb.c PJ_hammer.c PJ_lagrng.c PJ_larr.c 
    PJ_lask.c PJ_nocol.c PJ_ob_tran.c PJ_oea.c 
    PJ_tpeqd.c PJ_vandg.c PJ_vandg2.c PJ_vandg4.c 
    PJ_wag7.c PJ_lcca.c PJ_geos.c proj_etmerc.c 
    PJ_boggs.c PJ_collg.c PJ_comill.c PJ_crast.c PJ_denoy.c 
    PJ_eck1.c PJ_eck2.c PJ_eck3.c PJ_eck4.c 
    PJ_eck5.c PJ_fahey.c PJ_fouc_s.c PJ_gins8.c PJ_gstmerc.c 
    PJ_gn_sinu.c PJ_goode.c PJ_igh.c PJ_hatano.c PJ_loxim.c 
    PJ_mbt_fps.c PJ_mbtfpp.c PJ_mbtfpq.c PJ_moll.c 
    PJ_nell.c PJ_nell_h.c PJ_patterson.c PJ_putp2.c PJ_putp3.c 
    PJ_putp4p.c PJ_putp5.c PJ_putp6.c PJ_qsc.c PJ_robin.c 
    PJ_sch.c PJ_sts.c PJ_urm5.c PJ_urmfps.c PJ_wag2.c 
    PJ_wag3.c PJ_wink1.c PJ_wink2.c pj_latlong.c pj_geocent.c 
    aasincos.c adjlon.c bch2bps.c bchgen.c 
    biveval.c dmstor.c mk_cheby.c pj_auth.c 
    pj_deriv.c pj_ell_set.c pj_ellps.c pj_errno.c 
    pj_factors.c pj_fwd.c pj_init.c pj_inv.c pj_fwd3d.c pj_inv3d.c
    pj_list.c pj_malloc.c pj_mlfn.c pj_msfn.c proj_mdist.c 
    pj_open_lib.c pj_param.c pj_phi2.c pj_pr_list.c 
    pj_qsfn.c pj_generic_selftest.c pj_run_selftests.c pj_strerrno.c 
    pj_tsfn.c pj_units.c pj_ctx.c pj_log.c pj_zpoly1.c rtodms.c 
    vector1.c pj_release.c pj_gauss.c 
    PJ_healpix.c PJ_natearth.c PJ_natearth2.c PJ_calcofi.c pj_fileapi.c 
    pj_gc_reader.c pj_gridcatalog.c 
    nad_cvt.c nad_init.c nad_intr.c emess.c emess.h 
    pj_apply_gridshift.c pj_datums.c pj_datum_set.c pj_transform.c 
    geocent.c geocent.h pj_utils.c pj_gridinfo.c pj_gridlist.c 
    jniproj.c pj_mutex.c pj_initcache.c pj_apply_vgridshift.c geodesic.c 
    pj_strtod.c
)

list(TRANSFORM include_HEADERS PREPEND ${source_path}/)
list(TRANSFORM libproj_la_SOURCES PREPEND ${source_path}/)

# Define library
set(library_name proj)
add_library(${library_name} ${include_HEADERS}
                            ${libproj_la_SOURCES})

# Add additional includes
target_include_directories(${library_name} PRIVATE ${source_path})

# Set preprocessor definitions
if (WIN32)
    target_compile_definitions(${library_name} PRIVATE MUTEX_win32;PROJ_LIB="C:/OSGeo4W/share")
endif(WIN32)

# Create the folder structure in Visual Studio
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${headers} ${source} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/fortrangis)
//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
/*  gnf_data.h - Define the information for particular file types
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the actual information on the various file types
 *  that are suppoted via the Generic NEFIS routines.
*/

/*
 *  $Author: Markus $
 *  $Date: 12/06/06 15:12 $
 *  $Source$
*/

#ifndef GNF_DATA_H_INCLUDED
#define GNF_DATA_H_INCLUDED

/* Array holding the various date/time structures
   Note:
   Easiest to implement as a single array. As the number is of the order
   of the number of file types, this will not present any particular
   problem in terms of lengthy initialisations.
   CONSTANT:     for all parameters that are independent of time
   FLOW-TIME:    for map-series from Delft3D-FLOW map files (scale: from minutes to seconds)
   FLOW-TIMESED: for map-sed-series from Delft3D-FLOW map files
   FLOW-HISTIME: for his-series from Delft3D-FLOW history files
   FLOW-BALTIME: for bal-his-series from Delft3D-FLOW history files
   FLOW-SEDTIME: for his-series from Delft3D-FLOW history files (sediment quantities)
   FLOW-WAVTIME: for his-series from Delft3D-FLOW history files (wave quantities)
   COMM-BOTTIM:  for BOTTIM from Delft3D communication files
   COMM-CURTIM:  for CURTIM from Delft3D communication files
   COMM-DWQTIM:  for DWQTIM from Delft3D communication files
   COMM-WAVTIM:  for WAVTIM from Delft3D communication files
   COMM-TRATIM:  for TRANSTIM from Delft3D communication files
*/
static DateTimeInfoStruct nefis_time_data[] = {
"CONSTANT",    CONSTANT, "", "", "", "", "", "", "", "", 0.0,
"FLOW-TIME",   ITDATE, "map-info-series",   "ITMAPC", "map-const", "ITDATE", "map-const", "DT", "map-const", "TUNIT", 60.0,
"FLOW-TIMESED",ITDATE, "map-infsed-serie",  "ITMAPS", "map-const", "ITDATE", "map-const", "DT", "map-const", "TUNIT", 60.0,
"FLOW-TIMEAVG",ITDATE, "map-infavg-serie",  "ITAVGS", "map-const", "ITDATE", "map-const", "DT", "map-const", "TUNIT", 60.0,
"FLOW-TIMEROL",ITDATE, "map-infrol-serie",  "ITMAPS", "map-const", "ITDATE", "map-const", "DT", "map-const", "TUNIT", 60.0,
"FLOW-HISTIME",ITDATE, "his-info-series",   "ITHISC", "his-const", "ITDATE", "his-const", "DT", "his-const", "TUNIT", 60.0,
"FLOW-BALTIME",ITDATE, "his-bal-series",    "ITHISC", "his-const", "ITDATE", "his-const", "DT", "his-const", "TUNIT", 60.0,
"FLOW-SEDTIME",ITDATE, "his-infsed-serie",  "ITHISS", "his-const", "ITDATE", "his-const", "DT", "his-const", "TUNIT", 60.0,
"FLOW-WAVTIME",ITDATE, "his-infwav-serie",  "ITHISW", "his-const", "ITDATE", "his-const", "DT", "his-const", "TUNIT", 60.0,
"FLOW-DADTIME",IDTISO, "his-dad-series"  ,  "DATE_TIME",
                                                      ""         , ""      , ""         , ""  , ""         , ""     ,  1.0,
"FLOW-DISCHTM",IDTISO, "his-dis-series"  ,  "DATE_TIME",
                                                      ""         , ""      , ""         , ""  , ""         , ""     ,  1.0,
"COMM-BOTTIM" ,IT0102, "BOTTIM"         ,   "TIMBOT", "PARAMS"   , "IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"COMM-CURTIM" ,IT0102, "CURTIM"         ,   "TIMCUR", "PARAMS"   , "IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"COMM-DWQTIM" ,IT0102, "DWQTIM"         ,   "TIMCUR", "PARAMS"   , "IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"COMM-WAVTIM" ,IT0102, "WAVTIM"         ,   "TIMWAV", "PARAMS"   , "IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"COMM-TRATIM" ,IT0102, "TRANSTIM"       ,   "TSEDE" , "PARAMS"   , "IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"BOTH-BOTTIM" ,ITNONE, "HISBOTTIM"      ,   "ITBODE", ""         , ""       ,"HISBOT"   , "TSCALE", "HISBOT"   , "",   1.0, /* ??? */
"BOTM-BOTTIM" ,ITNONE, "MAPBOTTIM"      ,   "ITBODE", ""         , ""       ,"MAPBOT"   , "TSCALE", "MAPBOT"   , "",   1.0, /* ??? */
"HWGXY-TIME"  ,IT0102, "map-series"     ,   "TIME",   "PARAMS"   ,"IT01|IT02",
                                                                             "PARAMS"   , "TSCALE", "PARAMS"   , "",   1.0, /* ??? */
"TRAM-MPTTIME",IT0102, "MAPTTRAN"       ,   "T-TRAN", "MAPATRANNTR", "ITO1|ITO2"
                                                                            ,"MAPATRANNTR","TSCALE","MAPATRANNTR","",  1.0, /* ??? */
"TRAH-HISTIM" ,IT0102, "HISTRAN"        ,   "HISTIME","HISLTRAB" , "ITO1|ITO2"
                                                                            ,"HISLTRAB" , "TSCALE", "HISLTRAB" , "",   1.0, /* ??? */
"-none-",      END_OF_DATE_TYPES, "", "", "", "", "", "", "", "", 0.0
} ;

/* Array holding the various location structures
   Note:
   Easiest to implement as a single array. As the number is of the order
   of the number of file types, this will not present any particular
   problem in terms of lengthy initialisations.
   FLOW-HIS-LOCS: Delft3D-Flow history files
   FLOW-MAP-GRID: Delft3D-Flow map files
   COMM-MAP-GRID: Delft3D communication files

   TODO:
   TRAH-files: too complicated right now ...

   Note:
   CODW encoding is slightly different tha KCS, but the algorithm covers both
   (Delft-GPP does not need all the details)
*/
static LocationInfoStruct nefis_location_data[] = {
"FLOW-HIS-LOCS",  LOC_NAMES, "his-const", "NAMST"  , ""          , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-BAL-LOCS",  BAL_NAMES, "his-bal-const",
                                          "BALVOLNAMES"  , ""    , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-POLYGONS",  LOC_NAMES, "his-bal-const",
                                          "BALVOLNAMES"  , ""    , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-DAD-LOCS",  LOC_NAMES, "his-dad-const",
                                          "DREDGE_AREAS"  , ""    , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-DIS-LOCS",  LOC_NAMES, "his-dis-const",
                                          "DISCHARGES"    , ""    , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-HIS-TRANS", LOC_NAMES, "his-const", "NAMTRA" , ""          , ""     , "his-const", "DPS", "his-const", "ZK", "his-const", "THICK", "his-const", "ALFAS",
"FLOW-MAP-GRID",  GRID_KCSINV,
                             ""         , ""       , "map-const" , "KCS"  , "map-const", "DP0", "map-const", "ZK", "map-const", "THICK", "map-const", "ALFAS",
"COMM-MAP-GRID",  GRID_KCS,  ""         , ""       , "KENMCNST|TEMPOUT",
                                                                   "KCS|CODW","", "", "GRID", "ZK", "GRID", "THICK", "", "",
"BOTH-HIS-LOCS",  LOC_NAMES, "HISBOT"   , "NAMSTD" , ""          , ""     , "", "", "", "", "", "", "", "",
"BOTM-MAP-GRID",  GRID_KCS,  ""         , ""       , "TEMPOUT"   , "CODW" , "", "", "", "", "", "", "", "",
"TRAH-HIS-SED" ,  LOC_IDS,   "HISBOT"   , "NAMSTD" , ""          , ""     , "", "", "", "", "", "", "", "",
"TRAH-HIS-SED2",  LOC_IDS,   "HISBOT"   , "NAMSTD" , ""          , ""     , "", "", "", "", "", "", "", "",
"TRAM-MAP-GRID",  GRID_KCS,  ""         , ""       , "TEMPOUT"   , "CODW" , "", "", "", "", "", "", "", "",
"HWGXY-GRID",     GRID_KCSINV,
                             ""         , ""       , "map-series", "CODE" , "", "", "", "", "", "", "", "",
"TRAH-HIS-LOCS",  LOC_IDS,   "HISLTRAN" , "MC"     , "HISLTRAN"  , "NC"   , "", "", "", "", "", "", "", "",
"TRAH-SECT-X"  ,  LOC_IDS,   "HISLTRAN" , "MITX"   , "HISLTRAN"  , "NIT1" , "", "", "", "", "", "", "", "",
"TRAH-SECT-Y"  ,  LOC_IDS,   "HISLTRAN" , "MIT1"   , "HISLTRAN"  , "NITY" , "", "", "", "", "", "", "", "",
"TRAH-SECT-TX" ,  LOC_IDS,   "HISLTRAN" , "MITTX"  , "HISLTRAN"  , "NITT1", "", "", "", "", "", "", "", "",
"TRAH-SECT-TY" ,  LOC_IDS,   "HISLTRAN" , "MITT1"  , "HISLTRAN"  , "NITTY", "", "", "", "", "", "", "", "",
"-none-",         END_OF_LOC_TYPES, ""  , ""       , ""          , ""     , "", "", "", "", "", "", "", ""
} ;

/* Problem:
   - total water depth (use the special convention @)
     In original source: DPS is calculated from DRYFLP and DP0
   - names of turbulent quantities - filled up from LSTCI or not?

   Note:
   The z-coordinate is the first parameter, because then we can easily find it!
   (the depth-averaged parameters require it)
*/

static ParameterInfoStruct nefis_unknown_param_data[] = {
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_flow_map_param_data[] = {
"z-coordinate"        , COORD_DATA_3D      , "map-series"         , "S1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        ,
                                             "map-const"          , "DP0"                , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"water level"         , SCALAR_DATA        , "map-series"         , "S1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"total water depth"   , SCALAR_DATA_SUMMED , "map-series"         , "S1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SPECIAL_DEPTH_CENTRE ,
                                             "map-const"          , "DP0"                , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"depth (cell-average)", SPECIAL_DEPTH_CENTRE ,
                                             "map-const"          , "DP0"                , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Depth-vertex"        , SCALAR_DATA        ,
                                             "map-const"          , "DP0"                , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"dpt. aver. cur. u"   , DPT_AVERAGE | LOCAL_VECTOR_U
                                           , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"dpt. aver. cur. v"   , DPT_AVERAGE | LOCAL_VECTOR_V
                                           , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"dpt. aver. cur. mag" , DPT_AVERAGE | LOCAL_VECTOR_MAG
                                           , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"dpt. aver. cur. dir" , DPT_AVERAGE | LOCAL_VECTOR_DIR
                                           , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current u"           , LOCAL_VECTOR_U     , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current v"           , LOCAL_VECTOR_V     , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current mag. (horiz)", LOCAL_VECTOR_MAG   , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current dir. (horiz)", LOCAL_VECTOR_DIR   , "map-series"         , "U1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "V1"                 , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"XCOR"                , SCALAR_DATA        , "map-const"          , "XCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"YCOR"                , SCALAR_DATA        , "map-const"          , "YCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current w"           , SCALAR_DATA        , "map-series"         , "WPHY"               , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"eddy diffusivity"    , SCALAR_DATA_FACE   , "map-series"         , "DICWW"              , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"eddy viscosity"      , SCALAR_DATA_FACE   , "map-series"         , "VICWW"              , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"eddy viscosity (hor)", SCALAR_DATA        , "map-series"         , "VICUV"              , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"vorticity"           , SCALAR_DATA        , "map-series"         , "VORTIC"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"enstrophy"           , SCALAR_DATA        , "map-series"         , "ENSTRO"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"non-hyd. pressure"   , SCALAR_DATA        , "map-series"         , "HYDPRES"            , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"density"             , SCALAR_DATA        , "map-series"         , "RHO"                , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"bed stress u"        , LOCAL_VECTOR_U     , "map-series"         , "TAUKSI"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "TAUETA"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"bed stress v"        , LOCAL_VECTOR_V     , "map-series"         , "TAUKSI"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "TAUETA"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"bed stress mag."     , LOCAL_VECTOR_MAG   , "map-series"         , "TAUKSI"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "TAUETA"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"bed stress dir."     , LOCAL_VECTOR_DIR   , "map-series"         , "TAUKSI"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "TAUETA"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"max. bed stress"     , SCALAR_DATA        , "map-series"         , "TAUMAX"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  1, "map-const"         , "NAMCON"             ,  1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  2, "map-const"         , "NAMCON"             ,  2, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  3, "map-const"         , "NAMCON"             ,  3, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  4, "map-const"         , "NAMCON"             ,  4, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  5, "map-const"         , "NAMCON"             ,  5, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  6, "map-const"         , "NAMCON"             ,  6, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  7, "map-const"         , "NAMCON"             ,  7, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  8, "map-const"         , "NAMCON"             ,  8, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 ,  9, "map-const"         , "NAMCON"             ,  9, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 10, "map-const"         , "NAMCON"             , 10, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 11, "map-const"         , "NAMCON"             , 11, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 12, "map-const"         , "NAMCON"             , 12, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 13, "map-const"         , "NAMCON"             , 13, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 14, "map-const"         , "NAMCON"             , 14, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 15, "map-const"         , "NAMCON"             , 15, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 16, "map-const"         , "NAMCON"             , 16, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 17, "map-const"         , "NAMCON"             , 17, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 18, "map-const"         , "NAMCON"             , 18, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 19, "map-const"         , "NAMCON"             , 19, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 20, "map-const"         , "NAMCON"             , 20, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 21, "map-const"         , "NAMCON"             , 21, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 22, "map-const"         , "NAMCON"             , 22, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 23, "map-const"         , "NAMCON"             , 23, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 24, "map-const"         , "NAMCON"             , 24, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 25, "map-const"         , "NAMCON"             , 25, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 26, "map-const"         , "NAMCON"             , 26, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 27, "map-const"         , "NAMCON"             , 27, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 28, "map-const"         , "NAMCON"             , 28, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 29, "map-const"         , "NAMCON"             , 29, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 30, "map-const"         , "NAMCON"             , 30, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 31, "map-const"         , "NAMCON"             , 31, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 32, "map-const"         , "NAMCON"             , 32, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 33, "map-const"         , "NAMCON"             , 33, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 34, "map-const"         , "NAMCON"             , 34, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 35, "map-const"         , "NAMCON"             , 35, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 36, "map-const"         , "NAMCON"             , 36, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 37, "map-const"         , "NAMCON"             , 37, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 38, "map-const"         , "NAMCON"             , 38, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 39, "map-const"         , "NAMCON"             , 39, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 40, "map-const"         , "NAMCON"             , 40, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 41, "map-const"         , "NAMCON"             , 41, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 42, "map-const"         , "NAMCON"             , 42, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 43, "map-const"         , "NAMCON"             , 43, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 44, "map-const"         , "NAMCON"             , 44, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 45, "map-const"         , "NAMCON"             , 45, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 46, "map-const"         , "NAMCON"             , 46, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 47, "map-const"         , "NAMCON"             , 47, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 48, "map-const"         , "NAMCON"             , 48, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 49, "map-const"         , "NAMCON"             , 49, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 50, "map-const"         , "NAMCON"             , 50, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 51, "map-const"         , "NAMCON"             , 51, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 52, "map-const"         , "NAMCON"             , 52, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 53, "map-const"         , "NAMCON"             , 53, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 54, "map-const"         , "NAMCON"             , 54, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 55, "map-const"         , "NAMCON"             , 55, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 56, "map-const"         , "NAMCON"             , 56, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 57, "map-const"         , "NAMCON"             , 57, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 58, "map-const"         , "NAMCON"             , 58, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 59, "map-const"         , "NAMCON"             , 59, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 60, "map-const"         , "NAMCON"             , 60, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 61, "map-const"         , "NAMCON"             , 61, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 62, "map-const"         , "NAMCON"             , 62, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 63, "map-const"         , "NAMCON"             , 63, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 64, "map-const"         , "NAMCON"             , 64, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 65, "map-const"         , "NAMCON"             , 65, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 66, "map-const"         , "NAMCON"             , 66, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 67, "map-const"         , "NAMCON"             , 67, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 68, "map-const"         , "NAMCON"             , 68, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 69, "map-const"         , "NAMCON"             , 69, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 70, "map-const"         , "NAMCON"             , 70, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 71, "map-const"         , "NAMCON"             , 71, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 72, "map-const"         , "NAMCON"             , 72, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 73, "map-const"         , "NAMCON"             , 73, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 74, "map-const"         , "NAMCON"             , 74, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 75, "map-const"         , "NAMCON"             , 75, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 76, "map-const"         , "NAMCON"             , 76, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 77, "map-const"         , "NAMCON"             , 77, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 78, "map-const"         , "NAMCON"             , 78, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 79, "map-const"         , "NAMCON"             , 79, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 80, "map-const"         , "NAMCON"             , 80, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 81, "map-const"         , "NAMCON"             , 81, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 82, "map-const"         , "NAMCON"             , 82, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 83, "map-const"         , "NAMCON"             , 83, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 84, "map-const"         , "NAMCON"             , 84, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 85, "map-const"         , "NAMCON"             , 85, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 86, "map-const"         , "NAMCON"             , 86, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 87, "map-const"         , "NAMCON"             , 87, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 88, "map-const"         , "NAMCON"             , 88, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 89, "map-const"         , "NAMCON"             , 89, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 90, "map-const"         , "NAMCON"             , 90, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 91, "map-const"         , "NAMCON"             , 91, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 92, "map-const"         , "NAMCON"             , 92, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 93, "map-const"         , "NAMCON"             , 93, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 94, "map-const"         , "NAMCON"             , 94, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 95, "map-const"         , "NAMCON"             , 95, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 96, "map-const"         , "NAMCON"             , 96, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 97, "map-const"         , "NAMCON"             , 97, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 98, "map-const"         , "NAMCON"             , 98, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA        , "map-series"         , "R1"                 , 99, "map-const"         , "NAMCON"             , 99, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA_FACE   , "map-series"         , "RTUR1"              ,  2, "map-const"         , "NAMCON"             , LAST-1,
                                                                                                                                                "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"*"                   , SCALAR_DATA_FACE   , "map-series"         , "RTUR1"              ,  1, "map-const"         , "NAMCON"             , LAST-2,
                                                                                                                                                "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"current u (LES)"     , LOCAL_VECTOR_U     , "map-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current v (LES)"     , LOCAL_VECTOR_V     , "map-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current mag (LES)"   , LOCAL_VECTOR_MAG   , "map-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"current dir (LES)"   , LOCAL_VECTOR_DIR   , "map-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Richardson number"   , SCALAR_DATA_FACE   , "map-series"         , "RICH"               , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"Bottom depth (sed)"  , SCALAR_DATA        , "map-sed-series"     , "DPS"                , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Sediment thickness"  , SCALAR_DATA        , "map-sed-series"     , "DPSED"              , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bottom conc. fract 1", SCALAR_DATA        , "map-sed-series"     , "BODSED"             ,  1, NULL                , NULL                 ,  1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bottom conc. fract 2", SCALAR_DATA        , "map-sed-series"     , "BODSED"             ,  2, NULL                , NULL                 ,  2, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bottom conc. fract 3", SCALAR_DATA        , "map-sed-series"     , "BODSED"             ,  3, NULL                , NULL                 ,  3, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bottom conc. fract 4", SCALAR_DATA        , "map-sed-series"     , "BODSED"             ,  4, NULL                , NULL                 ,  4, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bottom conc. fract 5", SCALAR_DATA        , "map-sed-series"     , "BODSED"             ,  5, NULL                , NULL                 ,  5, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Equil. conc. fract 1", SCALAR_DATA_FACE   , "map-sed-series"     , "RSEDEQ"             ,  1, NULL                , NULL                 ,  1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Equil. conc. fract 2", SCALAR_DATA_FACE   , "map-sed-series"     , "RSEDEQ"             ,  2, NULL                , NULL                 ,  2, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Equil. conc. fract 3", SCALAR_DATA_FACE   , "map-sed-series"     , "RSEDEQ"             ,  3, NULL                , NULL                 ,  3, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Equil. conc. fract 4", SCALAR_DATA_FACE   , "map-sed-series"     , "RSEDEQ"             ,  4, NULL                , NULL                 ,  4, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Equil. conc. fract 5", SCALAR_DATA_FACE   , "map-sed-series"     , "RSEDEQ"             ,  5, NULL                , NULL                 ,  5, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Settling.vel.fract 1", SCALAR_DATA        , "map-sed-series"     , "WS"                 ,  1, NULL                , NULL                 ,  1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Settling.vel.fract 2", SCALAR_DATA        , "map-sed-series"     , "WS"                 ,  2, NULL                , NULL                 ,  2, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Settling.vel.fract 3", SCALAR_DATA        , "map-sed-series"     , "WS"                 ,  3, NULL                , NULL                 ,  3, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Settling.vel.fract 4", SCALAR_DATA        , "map-sed-series"     , "WS"                 ,  4, NULL                , NULL                 ,  4, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Settling.vel.fract 5", SCALAR_DATA        , "map-sed-series"     , "WS"                 ,  5, NULL                , NULL                 ,  5, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"Bedload transport u" , LOCAL_VECTOR_U     , "map-sed-series"     , "SBUU"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-sed-series"     , "SBVV"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Bedload transport v" , LOCAL_VECTOR_V     , "map-sed-series"     , "SBUU"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-sed-series"     , "SBVV"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Susp. transport u"   , LOCAL_VECTOR_U     , "map-sed-series"     , "SSUU"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-sed-series"     , "SSVV"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Susp. transport v"   , LOCAL_VECTOR_V     , "map-sed-series"     , "SSUU"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-sed-series"     , "SSVV"               , -1, NULL                , NULL                 , -1, "FLOW-TIMESED", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Near-bed ref.conc."  , SCALAR_DATA        , "map-sed-series"     , "RCA"                , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"Avg. bedload u"      , LOCAL_VECTOR_U     , "map-avg-series"     , "SBUUA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-avg-series"     , "SBVVA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Avg. bedload v"      , LOCAL_VECTOR_V     , "map-avg-series"     , "SBUUA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-avg-series"     , "SBVVA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Avg. susp. u"        , LOCAL_VECTOR_U     , "map-avg-series"     , "SSUUA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-avg-series"     , "SSVVA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"Avg. susp. v"        , LOCAL_VECTOR_V     , "map-avg-series"     , "SSUUA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-avg-series"     , "SSVVA"              , -1, NULL                , NULL                 , -1, "FLOW-TIMEAVG", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT" , NULL, "FLOW-MAP-GRID", NULL,
"UVDAMS"              , ENCODED_DATA_U     , "map-series"         , "KFU"                , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , ENCODED_DATA_V     , "map-series"         , "KFV"                , -1, NULL                , NULL                 , -1, "FLOW-TIME", NULL, "FLOW-MAP-GRID", NULL,
"Short-wave energy"   , SCALAR_DATA        , "map-rol-series"     , "EWAVE1"             , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"Roller energy"       , SCALAR_DATA        , "map-rol-series"     , "EROLL1"             , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"Roller transport u"  , LOCAL_VECTOR_U     , "map-rol-series"     , "QXKR"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "QYKR"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Roller transport v"  , LOCAL_VECTOR_V     , "map-rol-series"     , "QXKR"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "QYKR"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Wave energy transp.u", LOCAL_VECTOR_U     , "map-rol-series"     , "QXKW"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "QYKW"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Wave energy transp.v", LOCAL_VECTOR_V     , "map-rol-series"     , "QXKW"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "QYKW"               , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Wave force u"        , LOCAL_VECTOR_U     , "map-rol-series"     , "FXW"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "FYW"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Wave force v"        , LOCAL_VECTOR_V     , "map-rol-series"     , "FXW"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "FYW"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Roller force u"      , LOCAL_VECTOR_U     , "map-rol-series"     , "WSU"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "WSV"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Roller force v"      , LOCAL_VECTOR_V     , "map-rol-series"     , "WSU"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-rol-series"     , "WSV"                , -1, NULL                , NULL                 , -1, "FLOW-TIMEROL", NULL, "FLOW-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "map-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-MAP-GRID", NULL,
"Wind velocity U"     , SCALAR_DATA        , "map-series"         , "WINDU"              , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
"Wind velocity V"     , SCALAR_DATA        , "map-series"         , "WINDV"              , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
"Air humidity"        , SCALAR_DATA        , "map-series"         , "AIRHUM"             , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
"Air temperature"     , SCALAR_DATA        , "map-series"         , "AIRTEM"             , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
"Cloud cover"         , SCALAR_DATA        , "map-series"         , "CLOUDS"             , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
"Air pressure"        , SCALAR_DATA        , "map-series"         , "PATM"               , -1, NULL                , NULL                 , -1, "FLOW-TIME"   , NULL, "FLOW-MAP-GRID", NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_flow_history_param_data[] = {
"z-coordinate"        , COORD_DATA_3D      , "his-series"         , "ZWL"                , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "DPS"               , -1, NULL                , NULL                  , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"water level"         , SCALAR_DATA        , "his-series"         , "ZWL"                , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"total water depth"   , SCALAR_DATA_SUMMED , "his-series"         , "ZWL"                , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "DPS"                , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"dpt. aver. cur. u"   , DPT_AVERAGE | LOCAL_VECTOR_U
                                           , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"dpt. aver. cur. v"   , DPT_AVERAGE | LOCAL_VECTOR_V
                                           , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"dpt. aver. cur. mag" , DPT_AVERAGE | LOCAL_VECTOR_MAG
                                           , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"dpt. aver. cur. dir" , DPT_AVERAGE | LOCAL_VECTOR_DIR
                                           , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current u"           , LOCAL_VECTOR_U     , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current v"           , LOCAL_VECTOR_V     , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current mag. (horiz)", LOCAL_VECTOR_MAG   , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current dir. (horiz)", LOCAL_VECTOR_DIR   , "his-series"         , "ZCURU"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZCURV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current w"           , SCALAR_DATA        , "his-series"         , "ZCURW"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"flow rate u"         , SCALAR_DATA        , "his-series"         , "ZQXK"               , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"flow rate v"         , SCALAR_DATA        , "his-series"         , "ZQYK"               , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"eddy diffusivity"    , SCALAR_DATA_FACE   , "his-series"         , "ZDICWW"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"eddy viscosity"      , SCALAR_DATA_FACE   , "his-series"         , "ZVICWW"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"eddy viscosity (hor)", SCALAR_DATA        , "his-series"         , "VICUV"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"non-hyd. pressure"   , SCALAR_DATA        , "his-series"         , "HYDPRES"            , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"vorticity"           , SCALAR_DATA        , "his-series"         , "ZVORT"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"enstrophy"           , SCALAR_DATA        , "his-series"         , "ZENST"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"bed stress u"        , LOCAL_VECTOR_U     , "his-series"         , "ZTAUKS"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZTAUET"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"bed stress v"        , LOCAL_VECTOR_V     , "his-series"         , "ZTAUKS"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZTAUET"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"bed stress mag."     , LOCAL_VECTOR_MAG   , "his-series"         , "ZTAUKS"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZTAUET"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"bed stress dir."     , LOCAL_VECTOR_DIR   , "his-series"         , "ZTAUKS"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ZTAUET"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 10, "his-const"         , "NAMCON"             , 10, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 11, "his-const"         , "NAMCON"             , 11, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 12, "his-const"         , "NAMCON"             , 12, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 13, "his-const"         , "NAMCON"             , 13, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 14, "his-const"         , "NAMCON"             , 14, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 15, "his-const"         , "NAMCON"             , 15, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 16, "his-const"         , "NAMCON"             , 16, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 17, "his-const"         , "NAMCON"             , 17, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 18, "his-const"         , "NAMCON"             , 18, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 19, "his-const"         , "NAMCON"             , 19, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 20, "his-const"         , "NAMCON"             , 20, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 21, "his-const"         , "NAMCON"             , 21, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 22, "his-const"         , "NAMCON"             , 22, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 23, "his-const"         , "NAMCON"             , 23, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 24, "his-const"         , "NAMCON"             , 24, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 25, "his-const"         , "NAMCON"             , 25, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 26, "his-const"         , "NAMCON"             , 26, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 27, "his-const"         , "NAMCON"             , 27, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 28, "his-const"         , "NAMCON"             , 28, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 29, "his-const"         , "NAMCON"             , 29, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 30, "his-const"         , "NAMCON"             , 30, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 31, "his-const"         , "NAMCON"             , 31, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 32, "his-const"         , "NAMCON"             , 32, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 33, "his-const"         , "NAMCON"             , 33, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 34, "his-const"         , "NAMCON"             , 34, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 35, "his-const"         , "NAMCON"             , 35, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 36, "his-const"         , "NAMCON"             , 36, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 37, "his-const"         , "NAMCON"             , 37, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 38, "his-const"         , "NAMCON"             , 38, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 39, "his-const"         , "NAMCON"             , 39, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 40, "his-const"         , "NAMCON"             , 40, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 41, "his-const"         , "NAMCON"             , 41, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 42, "his-const"         , "NAMCON"             , 42, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 43, "his-const"         , "NAMCON"             , 43, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 44, "his-const"         , "NAMCON"             , 44, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 45, "his-const"         , "NAMCON"             , 45, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 46, "his-const"         , "NAMCON"             , 46, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 47, "his-const"         , "NAMCON"             , 47, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 48, "his-const"         , "NAMCON"             , 48, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 49, "his-const"         , "NAMCON"             , 49, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 50, "his-const"         , "NAMCON"             , 50, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 51, "his-const"         , "NAMCON"             , 51, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 52, "his-const"         , "NAMCON"             , 52, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 53, "his-const"         , "NAMCON"             , 53, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 54, "his-const"         , "NAMCON"             , 54, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 55, "his-const"         , "NAMCON"             , 55, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 56, "his-const"         , "NAMCON"             , 56, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 57, "his-const"         , "NAMCON"             , 57, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 58, "his-const"         , "NAMCON"             , 58, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 59, "his-const"         , "NAMCON"             , 59, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 60, "his-const"         , "NAMCON"             , 60, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 61, "his-const"         , "NAMCON"             , 61, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 62, "his-const"         , "NAMCON"             , 62, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 63, "his-const"         , "NAMCON"             , 63, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 64, "his-const"         , "NAMCON"             , 64, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 65, "his-const"         , "NAMCON"             , 65, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 66, "his-const"         , "NAMCON"             , 66, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 67, "his-const"         , "NAMCON"             , 67, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 68, "his-const"         , "NAMCON"             , 68, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 69, "his-const"         , "NAMCON"             , 69, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 70, "his-const"         , "NAMCON"             , 70, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 71, "his-const"         , "NAMCON"             , 71, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 72, "his-const"         , "NAMCON"             , 72, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 73, "his-const"         , "NAMCON"             , 73, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 74, "his-const"         , "NAMCON"             , 74, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 75, "his-const"         , "NAMCON"             , 75, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 76, "his-const"         , "NAMCON"             , 76, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 77, "his-const"         , "NAMCON"             , 77, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 78, "his-const"         , "NAMCON"             , 78, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 79, "his-const"         , "NAMCON"             , 79, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 80, "his-const"         , "NAMCON"             , 80, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 81, "his-const"         , "NAMCON"             , 81, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 82, "his-const"         , "NAMCON"             , 82, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 83, "his-const"         , "NAMCON"             , 83, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 84, "his-const"         , "NAMCON"             , 84, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 85, "his-const"         , "NAMCON"             , 85, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 86, "his-const"         , "NAMCON"             , 86, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 87, "his-const"         , "NAMCON"             , 87, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 88, "his-const"         , "NAMCON"             , 88, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 89, "his-const"         , "NAMCON"             , 89, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 90, "his-const"         , "NAMCON"             , 90, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 91, "his-const"         , "NAMCON"             , 91, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 92, "his-const"         , "NAMCON"             , 92, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 93, "his-const"         , "NAMCON"             , 93, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 94, "his-const"         , "NAMCON"             , 94, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 95, "his-const"         , "NAMCON"             , 95, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 96, "his-const"         , "NAMCON"             , 96, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 97, "his-const"         , "NAMCON"             , 97, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 98, "his-const"         , "NAMCON"             , 98, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA        , "his-series"         , "GRO"                , 99, "his-const"         , "NAMCON"             , 99, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA_FACE   , "his-series"         , "ZTUR"               ,  2, "his-const"         , "NAMCON"             , LAST-1,
                                                                                                                                                "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"*"                   , SCALAR_DATA_FACE   , "his-series"         , "ZTUR"               ,  1, "his-const"         , "NAMCON"             , LAST-2,
                                                                                                                                                "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"current u (LES)"     , LOCAL_VECTOR_U     , "his-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current v (LES)"     , LOCAL_VECTOR_V     , "his-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current mag (LES)"   , LOCAL_VECTOR_MAG   , "his-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"current dir (LES)"   , LOCAL_VECTOR_DIR   , "his-series"         , "UMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "VMNLDF"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"Richardson number"   , SCALAR_DATA_FACE   , "his-series"         , "ZRICH"              , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom depth (sed)"  , SCALAR_DATA        , "his-sed-series"     , "ZDPS"               , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Sediment thickness"  , SCALAR_DATA        , "his-sed-series"     , "ZDPSED"             , -1, NULL                , NULL                 , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom conc. fract 1", SCALAR_DATA        , "his-sed-series"     , "ZBDSED"             ,  1, NULL                , NULL                 ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom conc. fract 2", SCALAR_DATA        , "his-sed-series"     , "ZBDSED"             ,  2, NULL                , NULL                 ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom conc. fract 3", SCALAR_DATA        , "his-sed-series"     , "ZBDSED"             ,  3, NULL                , NULL                 ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom conc. fract 4", SCALAR_DATA        , "his-sed-series"     , "ZBDSED"             ,  4, NULL                , NULL                 ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bottom conc. fract 5", SCALAR_DATA        , "his-sed-series"     , "ZBDSED"             ,  5, NULL                , NULL                 ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Equil. conc. fract 1", SCALAR_DATA_FACE   , "his-sed-series"     , "ZRSDEQ"             ,  1, NULL                , NULL                 ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Equil. conc. fract 2", SCALAR_DATA_FACE   , "his-sed-series"     , "ZRSDEQ"             ,  2, NULL                , NULL                 ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Equil. conc. fract 3", SCALAR_DATA_FACE   , "his-sed-series"     , "ZRSDEQ"             ,  3, NULL                , NULL                 ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Equil. conc. fract 4", SCALAR_DATA_FACE   , "his-sed-series"     , "ZRSDEQ"             ,  4, NULL                , NULL                 ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Equil. conc. fract 5", SCALAR_DATA_FACE   , "his-sed-series"     , "ZRSDEQ"             ,  5, NULL                , NULL                 ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Settling.vel.fract 1", SCALAR_DATA        , "his-sed-series"     , "ZWS"                ,  1, NULL                , NULL                 ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Settling.vel.fract 2", SCALAR_DATA        , "his-sed-series"     , "ZWS"                ,  2, NULL                , NULL                 ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Settling.vel.fract 3", SCALAR_DATA        , "his-sed-series"     , "ZWS"                ,  3, NULL                , NULL                 ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Settling.vel.fract 4", SCALAR_DATA        , "his-sed-series"     , "ZWS"                ,  4, NULL                , NULL                 ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Settling.vel.fract 5", SCALAR_DATA        , "his-sed-series"     , "ZWS"                ,  5, NULL                , NULL                 ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Bedload transport u" , LOCAL_VECTOR_U     , "his-sed-series"     , "ZSBUU"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-sed-series"     , "ZSBVV"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"Bedload transport v" , LOCAL_VECTOR_V     , "his-sed-series"     , "ZSBUU"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-sed-series"     , "ZSBVV"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"Susp. transport u"   , LOCAL_VECTOR_U     , "his-sed-series"     , "ZSSUU"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-sed-series"     , "ZSSVV"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"Susp. transport v"   , LOCAL_VECTOR_V     , "his-sed-series"     , "ZSSUU"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-sed-series"     , "ZSSVV"              , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"@"                   , SCALAR_DATA        , "his-const"          , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "FLOW-HIS-LOCS", NULL,
"Near-bed ref.conc."  , SCALAR_DATA        , "his-sed-series"     , "ZRCA"               , -1, NULL                , NULL                 , -1, "FLOW-SEDTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Sign. wave height"   , SCALAR_DATA        , "his-wav-series"     , "ZHS"                , -1, NULL                , NULL                 , -1, "FLOW-WAVTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Peak wave period"    , SCALAR_DATA        , "his-wav-series"     , "ZTP"                , -1, NULL                , NULL                 , -1, "FLOW-WAVTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Wave direction"      , SCALAR_DATA        , "his-wav-series"     , "ZDIR"               , -1, NULL                , NULL                 , -1, "FLOW-WAVTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Wave length"         , SCALAR_DATA        , "his-wav-series"     , "ZRLABD"             , -1, NULL                , NULL                 , -1, "FLOW-WAVTIME", NULL, "FLOW-HIS-LOCS", NULL,
"Orbital velocity"    , SCALAR_DATA        , "his-wav-series"     , "ZUORB"              , -1, NULL                , NULL                 , -1, "FLOW-WAVTIME", NULL, "FLOW-HIS-LOCS", NULL,
"accumulated flow"    , SCALAR_DATA        , "his-series"         , "FLTR"               , -1, "his-const"         , "NAMCON"             , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"momentary flow"      , SCALAR_DATA        , "his-series"         , "CTR"                , -1, "his-const"         , "NAMCON"             , -1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"adv. flux *"         , SCALAR_DATA        , "his-series"         , "ATR"                , 10, "his-const"         , "NAMCON"             , 10, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,

"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"dis. flux *"         , SCALAR_DATA        , "his-series"         , "DTR"                , 10, "his-const"         , "NAMCON"             , 10, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,

"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"tot. flux *"         , SCALAR_DATA_SUMMED , "his-series"         , "DTR"                , 10, "his-const"         , "NAMCON"             , 10, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"@"                   , SCALAR_DATA        , "his-series"         , "ATR"                , 10, "his-const"         , "NAMCON"             , 10, "FLOW-HISTIME", NULL, "FLOW-HIS-TRANS", NULL,
"Dredged material"    , SCALAR_DATA        , "his-dad-series"     , "LINK_SUM"           , -1, NULL                , NULL                 , -1, "FLOW-DADTIME", NULL, "FLOW-DAD-LOCS" , NULL,
"Discharge"           , SCALAR_DATA        , "his-dis-series"     , "ZQ"                 , -1, NULL                , NULL                 , -1, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Cumulative discharge", SCALAR_DATA        , "his-dis-series"     , "ZQ_SUM"             , -1, NULL                , NULL                 , -1, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"X-coord discharge"   , SCALAR_DATA        , "his-dis-series"     , "XCOR"               , -1, NULL                , NULL                 , -1, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Y-coord discharge"   , SCALAR_DATA        , "his-dis-series"     , "YCOR"               , -1, NULL                , NULL                 , -1, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  2, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  3, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  4, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  5, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  6, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  7, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Discharge *"         , SCALAR_DATA        , "his-dis-series"     , "RINT"               ,  1, "his-const"         , "NAMCON"             ,  8, "FLOW-DISCHTM", NULL, "FLOW-DIS-LOCS" , NULL,
"Average depth"       , SCALAR_DATA        , "his-bal-series"     , "BALDPS"             , -1, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Average volume"      , SCALAR_DATA        , "his-bal-series"     , "BALVOLUME"          , -1, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  2, "his-const"         , "NAMCON"             ,  2, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  3, "his-const"         , "NAMCON"             ,  3, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  4, "his-const"         , "NAMCON"             ,  4, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  5, "his-const"         , "NAMCON"             ,  5, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  6, "his-const"         , "NAMCON"             ,  6, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  7, "his-const"         , "NAMCON"             ,  7, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  8, "his-const"         , "NAMCON"             ,  8, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          ,  9, "his-const"         , "NAMCON"             ,  9, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Av.conc. *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1CONC"          , 10, "his-const"         , "NAMCON"             , 10, "FLOW-BALTIME", NULL, "FLOW-POLYGONS" , NULL,
"Flux in"             , SCALAR_DATA        , "his-bal-series"     , "BALFLUX"            ,  1, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out"            , SCALAR_DATA        , "his-bal-series"     , "BALFLUX"            ,  2, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in/out"         , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALFLUX"            ,  1, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALFLUX"            ,  2, NULL                , NULL                 ,  0, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+2, "his-const"         , "NAMCON"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+2, "his-const"         , "NAMCON"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+2, "his-const"         , "NAMCON"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+2, "his-const"         , "NAMCON"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+3, "his-const"         , "NAMCON"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+3, "his-const"         , "NAMCON"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+3, "his-const"         , "NAMCON"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+3, "his-const"         , "NAMCON"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+4, "his-const"         , "NAMCON"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+4, "his-const"         , "NAMCON"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+4, "his-const"         , "NAMCON"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+4, "his-const"         , "NAMCON"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+5, "his-const"         , "NAMCON"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+5, "his-const"         , "NAMCON"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+5, "his-const"         , "NAMCON"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+5, "his-const"         , "NAMCON"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+6, "his-const"         , "NAMCON"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+6, "his-const"         , "NAMCON"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+6, "his-const"         , "NAMCON"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+6, "his-const"         , "NAMCON"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+7, "his-const"         , "NAMCON"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+7, "his-const"         , "NAMCON"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+7, "his-const"         , "NAMCON"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+7, "his-const"         , "NAMCON"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+8, "his-const"         , "NAMCON"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+8, "his-const"         , "NAMCON"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+8, "his-const"         , "NAMCON"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+8, "his-const"         , "NAMCON"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+9, "his-const"         , "NAMCON"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+9, "his-const"         , "NAMCON"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+9, "his-const"         , "NAMCON"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+9, "his-const"         , "NAMCON"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_1+10,"his-const"         , "NAMCON"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+10,"his-const"         , "NAMCON"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALR1FLUX", FIRST_IS_1+10,"his-const"         , "NAMCON"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALR1FLUX", FIRST_IS_2+10,"his-const"         , "NAMCON"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+1, "his-const"         , "NAMCON"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+1, "his-const"         , "NAMSED"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+1, "his-const"         , "NAMSED"             ,  1, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+2, "his-const"         , "NAMSED"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+2, "his-const"         , "NAMSED"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+2, "his-const"         , "NAMSED"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+2, "his-const"         , "NAMSED"             ,  2, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+3, "his-const"         , "NAMSED"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+3, "his-const"         , "NAMSED"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+3, "his-const"         , "NAMSED"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+3, "his-const"         , "NAMSED"             ,  3, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+4, "his-const"         , "NAMSED"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+4, "his-const"         , "NAMSED"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+4, "his-const"         , "NAMSED"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+4, "his-const"         , "NAMSED"             ,  4, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+5, "his-const"         , "NAMSED"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+5, "his-const"         , "NAMSED"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+5, "his-const"         , "NAMSED"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+5, "his-const"         , "NAMSED"             ,  5, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+6, "his-const"         , "NAMSED"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+6, "his-const"         , "NAMSED"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+6, "his-const"         , "NAMSED"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+6, "his-const"         , "NAMSED"             ,  6, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+7, "his-const"         , "NAMSED"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+7, "his-const"         , "NAMSED"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+7, "his-const"         , "NAMSED"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+7, "his-const"         , "NAMSED"             ,  7, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+8, "his-const"         , "NAMSED"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+8, "his-const"         , "NAMSED"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+8, "his-const"         , "NAMSED"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+8, "his-const"         , "NAMSED"             ,  8, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+9, "his-const"         , "NAMSED"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+9, "his-const"         , "NAMSED"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+9, "his-const"         , "NAMSED"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+9, "his-const"         , "NAMSED"             ,  9, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux in *"           , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_1+10,"his-const"         , "NAMSED"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux out *"          , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+10,"his-const"         , "NAMSED"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"Flux I/O *"          , SCALAR_DATA_SUBTRACTED, "his-bal-series"  , "BALSDFLUX", FIRST_IS_1+10,"his-const"         , "NAMSED"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
"@"                   , SCALAR_DATA        , "his-bal-series"     , "BALSDFLUX", FIRST_IS_2+10,"his-const"         , "NAMSED"             , 10, "FLOW-BALTIME", NULL, "FLOW-BAL-LOCS" , NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""            , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_comm_param_data[] = {
"water level"         , SCALAR_DATA        , "CURTIM"             , "S1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"total water depth"   , SCALAR_DATA_SUMMED , "CURTIM"             , "S1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "INITBOT"            , "DPS"                , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"current u"           , LOCAL_VECTOR_U     , "CURTIM"             , "U1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "CURTIM"             , "V1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"current v"           , LOCAL_VECTOR_V     , "CURTIM"             , "U1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "CURTIM"             , "V1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"current mag. (horiz)", LOCAL_VECTOR_MAG   , "CURTIM"             , "U1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "CURTIM"             , "V1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"current dir. (horiz)", LOCAL_VECTOR_DIR   , "CURTIM"             , "U1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "CURTIM"             , "V1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"XCOR"                , SCALAR_DATA        , "GRID"               , "XCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"YCOR"                , SCALAR_DATA        , "GRID"               , "YCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"ALFAS"               , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Spiral flow intens." , SCALAR_DATA        , "CURTIM"             , "RSP"                , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"Eddy diffusivity"    , SCALAR_DATA        , "DWQTIM"             , "DICWW"              , -1, NULL                , NULL                 , -1, "COMM-DWQTIM", NULL, "COMM-MAP-GRID", NULL,
"Salinity"            , SCALAR_DATA        , "DWQTIM"             , "RSAL"               , -1, NULL                , NULL                 , -1, "COMM-DWQTIM", NULL, "COMM-MAP-GRID", NULL,
"Temperature"         , SCALAR_DATA        , "DWQTIM"             , "RTEM"               , -1, NULL                , NULL                 , -1, "COMM-DWQTIM", NULL, "COMM-MAP-GRID", NULL,
"Bottom depth-vertex" , SCALAR_DATA        , "BOTTIM"             , "DP"                 , -1, NULL                , NULL                 , -1, "COMM-BOTTIM", NULL, "COMM-MAP-GRID", NULL,
"Maximum shear stress", SCALAR_DATA        , "TAUTIM"             , "TAUMAX"             , -1, NULL                , NULL                 , -1, "COMM-DWQTIM", NULL, "COMM-MAP-GRID", NULL,
"Wave direction"      , SCALAR_DATA        , "WAVTIM"             , "DIR"                , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"Wave energy dissip." , SCALAR_DATA        , "WAVTIM"             , "DISS"               , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"Wave height (RMS)"   , SCALAR_DATA        , "WAVTIM"             , "HRMS"               , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"Peak wave period"    , SCALAR_DATA        , "WAVTIM"             , "TP"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"Wave forcing u"      , LOCAL_VECTOR_U     , "WAVTIM"             , "FX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "FY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave forcing v"      , LOCAL_VECTOR_V     , "WAVTIM"             , "FX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "FY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave forcing (mag.)" , LOCAL_VECTOR_MAG   , "WAVTIM"             , "FX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "FY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave forcing (dir.)" , LOCAL_VECTOR_DIR   , "WAVTIM"             , "FX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "FY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave volume flux u"  , LOCAL_VECTOR_U     , "WAVTIM"             , "MX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "MY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave volume flux v"  , LOCAL_VECTOR_V     , "WAVTIM"             , "MX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "MY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave vol.flux (mag.)", LOCAL_VECTOR_MAG   , "WAVTIM"             , "MX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "MY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Wave vol.flux (dir.)", LOCAL_VECTOR_DIR   , "WAVTIM"             , "MX"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "WAVTIM"             , "MY"                 , -1, NULL                , NULL                 , -1, "COMM-WAVTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Av.Bed Load u"       , LOCAL_VECTOR_U     , "TRANSTIM"           , "TTXA"               , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "TRANSTIM"           , "TTYA"               , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Av.Bed Load v"       , LOCAL_VECTOR_V     , "TRANSTIM"           , "TTXA"               , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "TRANSTIM"           , "TTYA"               , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Av.Susp.Trans u"     , LOCAL_VECTOR_U     , "TRANSTIM"           , "TTXSA"              , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "TRANSTIM"           , "TTYSA"              , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"Av.Susp.Trans v"     , LOCAL_VECTOR_V     , "TRANSTIM"           , "TTXSA"              , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "TRANSTIM"           , "TTYSA"              , -1, NULL                , NULL                 , -1, "COMM-TRATIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
"UVDAMS"              , ENCODED_DATA_U     , "KENMTIM"            , "KFU"                , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , ENCODED_DATA_V     , "KENMTIM"            , "KFV"                , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"z-coordinate"        , COORD_DATA_3D      , "CURTIM"             , "S1"                 , -1, NULL                , NULL                 , -1, "COMM-CURTIM", NULL, "COMM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "INITBOT"            , "DPS"                , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "COMM-MAP-GRID", NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""           , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_both_param_data[] = {
"Entrainment"         , SCALAR_DATA        , "HISBOTTIM"          , "ENHIS"              , -1, NULL                , NULL                 , -1, "BOTH-BOTTIM", NULL, "BOTH-HIS-LOCS", NULL,
"Bed level"           , SCALAR_DATA        , "HISBOTTIM"          , "DPHIS"              , -1, NULL                , NULL                 , -1, "BOTH-BOTTIM", NULL, "BOTH-HIS-LOCS", NULL,
"Bedload transport X" , SCALAR_DATA        , "HISBOTTIM"          , "TXHIS"              , -1, NULL                , NULL                 , -1, "BOTH-BOTTIM", NULL, "BOTH-HIS-LOCS", NULL,
"Bedload transport Y" , SCALAR_DATA        , "HISBOTTIM"          , "TYHIS"              , -1, NULL                , NULL                 , -1, "BOTH-BOTTIM", NULL, "BOTH-HIS-LOCS", NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_botm_param_data[] = {
"XCOR"                , SCALAR_DATA        , "GRID"               , "XCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "BOTM-MAP-GRID", NULL,
"YCOR"                , SCALAR_DATA        , "GRID"               , "YCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "BOTM-MAP-GRID", NULL,
"Entrainment"         , SCALAR_DATA        , "MAPBOTTIM"          , "EN"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"Bed level"           , SCALAR_DATA        , "MAPBOTTIM"          , "DP"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"Bedload transport X" , LOCAL_VECTOR_U     , "MAPBOTTIM"          , "TX"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPBOTTIM"          , "TY"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "BOTM-MAP-GRID", NULL,
"Bedload transport Y" , LOCAL_VECTOR_V     , "MAPBOTTIM"          , "TX"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPBOTTIM"          , "TY"                 , -1, NULL                , NULL                 , -1, "BOTM-BOTTIM", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"   , NULL, "BOTM-MAP-GRID", NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_hwgxy_param_data[] = {
"Sign.wave height"    , SCALAR_DATA        , "map-series"         , "HSIGN"              , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Mean wave direction" , SCALAR_DATA        , "map-series"         , "DIR"                , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Mean wave period"    , SCALAR_DATA        , "map-series"         , "PERIOD"             , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Peak wave direction" , SCALAR_DATA        , "map-series"         , "PDIR"               , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Peak wave period"    , SCALAR_DATA        , "map-series"         , "RTP"                , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Water depth"         , SCALAR_DATA        , "map-series"         , "DEPTH"              , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Current velocity X"  , SCALAR_DATA        , "map-series"         , "VELOC-X"            , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Current velocity Y"  , SCALAR_DATA        , "map-series"         , "VELOC-Y"            , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Energy transport X"  , SCALAR_DATA        , "map-series"         , "TRANSP-X"           , -1, NULL                , NULL                , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Energy transport Y"  , SCALAR_DATA        , "map-series"         , "TRANSP-Y"           , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Directional spread"  , SCALAR_DATA        , "map-series"         , "DSPR"               , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Energy dissipation"  , SCALAR_DATA        , "map-series"         , "DISSIP"             , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Energy leakage"      , SCALAR_DATA        , "map-series"         , "LEAK"               , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Fraction breaking"   , SCALAR_DATA        , "map-series"         , "QB"                 , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Orbital velocity"    , SCALAR_DATA        , "map-series"         , "UBOT"               , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Mean steepness"      , SCALAR_DATA        , "map-series"         , "STEEPW"             , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"Mean wave length"    , SCALAR_DATA        , "map-series"         , "WLENGTH"            , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"XP"                  , SCALAR_MISSING_ZERO, "map-series"         , "XP"                 , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
"YP"                  , SCALAR_MISSING_ZERO, "map-series"         , "YP"                 , -1, NULL                , NULL                 , -1, "HWGXY-TIME" , NULL, "HWGXY-GRID"   , NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

static ParameterInfoStruct d3d_tram_param_data[] = {
"XCOR"                , SCALAR_DATA        , "GRID"               , "XCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "TRAM-MAP-GRID", NULL,
"YCOR"                , SCALAR_DATA        , "GRID"               , "YCOR"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "TRAM-MAP-GRID", NULL,
"Chezy-coefficient X" , SCALAR_DATA        , "MAPTTRAN"           , "CZU"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "TRAM-MAP-GRID", NULL,
"Chezy-coefficient Y" , SCALAR_DATA        , "MAPTTRAN"           , "CZV"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "TRAM-MAP-GRID", NULL,
"Spiral flow intens." , SCALAR_DATA        , "MAPTTRAN"           , "RSP"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"Bedload tr. fract X" , LOCAL_VECTOR_U     , "MAPTTRAN"           , "SX"                 , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SY"                 , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Bedload tr. fract Y" , LOCAL_VECTOR_V     , "MAPTTRAN"           , "SX"                 , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SY"                 , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Bedload tr. total X" , LOCAL_VECTOR_U     , "MAPTTRAN"           , "STX"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "STY"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Bedload tr. total Y" , LOCAL_VECTOR_V     , "MAPTTRAN"           , "STX"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "STY"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Susp.sed.tr.fract X" , LOCAL_VECTOR_U     , "MAPTTRAN"           , "SXS"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SYS"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Susp.sed.tr.fract Y" , LOCAL_VECTOR_V     , "MAPTTRAN"           , "SXS"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SYS"                , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Susp.sed.tr.total X" , LOCAL_VECTOR_U     , "MAPTTRAN"           , "SXTS"               , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SYTS"               , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"Susp.sed.tr.total Y" , LOCAL_VECTOR_V     , "MAPTTRAN"           , "SXTS"               , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPTTRAN"           , "SYTS"               , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Sediment conc."      , SCALAR_DATA        , "MAPTTRAN"           , "CONC"               , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"Bed exchange"        , SCALAR_DATA        , "MAPTTRAN"           , "RLSILT"             , -1, NULL                , NULL                 , -1, "TRAM-MPTTIME", NULL, "BOTM-MAP-GRID", NULL,
"Initial total tr. X" , LOCAL_VECTOR_U     , "MAPATRAN"           , "TTXI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Initial total tr. Y" , LOCAL_VECTOR_V     , "MAPATRAN"           , "TTXI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Average total tr. X" , LOCAL_VECTOR_U     , "MAPATRAN"           , "TTXA"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYA"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Average total tr. Y" , LOCAL_VECTOR_V     , "MAPATRAN"           , "TTXA"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYA"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Initial susp. tr. X" , LOCAL_VECTOR_U     , "MAPATRAN"           , "STXI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "STYI"               , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Initial susp. tr. Y" , LOCAL_VECTOR_V     , "MAPATRAN"           , "TTXSI"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYSI"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Average susp. tr. X" , LOCAL_VECTOR_U     , "MAPATRAN"           , "TTXSA"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYSA"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Average susp. tr. Y" , LOCAL_VECTOR_V     , "MAPATRAN"           , "TTXSA"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "MAPATRAN"           , "TTYSA"              , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
"@"                   , SCALAR_DATA        , "GRID"               , "ALFAS"              , -1, NULL                , NULL                 , -1, "CONSTANT",     NULL, "BOTM-MAP-GRID", NULL,
"Bed level increment" , SCALAR_DATA        , "MAPATRAN"           , "DZBSIL"             , -1, NULL                , NULL                 , -1, "CONSTANT"    , NULL, "BOTM-MAP-GRID", NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""            , NULL, ""     , NULL,
} ;

/* Difficult one! */
static ParameterInfoStruct d3d_trah_param_data[] = {
"Entrainment"         , SCALAR_DATA        , "HISTRAN"            , "RLSILS"             , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Concentration"       , SCALAR_DATA        , "HISTRAN"            , "CONSED"             , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Water depth"         , SCALAR_DATA        , "HISTRAN"            , "ZZB"                , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Water level"         , SCALAR_DATA        , "HISTRAN"            , "ZSL"                , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Velocity (U)"        , SCALAR_DATA        , "HISTRAN"            , "ZU"                 , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Velocity (V)"        , SCALAR_DATA        , "HISTRAN"            , "ZV"                 , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Sediment transp. X"  , SCALAR_DATA        , "HISTRAN"            , "ZSEDX"              , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Sediment transp. Y"  , SCALAR_DATA        , "HISTRAN"            , "ZSEDY"              , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Total sed.transp. X" , SCALAR_DATA        , "HISTRAN"            , "ZSEDTX"             , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Total sed.transp. Y" , SCALAR_DATA        , "HISTRAN"            , "ZSEDTY"             , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Magn. sed.transport" , SCALAR_DATA        , "HISTRAN"            , "ZSEDR"              , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Magn. tot.sed.tr."   , SCALAR_DATA        , "HISTRAN"            , "ZSEDTR"             , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Chezy-coeff. X"      , SCALAR_DATA        , "HISTRAN"            , "ZCZU"               , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Chezy-coeff. Y"      , SCALAR_DATA        , "HISTRAN"            , "ZCZV"               , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-HIS-LOCS", NULL,
"Sed.transp. cross X" , SCALAR_DATA        , "HISTRAN"            , "SXRA"               , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-SECT-X"  , NULL,
"Sed.transp. cross Y" , SCALAR_DATA        , "HISTRAN"            , "SYRA"               , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-SECT-Y"  , NULL,
"Tot.sed.tr. cross X" , SCALAR_DATA        , "HISTRAN"            , "STXRA"              , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-SECT-TX" , NULL,
"Tot.sed.tr. cross Y" , SCALAR_DATA        , "HISTRAN"            , "STYRA"              , -1, NULL                , NULL                 , -1, "TRAH-HISTIM", NULL, "TRAH-SECT-TY" , NULL,
""                    , END_OF_ARRAY       , ""                   , ""                   ,  0, NULL                , NULL                 ,  0, ""         , NULL, ""     , NULL,
} ;

/* Adjust the routines below when introducing new types */

/* -------------------------------------------------------------------
    Function: GNF_IdentifyDefinition()
    Author:   Arjen Markus
    Purpose:  Translate file type to appropriate definition array
    Context:  Used by various routines
    Pseudo Code:
              Switch on the file type. Return a pointer to the
              corresponding array.
------------------------------------------------------------------- */

static void
   GNF_IdentifyDefinition(
      NefisFileInfoPtr    file_info,     /* I file information                */
      TInt4               filetype       /* I Type of the file                */
   )
{
   TInt4       i ;
   TInt4       j ;

   switch( filetype )
   {
   case ODS_TRISULA_HIS_NEFIS:
      file_info->defined_params = d3d_flow_history_param_data ;
      break ;

   case ODS_TRISULA_MAP_NEFIS:
      file_info->defined_params = d3d_flow_map_param_data ;
      break ;

   case ODS_MORSYS_MAP_NEFIS:
      file_info->defined_params = d3d_comm_param_data ;
      break ;

   case ODS_MORSYS_BOTH_NEFIS:
      file_info->defined_params = d3d_both_param_data ;
      break ;

   case ODS_MORSYS_BOTM_NEFIS:
      file_info->defined_params = d3d_botm_param_data ;
      break ;

   case ODS_MORSYS_HWGXY_NEFIS:
   case ODS_MORSYS_SWAN_NEFIS:   /* Identical */
      file_info->defined_params = d3d_hwgxy_param_data ;
      break ;

   case ODS_MORSYS_TRAM_NEFIS:
      file_info->defined_params = d3d_tram_param_data ;
      break ;

   case ODS_MORSYS_TRAH_NEFIS:
      file_info->defined_params = d3d_trah_param_data ;
      break ;

   default:
      file_info->defined_params = nefis_unknown_param_data ;
   }

   /* Now identify the times and the locations structure per parameter
      Note:
      Perhaps a separate routine?
   */
   i = 0 ;
   while ( file_info->defined_params[i].type != END_OF_ARRAY )
   {
      j = 0 ;
      while( nefis_time_data[j].type != END_OF_DATE_TYPES )
      {
         if ( strcmp( file_info->defined_params[i].datetime_id,
                      nefis_time_data[j].datetime_id ) == 0 )
         {
            file_info->defined_params[i].datetime_info = &nefis_time_data[j] ;
         }
         j ++ ;
      }

      j = 0 ;
      while( nefis_location_data[j].type != END_OF_LOC_TYPES )
      {
         if ( strcmp( file_info->defined_params[i].location_id,
                      nefis_location_data[j].location_id ) == 0 )
         {
            file_info->defined_params[i].location_info = &nefis_location_data[j] ;
         }
         j ++ ;
      }
      i ++ ;
   }
}

/* end GNF_DATA_H_INCLUDED */
#endif

#pragma once

/*
 * TECXXX.h: Copyright (C) 1988-2014 Tecplot, Inc.
 */

#if !defined CRAY
#  define TECINI142       tecini142
#  define TECZNE142       teczne142
#  define TECDAT142       tecdat142
#  define TECNOD142       tecnod142
#  define TECNODE142      tecnode142
#  define TECGEO142       tecgeo142
#  define TECTXT142       tectxt142
#  define TECLAB142       teclab142
#  define TECFIL142       tecfil142
#  define TECFOREIGN142   tecforeign142
#  define TECEND142       tecend142
#  define TECUSR142       tecusr142
#  define TECAUXSTR142    tecauxstr142
#  define TECZAUXSTR142   teczauxstr142
#  define TECVAUXSTR142   tecvauxstr142
#  define TECFACE142      tecface142
#  define TECPOLY142      tecpoly142
#  define TECPOLYFACE142  tecpolyface142
#  define TECPOLYBCONN142 tecpolybconn142

#  define TECINI112       tecini112
#  define TECZNE112       teczne112
#  define TECDAT112       tecdat112
#  define TECNOD112       tecnod112
#  define TECNODE112      tecnode112
#  define TECGEO112       tecgeo112
#  define TECTXT112       tectxt112
#  define TECLAB112       teclab112
#  define TECFIL112       tecfil112
#  define TECFOREIGN112   tecforeign112
#  define TECEND112       tecend112
#  define TECUSR112       tecusr112
#  define TECAUXSTR112    tecauxstr112
#  define TECZAUXSTR112   teczauxstr112
#  define TECVAUXSTR112   tecvauxstr112
#  define TECFACE112      tecface112
#  define TECPOLY112      tecpoly112
#  define TECPOLYFACE112  tecpolyface112
#  define TECPOLYBCONN112 tecpolybconn112

#  define TECINI111     tecini111
#  define TECZNE111     teczne111
#  define TECDAT111     tecdat111
#  define TECNOD111     tecnod111
#  define TECGEO111     tecgeo111
#  define TECTXT111     tectxt111
#  define TECLAB111     teclab111
#  define TECFIL111     tecfil111
#  define TECFOREIGN111 tecforeign111
#  define TECEND111     tecend111
#  define TECUSR111     tecusr111
#  define TECAUXSTR111  tecauxstr111
#  define TECZAUXSTR111 teczauxstr111
#  define TECVAUXSTR111 tecvauxstr111
#  define TECFACE111    tecface111
#  define TECPOLY111    tecpoly111

#  define TECINI110     tecini110
#  define TECZNE110     teczne110
#  define TECDAT110     tecdat110
#  define TECNOD110     tecnod110
#  define TECGEO110     tecgeo110
#  define TECTXT110     tectxt110
#  define TECLAB110     teclab110
#  define TECFIL110     tecfil110
#  define TECFOREIGN110 tecforeign110
#  define TECEND110     tecend110
#  define TECUSR110     tecusr110
#  define TECAUXSTR110  tecauxstr110
#  define TECZAUXSTR110 teczauxstr110
#  define TECVAUXSTR110 tecvauxstr110
#  define TECFACE110    tecface110

#  define TECINI100     tecini100
#  define TECZNE100     teczne100
#  define TECDAT100     tecdat100
#  define TECNOD100     tecnod100
#  define TECGEO100     tecgeo100
#  define TECTXT100     tectxt100
#  define TECLAB100     teclab100
#  define TECFIL100     tecfil100
#  define TECFOREIGN100 tecforeign100
#  define TECEND100     tecend100
#  define TECUSR100     tecusr100
#  define TECAUXSTR100  tecauxstr100
#  define TECZAUXSTR100 teczauxstr100
#  define TECVAUXSTR100 tecvauxstr100
#  define TECFACE100    tecface100

#  define TECINI  tecini
#  define TECZNE  teczne
#  define TECDAT  tecdat
#  define TECNOD  tecnod
#  define TECGEO  tecgeo
#  define TECTXT  tectxt
#  define TECLAB  teclab
#  define TECFIL  tecfil
#  define TECEND  tecend
#  define TECUSR  tecusr
#endif

#define INTEGER4  int
#define INTEGER2  short

#if defined _WIN32
# if !defined MSWIN
#   define MSWIN
# endif
#endif

#include "dataio_tecio_Exports.h"

#if !defined STDCALL
# if defined MSWIN
#   define STDCALL __stdcall
# else
#   define STDCALL
# endif
#endif

#if !defined EXTERNC
# if defined __cplusplus
#  define EXTERNC extern "C"
# else
#  define EXTERNC
# endif
#endif

/*
 * The latest TecIO API, version 142, introduced the ability write both PLT and SZL formatted files.
 */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI142(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* FileFormat,
    INTEGER4 const* FileType,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE142(
    char const*     ZoneTitle,
    INTEGER4 const* ZoneType,
    INTEGER4 const* IMxOrNumPts,
    INTEGER4 const* JMxOrNumElements,
    INTEGER4 const* KMxOrNumFaces,
    INTEGER4 const* ICellMx,
    INTEGER4 const* JCellMx,
    INTEGER4 const* KCellMx,
    double const*   SolutionTime,
    INTEGER4 const* StrandID,
    INTEGER4 const* ParentZone,
    INTEGER4 const* IsBlock,
    INTEGER4 const* NumFaceConnections,
    INTEGER4 const* FaceNeighborMode,
    INTEGER4 const* TotalNumFaceNodes,
    INTEGER4 const* NumConnectedBoundaryFaces,
    INTEGER4 const* TotalNumBoundaryConnections,
    INTEGER4 const* PassiveVarList,
    INTEGER4 const* ValueLocation,
    INTEGER4 const* ShareVarFromZone,
    INTEGER4 const* ShareConnectivityFromZone);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT142(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD142(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNODE142(
    INTEGER4 const* N,
    INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND142(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB142(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR142(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO142(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT142(
    double const*   XOrThetaPos,
    double const*   YOrRPos,
    double const*   ZOrUnusedPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    char const*     String,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL142(INTEGER4 const* F);

EXTERNC dataio_tecio_API void STDCALL TECFOREIGN142(INTEGER4 const* OutputForeignByteOrder);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECAUXSTR142(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZAUXSTR142(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECVAUXSTR142(
    INTEGER4 const* Var,
    char const*     Name,
    char const*     Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFACE142(INTEGER4 const* FaceConnections);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLY142(
    INTEGER4 const* FaceNodeCounts,
    INTEGER4 const* FaceNodes,
    INTEGER4 const* FaceLeftElems,
    INTEGER4 const* FaceRightElems,
    INTEGER4 const* FaceBndryConnectionCounts,
    INTEGER4 const* FaceBndryConnectionElems,
    INTEGER4 const* FaceBndryConnectionZones);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLYFACE142(
    INTEGER4 const* NumFaces,
    INTEGER4 const* FaceNodeCounts,
    INTEGER4 const* FaceNodes,     
    INTEGER4 const* FaceLeftElems, 
    INTEGER4 const* FaceRightElems);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLYBCONN142(
    INTEGER4 const* NumBndryFaces,
    INTEGER4 const* FaceBndryConnectionCounts,
    INTEGER4 const* FaceBndryConnectionElems, 
    INTEGER4 const* FaceBndryConnectionZones);

/*
 *  V11.3 tecio functions
 */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI112(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* FileType,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE112(
    char const*     ZoneTitle,
    INTEGER4 const* ZoneType,
    INTEGER4 const* IMxOrNumPts,
    INTEGER4 const* JMxOrNumElements,
    INTEGER4 const* KMxOrNumFaces,
    INTEGER4 const* ICellMx,
    INTEGER4 const* JCellMx,
    INTEGER4 const* KCellMx,
    double const*   SolutionTime,
    INTEGER4 const* StrandID,
    INTEGER4 const* ParentZone,
    INTEGER4 const* IsBlock,
    INTEGER4 const* NumFaceConnections,
    INTEGER4 const* FaceNeighborMode,
    INTEGER4 const* TotalNumFaceNodes,
    INTEGER4 const* NumConnectedBoundaryFaces,
    INTEGER4 const* TotalNumBoundaryConnections,
    INTEGER4 const* PassiveVarList,
    INTEGER4 const* ValueLocation,
    INTEGER4 const* ShareVarFromZone,
    INTEGER4 const* ShareConnectivityFromZone);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT112(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD112(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNODE112(
    INTEGER4 const* N,
    INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND112(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB112(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR112(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO112(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT112(
    double const*   XOrThetaPos,
    double const*   YOrRPos,
    double const*   ZOrUnusedPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    char const*     String,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL112(INTEGER4 const* F);

EXTERNC dataio_tecio_API void STDCALL TECFOREIGN112(INTEGER4 const* OutputForeignByteOrder);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECAUXSTR112(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZAUXSTR112(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECVAUXSTR112(
    INTEGER4 const* Var,
    char const*     Name,
    char const*     Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFACE112(INTEGER4 const* FaceConnections);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLY112(
    INTEGER4 const* FaceNodeCounts,
    INTEGER4 const* FaceNodes,
    INTEGER4 const* FaceLeftElems,
    INTEGER4 const* FaceRightElems,
    INTEGER4 const* FaceBndryConnectionCounts,
    INTEGER4 const* FaceBndryConnectionElems,
    INTEGER4 const* FaceBndryConnectionZones);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLYFACE112(
    INTEGER4 const* NumFaces,
    INTEGER4 const* FaceNodeCounts,
    INTEGER4 const* FaceNodes,     
    INTEGER4 const* FaceLeftElems, 
    INTEGER4 const* FaceRightElems);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLYBCONN112(
    INTEGER4 const* NumBndryFaces,
    INTEGER4 const* FaceBndryConnectionCounts,
    INTEGER4 const* FaceBndryConnectionElems, 
    INTEGER4 const* FaceBndryConnectionZones);

/*
 *  V11.1 tecio functions   TODO (JN): Tecplot's version is still in flux so the .1 may change
 */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI111(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* FileType,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE111(
    char const*     ZoneTitle,
    INTEGER4 const* ZoneType,
    INTEGER4 const* IMxOrNumPts,
    INTEGER4 const* JMxOrNumElements,
    INTEGER4 const* KMxOrNumFaces,
    INTEGER4 const* ICellMx,
    INTEGER4 const* JCellMx,
    INTEGER4 const* KCellMx,
    double const*   SolutionTime,
    INTEGER4 const* StrandID,
    INTEGER4 const* ParentZone,
    INTEGER4 const* IsBlock,
    INTEGER4 const* NumFaceConnections,
    INTEGER4 const* FaceNeighborMode,
    INTEGER4 const* TotalNumFaceNodes,
    INTEGER4 const* NumConnectedBoundaryFaces,
    INTEGER4 const* TotalNumBoundaryConnections,
    INTEGER4 const* PassiveVarList,
    INTEGER4 const* ValueLocation,
    INTEGER4 const* ShareVarFromZone,
    INTEGER4 const* ShareConnectivityFromZone);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT111(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD111(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND111(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB111(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR111(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO111(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT111(
    double const*   XOrThetaPos,
    double const*   YOrRPos,
    double const*   ZOrUnusedPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    char const*     String,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL111(INTEGER4 const* F);

EXTERNC dataio_tecio_API void STDCALL TECFOREIGN111(INTEGER4 const* OutputForeignByteOrder);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECAUXSTR111(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZAUXSTR111(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECVAUXSTR111(
    INTEGER4 const* Var,
    char const*     Name,
    char const*     Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFACE111(INTEGER4 const* FaceConnections);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECPOLY111(
    INTEGER4 const* FaceNodeCounts,
    INTEGER4 const* FaceNodes,
    INTEGER4 const* FaceLeftElems,
    INTEGER4 const* FaceRightElems,
    INTEGER4 const* FaceBndryConnectionCounts,
    INTEGER4 const* FaceBndryConnectionElems,
    INTEGER2 const* FaceBndryConnectionZones);


/*
 * V11 tecio functions
 */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI110(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE110(
    char const*     ZoneTitle,
    INTEGER4 const* ZoneType,
    INTEGER4 const* IMxOrNumPts,
    INTEGER4 const* JMxOrNumElements,
    INTEGER4 const* KMxOrNumFaces,
    INTEGER4 const* ICellMx,
    INTEGER4 const* JCellMx,
    INTEGER4 const* KCellMx,
    double const*   SolutionTime,
    INTEGER4 const* StrandID,
    INTEGER4 const* ParentZone,
    INTEGER4 const* IsBlock,
    INTEGER4 const* NumFaceConnections,
    INTEGER4 const* FaceNeighborMode,
    INTEGER4 const* PassiveVarList,
    INTEGER4 const* ValueLocation,
    INTEGER4 const* ShareVarFromZone,
    INTEGER4 const* ShareConnectivityFromZone);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT110(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD110(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND110(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB110(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR110(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO110(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT110(
    double const*   XOrThetaPos,
    double const*   YOrRPos,
    double const*   ZOrUnusedPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    char const*     String,
    char const*     mfc);

EXTERNC dataio_tecio_API void STDCALL TECFOREIGN110(INTEGER4 const* OutputForeignByteOrder);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL110(INTEGER4 const* F);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECAUXSTR110(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZAUXSTR110(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECVAUXSTR110(
    INTEGER4 const* Var,
    char const*     Name,
    char const*     Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFACE110(INTEGER4 const* FaceConnections);


/*
 * V10 tecio functions kept for backward compatability.
 */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI100(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE100(
    char const*     ZoneTitle,
    INTEGER4 const* ZoneType,
    INTEGER4 const* IMxOrNumPts,
    INTEGER4 const* JMxOrNumElements,
    INTEGER4 const* KMxOrNumFaces,
    INTEGER4 const* ICellMx,
    INTEGER4 const* JCellMx,
    INTEGER4 const* KCellMx,
    INTEGER4 const* IsBlock,
    INTEGER4 const* NumFaceConnections,
    INTEGER4 const* FaceNeighborMode,
    INTEGER4 const* ValueLocation,
    INTEGER4 const* ShareVarFromZone,
    INTEGER4 const* ShareConnectivityFromZone);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT100(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD100(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND100(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB100(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR100(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO100(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT100(
    double const*   XOrThetaPos,
    double const*   YOrRPos,
    double const*   ZOrUnusedPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    INTEGER4 const* Clipping,
    char const*     String,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL100(INTEGER4 const* F);

EXTERNC dataio_tecio_API void STDCALL TECFOREIGN100(INTEGER4 const* OutputForeignByteOrder);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECAUXSTR100(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZAUXSTR100(
    char const* Name,
    char const* Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECVAUXSTR100(
    INTEGER4 const* Var,
    char const*     Name,
    char const*     Value);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFACE100(INTEGER4 const* FaceConnections);

/* Old V9 functions retained for backward compatibility */

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECINI(
    char const*     Title,
    char const*     Variables,
    char const*     FName,
    char const*     ScratchDir,
    INTEGER4 const* Debug,
    INTEGER4 const* VIsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECZNE(
    char const*     ZoneTitle,
    INTEGER4 const* IMx,
    INTEGER4 const* JMx,
    INTEGER4 const* KMx,
    char const*     ZFormat,
    char const*     DupList);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECDAT(
    INTEGER4 const* N,
    void const*     FieldData,
    INTEGER4 const* IsDouble);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECNOD(INTEGER4 const* NData);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECEND(void);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECLAB(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECUSR(char const* S);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECGEO(
    double const*   XPos,
    double const*   YPos,
    double const*   ZPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* Color,
    INTEGER4 const* FillColor,
    INTEGER4 const* IsFilled,
    INTEGER4 const* GeomType,
    INTEGER4 const* LinePattern,
    double const*   PatternLength,
    double const*   LineThickness,
    INTEGER4 const* NumEllipsePts,
    INTEGER4 const* ArrowheadStyle,
    INTEGER4 const* ArrowheadAttachment,
    double const*   ArrowheadSize,
    double const*   ArrowheadAngle,
    INTEGER4 const* Scope,
    INTEGER4 const* NumSegments,
    INTEGER4 const* NumSegPts,
    float const*    XGeomData,
    float const*    YGeomData,
    float const*    ZGeomData,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECTXT(
    double const*   XPos,
    double const*   YPos,
    INTEGER4 const* PosCoordMode,
    INTEGER4 const* AttachToZone,
    INTEGER4 const* Zone,
    INTEGER4 const* BFont,
    INTEGER4 const* FontHeightUnits,
    double const*   FontHeight,
    INTEGER4 const* BoxType,
    double const*   BoxMargin,
    double const*   BoxLineThickness,
    INTEGER4 const* BoxColor,
    INTEGER4 const* BoxFillColor,
    double const*   Angle,
    INTEGER4 const* Anchor,
    double const*   LineSpacing,
    INTEGER4 const* TextColor,
    INTEGER4 const* Scope,
    char const*     Text,
    char const*     mfc);

EXTERNC dataio_tecio_API INTEGER4 STDCALL TECFIL(INTEGER4 const* F);

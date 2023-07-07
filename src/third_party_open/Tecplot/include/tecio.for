      INTERFACE


      INTEGER(4) FUNCTION tecini142
     & (Title,
     &  Variables,
     &  FName,
     &  ScratchDir,
     &  FileFormat,
     &  FileType,
     &  Debug,
     &  VIsDouble)
        !MS$ATTRIBUTES STDCALL :: tecini142
        !MS$ATTRIBUTES REFERENCE :: Title,Variables,FName,ScratchDir
        !MS$ATTRIBUTES REFERENCE :: FileFormat,FileType,Debug,VIsDouble
        CHARACTER(LEN=*) Title
        CHARACTER(LEN=*) Variables
        CHARACTER(LEN=*) FName
        CHARACTER(LEN=*) ScratchDir
        INTEGER(4)       FileFormat
        INTEGER(4)       FileType
        INTEGER(4)       Debug
        INTEGER(4)       VIsDouble
      END FUNCTION tecini142

      INTEGER(4) FUNCTION teczne142
     & (ZoneTitle,
     &  ZoneType,
     &  IMxOrNumPts,
     &  JMxOrNumElements,
     &  KMxOrNumFaces,
     &  ICellMax,
     &  JCellMax,
     &  KCellMax,
     &  SolutionTime,
     &  StrandID,
     &  ParentZone,
     &  IsBlock,
     &  NumFaceConnections,
     &  FaceNeighborMode,
     &  TotalNumFaceNodes,
     &  NumConnectedBoundaryFaces,
     &  TotalNumBoundaryConnections,
     &  PassiveVarList,
     &  ValueLocation,
     &  ShareVarFromZone,
     &  ShareConnectivityFromZone)
        !MS$ATTRIBUTES STDCALL :: teczne142
        !MS$ATTRIBUTES REFERENCE :: ZoneTitle,ZoneType,IMxOrNumPts
        !MS$ATTRIBUTES REFERENCE :: JMxOrNumElements,KMxOrNumFaces
        !MS$ATTRIBUTES REFERENCE :: ICellMax,JCellMax,KCellMax
        !MS$ATTRIBUTES REFERENCE :: SolutionTime,StrandID,ParentZone
        !MS$ATTRIBUTES REFERENCE :: IsBlock,PassiveVarList
        !MS$ATTRIBUTES REFERENCE :: NumFaceConnections,FaceNeighborMode
        !MS$ATTRIBUTES REFERENCE :: TotalNumFaceNodes
        !MS$ATTRIBUTES REFERENCE :: NumConnectedBoundaryFaces
        !MS$ATTRIBUTES REFERENCE :: TotalNumBoundaryConnections
        !MS$ATTRIBUTES REFERENCE :: ValueLocation,ShareVarFromZone
        !MS$ATTRIBUTES REFERENCE :: ShareConnectivityFromZone
        CHARACTER(LEN=*) ZoneTitle
        INTEGER(4)       ZoneType
        INTEGER(4)       IMxOrNumPts
        INTEGER(4)       JMxOrNumElements
        INTEGER(4)       KMxOrNumFaces
        INTEGER(4)       ICellMax
        INTEGER(4)       JCellMax
        INTEGER(4)       KCellMax
        REAL(8)          SolutionTime
        INTEGER(4)       StrandID
        INTEGER(4)       ParentZone
        INTEGER(4)       IsBlock
        INTEGER(4)       NumFaceConnections
        INTEGER(4)       FaceNeighborMode
        INTEGER(4)       TotalNumFaceNodes
        INTEGER(4)       NumConnectedBoundaryFaces
        INTEGER(4)       TotalNumBoundaryConnections
        INTEGER(4)       PassiveVarList(*)
        INTEGER(4)       ValueLocation(*)
        INTEGER(4)       ShareVarFromZone(*)
        INTEGER(4)       ShareConnectivityFromZone
      END FUNCTION teczne142

      INTEGER(4) FUNCTION tecdat142
     & (N,
     &  FieldData,
     &  IsDouble)
        !MS$ATTRIBUTES STDCALL :: tecdat142
        !MS$ATTRIBUTES REFERENCE :: N,FieldData,IsDouble
        INTEGER(4)  N
        REAL        FieldData(*)
        INTEGER(4)  IsDouble
      END FUNCTION tecdat142

      INTEGER(4) FUNCTION tecnod142
     & (NData)
        !MS$ATTRIBUTES STDCALL :: tecnod142
        !MS$ATTRIBUTES REFERENCE :: NData
        INTEGER(4)  NData(*)
      END FUNCTION tecnod142

      INTEGER(4) FUNCTION tecgeo142
     & (XPos,
     &  YPos,
     &  ZPos,
     &  PosCoordMode,
     &  AttachToZone,
     &  Zone,
     &  Color,
     &  FillColor,
     &  IsFilled,
     &  GeomType,
     &  LinePattern,
     &  PatternLength,
     &  LineThickness,
     &  NumEllipsePts,
     &  ArrowheadStyle,
     &  ArrowheadAttachment,
     &  ArrowheadSize,
     &  ArrowheadAngle,
     &  Scope,
     &  Clipping,
     &  NumSegments,
     &  NumSegPts,
     &  XGeomData,
     &  YGeomData,
     &  ZGeomData,
     &  mfc)
        !MS$ATTRIBUTES STDCALL :: tecgeo142
        !MS$ATTRIBUTES REFERENCE :: XPos,YPos,ZPos,PosCoordMode
        !MS$ATTRIBUTES REFERENCE :: AttachToZone,Zone,Color,FillColor
        !MS$ATTRIBUTES REFERENCE :: IsFilled,GeomType,LinePattern
        !MS$ATTRIBUTES REFERENCE :: PatternLength,LineThickness
        !MS$ATTRIBUTES REFERENCE :: NumEllipsePts,ArrowheadStyle
        !MS$ATTRIBUTES REFERENCE :: ArrowheadAttachment,ArrowheadSize
        !MS$ATTRIBUTES REFERENCE :: ArrowheadAngle,Scope,Clipping
        !MS$ATTRIBUTES REFERENCE :: NumSegments,NumSegPts
        !MS$ATTRIBUTES REFERENCE :: XGeomData,YGeomData
        !MS$ATTRIBUTES REFERENCE :: ZGeomData,mfc
        REAL(8)        XPos
        REAL(8)        YPos
        REAL(8)        ZPos
        INTEGER(4)     PosCoordMode
        INTEGER(4)     AttachToZone
        INTEGER(4)     Zone
        INTEGER(4)     Color
        INTEGER(4)     FillColor
        INTEGER(4)     IsFilled
        INTEGER(4)     GeomType
        INTEGER(4)     LinePattern
        REAL(8)        PatternLength
        REAL(8)        LineThickness
        INTEGER(4)     NumEllipsePts
        INTEGER(4)     ArrowheadStyle
        INTEGER(4)     ArrowheadAttachment
        REAL(8)        ArrowheadSize
        REAL(8)        ArrowheadAngle
        INTEGER(4)     Scope
        INTEGER(4)     Clipping
        INTEGER(4)     NumSegments
        INTEGER(4)     NumSegPts(*)
        REAL(4)        XGeomData(*)
        REAL(4)        YGeomData(*)
        REAL(4)        ZGeomData(*)
        character(len=*) mfc
      END FUNCTION tecgeo142

      INTEGER(4) FUNCTION tectxt142
     & (XOrThetaPos,
     &  YOrRPos,
     &  ZOrUnusedPos,
     &  PosCoordMode,
     &  AttachToZone,
     &  Zone,
     &  Font,
     &  FontHeightUnits,
     &  FontHeight,
     &  BoxType,
     &  BoxMargin,
     &  BoxLineThickness,
     &  BoxColor,
     &  BoxFillColor,
     &  Angle,
     &  Anchor,
     &  LineSpacing,
     &  TextColor,
     &  Scope,
     &  Clipping,
     &  Text,
     &  mfc)
        !MS$ATTRIBUTES STDCALL :: tectxt142
        !MS$ATTRIBUTES REFERENCE :: XOrThetaPos,YOrRPos
        !MS$ATTRIBUTES REFERENCE :: ZOrUnusedPos,PosCoordMode
        !MS$ATTRIBUTES REFERENCE :: AttachToZone,Zone,Font
        !MS$ATTRIBUTES REFERENCE :: FontHeightUnits
        !MS$ATTRIBUTES REFERENCE :: FontHeight,BoxType,BoxMargin
        !MS$ATTRIBUTES REFERENCE :: BoxLineThickness,BoxColor
        !MS$ATTRIBUTES REFERENCE :: BoxFillColor,Angle,Anchor
        !MS$ATTRIBUTES REFERENCE :: LineSpacing,TextColor,Scope,Clipping
        !MS$ATTRIBUTES REFERENCE :: Text,mfc
        REAL(8)          XOrThetaPos
        REAL(8)          YOrRPos
        REAL(8)          ZOrUnusedPos
        INTEGER(4)       PosCoordMode
        INTEGER(4)       AttachToZone
        INTEGER(4)       Zone
        INTEGER(4)       Font
        INTEGER(4)       FontHeightUnits
        REAL(8)          FontHeight
        INTEGER(4)       BoxType
        REAL(8)          BoxMargin
        REAL(8)          BoxLineThickness
        INTEGER(4)       BoxColor
        INTEGER(4)       BoxFillColor
        REAL(8)          Angle
        INTEGER(4)       Anchor
        REAL(8)          LineSpacing
        INTEGER(4)       TextColor
        INTEGER(4)       Scope
        INTEGER(4)       Clipping
        CHARACTER(LEN=*) Text
        CHARACTER(LEN=*) mfc
      END FUNCTION tectxt142

      INTEGER(4) FUNCTION teclab142
     & (S)
        !MS$ATTRIBUTES STDCALL :: teclab142
        !MS$ATTRIBUTES REFERENCE :: S
        character(len=*) S
      END FUNCTION teclab142

      INTEGER(4) FUNCTION tecfil142
     & (F)
        !MS$ATTRIBUTES STDCALL :: tecfil142
        !MS$ATTRIBUTES REFERENCE :: F
        INTEGER(4)  F
      END FUNCTION tecfil142

      SUBROUTINE tecforeign142
     & (OutputForeignByteOrder)
        !MS$ATTRIBUTES STDCALL :: tecforeign142
        !MS$ATTRIBUTES REFERENCE :: OutputForeignByteOrder
        INTEGER(4) OutputForeignByteOrder
      END SUBROUTINE tecforeign142

      INTEGER(4) FUNCTION tecend142()
        !MS$ATTRIBUTES STDCALL :: tecend142
      END FUNCTION tecend142

      INTEGER(4) FUNCTION tecusr142
     & (S)
        !MS$ATTRIBUTES STDCALL :: tecusr142
        !MS$ATTRIBUTES REFERENCE :: S
        character(len=*) S
      END FUNCTION tecusr142

      INTEGER(4) FUNCTION tecauxstr142
     & (Name,
     &  Value)
        !MS$ATTRIBUTES STDCALL :: tecauxstr142
        !MS$ATTRIBUTES REFERENCE :: Name, Value
        CHARACTER(LEN=*) Name 
        CHARACTER(LEN=*) Value
      END FUNCTION tecauxstr142

      INTEGER(4) FUNCTION teczauxstr142
     & (Name,
     &  Value)
        !MS$ATTRIBUTES STDCALL :: teczauxstr142
        !MS$ATTRIBUTES REFERENCE :: Name, Value
        CHARACTER(LEN=*) Name 
        CHARACTER(LEN=*) Value
      END FUNCTION teczauxstr142

      INTEGER(4) FUNCTION tecvauxstr142
     & (Name,
     &  Value)
        !MS$ATTRIBUTES STDCALL :: tecvauxstr142
        !MS$ATTRIBUTES REFERENCE :: Name, Value
        CHARACTER(LEN=*) Name 
        CHARACTER(LEN=*) Value
      END FUNCTION tecvauxstr142

      INTEGER(4) FUNCTION tecface142
     & (FaceConnections)
        !MS$ATTRIBUTES STDCALL :: tecface142
        !MS$ATTRIBUTES REFERENCE :: FaceConnections
        INTEGER(4) FaceConnections(*)
      END FUNCTION tecface142

      INTEGER(4) FUNCTION tecpoly142
     & (FaceNodeCounts,
     &  FaceNodes,
     &  FaceLeftElems,
     &  FaceRightElems,
     &  FaceBndryConnectionCounts,
     &  FaceBndryConnectionElems,
     &  FaceBndryConnectionZones)
        !MS$ATTRIBUTES STDCALL   :: tecpoly142
        !MS$ATTRIBUTES REFERENCE :: FaceNodes
        !MS$ATTRIBUTES REFERENCE :: FaceLeftElems
        !MS$ATTRIBUTES REFERENCE :: FaceRightElems
        !MS$ATTRIBUTES REFERENCE :: FaceBndryConnectionCounts
        !MS$ATTRIBUTES REFERENCE :: FaceBndryConnectionElems
        !MS$ATTRIBUTES REFERENCE :: FaceBndryConnectionZones
        INTEGER(4) FaceNodeCounts(*)
        INTEGER(4) FaceNodes(*)
        INTEGER(4) FaceLeftElems(*)
        INTEGER(4) FaceRightElems(*)
        INTEGER(4) FaceBndryConnectionCounts(*)
        INTEGER(4) FaceBndryConnectionElems(*)
        INTEGER(2) FaceBndryConnectionZones(*)
      END FUNCTION tecpoly142


      END INTERFACE

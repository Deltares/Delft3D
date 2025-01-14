      interface

         integer(4) function tecini142 &
            (Title, &
             Variables, &
             FName, &
             ScratchDir, &
             FileFormat, &
             FileType, &
             Debug, &
             VIsDouble) bind(C)
            character(LEN=*) Title
            character(LEN=*) Variables
            character(LEN=*) FName
            character(LEN=*) ScratchDir
            integer(4) FileFormat
            integer(4) FileType
            integer(4) Debug
            integer(4) VIsDouble
         end function tecini142

         integer(4) function teczne142 &
            (ZoneTitle, &
             ZoneType, &
             IMxOrNumPts, &
             JMxOrNumElements, &
             KMxOrNumFaces, &
             ICellMax, &
             JCellMax, &
             KCellMax, &
             SolutionTime, &
             StrandID, &
             ParentZone, &
             IsBlock, &
             NumFaceConnections, &
             FaceNeighborMode, &
             TotalNumFaceNodes, &
             NumConnectedBoundaryFaces, &
             TotalNumBoundaryConnections, &
             PassiveVarList, &
             ValueLocation, &
             ShareVarFromZone, &
             ShareConnectivityFromZone) bind(C)
            character(LEN=*) ZoneTitle
            integer(4) ZoneType
            integer(4) IMxOrNumPts
            integer(4) JMxOrNumElements
            integer(4) KMxOrNumFaces
            integer(4) ICellMax
            integer(4) JCellMax
            integer(4) KCellMax
            real(8) SolutionTime
            integer(4) StrandID
            integer(4) ParentZone
            integer(4) IsBlock
            integer(4) NumFaceConnections
            integer(4) FaceNeighborMode
            integer(4) TotalNumFaceNodes
            integer(4) NumConnectedBoundaryFaces
            integer(4) TotalNumBoundaryConnections
            integer(4) PassiveVarList(*)
            integer(4) ValueLocation(*)
            integer(4) ShareVarFromZone(*)
            integer(4) ShareConnectivityFromZone
         end function teczne142

         integer(4) function tecdat142 &
            (N, &
             FieldData, &
             IsDouble) bind(C)
            integer(4) N
            real FieldData(*)
            integer(4) IsDouble
         end function tecdat142

         integer(4) function tecnod142 &
            (NData) bind(C)
            integer(4) NData(*)
         end function tecnod142

         integer(4) function tecgeo142 &
            (XPos, &
             YPos, &
             ZPos, &
             PosCoordMode, &
             AttachToZone, &
             Zone, &
             Color, &
             FillColor, &
             IsFilled, &
             GeomType, &
             LinePattern, &
             PatternLength, &
             LineThickness, &
             NumEllipsePts, &
             ArrowheadStyle, &
             ArrowheadAttachment, &
             ArrowheadSize, &
             ArrowheadAngle, &
             Scope, &
             Clipping, &
             NumSegments, &
             NumSegPts, &
             XGeomData, &
             YGeomData, &
             ZGeomData, &
             mfc) bind(C)
            real(8) XPos
            real(8) YPos
            real(8) ZPos
            integer(4) PosCoordMode
            integer(4) AttachToZone
            integer(4) Zone
            integer(4) Color
            integer(4) FillColor
            integer(4) IsFilled
            integer(4) GeomType
            integer(4) LinePattern
            real(8) PatternLength
            real(8) LineThickness
            integer(4) NumEllipsePts
            integer(4) ArrowheadStyle
            integer(4) ArrowheadAttachment
            real(8) ArrowheadSize
            real(8) ArrowheadAngle
            integer(4) Scope
            integer(4) Clipping
            integer(4) NumSegments
            integer(4) NumSegPts(*)
            real(4) XGeomData(*)
            real(4) YGeomData(*)
            real(4) ZGeomData(*)
            character(len=*) mfc
         end function tecgeo142

         integer(4) function tectxt142 &
            (XOrThetaPos, &
             YOrRPos, &
             ZOrUnusedPos, &
             PosCoordMode, &
             AttachToZone, &
             Zone, &
             Font, &
             FontHeightUnits, &
             FontHeight, &
             BoxType, &
             BoxMargin, &
             BoxLineThickness, &
             BoxColor, &
             BoxFillColor, &
             Angle, &
             Anchor, &
             LineSpacing, &
             TextColor, &
             Scope, &
             Clipping, &
             Text, &
             mfc) bind(C)
            real(8) XOrThetaPos
            real(8) YOrRPos
            real(8) ZOrUnusedPos
            integer(4) PosCoordMode
            integer(4) AttachToZone
            integer(4) Zone
            integer(4) Font
            integer(4) FontHeightUnits
            real(8) FontHeight
            integer(4) BoxType
            real(8) BoxMargin
            real(8) BoxLineThickness
            integer(4) BoxColor
            integer(4) BoxFillColor
            real(8) Angle
            integer(4) Anchor
            real(8) LineSpacing
            integer(4) TextColor
            integer(4) Scope
            integer(4) Clipping
            character(LEN=*) Text
            character(LEN=*) mfc
         end function tectxt142

         integer(4) function teclab142 &
            (S) bind(C)
            character(len=*) S
         end function teclab142

         integer(4) function tecfil142 &
            (F) bind(C)
            integer(4) F
         end function tecfil142

         subroutine tecforeign142 &
            (OutputForeignByteOrder) bind(C)
            integer(4) OutputForeignByteOrder
         end subroutine tecforeign142

         integer(4) function tecend142() bind(C)
         end function tecend142

         integer(4) function tecusr142 &
            (S) bind(C)
            character(len=*) S
         end function tecusr142

         integer(4) function tecauxstr142 &
            (Name, &
             value) bind(C)
            character(LEN=*) Name
            character(LEN=*) value
         end function tecauxstr142

         integer(4) function teczauxstr142 &
            (Name, &
             value) bind(C)
            character(LEN=*) Name
            character(LEN=*) value
         end function teczauxstr142

         integer(4) function tecvauxstr142 &
            (Name, &
             value) bind(C)
            character(LEN=*) Name
            character(LEN=*) value
         end function tecvauxstr142

         integer(4) function tecface142 &
            (FaceConnections) bind(C)
            integer(4) FaceConnections(*)
         end function tecface142

         integer(4) function tecpoly142 &
            (FaceNodeCounts, &
             FaceNodes, &
             FaceLeftElems, &
             FaceRightElems, &
             FaceBndryConnectionCounts, &
             FaceBndryConnectionElems, &
             FaceBndryConnectionZones) bind(C)
            integer(4) FaceNodeCounts(*)
            integer(4) FaceNodes(*)
            integer(4) FaceLeftElems(*)
            integer(4) FaceRightElems(*)
            integer(4) FaceBndryConnectionCounts(*)
            integer(4) FaceBndryConnectionElems(*)
            integer(2) FaceBndryConnectionZones(*)
         end function tecpoly142

      end interface

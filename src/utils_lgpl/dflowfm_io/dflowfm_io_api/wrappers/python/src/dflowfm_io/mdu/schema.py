"""GENERATED from mdu.json by codegen/generate_schema.py - do not edit.

Layer 2: typed per-keyword access to MDU properties. Each section is a class of typed
properties over the generated MduModel (Layer 1); MduSchema exposes them keyed by section.
"""

from datetime import datetime
from pathlib import Path

from dflowfm_io.mdu.model import MduModel


class GeneralSection:
    """Typed access to the [general] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def program(self) -> str:
        """Program."""
        return self._model.get_string("general.program")

    @program.setter
    def program(self, value: str) -> None:
        self._model.set_string("general.program", value)

    @property
    def version(self) -> str:
        """Version number of computational kernel."""
        return self._model.get_string("general.version")

    @version.setter
    def version(self, value: str) -> None:
        self._model.set_string("general.version", value)

    @property
    def fileType(self) -> str:
        """File type. Do not edit this."""
        return self._model.get_enum_name("general.filetype")

    @fileType.setter
    def fileType(self, value: str) -> None:
        self._model.set_enum_name("general.filetype", value)

    @property
    def fileVersion(self) -> str:
        """File version. Do not edit this."""
        return self._model.get_string("general.fileversion")

    @fileVersion.setter
    def fileVersion(self, value: str) -> None:
        self._model.set_string("general.fileversion", value)

    @property
    def guiVersion(self) -> str:
        """Version number of GUI."""
        return self._model.get_string("general.guiversion")

    @guiVersion.setter
    def guiVersion(self, value: str) -> None:
        self._model.set_string("general.guiversion", value)

    @property
    def autoStart(self) -> str:
        """Autostart simulation after loading MDU or not."""
        return self._model.get_enum_name("general.autostart")

    @autoStart.setter
    def autoStart(self, value: str) -> None:
        self._model.set_enum_name("general.autostart", value)

    @property
    def pathsRelativeToParent(self) -> bool:
        """Resolve file names (e.g. inside the *.ext file) relative to their direct parent, instead of to the top-level MDU working dir."""
        return self._model.get_bool("general.pathsrelativetoparent")

    @pathsRelativeToParent.setter
    def pathsRelativeToParent(self, value: bool) -> None:
        self._model.set_bool("general.pathsrelativetoparent", value)

    @property
    def modelSpecific(self) -> str:
        """Optional 'model specific ID', to enable certain custom runtime function calls (instead of via MDU name)."""
        return self._model.get_string("general.modelspecific")

    @modelSpecific.setter
    def modelSpecific(self, value: str) -> None:
        self._model.set_string("general.modelspecific", value)

    @property
    def inputSpecific(self) -> bool:
        """Use of hardcoded specific inputs, shall not be used by users."""
        return self._model.get_bool("general.inputspecific")

    @inputSpecific.setter
    def inputSpecific(self, value: bool) -> None:
        self._model.set_bool("general.inputspecific", value)

    @property
    def convertLongCulverts(self) -> bool:
        """Convert long culvert input to 1D2D long culverts."""
        return self._model.get_bool("general.convertlongculverts")

    @convertLongCulverts.setter
    def convertLongCulverts(self, value: bool) -> None:
        self._model.set_bool("general.convertlongculverts", value)


class GeometrySection:
    """Typed access to the [geometry] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def netFile(self) -> Path:
        """Net file (*_net.nc) containing mesh information."""
        return self._model.get_path("geometry.netfile")

    @netFile.setter
    def netFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.netfile", value)

    @property
    def dryPointsFile(self) -> list[Path]:
        """Dry points file (*.xyz), third column dummy z values, or polygon file (*.pol)."""
        return self._model.get_path_list("geometry.drypointsfile")

    @dryPointsFile.setter
    def dryPointsFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.drypointsfile", value)

    @property
    def gridEnclosureFile(self) -> Path:
        """Enclosure file (*.pol) to clip outer parts from the grid."""
        return self._model.get_path("geometry.gridenclosurefile")

    @gridEnclosureFile.setter
    def gridEnclosureFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.gridenclosurefile", value)

    @property
    def structureFile(self) -> list[Path]:
        """File (*.ini) containing list of hydraulic structures. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes."""
        return self._model.get_path_list("geometry.structurefile")

    @structureFile.setter
    def structureFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.structurefile", value)

    @property
    def gulliesFile(self) -> Path:
        """Polyline file (*_gul.pliz), containing lowest bed level along talweg x, y, z level."""
        return self._model.get_path("geometry.gulliesfile")

    @gulliesFile.setter
    def gulliesFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.gulliesfile", value)

    @property
    def roofsFile(self) -> Path:
        """Polyline file (*_roof.pliz), containing roofgutter heights x, y, z level."""
        return self._model.get_path("geometry.roofsfile")

    @roofsFile.setter
    def roofsFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.roofsfile", value)

    @property
    def iniFieldFile(self) -> Path:
        """Initial and parameter field file (*.ini)."""
        return self._model.get_path("geometry.inifieldfile")

    @iniFieldFile.setter
    def iniFieldFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.inifieldfile", value)

    @property
    def waterLevIniFile(self) -> Path:
        """Initial water levels sample file (*.xyz)."""
        return self._model.get_path("geometry.waterlevinifile")

    @waterLevIniFile.setter
    def waterLevIniFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.waterlevinifile", value)

    @property
    def landBoundaryFile(self) -> list[Path]:
        """Land boundary file (*.ldb), only used for plotting."""
        return self._model.get_path_list("geometry.landboundaryfile")

    @landBoundaryFile.setter
    def landBoundaryFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.landboundaryfile", value)

    @property
    def thinDamFile(self) -> list[Path]:
        """Polyline file (*_thd.pli), containing polyline(s) for tracing thin dams."""
        return self._model.get_path_list("geometry.thindamfile")

    @thinDamFile.setter
    def thinDamFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.thindamfile", value)

    @property
    def fixedWeirFile(self) -> list[Path]:
        """Polyline file (*_fxw.pliz), containing polyline(s) with x, y, z where z = fixed weir top levels (formerly fixed weir)."""
        return self._model.get_path_list("geometry.fixedweirfile")

    @fixedWeirFile.setter
    def fixedWeirFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.fixedweirfile", value)

    @property
    def pillarFile(self) -> list[Path]:
        """Polyline file (*_pillar.pliz), containing four colums with x, y, diameter and Cd coefficient for bridge pillars."""
        return self._model.get_path_list("geometry.pillarfile")

    @pillarFile.setter
    def pillarFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.pillarfile", value)

    @property
    def useCaching(self) -> bool:
        """Use caching for geometrical/network-related items."""
        return self._model.get_bool("geometry.usecaching")

    @useCaching.setter
    def useCaching(self, value: bool) -> None:
        self._model.set_bool("geometry.usecaching", value)

    @property
    def vertPlizFile(self) -> Path:
        """Polyline file (*_vlay.pliz), containing x, y, Z where first Z = nr of layers, second Z = laytyp. For `layerType` = 3 (mixed layering in polygon regions)."""
        return self._model.get_path("geometry.vertplizfile")

    @vertPlizFile.setter
    def vertPlizFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.vertplizfile", value)

    @property
    def frictFile(self) -> list[Path]:
        """Files with roughness data for 1D (space separated)."""
        return self._model.get_path_list("geometry.frictfile")

    @frictFile.setter
    def frictFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("geometry.frictfile", value)

    @property
    def crossDefFile(self) -> Path:
        """File containing the cross section definitions for all cross section shapes."""
        return self._model.get_path("geometry.crossdeffile")

    @crossDefFile.setter
    def crossDefFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.crossdeffile", value)

    @property
    def crossLocFile(self) -> Path:
        """File containing the location definitions of the cross sections on a 1D network."""
        return self._model.get_path("geometry.crosslocfile")

    @crossLocFile.setter
    def crossLocFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.crosslocfile", value)

    @property
    def storageNodeFile(self) -> Path:
        """File containing the specification of storage nodes and/or manholes to add extra storage to 1D models."""
        return self._model.get_path("geometry.storagenodefile")

    @storageNodeFile.setter
    def storageNodeFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.storagenodefile", value)

    @property
    def _1D2DLinkFile(self) -> Path:
        """File containing the custom parameterization of 1D-2D links."""
        return self._model.get_path("geometry.1d2dlinkfile")

    @_1D2DLinkFile.setter
    def _1D2DLinkFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.1d2dlinkfile", value)

    @property
    def allowBndAtBifurcation(self) -> bool:
        """Allow 1D boundary node when connecting branch leads to bifurcation."""
        return self._model.get_bool("geometry.allowbndatbifurcation")

    @allowBndAtBifurcation.setter
    def allowBndAtBifurcation(self, value: bool) -> None:
        self._model.set_bool("geometry.allowbndatbifurcation", value)

    @property
    def profLocFile(self) -> Path:
        """(*_proflocation.xyz) x, y, z, z = profile refnumber."""
        return self._model.get_path("geometry.proflocfile")

    @profLocFile.setter
    def profLocFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.proflocfile", value)

    @property
    def profDefFile(self) -> Path:
        """(*_profdefinition.def) definition for all profile nrs."""
        return self._model.get_path("geometry.profdeffile")

    @profDefFile.setter
    def profDefFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.profdeffile", value)

    @property
    def profDefXyzFile(self) -> Path:
        """(*_profdefinition.def) definition for all profile nrs."""
        return self._model.get_path("geometry.profdefxyzfile")

    @profDefXyzFile.setter
    def profDefXyzFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.profdefxyzfile", value)

    @property
    def partitionFile(self) -> Path:
        """(*_part.pol), polyline(s) x, y."""
        return self._model.get_path("geometry.partitionfile")

    @partitionFile.setter
    def partitionFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.partitionfile", value)

    @property
    def dxWuiMin2D(self) -> float:
        """Smallest fraction dx/wu , set dx > `dxWuiMin2D`*wu."""
        return self._model.get_double("geometry.dxwuimin2d")

    @dxWuiMin2D.setter
    def dxWuiMin2D(self, value: float) -> None:
        self._model.set_double("geometry.dxwuimin2d", value)

    @property
    def waterLevIni(self) -> float:
        """Initial water level."""
        return self._model.get_double("geometry.waterlevini")

    @waterLevIni.setter
    def waterLevIni(self, value: float) -> None:
        self._model.set_double("geometry.waterlevini", value)

    @property
    def bedLevUni(self) -> float:
        """Uniform bed level (only if `bedLevType`>=3), used at missing z-values in `netFile`."""
        return self._model.get_double("geometry.bedlevuni")

    @bedLevUni.setter
    def bedLevUni(self, value: float) -> None:
        self._model.set_double("geometry.bedlevuni", value)

    @property
    def bedSlope(self) -> float:
        """Bed slope inclination, sets zk = `bedLevUni` + x*`bedSlope` and sets zbndz = xbndz*`bedSlope`."""
        return self._model.get_double("geometry.bedslope")

    @bedSlope.setter
    def bedSlope(self, value: float) -> None:
        self._model.set_double("geometry.bedslope", value)

    @property
    def bedLevType(self) -> str:
        """Bed level definition type."""
        return self._model.get_enum_name("geometry.bedlevtype")

    @bedLevType.setter
    def bedLevType(self, value: str) -> None:
        self._model.set_enum_name("geometry.bedlevtype", value)

    @property
    def blMeanBelow(self) -> float:
        """If not -999.0, below this level the cell centre bedlevel is the mean of surrounding netnodes."""
        return self._model.get_double("geometry.blmeanbelow")

    @blMeanBelow.setter
    def blMeanBelow(self, value: float) -> None:
        self._model.set_double("geometry.blmeanbelow", value)

    @property
    def blMinAbove(self) -> float:
        """If not -999.0, above this level the cell centre bedlevel is the min of surrounding netnodes."""
        return self._model.get_double("geometry.blminabove")

    @blMinAbove.setter
    def blMinAbove(self, value: float) -> None:
        self._model.set_double("geometry.blminabove", value)

    @property
    def angLat(self) -> float:
        """Angle of latitude S-N, 0=on Equator (and thus no Coriolis force). Only required for Coriolis on Cartesian grids and for heat flux modelling."""
        return self._model.get_double("geometry.anglat")

    @angLat.setter
    def angLat(self, value: float) -> None:
        self._model.set_double("geometry.anglat", value)

    @property
    def angLon(self) -> float:
        """Angle of longitude E-W, 0=Greenwich Mean Time. Only required for heat flux modelling."""
        return self._model.get_double("geometry.anglon")

    @angLon.setter
    def angLon(self, value: float) -> None:
        self._model.set_double("geometry.anglon", value)

    @property
    def conveyance2D(self) -> str:
        """2D analytic conveyance description."""
        return self._model.get_enum_name("geometry.conveyance2d")

    @conveyance2D.setter
    def conveyance2D(self, value: str) -> None:
        self._model.set_enum_name("geometry.conveyance2d", value)

    @property
    def nonLin1D(self) -> str:
        """Non-linear 1D volumes, applicable for models with closed cross sections."""
        return self._model.get_enum_name("geometry.nonlin1d")

    @nonLin1D.setter
    def nonLin1D(self, value: str) -> None:
        self._model.set_enum_name("geometry.nonlin1d", value)

    @property
    def nonLin2D(self) -> bool:
        """Non-linear 2D volumes, only i.c.m. `bedLevType`=3 and `conveyance2D`>=1."""
        return self._model.get_bool("geometry.nonlin2d")

    @nonLin2D.setter
    def nonLin2D(self, value: bool) -> None:
        self._model.set_bool("geometry.nonlin2d", value)

    @property
    def slotw1D(self) -> float:
        """Minimum slotwidth 1D."""
        return self._model.get_double("geometry.slotw1d")

    @slotw1D.setter
    def slotw1D(self, value: float) -> None:
        self._model.set_double("geometry.slotw1d", value)

    @property
    def slotw2D(self) -> float:
        """Minimum slotwidth 2D."""
        return self._model.get_double("geometry.slotw2d")

    @slotw2D.setter
    def slotw2D(self, value: float) -> None:
        self._model.set_double("geometry.slotw2d", value)

    @property
    def uniformWidth1D(self) -> float:
        """Uniform width for 1D profiles and 1D2D internal links."""
        return self._model.get_double("geometry.uniformwidth1d")

    @uniformWidth1D.setter
    def uniformWidth1D(self, value: float) -> None:
        self._model.set_double("geometry.uniformwidth1d", value)

    @property
    def uniformHeight1D(self) -> float:
        """Uniform height for 1D profiles and 1D2D internal links."""
        return self._model.get_double("geometry.uniformheight1d")

    @uniformHeight1D.setter
    def uniformHeight1D(self, value: float) -> None:
        self._model.set_double("geometry.uniformheight1d", value)

    @property
    def uniformWidth1DStreetInlets(self) -> float:
        """Uniform width for street inlets."""
        return self._model.get_double("geometry.uniformwidth1dstreetinlets")

    @uniformWidth1DStreetInlets.setter
    def uniformWidth1DStreetInlets(self, value: float) -> None:
        self._model.set_double("geometry.uniformwidth1dstreetinlets", value)

    @property
    def uniformHeight1DStreetInlets(self) -> float:
        """Uniform height for street inlets."""
        return self._model.get_double("geometry.uniformheight1dstreetinlets")

    @uniformHeight1DStreetInlets.setter
    def uniformHeight1DStreetInlets(self, value: float) -> None:
        self._model.set_double("geometry.uniformheight1dstreetinlets", value)

    @property
    def uniformTyp1DStreetInlets(self) -> str:
        """Uniform cross section type for street inlets."""
        return self._model.get_enum_name("geometry.uniformtyp1dstreetinlets")

    @uniformTyp1DStreetInlets.setter
    def uniformTyp1DStreetInlets(self, value: str) -> None:
        self._model.set_enum_name("geometry.uniformtyp1dstreetinlets", value)

    @property
    def uniformWidth1DRoofGutterPipes(self) -> float:
        """Uniform width for roof gutter pipes."""
        return self._model.get_double("geometry.uniformwidth1droofgutterpipes")

    @uniformWidth1DRoofGutterPipes.setter
    def uniformWidth1DRoofGutterPipes(self, value: float) -> None:
        self._model.set_double("geometry.uniformwidth1droofgutterpipes", value)

    @property
    def uniformHeight1DRoofGutterPipes(self) -> float:
        """Uniform height for roof gutter pipes."""
        return self._model.get_double("geometry.uniformheight1droofgutterpipes")

    @uniformHeight1DRoofGutterPipes.setter
    def uniformHeight1DRoofGutterPipes(self, value: float) -> None:
        self._model.set_double("geometry.uniformheight1droofgutterpipes", value)

    @property
    def uniformTyp1DRoofGutterPipes(self) -> str:
        """Uniform cross section type for type roof gutter pipes."""
        return self._model.get_enum_name("geometry.uniformtyp1droofgutterpipes")

    @uniformTyp1DRoofGutterPipes.setter
    def uniformTyp1DRoofGutterPipes(self, value: str) -> None:
        self._model.set_enum_name("geometry.uniformtyp1droofgutterpipes", value)

    @property
    def sillHeightMin(self) -> float:
        """Fixed weir only active if both ground heights are larger than this value."""
        return self._model.get_double("geometry.sillheightmin")

    @sillHeightMin.setter
    def sillHeightMin(self, value: float) -> None:
        self._model.set_double("geometry.sillheightmin", value)

    @property
    def makeOrthoCenters(self) -> bool:
        """Switch from circumcentres to orthocenters in geominit."""
        return self._model.get_bool("geometry.makeorthocenters")

    @makeOrthoCenters.setter
    def makeOrthoCenters(self, value: bool) -> None:
        self._model.set_bool("geometry.makeorthocenters", value)

    @property
    def dCenterInside(self) -> float:
        """Limit cell center; 1.0:in cell <-> 0.0:on c/g."""
        return self._model.get_double("geometry.dcenterinside")

    @dCenterInside.setter
    def dCenterInside(self, value: float) -> None:
        self._model.set_double("geometry.dcenterinside", value)

    @property
    def circumcenterMethod(self) -> str:
        """Circumcenter computation method."""
        return self._model.get_enum_name("geometry.circumcentermethod")

    @circumcenterMethod.setter
    def circumcenterMethod(self, value: str) -> None:
        self._model.set_enum_name("geometry.circumcentermethod", value)

    @property
    def circumcenterTolerance(self) -> float:
        """Tolerance for convergence of circumcenter method."""
        return self._model.get_double("geometry.circumcentertolerance")

    @circumcenterTolerance.setter
    def circumcenterTolerance(self, value: float) -> None:
        self._model.set_double("geometry.circumcentertolerance", value)

    @property
    def baMin(self) -> float:
        """Minimum grid cell area, i.c.m. cutcells."""
        return self._model.get_double("geometry.bamin")

    @baMin.setter
    def baMin(self, value: float) -> None:
        self._model.set_double("geometry.bamin", value)

    @property
    def openBoundaryTolerance(self) -> float:
        """Search tolerance factor between boundary polyline and grid cells."""
        return self._model.get_double("geometry.openboundarytolerance")

    @openBoundaryTolerance.setter
    def openBoundaryTolerance(self, value: float) -> None:
        self._model.set_double("geometry.openboundarytolerance", value)

    @property
    def renumberFlowNodes(self) -> bool:
        """Renumber the flow nodes."""
        return self._model.get_bool("geometry.renumberflownodes")

    @renumberFlowNodes.setter
    def renumberFlowNodes(self, value: bool) -> None:
        self._model.set_bool("geometry.renumberflownodes", value)

    @property
    def kmx(self) -> int:
        """Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM."""
        return self._model.get_int("geometry.kmx")

    @kmx.setter
    def kmx(self, value: int) -> None:
        self._model.set_int("geometry.kmx", value)

    @property
    def layerType(self) -> str:
        """Vertical layer type."""
        return self._model.get_enum_name("geometry.layertype")

    @layerType.setter
    def layerType(self, value: str) -> None:
        self._model.set_enum_name("geometry.layertype", value)

    @property
    def numTopSig(self) -> int:
        """Number of sigma-layers on top of z-layers in case of z-sigma-layers."""
        return self._model.get_int("geometry.numtopsig")

    @numTopSig.setter
    def numTopSig(self, value: int) -> None:
        self._model.set_int("geometry.numtopsig", value)

    @property
    def sigmaGrowthFactor(self) -> float:
        """Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed."""
        return self._model.get_double("geometry.sigmagrowthfactor")

    @sigmaGrowthFactor.setter
    def sigmaGrowthFactor(self, value: float) -> None:
        self._model.set_double("geometry.sigmagrowthfactor", value)

    @property
    def zLayerGrowthFactor(self) -> float:
        """Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed."""
        return self._model.get_double("geometry.zlayergrowthfactor")

    @zLayerGrowthFactor.setter
    def zLayerGrowthFactor(self, value: float) -> None:
        self._model.set_double("geometry.zlayergrowthfactor", value)

    @property
    def floorLevTopLay(self) -> float:
        """The floor level of the top layer."""
        return self._model.get_double("geometry.floorlevtoplay")

    @floorLevTopLay.setter
    def floorLevTopLay(self, value: float) -> None:
        self._model.set_double("geometry.floorlevtoplay", value)

    @property
    def dzTop(self) -> float:
        """z-layer thickness of layers above level `dzTopUniAboveZ`."""
        return self._model.get_double("geometry.dztop")

    @dzTop.setter
    def dzTop(self, value: float) -> None:
        self._model.set_double("geometry.dztop", value)

    @property
    def dzTopUniAboveZ(self) -> float:
        """The level above which the layers will have uniform thickness of `dzTop`."""
        return self._model.get_double("geometry.dztopuniabovez")

    @dzTopUniAboveZ.setter
    def dzTopUniAboveZ(self, value: float) -> None:
        self._model.set_double("geometry.dztopuniabovez", value)

    @property
    def numTopSigUniform(self) -> str:
        """The number of sigma-layers in a z-sigma-model is constant or decreasing (depending on local depth)."""
        return self._model.get_enum_name("geometry.numtopsiguniform")

    @numTopSigUniform.setter
    def numTopSigUniform(self, value: str) -> None:
        self._model.set_enum_name("geometry.numtopsiguniform", value)

    @property
    def zLayBot(self) -> float:
        """If specified, first z-layer starts from `zLayBot`, if not, it starts from the lowest bed point."""
        return self._model.get_double("geometry.zlaybot")

    @zLayBot.setter
    def zLayBot(self, value: float) -> None:
        self._model.set_double("geometry.zlaybot", value)

    @property
    def zLayTop(self) -> float:
        """If specified, highest z-layer starts from `zLayTop`, if not, it ends at the initial water level."""
        return self._model.get_double("geometry.zlaytop")

    @zLayTop.setter
    def zLayTop(self, value: float) -> None:
        self._model.set_double("geometry.zlaytop", value)

    @property
    def stretchType(self) -> str:
        """Stretching type for non-uniform layers."""
        return self._model.get_enum_name("geometry.stretchtype")

    @stretchType.setter
    def stretchType(self, value: str) -> None:
        self._model.set_enum_name("geometry.stretchtype", value)

    @property
    def stretchCoef(self) -> list[float]:
        """Coefficients for sigma layer. For `stretchType`=1: percentages of the layers, user defined, laycof(`kmx`). For `stretchType`=2: Stretching level, and two coefficients for layer growth, laycof(3)."""
        return self._model.get_double_list("geometry.stretchcoef")

    @stretchCoef.setter
    def stretchCoef(self, value: list[float]) -> None:
        self._model.set_double_list("geometry.stretchcoef", value)

    @property
    def dxMin1D(self) -> float:
        """Minimum 1D link length."""
        return self._model.get_double("geometry.dxmin1d")

    @dxMin1D.setter
    def dxMin1D(self, value: float) -> None:
        self._model.set_double("geometry.dxmin1d", value)

    @property
    def dxDoubleAt1DEndNodes(self) -> bool:
        """Extend a 1D grid cell at the end of a network with 0.5∆x."""
        return self._model.get_bool("geometry.dxdoubleat1dendnodes")

    @dxDoubleAt1DEndNodes.setter
    def dxDoubleAt1DEndNodes(self, value: bool) -> None:
        self._model.set_bool("geometry.dxdoubleat1dendnodes", value)

    @property
    def changeVelocityAtStructures(self) -> bool:
        """Ignore structure dimensions for the velocity at hydraulic structures, when calculating the surrounding cell centered flow velocities."""
        return self._model.get_bool("geometry.changevelocityatstructures")

    @changeVelocityAtStructures.setter
    def changeVelocityAtStructures(self, value: bool) -> None:
        self._model.set_bool("geometry.changevelocityatstructures", value)

    @property
    def changeStructureDimensions(self) -> bool:
        """Change the structure dimensions in case these are inconsistent with the channel dimensions. ⋄ weirs, orifices, general structures: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the crest level is lower than the bed level, the crest level is set to the bed level. ⋄ bridges: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the flow area of the bridge exceeds the upstream flow area the flow area of the bridge is set to the upstream flow area. ⋄ universal weirs: only the crest level is checked and changed. NOTE: It is strongly advised not to change this parameter (true). Since turning this option off can lead to instabilities and unrealistic results."""
        return self._model.get_bool("geometry.changestructuredimensions")

    @changeStructureDimensions.setter
    def changeStructureDimensions(self, value: bool) -> None:
        self._model.set_bool("geometry.changestructuredimensions", value)

    @property
    def calculateBedLevelOverNonActiveLinks(self) -> bool:
        """Specifies whether the bed levels must be computed over all links of a cell including the closed boundaries, thin dams and dry points (= 0), or only the flow links. (= 1)"""
        return self._model.get_bool("geometry.calculatebedlevelovernonactivelinks")

    @calculateBedLevelOverNonActiveLinks.setter
    def calculateBedLevelOverNonActiveLinks(self, value: bool) -> None:
        self._model.set_bool("geometry.calculatebedlevelovernonactivelinks", value)

    @property
    def stripMesh(self) -> bool:
        """Strip unused nodes and links from the mesh after clipping."""
        return self._model.get_bool("geometry.stripmesh")

    @stripMesh.setter
    def stripMesh(self, value: bool) -> None:
        self._model.set_bool("geometry.stripmesh", value)

    @property
    def topLayMinThick(self) -> float:
        """Minimum top layer thickness, only for Z-layers."""
        return self._model.get_double("geometry.toplayminthick")

    @topLayMinThick.setter
    def topLayMinThick(self, value: float) -> None:
        self._model.set_double("geometry.toplayminthick", value)

    @property
    def helmert(self) -> bool:
        """Use Helmert."""
        return self._model.get_bool("geometry.helmert")

    @helmert.setter
    def helmert(self, value: bool) -> None:
        self._model.set_bool("geometry.helmert", value)

    @property
    def waterDepthIni1D(self) -> float:
        """Initial waterdepth in 1D."""
        return self._model.get_double("geometry.waterdepthini1d")

    @waterDepthIni1D.setter
    def waterDepthIni1D(self, value: float) -> None:
        self._model.set_double("geometry.waterdepthini1d", value)

    @property
    def zLayerAtuByBob(self) -> bool:
        """Lowest connected cells governed by bob instead of by bL L/R."""
        return self._model.get_bool("geometry.zlayeratubybob")

    @zLayerAtuByBob.setter
    def zLayerAtuByBob(self, value: bool) -> None:
        self._model.set_bool("geometry.zlayeratubybob", value)

    @property
    def shipDefFile(self) -> Path:
        """File *.shd containing ship definitions."""
        return self._model.get_path("geometry.shipdeffile")

    @shipDefFile.setter
    def shipDefFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.shipdeffile", value)

    @property
    def bedWaveLength(self) -> float:
        """Bed testcases."""
        return self._model.get_double("geometry.bedwavelength")

    @bedWaveLength.setter
    def bedWaveLength(self, value: float) -> None:
        self._model.set_double("geometry.bedwavelength", value)

    @property
    def removeSmallLinksTrsh(self) -> float:
        """Remove small links."""
        return self._model.get_double("geometry.removesmalllinkstrsh")

    @removeSmallLinksTrsh.setter
    def removeSmallLinksTrsh(self, value: float) -> None:
        self._model.set_double("geometry.removesmalllinkstrsh", value)

    @property
    def createLinks1D2D(self) -> bool:
        """Rashly create links between 1D nodes and 2D cells when initializing model."""
        return self._model.get_bool("geometry.createlinks1d2d")

    @createLinks1D2D.setter
    def createLinks1D2D(self, value: bool) -> None:
        self._model.set_bool("geometry.createlinks1d2d", value)

    @property
    def bedWaveAmplitude(self) -> float:
        """Bed testcases."""
        return self._model.get_double("geometry.bedwaveamplitude")

    @bedWaveAmplitude.setter
    def bedWaveAmplitude(self, value: float) -> None:
        self._model.set_double("geometry.bedwaveamplitude", value)

    @property
    def uniformHu(self) -> float:
        """Waterdepth in rigid-lid-like solution."""
        return self._model.get_double("geometry.uniformhu")

    @uniformHu.setter
    def uniformHu(self, value: float) -> None:
        self._model.set_double("geometry.uniformhu", value)

    @property
    def tSigma(self) -> float:
        """Sigma adaptation period for `layerType` = 4 (density controlled sigma-layers)."""
        return self._model.get_double("geometry.tsigma")

    @tSigma.setter
    def tSigma(self, value: float) -> None:
        self._model.set_double("geometry.tsigma", value)

    @property
    def dpuopt(self) -> str:
        """Bed level interpolation at velocity point in case of tile approach bed level."""
        return self._model.get_enum_name("geometry.dpuopt")

    @dpuopt.setter
    def dpuopt(self, value: str) -> None:
        self._model.set_enum_name("geometry.dpuopt", value)

    @property
    def keepZLayeringAtBed(self) -> str:
        """Z-layering at bed."""
        return self._model.get_enum_name("geometry.keepzlayeringatbed")

    @keepZLayeringAtBed.setter
    def keepZLayeringAtBed(self, value: str) -> None:
        self._model.set_enum_name("geometry.keepzlayeringatbed", value)

    @property
    def ihuzcSig(self) -> str:
        """If `keepZLayeringAtBed`>=2."""
        return self._model.get_enum_name("geometry.ihuzcsig")

    @ihuzcSig.setter
    def ihuzcSig(self, value: str) -> None:
        self._model.set_enum_name("geometry.ihuzcsig", value)

    @property
    def ihuz(self) -> int:
        """TODO"""
        return self._model.get_int("geometry.ihuz")

    @ihuz.setter
    def ihuz(self, value: int) -> None:
        self._model.set_int("geometry.ihuz", value)

    @property
    def cosphiutrsh(self) -> float:
        """1.0=no bad orthos."""
        return self._model.get_double("geometry.cosphiutrsh")

    @cosphiutrsh.setter
    def cosphiutrsh(self, value: float) -> None:
        self._model.set_double("geometry.cosphiutrsh", value)

    @property
    def cutCellList(self) -> Path:
        """File with names of cutcell polygons, e.g. cutcellpolygons.lst."""
        return self._model.get_path("geometry.cutcelllist")

    @cutCellList.setter
    def cutCellList(self, value: Path | str) -> None:
        self._model.set_path("geometry.cutcelllist", value)

    @property
    def uniformTyp1D(self) -> int:
        """Uniform type for channel profiles not specified by profloc."""
        return self._model.get_int("geometry.uniformtyp1d")

    @uniformTyp1D.setter
    def uniformTyp1D(self, value: int) -> None:
        self._model.set_int("geometry.uniformtyp1d", value)

    @property
    def _1D2DInternalLinkType(self) -> int:
        """Link treatment method for type-3 internal links."""
        return self._model.get_int("geometry.1d2dinternallinktype")

    @_1D2DInternalLinkType.setter
    def _1D2DInternalLinkType(self, value: int) -> None:
        self._model.set_int("geometry.1d2dinternallinktype", value)

    @property
    def pipeFile(self) -> Path:
        """File *.pliz containing pipe-based 'culverts'."""
        return self._model.get_path("geometry.pipefile")

    @pipeFile.setter
    def pipeFile(self, value: Path | str) -> None:
        self._model.set_path("geometry.pipefile", value)

    @property
    def groundLayerThickness(self) -> float:
        """Only in pipes: groundlayer thickness."""
        return self._model.get_double("geometry.groundlayerthickness")

    @groundLayerThickness.setter
    def groundLayerThickness(self, value: float) -> None:
        self._model.set_double("geometry.groundlayerthickness", value)

    @property
    def extrBl(self) -> bool:
        """Extrapolate bed level at boundaries according to the slope."""
        return self._model.get_bool("geometry.extrbl")

    @extrBl.setter
    def extrBl(self, value: bool) -> None:
        self._model.set_bool("geometry.extrbl", value)

    @property
    def keepZLay1BedVol(self) -> bool:
        """Correct volumes when `keepZLayeringAtBed`=1."""
        return self._model.get_bool("geometry.keepzlay1bedvol")

    @keepZLay1BedVol.setter
    def keepZLay1BedVol(self, value: bool) -> None:
        self._model.set_bool("geometry.keepzlay1bedvol", value)

    @property
    def orgFloorLevTopLayDef(self) -> bool:
        """Backward compatibility option: floorlevtoplay is dztop below specified level."""
        return self._model.get_bool("geometry.orgfloorlevtoplaydef")

    @orgFloorLevTopLayDef.setter
    def orgFloorLevTopLayDef(self, value: bool) -> None:
        self._model.set_bool("geometry.orgfloorlevtoplaydef", value)

    @property
    def bedLevMode(self) -> int:
        """Bed level mode."""
        return self._model.get_int("geometry.bedlevmode")

    @bedLevMode.setter
    def bedLevMode(self, value: int) -> None:
        self._model.set_int("geometry.bedlevmode", value)

    @property
    def circumcenter(self) -> str:
        """"""
        return self._model.get_string("geometry.circumcenter")

    @circumcenter.setter
    def circumcenter(self, value: str) -> None:
        self._model.set_string("geometry.circumcenter", value)

    @property
    def bathymetryFile(self) -> str:
        """"""
        return self._model.get_string("geometry.bathymetryfile")

    @bathymetryFile.setter
    def bathymetryFile(self, value: str) -> None:
        self._model.set_string("geometry.bathymetryfile", value)

    @property
    def bedLevelFile(self) -> str:
        """"""
        return self._model.get_string("geometry.bedlevelfile")

    @bedLevelFile.setter
    def bedLevelFile(self, value: str) -> None:
        self._model.set_string("geometry.bedlevelfile", value)

    @property
    def botLevUni(self) -> str:
        """"""
        return self._model.get_string("geometry.botlevuni")

    @botLevUni.setter
    def botLevUni(self, value: str) -> None:
        self._model.set_string("geometry.botlevuni", value)

    @property
    def botLevType(self) -> str:
        """"""
        return self._model.get_string("geometry.botlevtype")

    @botLevType.setter
    def botLevType(self, value: str) -> None:
        self._model.set_string("geometry.botlevtype", value)

    @property
    def manholeFile(self) -> str:
        """"""
        return self._model.get_string("geometry.manholefile")

    @manholeFile.setter
    def manholeFile(self, value: str) -> None:
        self._model.set_string("geometry.manholefile", value)

    @property
    def noOptimizedPolygon(self) -> str:
        """"""
        return self._model.get_string("geometry.nooptimizedpolygon")

    @noOptimizedPolygon.setter
    def noOptimizedPolygon(self, value: str) -> None:
        self._model.set_string("geometry.nooptimizedpolygon", value)

    @property
    def thinDykeFile(self) -> str:
        """"""
        return self._model.get_string("geometry.thindykefile")

    @thinDykeFile.setter
    def thinDykeFile(self, value: str) -> None:
        self._model.set_string("geometry.thindykefile", value)

    @property
    def iThinDykeScheme(self) -> str:
        """"""
        return self._model.get_string("geometry.ithindykescheme")

    @iThinDykeScheme.setter
    def iThinDykeScheme(self, value: str) -> None:
        self._model.set_string("geometry.ithindykescheme", value)

    @property
    def _1dNetworkFile(self) -> str:
        """"""
        return self._model.get_string("geometry.1dnetworkfile")

    @_1dNetworkFile.setter
    def _1dNetworkFile(self, value: str) -> None:
        self._model.set_string("geometry.1dnetworkfile", value)

    @property
    def roughnessFiles(self) -> str:
        """"""
        return self._model.get_string("geometry.roughnessfiles")

    @roughnessFiles.setter
    def roughnessFiles(self, value: str) -> None:
        self._model.set_string("geometry.roughnessfiles", value)

    @property
    def nodeFile(self) -> str:
        """"""
        return self._model.get_string("geometry.nodefile")

    @nodeFile.setter
    def nodeFile(self, value: str) -> None:
        self._model.set_string("geometry.nodefile", value)

    @property
    def Grdang(self) -> str:
        """"""
        return self._model.get_string("geometry.grdang")

    @Grdang.setter
    def Grdang(self, value: str) -> None:
        self._model.set_string("geometry.grdang", value)


class VolumeTablesSection:
    """Typed access to the [volumeTables] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def useVolumeTables(self) -> bool:
        """Use volume tables for 1D grid cells."""
        return self._model.get_bool("volumetables.usevolumetables")

    @useVolumeTables.setter
    def useVolumeTables(self, value: bool) -> None:
        self._model.set_bool("volumetables.usevolumetables", value)

    @property
    def increment(self) -> float:
        """The height increment for the volume tables."""
        return self._model.get_double("volumetables.increment")

    @increment.setter
    def increment(self, value: float) -> None:
        self._model.set_double("volumetables.increment", value)

    @property
    def useVolumeTableFile(self) -> bool:
        """Read and write the volume table from/to file."""
        return self._model.get_bool("volumetables.usevolumetablefile")

    @useVolumeTableFile.setter
    def useVolumeTableFile(self, value: bool) -> None:
        self._model.set_bool("volumetables.usevolumetablefile", value)


class NumericsSection:
    """Typed access to the [numerics] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def cflMax(self) -> float:
        """Maximum Courant nr."""
        return self._model.get_double("numerics.cflmax")

    @cflMax.setter
    def cflMax(self, value: float) -> None:
        self._model.set_double("numerics.cflmax", value)

    @property
    def advecType(self) -> int:
        """Advection type."""
        return self._model.get_int("numerics.advectype")

    @advecType.setter
    def advecType(self, value: int) -> None:
        self._model.set_int("numerics.advectype", value)

    @property
    def advecCorrection1D2D(self) -> str:
        """Advection correction of 1D2D link volume."""
        return self._model.get_enum_name("numerics.adveccorrection1d2d")

    @advecCorrection1D2D.setter
    def advecCorrection1D2D(self, value: str) -> None:
        self._model.set_enum_name("numerics.adveccorrection1d2d", value)

    @property
    def timeStepType(self) -> str:
        """Type of time stepping."""
        return self._model.get_enum_name("numerics.timesteptype")

    @timeStepType.setter
    def timeStepType(self, value: str) -> None:
        self._model.set_enum_name("numerics.timesteptype", value)

    @property
    def maxNonLinearIterations(self) -> int:
        """Maximal iterations in non-linear iteration loop before a time step reduction is applied."""
        return self._model.get_int("numerics.maxnonlineariterations")

    @maxNonLinearIterations.setter
    def maxNonLinearIterations(self, value: int) -> None:
        self._model.set_int("numerics.maxnonlineariterations", value)

    @property
    def setHorizontalBobsFor1D2D(self) -> bool:
        """Bobs are set to 2D bedlevel, to prevent incorrect storage in sewer system."""
        return self._model.get_bool("numerics.sethorizontalbobsfor1d2d")

    @setHorizontalBobsFor1D2D.setter
    def setHorizontalBobsFor1D2D(self, value: bool) -> None:
        self._model.set_bool("numerics.sethorizontalbobsfor1d2d", value)

    @property
    def limTypHu(self) -> str:
        """Limiter type for waterdepth in continuity eq."""
        return self._model.get_enum_name("numerics.limtyphu")

    @limTypHu.setter
    def limTypHu(self, value: str) -> None:
        self._model.set_enum_name("numerics.limtyphu", value)

    @property
    def limTypMom(self) -> str:
        """Limiter type for cell center advection velocity."""
        return self._model.get_enum_name("numerics.limtypmom")

    @limTypMom.setter
    def limTypMom(self, value: str) -> None:
        self._model.set_enum_name("numerics.limtypmom", value)

    @property
    def limTypSa(self) -> str:
        """Limiter type for salinity transport."""
        return self._model.get_enum_name("numerics.limtypsa")

    @limTypSa.setter
    def limTypSa(self, value: str) -> None:
        self._model.set_enum_name("numerics.limtypsa", value)

    @property
    def pure1D(self) -> str:
        """Purely 1D advection."""
        return self._model.get_enum_name("numerics.pure1d")

    @pure1D.setter
    def pure1D(self, value: str) -> None:
        self._model.set_enum_name("numerics.pure1d", value)

    @property
    def junction1D(self) -> str:
        """Advection at 1D junctions."""
        return self._model.get_enum_name("numerics.junction1d")

    @junction1D.setter
    def junction1D(self, value: str) -> None:
        self._model.set_enum_name("numerics.junction1d", value)

    @property
    def icgSolver(self) -> str:
        """Solver type."""
        return self._model.get_enum_name("numerics.icgsolver")

    @icgSolver.setter
    def icgSolver(self, value: str) -> None:
        self._model.set_enum_name("numerics.icgsolver", value)

    @property
    def logSolverConvergence(self) -> bool:
        """Print time step, number of solver iterations and solver residual to diagnostic output."""
        return self._model.get_bool("numerics.logsolverconvergence")

    @logSolverConvergence.setter
    def logSolverConvergence(self, value: bool) -> None:
        self._model.set_bool("numerics.logsolverconvergence", value)

    @property
    def maxDegree(self) -> int:
        """Maximum degree in Gauss elimination."""
        return self._model.get_int("numerics.maxdegree")

    @maxDegree.setter
    def maxDegree(self, value: int) -> None:
        self._model.set_int("numerics.maxdegree", value)

    @property
    def fixedWeirScheme(self) -> str:
        """Fixed weir scheme."""
        return self._model.get_enum_name("numerics.fixedweirscheme")

    @fixedWeirScheme.setter
    def fixedWeirScheme(self, value: str) -> None:
        self._model.set_enum_name("numerics.fixedweirscheme", value)

    @property
    def fixedWeirContraction(self) -> float:
        """flow width = flow width*`fixedWeirContraction`."""
        return self._model.get_double("numerics.fixedweircontraction")

    @fixedWeirContraction.setter
    def fixedWeirContraction(self, value: float) -> None:
        self._model.set_double("numerics.fixedweircontraction", value)

    @property
    def fixedWeirTopWidth(self) -> float:
        """Uniform width of the groyne part of fixed weirs."""
        return self._model.get_double("numerics.fixedweirtopwidth")

    @fixedWeirTopWidth.setter
    def fixedWeirTopWidth(self, value: float) -> None:
        self._model.set_double("numerics.fixedweirtopwidth", value)

    @property
    def fixedWeirTalud(self) -> float:
        """Uniform talud slope of fixed weirs."""
        return self._model.get_double("numerics.fixedweirtalud")

    @fixedWeirTalud.setter
    def fixedWeirTalud(self, value: float) -> None:
        self._model.set_double("numerics.fixedweirtalud", value)

    @property
    def fixedWeirTopFrictCoef(self) -> float:
        """Uniform friction coefficient of the groyne part of fixed weirs."""
        return self._model.get_double("numerics.fixedweirtopfrictcoef")

    @fixedWeirTopFrictCoef.setter
    def fixedWeirTopFrictCoef(self, value: float) -> None:
        self._model.set_double("numerics.fixedweirtopfrictcoef", value)

    @property
    def fixedWeirRelaxationCoef(self) -> float:
        """Fixed weir relaxation coefficient for computation of energy loss."""
        return self._model.get_double("numerics.fixedweirrelaxationcoef")

    @fixedWeirRelaxationCoef.setter
    def fixedWeirRelaxationCoef(self, value: float) -> None:
        self._model.set_double("numerics.fixedweirrelaxationcoef", value)

    @property
    def fixedWeirScheme1D2D(self) -> str:
        """Fixed weir scheme for 1D2D links."""
        return self._model.get_enum_name("numerics.fixedweirscheme1d2d")

    @fixedWeirScheme1D2D.setter
    def fixedWeirScheme1D2D(self, value: str) -> None:
        self._model.set_enum_name("numerics.fixedweirscheme1d2d", value)

    @property
    def fixedWeir1D2D_dx(self) -> float:
        """Extra delta x for lateral 1D2D fixed weirs."""
        return self._model.get_double("numerics.fixedweir1d2d_dx")

    @fixedWeir1D2D_dx.setter
    def fixedWeir1D2D_dx(self, value: float) -> None:
        self._model.set_double("numerics.fixedweir1d2d_dx", value)

    @property
    def izBndPos(self) -> str:
        """Position of z boundary."""
        return self._model.get_enum_name("numerics.izbndpos")

    @izBndPos.setter
    def izBndPos(self, value: str) -> None:
        self._model.set_enum_name("numerics.izbndpos", value)

    @property
    def tlfSmo(self) -> float:
        """Fourier smoothing time on water level boundaries."""
        return self._model.get_double("numerics.tlfsmo")

    @tlfSmo.setter
    def tlfSmo(self, value: float) -> None:
        self._model.set_double("numerics.tlfsmo", value)

    @property
    def slopeDrop2D(self) -> float:
        """Droplosses are applied if local bottom slope > `slopeDrop2D`, <=0 = no droplosses."""
        return self._model.get_double("numerics.slopedrop2d")

    @slopeDrop2D.setter
    def slopeDrop2D(self, value: float) -> None:
        self._model.set_double("numerics.slopedrop2d", value)

    @property
    def drop1D(self) -> bool:
        """Limit the downstream water level in the momentum equation to the downstream invert level, BOBdown (ζ ∗ down = max (BOBdown, ζdown))."""
        return self._model.get_bool("numerics.drop1d")

    @drop1D.setter
    def drop1D(self, value: bool) -> None:
        self._model.set_bool("numerics.drop1d", value)

    @property
    def chkAdvd(self) -> float:
        """Check advection terms if depth < `chkAdvd`."""
        return self._model.get_double("numerics.chkadvd")

    @chkAdvd.setter
    def chkAdvd(self, value: float) -> None:
        self._model.set_double("numerics.chkadvd", value)

    @property
    def teta0(self) -> float:
        """Theta (implicitness) of time integration."""
        return self._model.get_double("numerics.teta0")

    @teta0.setter
    def teta0(self, value: float) -> None:
        self._model.set_double("numerics.teta0", value)

    @property
    def qhRelax(self) -> float:
        """Relaxation on Q-h open boundaries."""
        return self._model.get_double("numerics.qhrelax")

    @qhRelax.setter
    def qhRelax(self, value: float) -> None:
        self._model.set_double("numerics.qhrelax", value)

    @property
    def filter(self) -> str:
        """"""
        return self._model.get_string("numerics.filter")

    @filter.setter
    def filter(self, value: str) -> None:
        self._model.set_string("numerics.filter", value)

    @property
    def cstBnd(self) -> bool:
        """Delft3D-FLOW type velocity treatment near boundaries for small coastal models or not."""
        return self._model.get_bool("numerics.cstbnd")

    @cstBnd.setter
    def cstBnd(self, value: bool) -> None:
        self._model.set_bool("numerics.cstbnd", value)

    @property
    def maxItVerticalForesterSal(self) -> int:
        """Forester iterations for salinity (0: no vertical filter for salinity, > 0: max nr of iterations)."""
        return self._model.get_int("numerics.maxitverticalforestersal")

    @maxItVerticalForesterSal.setter
    def maxItVerticalForesterSal(self, value: int) -> None:
        self._model.set_int("numerics.maxitverticalforestersal", value)

    @property
    def maxItVerticalForester(self) -> int:
        """Forester iterations for all constituents (0: no vertical filter, > 0: max nr of iterations)."""
        return self._model.get_int("numerics.maxitverticalforester")

    @maxItVerticalForester.setter
    def maxItVerticalForester(self, value: int) -> None:
        self._model.set_int("numerics.maxitverticalforester", value)

    @property
    def maxItVerticalForesterTem(self) -> int:
        """Forester iterations for temperature (0: no vertical filter for temperature, > 0: max nr of iterations)."""
        return self._model.get_int("numerics.maxitverticalforestertem")

    @maxItVerticalForesterTem.setter
    def maxItVerticalForesterTem(self, value: int) -> None:
        self._model.set_int("numerics.maxitverticalforestertem", value)

    @property
    def transportAutoTimeStepDiff(self) -> str:
        """Auto timestepdiff in transport."""
        return self._model.get_enum_name("numerics.transportautotimestepdiff")

    @transportAutoTimeStepDiff.setter
    def transportAutoTimeStepDiff(self, value: str) -> None:
        self._model.set_enum_name("numerics.transportautotimestepdiff", value)

    @property
    def implicitDiffusion2D(self) -> bool:
        """Implicit diffusion 2D."""
        return self._model.get_bool("numerics.implicitdiffusion2d")

    @implicitDiffusion2D.setter
    def implicitDiffusion2D(self, value: bool) -> None:
        self._model.set_bool("numerics.implicitdiffusion2d", value)

    @property
    def turbulenceModel(self) -> str:
        """Turbulence model."""
        return self._model.get_enum_name("numerics.turbulencemodel")

    @turbulenceModel.setter
    def turbulenceModel(self, value: str) -> None:
        self._model.set_enum_name("numerics.turbulencemodel", value)

    @property
    def c1e(self) -> float:
        """c1 coefficient in turbulence model."""
        return self._model.get_double("numerics.c1e")

    @c1e.setter
    def c1e(self, value: float) -> None:
        self._model.set_double("numerics.c1e", value)

    @property
    def c3eStable(self) -> float:
        """c3e coefficient (for stable stratification) in k-eps turbulence model."""
        return self._model.get_double("numerics.c3estable")

    @c3eStable.setter
    def c3eStable(self, value: float) -> None:
        self._model.set_double("numerics.c3estable", value)

    @property
    def c3eUnstable(self) -> float:
        """c3e coefficient (for unstable stratification) in k-eps turbulence model."""
        return self._model.get_double("numerics.c3eunstable")

    @c3eUnstable.setter
    def c3eUnstable(self, value: float) -> None:
        self._model.set_double("numerics.c3eunstable", value)

    @property
    def antiCreep(self) -> bool:
        """Include anti-creep to suppress artificial vertical diffusion."""
        return self._model.get_bool("numerics.anticreep")

    @antiCreep.setter
    def antiCreep(self, value: bool) -> None:
        self._model.set_bool("numerics.anticreep", value)

    @property
    def barocPOnBnd(self) -> bool:
        """Include baroclinic pressure on open boundaries."""
        return self._model.get_bool("numerics.barocponbnd")

    @barocPOnBnd.setter
    def barocPOnBnd(self, value: bool) -> None:
        self._model.set_bool("numerics.barocponbnd", value)

    @property
    def maxItPresDens(self) -> int:
        """Max number of iterations in pressure-density coupling, only if `thermobaricity`=true."""
        return self._model.get_int("numerics.maxitpresdens")

    @maxItPresDens.setter
    def maxItPresDens(self, value: int) -> None:
        self._model.set_int("numerics.maxitpresdens", value)

    @property
    def diagnosticTransport(self) -> bool:
        """No update of transport quantities, also known as diagnostic transport."""
        return self._model.get_bool("numerics.diagnostictransport")

    @diagnosticTransport.setter
    def diagnosticTransport(self, value: bool) -> None:
        self._model.set_bool("numerics.diagnostictransport", value)

    @property
    def maxWaterLevelDiff(self) -> float:
        """Upper bound on water level changes, (<= 0: no bounds). Run will abort when violated."""
        return self._model.get_double("numerics.maxwaterleveldiff")

    @maxWaterLevelDiff.setter
    def maxWaterLevelDiff(self, value: float) -> None:
        self._model.set_double("numerics.maxwaterleveldiff", value)

    @property
    def maxVelocityDiff(self) -> float:
        """Upper bound on velocity changes, (<= 0: no bounds). Run will abort when violated."""
        return self._model.get_double("numerics.maxvelocitydiff")

    @maxVelocityDiff.setter
    def maxVelocityDiff(self, value: float) -> None:
        self._model.set_double("numerics.maxvelocitydiff", value)

    @property
    def maxVelocity(self) -> float:
        """Upper bound on velocity (<= 0: no bounds). Run will abort when violated."""
        return self._model.get_double("numerics.maxvelocity")

    @maxVelocity.setter
    def maxVelocity(self, value: float) -> None:
        self._model.set_double("numerics.maxvelocity", value)

    @property
    def waterLevelWarn(self) -> float:
        """Warning level on water level (<= 0: no check)."""
        return self._model.get_double("numerics.waterlevelwarn")

    @waterLevelWarn.setter
    def waterLevelWarn(self, value: float) -> None:
        self._model.set_double("numerics.waterlevelwarn", value)

    @property
    def velocityWarn(self) -> float:
        """Warning level on velocity (<= 0: no check)."""
        return self._model.get_double("numerics.velocitywarn")

    @velocityWarn.setter
    def velocityWarn(self, value: float) -> None:
        self._model.set_double("numerics.velocitywarn", value)

    @property
    def velMagnWarn(self) -> float:
        """Warning level on velocity magnitude (<= 0: no check)."""
        return self._model.get_double("numerics.velmagnwarn")

    @velMagnWarn.setter
    def velMagnWarn(self, value: float) -> None:
        self._model.set_double("numerics.velmagnwarn", value)

    @property
    def minTimeStepBreak(self) -> float:
        """Smallest allowed timestep, checked on a sliding average of several timesteps. Run will abort when violated."""
        return self._model.get_double("numerics.mintimestepbreak")

    @minTimeStepBreak.setter
    def minTimeStepBreak(self, value: float) -> None:
        self._model.set_double("numerics.mintimestepbreak", value)

    @property
    def epshu(self) -> float:
        """Threshold water depth for wetting and drying."""
        return self._model.get_double("numerics.epshu")

    @epshu.setter
    def epshu(self, value: float) -> None:
        self._model.set_double("numerics.epshu", value)

    @property
    def epsMaxLev(self) -> float:
        """Stop criterion for non linear iteration."""
        return self._model.get_double("numerics.epsmaxlev")

    @epsMaxLev.setter
    def epsMaxLev(self, value: float) -> None:
        self._model.set_double("numerics.epsmaxlev", value)

    @property
    def epsMaxLevM(self) -> float:
        """Stop criterion for Nested Newton loop in non-linear iteration."""
        return self._model.get_double("numerics.epsmaxlevm")

    @epsMaxLevM.setter
    def epsMaxLevM(self, value: float) -> None:
        self._model.set_double("numerics.epsmaxlevm", value)

    @property
    def flowSolver(self) -> str:
        """Flow solver."""
        return self._model.get_enum_name("numerics.flowsolver")

    @flowSolver.setter
    def flowSolver(self, value: str) -> None:
        self._model.set_enum_name("numerics.flowsolver", value)

    @property
    def lateral_fixedweir_umin(self) -> float:
        """Minimal velocity threshold for weir losses in iterative lateral 1D2D weir coupling."""
        return self._model.get_double("numerics.lateral_fixedweir_umin")

    @lateral_fixedweir_umin.setter
    def lateral_fixedweir_umin(self, value: float) -> None:
        self._model.set_double("numerics.lateral_fixedweir_umin", value)

    @property
    def jasfer3D(self) -> bool:
        """Corrections for spherical coordinates."""
        return self._model.get_bool("numerics.jasfer3d")

    @jasfer3D.setter
    def jasfer3D(self, value: bool) -> None:
        self._model.set_bool("numerics.jasfer3d", value)

    @property
    def cfFacVer(self) -> float:
        """Factor for including (1-CFL) in HO term vertical."""
        return self._model.get_double("numerics.cffacver")

    @cfFacVer.setter
    def cfFacVer(self, value: float) -> None:
        self._model.set_double("numerics.cffacver", value)

    @property
    def eddyViscosityBedFacmax(self) -> float:
        """Limit eddy viscosity at bed by factor of first layer above."""
        return self._model.get_double("numerics.eddyviscositybedfacmax")

    @eddyViscosityBedFacmax.setter
    def eddyViscosityBedFacmax(self, value: float) -> None:
        self._model.set_double("numerics.eddyviscositybedfacmax", value)

    @property
    def lateral_fixedweir_umin_method(self) -> int:
        """Method for minimal velocity threshold for weir losses in iterative lateral 1D2D weir coupling."""
        return self._model.get_int("numerics.lateral_fixedweir_umin_method")

    @lateral_fixedweir_umin_method.setter
    def lateral_fixedweir_umin_method(self, value: int) -> None:
        self._model.set_int("numerics.lateral_fixedweir_umin_method", value)

    @property
    def lateral_fixedweir_minimal_1d2d_embankment(self) -> float:
        """Minimal crest height of 1D2D SOBEK-DFM embankments."""
        return self._model.get_double("numerics.lateral_fixedweir_minimal_1d2d_embankment")

    @lateral_fixedweir_minimal_1d2d_embankment.setter
    def lateral_fixedweir_minimal_1d2d_embankment(self, value: float) -> None:
        self._model.set_double("numerics.lateral_fixedweir_minimal_1d2d_embankment", value)

    @property
    def testFixedWeirs(self) -> str:
        """Test for fixed weir algorithms."""
        return self._model.get_enum_name("numerics.testfixedweirs")

    @testFixedWeirs.setter
    def testFixedWeirs(self, value: str) -> None:
        self._model.set_enum_name("numerics.testfixedweirs", value)

    @property
    def jposhchk(self) -> str:
        """Check for positive waterdepth."""
        return self._model.get_enum_name("numerics.jposhchk")

    @jposhchk.setter
    def jposhchk(self, value: str) -> None:
        self._model.set_enum_name("numerics.jposhchk", value)

    @property
    def cfConHorMom(self) -> float:
        """Constant for including (1-CFL) in HO term horizontal momentum."""
        return self._model.get_double("numerics.cfconhormom")

    @cfConHorMom.setter
    def cfConHorMom(self, value: float) -> None:
        self._model.set_double("numerics.cfconhormom", value)

    @property
    def cfFacHorMom(self) -> float:
        """Factor for including (1-CFL) in HO term horizontal momentum."""
        return self._model.get_double("numerics.cffachormom")

    @cfFacHorMom.setter
    def cfFacHorMom(self, value: float) -> None:
        self._model.set_double("numerics.cffachormom", value)

    @property
    def trsh_u1lb(self) -> float:
        """2D bedfriction in 3D below this threshold."""
        return self._model.get_double("numerics.trsh_u1lb")

    @trsh_u1lb.setter
    def trsh_u1lb(self, value: float) -> None:
        self._model.set_double("numerics.trsh_u1lb", value)

    @property
    def jaupwindsrc(self) -> str:
        """Upwind advection discretization at sources/sinks."""
        return self._model.get_enum_name("numerics.jaupwindsrc")

    @jaupwindsrc.setter
    def jaupwindsrc(self, value: str) -> None:
        self._model.set_enum_name("numerics.jaupwindsrc", value)

    @property
    def corioAdamsBashfordFac(self) -> float:
        """Only when `newCorio`=1, Adams-Bashford factor in Coriolis term."""
        return self._model.get_double("numerics.corioadamsbashfordfac")

    @corioAdamsBashfordFac.setter
    def corioAdamsBashfordFac(self, value: float) -> None:
        self._model.set_double("numerics.corioadamsbashfordfac", value)

    @property
    def corioConstant(self) -> str:
        """Coriolis constant."""
        return self._model.get_enum_name("numerics.corioconstant")

    @corioConstant.setter
    def corioConstant(self, value: str) -> None:
        self._model.set_enum_name("numerics.corioconstant", value)

    @property
    def drop3D(self) -> float:
        """Drop losses in 3D are applied if z upwind is below bob + 2/3 hu*`drop3D`."""
        return self._model.get_double("numerics.drop3d")

    @drop3D.setter
    def drop3D(self, value: float) -> None:
        self._model.set_double("numerics.drop3d", value)

    @property
    def zLayerCenterBedVel(self) -> bool:
        """Reconstruction of center velocity at half closed bed cells."""
        return self._model.get_bool("numerics.zlayercenterbedvel")

    @zLayerCenterBedVel.setter
    def zLayerCenterBedVel(self, value: bool) -> None:
        self._model.set_bool("numerics.zlayercenterbedvel", value)

    @property
    def horAdvTypZLayer(self) -> str:
        """Horizontal advection treatment of z-layers for dambreaks."""
        return self._model.get_enum_name("numerics.horadvtypzlayer")

    @horAdvTypZLayer.setter
    def horAdvTypZLayer(self, value: str) -> None:
        self._model.set_enum_name("numerics.horadvtypzlayer", value)

    @property
    def iCoriolisType(self) -> str:
        """Coriolis type."""
        return self._model.get_enum_name("numerics.icoriolistype")

    @iCoriolisType.setter
    def iCoriolisType(self, value: str) -> None:
        self._model.set_enum_name("numerics.icoriolistype", value)

    @property
    def zwsbTol(self) -> float:
        """Tolerance for zws(kb-1) at bed."""
        return self._model.get_double("numerics.zwsbtol")

    @zwsbTol.setter
    def zwsbTol(self, value: float) -> None:
        self._model.set_double("numerics.zwsbtol", value)

    @property
    def cfExpHu(self) -> float:
        """Exponent for including (1-CFL) in sethu."""
        return self._model.get_double("numerics.cfexphu")

    @cfExpHu.setter
    def cfExpHu(self, value: float) -> None:
        self._model.set_double("numerics.cfexphu", value)

    @property
    def jbasqbnddownwindhs(self) -> str:
        """Water depth scheme at discharge boundaries."""
        return self._model.get_enum_name("numerics.jbasqbnddownwindhs")

    @jbasqbnddownwindhs.setter
    def jbasqbnddownwindhs(self, value: str) -> None:
        self._model.set_enum_name("numerics.jbasqbnddownwindhs", value)

    @property
    def filterOrder(self) -> str:
        """First-order or second order filter to suppress checkerboarding."""
        return self._model.get_enum_name("numerics.filterorder")

    @filterOrder.setter
    def filterOrder(self, value: str) -> None:
        self._model.set_enum_name("numerics.filterorder", value)

    @property
    def keepSTBndOnOutflow(self) -> str:
        """Keep salinity and temperature signals on boundary cells at outflow."""
        return self._model.get_enum_name("numerics.keepstbndonoutflow")

    @keepSTBndOnOutflow.setter
    def keepSTBndOnOutflow(self, value: str) -> None:
        self._model.set_enum_name("numerics.keepstbndonoutflow", value)

    @property
    def keepZLayeringAtBed(self) -> str:
        """Z-layering at bed."""
        return self._model.get_enum_name("numerics.keepzlayeringatbed")

    @keepZLayeringAtBed.setter
    def keepZLayeringAtBed(self, value: str) -> None:
        self._model.set_enum_name("numerics.keepzlayeringatbed", value)

    @property
    def logProfAtUBndIn(self) -> str:
        """ubnds inflow."""
        return self._model.get_enum_name("numerics.logprofatubndin")

    @logProfAtUBndIn.setter
    def logProfAtUBndIn(self, value: str) -> None:
        self._model.set_enum_name("numerics.logprofatubndin", value)

    @property
    def logProfKepsBndIn(self) -> str:
        """3D profile at open boundaries."""
        return self._model.get_enum_name("numerics.logprofkepsbndin")

    @logProfKepsBndIn.setter
    def logProfKepsBndIn(self, value: str) -> None:
        self._model.set_enum_name("numerics.logprofkepsbndin", value)

    @property
    def epshstem(self) -> float:
        """Only compute heat flux + evaporation if depth > `epshstem`."""
        return self._model.get_double("numerics.epshstem")

    @epshstem.setter
    def epshstem(self, value: float) -> None:
        self._model.set_double("numerics.epshstem", value)

    @property
    def diffusionOnBnd(self) -> bool:
        """Horizontal diffusion on open boundaries."""
        return self._model.get_bool("numerics.diffusiononbnd")

    @diffusionOnBnd.setter
    def diffusionOnBnd(self, value: bool) -> None:
        self._model.set_bool("numerics.diffusiononbnd", value)

    @property
    def newCorio(self) -> bool:
        """New standard way of Coriolis term calculation."""
        return self._model.get_bool("numerics.newcorio")

    @newCorio.setter
    def newCorio(self, value: bool) -> None:
        self._model.set_bool("numerics.newcorio", value)

    @property
    def barrierAdvection(self) -> str:
        """Advection modelling at barriers."""
        return self._model.get_enum_name("numerics.barrieradvection")

    @barrierAdvection.setter
    def barrierAdvection(self, value: str) -> None:
        self._model.set_enum_name("numerics.barrieradvection", value)

    @property
    def rhoInterfaces(self) -> str:
        """Estimate rho at 3D layer interfaces for baroclinic pressure gradient method."""
        return self._model.get_enum_name("numerics.rhointerfaces")

    @rhoInterfaces.setter
    def rhoInterfaces(self, value: str) -> None:
        self._model.set_enum_name("numerics.rhointerfaces", value)

    @property
    def chkdifd(self) -> float:
        """Check diffusion terms if depth < `chkdifd`, only if `transportAutoTimeStepDiff`=1."""
        return self._model.get_double("numerics.chkdifd")

    @chkdifd.setter
    def chkdifd(self, value: float) -> None:
        self._model.set_double("numerics.chkdifd", value)

    @property
    def fixedWeirFrictScheme(self) -> str:
        """Fixed weir friction scheme."""
        return self._model.get_enum_name("numerics.fixedweirfrictscheme")

    @fixedWeirFrictScheme.setter
    def fixedWeirFrictScheme(self, value: str) -> None:
        self._model.set_enum_name("numerics.fixedweirfrictscheme", value)

    @property
    def testDryingFlooding(self) -> str:
        """Drying flooding algorithm."""
        return self._model.get_enum_name("numerics.testdryingflooding")

    @testDryingFlooding.setter
    def testDryingFlooding(self, value: str) -> None:
        self._model.set_enum_name("numerics.testdryingflooding", value)

    @property
    def turbulenceAdvection(self) -> str:
        """Turbulence advection."""
        return self._model.get_enum_name("numerics.turbulenceadvection")

    @turbulenceAdvection.setter
    def turbulenceAdvection(self, value: str) -> None:
        self._model.set_enum_name("numerics.turbulenceadvection", value)

    @property
    def horizontalMomentumfilter(self) -> bool:
        """Filter for reduction of checkerboarding."""
        return self._model.get_bool("numerics.horizontalmomentumfilter")

    @horizontalMomentumfilter.setter
    def horizontalMomentumfilter(self, value: bool) -> None:
        self._model.set_bool("numerics.horizontalmomentumfilter", value)

    @property
    def checkerboardMonitor(self) -> bool:
        """Flag for checkerboarding output on history file (only for sigma layers yet)."""
        return self._model.get_bool("numerics.checkerboardmonitor")

    @checkerboardMonitor.setter
    def checkerboardMonitor(self, value: bool) -> None:
        self._model.set_bool("numerics.checkerboardmonitor", value)

    @property
    def tSpinUpTurbLogProf(self) -> float:
        """Spin up time when starting with a parabolic viscosity profile in whole model domain."""
        return self._model.get_double("numerics.tspinupturblogprof")

    @tSpinUpTurbLogProf.setter
    def tSpinUpTurbLogProf(self, value: float) -> None:
        self._model.set_double("numerics.tspinupturblogprof", value)

    @property
    def vertAdvTypMom(self) -> str:
        """Vertical advection type in momentum equation."""
        return self._model.get_enum_name("numerics.vertadvtypmom")

    @vertAdvTypMom.setter
    def vertAdvTypMom(self, value: str) -> None:
        self._model.set_enum_name("numerics.vertadvtypmom", value)

    @property
    def verticalAdvectionType(self) -> str:
        """Vertical advection type for salinity. Note that `verticalAdvectionType`=`centralImplicit` leads to less numerical dissipation than `verticalAdvectionType`=`higherOrderUpwindExplicit`."""
        return self._model.get_enum_name("numerics.verticaladvectiontype")

    @verticalAdvectionType.setter
    def verticalAdvectionType(self, value: str) -> None:
        self._model.set_enum_name("numerics.verticaladvectiontype", value)

    @property
    def vertAdvTypSal(self) -> str:
        """Vertical advection type for salinity. Note that `vertAdvTypSal`=4 leads to less numerical dissipation than `vertAdvTypSal`=6."""
        return self._model.get_enum_name("numerics.vertadvtypsal")

    @vertAdvTypSal.setter
    def vertAdvTypSal(self, value: str) -> None:
        self._model.set_enum_name("numerics.vertadvtypsal", value)

    @property
    def vertAdvTypTem(self) -> str:
        """Vertical advection type for temperature. Note that `vertAdvTypTem`=4 leads to less numerical dissipation than `vertAdvTypTem`=6."""
        return self._model.get_enum_name("numerics.vertadvtyptem")

    @vertAdvTypTem.setter
    def vertAdvTypTem(self, value: str) -> None:
        self._model.set_enum_name("numerics.vertadvtyptem", value)

    @property
    def zeroZBndInflowAdvection(self) -> str:
        """Switch for advection at open boundary."""
        return self._model.get_enum_name("numerics.zerozbndinflowadvection")

    @zeroZBndInflowAdvection.setter
    def zeroZBndInflowAdvection(self, value: str) -> None:
        self._model.set_enum_name("numerics.zerozbndinflowadvection", value)

    @property
    def turbulenceTimeIntegrationFactor(self) -> float:
        """0.0=Tur0 from links, 1.0=Tur0 maximal mix of values from links with nodes"""
        return self._model.get_double("numerics.turbulencetimeintegrationfactor")

    @turbulenceTimeIntegrationFactor.setter
    def turbulenceTimeIntegrationFactor(self, value: float) -> None:
        self._model.set_double("numerics.turbulencetimeintegrationfactor", value)

    @property
    def turbulenceTimeIntegrationMethod(self) -> str:
        """Where to apply `turbulenceTimeIntegrationFactor`"""
        return self._model.get_enum_name("numerics.turbulencetimeintegrationmethod")

    @turbulenceTimeIntegrationMethod.setter
    def turbulenceTimeIntegrationMethod(self, value: str) -> None:
        self._model.set_enum_name("numerics.turbulencetimeintegrationmethod", value)

    @property
    def locSaltMin(self) -> float:
        """Minimum salinity for case of lock exchange."""
        return self._model.get_double("numerics.locsaltmin")

    @locSaltMin.setter
    def locSaltMin(self, value: float) -> None:
        self._model.set_double("numerics.locsaltmin", value)

    @property
    def locSaltMax(self) -> float:
        """Maximum salinity for case of lock exchange."""
        return self._model.get_double("numerics.locsaltmax")

    @locSaltMax.setter
    def locSaltMax(self, value: float) -> None:
        self._model.set_double("numerics.locsaltmax", value)

    @property
    def locSaltLev(self) -> float:
        """Salinity level for case of lock exchange."""
        return self._model.get_double("numerics.locsaltlev")

    @locSaltLev.setter
    def locSaltLev(self, value: float) -> None:
        self._model.set_double("numerics.locsaltlev", value)

    @property
    def linContin(self) -> bool:
        """Linear continuity."""
        return self._model.get_bool("numerics.lincontin")

    @linContin.setter
    def linContin(self, value: bool) -> None:
        self._model.set_bool("numerics.lincontin", value)

    @property
    def cfExpHorMom(self) -> float:
        """Exponent for including (1-CFL) in HO term horizontal momentum."""
        return self._model.get_double("numerics.cfexphormom")

    @cfExpHorMom.setter
    def cfExpHorMom(self, value: float) -> None:
        self._model.set_double("numerics.cfexphormom", value)

    @property
    def coriohhtrsh(self) -> float:
        """Only when `newCorio`=1, 0.0=no safety in hu/hus weightings."""
        return self._model.get_double("numerics.coriohhtrsh")

    @coriohhtrsh.setter
    def coriohhtrsh(self, value: float) -> None:
        self._model.set_double("numerics.coriohhtrsh", value)

    @property
    def limTypW(self) -> str:
        """Limiter type for wave action transport."""
        return self._model.get_enum_name("numerics.limtypw")

    @limTypW.setter
    def limTypW(self, value: str) -> None:
        self._model.set_enum_name("numerics.limtypw", value)

    @property
    def huWeirRegular(self) -> float:
        """For Villemonte and Tabellenboek, regular hu below `huWeirRegular`."""
        return self._model.get_double("numerics.huweirregular")

    @huWeirRegular.setter
    def huWeirRegular(self, value: float) -> None:
        self._model.set_double("numerics.huweirregular", value)

    @property
    def structureLayersActive(self) -> str:
        """For Villemonte and Tabellenboek, regular hu below `huWeirRegular`."""
        return self._model.get_enum_name("numerics.structurelayersactive")

    @structureLayersActive.setter
    def structureLayersActive(self, value: str) -> None:
        self._model.set_enum_name("numerics.structurelayersactive", value)

    @property
    def baOrgFracMin(self) -> float:
        """Cell area = max(orgcellarea*`baOrgFracMin`, cut cell area)."""
        return self._model.get_double("numerics.baorgfracmin")

    @baOrgFracMin.setter
    def baOrgFracMin(self, value: float) -> None:
        self._model.set_double("numerics.baorgfracmin", value)

    @property
    def subsuplupdates1(self) -> bool:
        """Update water levels (s1) due to subsidence/uplift."""
        return self._model.get_bool("numerics.subsuplupdates1")

    @subsuplupdates1.setter
    def subsuplupdates1(self, value: bool) -> None:
        self._model.set_bool("numerics.subsuplupdates1", value)

    @property
    def linkDriedMx(self) -> int:
        """Maximum number of Au growth steps after having dried."""
        return self._model.get_int("numerics.linkdriedmx")

    @linkDriedMx.setter
    def linkDriedMx(self, value: int) -> None:
        self._model.set_int("numerics.linkdriedmx", value)

    @property
    def lateral_fixedweir_relax(self) -> float:
        """Relaxation factor for iterative lateral 1D2D weir coupling algorithm."""
        return self._model.get_double("numerics.lateral_fixedweir_relax")

    @lateral_fixedweir_relax.setter
    def lateral_fixedweir_relax(self, value: float) -> None:
        self._model.set_double("numerics.lateral_fixedweir_relax", value)

    @property
    def numlimdt_baorg(self) -> float:
        """If previous numlimdt > `numlimdt_baorg` keep original cell area ba in cut cell."""
        return self._model.get_double("numerics.numlimdt_baorg")

    @numlimdt_baorg.setter
    def numlimdt_baorg(self, value: float) -> None:
        self._model.set_double("numerics.numlimdt_baorg", value)

    @property
    def cfFacHu(self) -> float:
        """Factor for including (1-CFL) in sethu."""
        return self._model.get_double("numerics.cffachu")

    @cfFacHu.setter
    def cfFacHu(self, value: float) -> None:
        self._model.set_double("numerics.cffachu", value)

    @property
    def vertAdvTypMom3OnBnd(self) -> str:
        """Vertical advection type u1 bnd UpwimpL."""
        return self._model.get_enum_name("numerics.vertadvtypmom3onbnd")

    @vertAdvTypMom3OnBnd.setter
    def vertAdvTypMom3OnBnd(self, value: str) -> None:
        self._model.set_enum_name("numerics.vertadvtypmom3onbnd", value)

    @property
    def noDerivedTypes(self) -> str:
        """Use derived types."""
        return self._model.get_enum_name("numerics.noderivedtypes")

    @noDerivedTypes.setter
    def noDerivedTypes(self, value: str) -> None:
        self._model.set_enum_name("numerics.noderivedtypes", value)

    @property
    def jarhoxu(self) -> str:
        """Include density gradient in advection term."""
        return self._model.get_enum_name("numerics.jarhoxu")

    @jarhoxu.setter
    def jarhoxu(self, value: str) -> None:
        self._model.set_enum_name("numerics.jarhoxu", value)

    @property
    def ilutype(self) -> str:
        """TODO."""
        return self._model.get_string("numerics.ilutype")

    @ilutype.setter
    def ilutype(self, value: str) -> None:
        self._model.set_string("numerics.ilutype", value)

    @property
    def nlevel(self) -> str:
        """TODO."""
        return self._model.get_string("numerics.nlevel")

    @nlevel.setter
    def nlevel(self, value: str) -> None:
        self._model.set_string("numerics.nlevel", value)

    @property
    def dtol(self) -> str:
        """TODO."""
        return self._model.get_string("numerics.dtol")

    @dtol.setter
    def dtol(self, value: str) -> None:
        self._model.set_string("numerics.dtol", value)

    @property
    def pillarFarFieldVelocity(self) -> bool:
        """Use far-field velocity for pillars."""
        return self._model.get_bool("numerics.pillarfarfieldvelocity")

    @pillarFarFieldVelocity.setter
    def pillarFarFieldVelocity(self, value: bool) -> None:
        self._model.set_bool("numerics.pillarfarfieldvelocity", value)

    @property
    def minWaterlevelChangeBreak(self) -> float:
        """Stop the simulation when the rolling mean of the maximum water level change is below this value (considered when larger than 0.0)."""
        return self._model.get_double("numerics.minwaterlevelchangebreak")

    @minWaterlevelChangeBreak.setter
    def minWaterlevelChangeBreak(self, value: float) -> None:
        self._model.set_double("numerics.minwaterlevelchangebreak", value)

    @property
    def facLaxTurb(self) -> str:
        """"""
        return self._model.get_string("numerics.faclaxturb")

    @facLaxTurb.setter
    def facLaxTurb(self, value: str) -> None:
        self._model.set_string("numerics.faclaxturb", value)

    @property
    def facLaxTurbHor(self) -> str:
        """"""
        return self._model.get_string("numerics.faclaxturbhor")

    @facLaxTurbHor.setter
    def facLaxTurbHor(self, value: str) -> None:
        self._model.set_string("numerics.faclaxturbhor", value)

    @property
    def facLaxTurbVer(self) -> str:
        """"""
        return self._model.get_string("numerics.faclaxturbver")

    @facLaxTurbVer.setter
    def facLaxTurbVer(self, value: str) -> None:
        self._model.set_string("numerics.faclaxturbver", value)

    @property
    def barocZLayBed(self) -> str:
        """"""
        return self._model.get_string("numerics.baroczlaybed")

    @barocZLayBed.setter
    def barocZLayBed(self, value: str) -> None:
        self._model.set_string("numerics.baroczlaybed", value)

    @property
    def orgBarocKeywords(self) -> str:
        """"""
        return self._model.get_string("numerics.orgbarockeywords")

    @orgBarocKeywords.setter
    def orgBarocKeywords(self, value: str) -> None:
        self._model.set_string("numerics.orgbarockeywords", value)

    @property
    def barocTerm(self) -> str:
        """"""
        return self._model.get_string("numerics.barocterm")

    @barocTerm.setter
    def barocTerm(self, value: str) -> None:
        self._model.set_string("numerics.barocterm", value)

    @property
    def barocTimeInt(self) -> str:
        """"""
        return self._model.get_string("numerics.baroctimeint")

    @barocTimeInt.setter
    def barocTimeInt(self, value: str) -> None:
        self._model.set_string("numerics.baroctimeint", value)

    @property
    def jaDrhoDz(self) -> str:
        """"""
        return self._model.get_string("numerics.jadrhodz")

    @jaDrhoDz.setter
    def jaDrhoDz(self, value: str) -> None:
        self._model.set_string("numerics.jadrhodz", value)

    @property
    def epsTKE(self) -> str:
        """"""
        return self._model.get_string("numerics.epstke")

    @epsTKE.setter
    def epsTKE(self, value: str) -> None:
        self._model.set_string("numerics.epstke", value)

    @property
    def epsEPS(self) -> str:
        """"""
        return self._model.get_string("numerics.epseps")

    @epsEPS.setter
    def epsEPS(self, value: str) -> None:
        self._model.set_string("numerics.epseps", value)

    @property
    def transportTimeStepping(self) -> str:
        """"""
        return self._model.get_string("numerics.transporttimestepping")

    @transportTimeStepping.setter
    def transportTimeStepping(self, value: str) -> None:
        self._model.set_string("numerics.transporttimestepping", value)

    @property
    def transportMethod(self) -> str:
        """"""
        return self._model.get_string("numerics.transportmethod")

    @transportMethod.setter
    def transportMethod(self, value: str) -> None:
        self._model.set_string("numerics.transportmethod", value)

    @property
    def hkad(self) -> str:
        """"""
        return self._model.get_string("numerics.hkad")

    @hkad.setter
    def hkad(self, value: str) -> None:
        self._model.set_string("numerics.hkad", value)

    @property
    def iThinDykeScheme(self) -> str:
        """"""
        return self._model.get_string("numerics.ithindykescheme")

    @iThinDykeScheme.setter
    def iThinDykeScheme(self, value: str) -> None:
        self._model.set_string("numerics.ithindykescheme", value)

    @property
    def thinDykeContraction(self) -> str:
        """"""
        return self._model.get_string("numerics.thindykecontraction")

    @thinDykeContraction.setter
    def thinDykeContraction(self, value: str) -> None:
        self._model.set_string("numerics.thindykecontraction", value)

    @property
    def jaOrgSethu(self) -> str:
        """"""
        return self._model.get_string("numerics.jaorgsethu")

    @jaOrgSethu.setter
    def jaOrgSethu(self, value: str) -> None:
        self._model.set_string("numerics.jaorgsethu", value)

    @property
    def CFLWaveFrac(self) -> str:
        """"""
        return self._model.get_string("numerics.cflwavefrac")

    @CFLWaveFrac.setter
    def CFLWaveFrac(self, value: str) -> None:
        self._model.set_string("numerics.cflwavefrac", value)

    @property
    def jaEmbed1d(self) -> str:
        """"""
        return self._model.get_string("numerics.jaembed1d")

    @jaEmbed1d.setter
    def jaEmbed1d(self, value: str) -> None:
        self._model.set_string("numerics.jaembed1d", value)

    @property
    def sobekdfm_umin(self) -> str:
        """"""
        return self._model.get_string("numerics.sobekdfm_umin")

    @sobekdfm_umin.setter
    def sobekdfm_umin(self, value: str) -> None:
        self._model.set_string("numerics.sobekdfm_umin", value)

    @property
    def sobekdfm_umin_method(self) -> str:
        """"""
        return self._model.get_string("numerics.sobekdfm_umin_method")

    @sobekdfm_umin_method.setter
    def sobekdfm_umin_method(self, value: str) -> None:
        self._model.set_string("numerics.sobekdfm_umin_method", value)

    @property
    def sobekdfm_minimal_1d2d_embankment(self) -> str:
        """"""
        return self._model.get_string("numerics.sobekdfm_minimal_1d2d_embankment")

    @sobekdfm_minimal_1d2d_embankment.setter
    def sobekdfm_minimal_1d2d_embankment(self, value: str) -> None:
        self._model.set_string("numerics.sobekdfm_minimal_1d2d_embankment", value)

    @property
    def wridia_viscosity_diffusivity_limit(self) -> str:
        """"""
        return self._model.get_string("numerics.wridia_viscosity_diffusivity_limit")

    @wridia_viscosity_diffusivity_limit.setter
    def wridia_viscosity_diffusivity_limit(self, value: str) -> None:
        self._model.set_string("numerics.wridia_viscosity_diffusivity_limit", value)

    @property
    def maxSSC(self) -> float:
        """Upper limit of cell centre SSC concentration after transport timestep. Default 1e6 (effectively switched off)"""
        return self._model.get_double("numerics.maxssc")

    @maxSSC.setter
    def maxSSC(self, value: float) -> None:
        self._model.set_double("numerics.maxssc", value)

    @property
    def perotWeightUpdate(self) -> int:
        """Perot weight update option."""
        return self._model.get_int("numerics.perotweightupdate")

    @perotWeightUpdate.setter
    def perotWeightUpdate(self, value: int) -> None:
        self._model.set_int("numerics.perotweightupdate", value)

    @property
    def perotType(self) -> int:
        """Perot discretization type."""
        return self._model.get_int("numerics.perottype")

    @perotType.setter
    def perotType(self, value: int) -> None:
        self._model.set_int("numerics.perottype", value)


class PhysicsSection:
    """Typed access to the [physics] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def unifFrictCoef(self) -> float:
        """Uniform friction coefficient."""
        return self._model.get_double("physics.uniffrictcoef")

    @unifFrictCoef.setter
    def unifFrictCoef(self, value: float) -> None:
        self._model.set_double("physics.uniffrictcoef", value)

    @property
    def unifFrictType(self) -> str:
        """Uniform friction type."""
        return self._model.get_enum_name("physics.uniffricttype")

    @unifFrictType.setter
    def unifFrictType(self, value: str) -> None:
        self._model.set_enum_name("physics.uniffricttype", value)

    @property
    def unifFrictCoef1D(self) -> float:
        """Uniform friction coefficient in 1D links."""
        return self._model.get_double("physics.uniffrictcoef1d")

    @unifFrictCoef1D.setter
    def unifFrictCoef1D(self, value: float) -> None:
        self._model.set_double("physics.uniffrictcoef1d", value)

    @property
    def unifFrictCoef1D2D(self) -> float:
        """Uniform friction coefficient in 1D2D links."""
        return self._model.get_double("physics.uniffrictcoef1d2d")

    @unifFrictCoef1D2D.setter
    def unifFrictCoef1D2D(self, value: float) -> None:
        self._model.set_double("physics.uniffrictcoef1d2d", value)

    @property
    def unifFrictCoefLin(self) -> float:
        """Uniform linear friction coefficient."""
        return self._model.get_double("physics.uniffrictcoeflin")

    @unifFrictCoefLin.setter
    def unifFrictCoefLin(self, value: float) -> None:
        self._model.set_double("physics.uniffrictcoeflin", value)

    @property
    def vicouv(self) -> float:
        """Uniform horizontal eddy viscosity."""
        return self._model.get_double("physics.vicouv")

    @vicouv.setter
    def vicouv(self, value: float) -> None:
        self._model.set_double("physics.vicouv", value)

    @property
    def dicouv(self) -> float:
        """Uniform horizontal eddy diffusivity."""
        return self._model.get_double("physics.dicouv")

    @dicouv.setter
    def dicouv(self, value: float) -> None:
        self._model.set_double("physics.dicouv", value)

    @property
    def vicoww(self) -> float:
        """Background vertical eddy viscosity."""
        return self._model.get_double("physics.vicoww")

    @vicoww.setter
    def vicoww(self, value: float) -> None:
        self._model.set_double("physics.vicoww", value)

    @property
    def dicoww(self) -> float:
        """Background vertical eddy diffusivity."""
        return self._model.get_double("physics.dicoww")

    @dicoww.setter
    def dicoww(self, value: float) -> None:
        self._model.set_double("physics.dicoww", value)

    @property
    def vicwminb(self) -> float:
        """Minimum viscosity in production and buoyancy term."""
        return self._model.get_double("physics.vicwminb")

    @vicwminb.setter
    def vicwminb(self, value: float) -> None:
        self._model.set_double("physics.vicwminb", value)

    @property
    def xlOzmidov(self) -> float:
        """Ozmidov length scale, 0.0=no contribution of internal waves to vertical diffusion."""
        return self._model.get_double("physics.xlozmidov")

    @xlOzmidov.setter
    def xlOzmidov(self, value: float) -> None:
        self._model.set_double("physics.xlozmidov", value)

    @property
    def TKEMin(self) -> float:
        """Minimum turbulence kinetic energy (TKE) value in k-eps turbulence model."""
        return self._model.get_double("physics.tkemin")

    @TKEMin.setter
    def TKEMin(self, value: float) -> None:
        self._model.set_double("physics.tkemin", value)

    @property
    def EPSMin(self) -> float:
        """Minimum turbulent dissipation rate (EPS) value in k-eps turbulence model."""
        return self._model.get_double("physics.epsmin")

    @EPSMin.setter
    def EPSMin(self, value: float) -> None:
        self._model.set_double("physics.epsmin", value)

    @property
    def TAUMin(self) -> float:
        """Minimum turbulent time scale (TAU) value in k-tau turbulence model."""
        return self._model.get_double("physics.taumin")

    @TAUMin.setter
    def TAUMin(self, value: float) -> None:
        self._model.set_double("physics.taumin", value)

    @property
    def Smagorinsky(self) -> float:
        """Add Smagorinsky horizontal turbulence: vicu = vicu + ((`Smagorinsky`*dx)^2)*S."""
        return self._model.get_double("physics.smagorinsky")

    @Smagorinsky.setter
    def Smagorinsky(self, value: float) -> None:
        self._model.set_double("physics.smagorinsky", value)

    @property
    def Elder(self) -> float:
        """Add Elder contribution: vicu = vicu + (`Elder`*kappa*ustar*H/6); e.g. 1.0."""
        return self._model.get_double("physics.elder")

    @Elder.setter
    def Elder(self, value: float) -> None:
        self._model.set_double("physics.elder", value)

    @property
    def irov(self) -> str:
        """Wall friction."""
        return self._model.get_enum_name("physics.irov")

    @irov.setter
    def irov(self, value: str) -> None:
        self._model.set_enum_name("physics.irov", value)

    @property
    def wall_ks(self) -> float:
        """Nikuradse roughness for side walls, wall_z0=`wall_ks`/30."""
        return self._model.get_double("physics.wall_ks")

    @wall_ks.setter
    def wall_ks(self, value: float) -> None:
        self._model.set_double("physics.wall_ks", value)

    @property
    def rhoMean(self) -> float:
        """Average water density."""
        return self._model.get_double("physics.rhomean")

    @rhoMean.setter
    def rhoMean(self, value: float) -> None:
        self._model.set_double("physics.rhomean", value)

    @property
    def iDensForm(self) -> str:
        """Density calculation."""
        return self._model.get_enum_name("physics.idensform")

    @iDensForm.setter
    def iDensForm(self, value: str) -> None:
        self._model.set_enum_name("physics.idensform", value)

    @property
    def thermobaricity(self) -> bool:
        """Include pressure effects on water density. Only works for `iDensForm`=3 (UNESCO83)."""
        return self._model.get_bool("physics.thermobaricity")

    @thermobaricity.setter
    def thermobaricity(self, value: bool) -> None:
        self._model.set_bool("physics.thermobaricity", value)

    @property
    def ag(self) -> float:
        """Gravitational acceleration."""
        return self._model.get_double("physics.ag")

    @ag.setter
    def ag(self, value: float) -> None:
        self._model.set_double("physics.ag", value)

    @property
    def tidalForcing(self) -> bool:
        """Tidal forcing, if jserfic=1."""
        return self._model.get_bool("physics.tidalforcing")

    @tidalForcing.setter
    def tidalForcing(self, value: bool) -> None:
        self._model.set_bool("physics.tidalforcing", value)

    @property
    def itcap(self) -> float:
        """Upper limit on internal tides dissipation."""
        return self._model.get_double("physics.itcap")

    @itcap.setter
    def itcap(self, value: float) -> None:
        self._model.set_double("physics.itcap", value)

    @property
    def doodsonStart(self) -> float:
        """Doodson start time for tidal forcing."""
        return self._model.get_double("physics.doodsonstart")

    @doodsonStart.setter
    def doodsonStart(self, value: float) -> None:
        self._model.set_double("physics.doodsonstart", value)

    @property
    def doodsonStop(self) -> float:
        """Doodson stop time for tidal forcing."""
        return self._model.get_double("physics.doodsonstop")

    @doodsonStop.setter
    def doodsonStop(self, value: float) -> None:
        self._model.set_double("physics.doodsonstop", value)

    @property
    def doodsonEps(self) -> float:
        """Doodson tolerance level for tidal forcing."""
        return self._model.get_double("physics.doodsoneps")

    @doodsonEps.setter
    def doodsonEps(self, value: float) -> None:
        self._model.set_double("physics.doodsoneps", value)

    @property
    def villemonteCD1(self) -> float:
        """Calibration coefficient for Villemonte."""
        return self._model.get_double("physics.villemontecd1")

    @villemonteCD1.setter
    def villemonteCD1(self, value: float) -> None:
        self._model.set_double("physics.villemontecd1", value)

    @property
    def villemonteCD2(self) -> float:
        """Calibration coefficient for Villemonte."""
        return self._model.get_double("physics.villemontecd2")

    @villemonteCD2.setter
    def villemonteCD2(self, value: float) -> None:
        self._model.set_double("physics.villemontecd2", value)

    @property
    def salinity(self) -> bool:
        """Include salinity."""
        return self._model.get_bool("physics.salinity")

    @salinity.setter
    def salinity(self, value: bool) -> None:
        self._model.set_bool("physics.salinity", value)

    @property
    def initialSalinity(self) -> float:
        """Initial salinity concentration."""
        return self._model.get_double("physics.initialsalinity")

    @initialSalinity.setter
    def initialSalinity(self, value: float) -> None:
        self._model.set_double("physics.initialsalinity", value)

    @property
    def sal0AboveZLev(self) -> float:
        """Salinity 0 above level."""
        return self._model.get_double("physics.sal0abovezlev")

    @sal0AboveZLev.setter
    def sal0AboveZLev(self, value: float) -> None:
        self._model.set_double("physics.sal0abovezlev", value)

    @property
    def deltaSalinity(self) -> float:
        """Uniform initial salinity."""
        return self._model.get_double("physics.deltasalinity")

    @deltaSalinity.setter
    def deltaSalinity(self, value: float) -> None:
        self._model.set_double("physics.deltasalinity", value)

    @property
    def backgroundSalinity(self) -> float:
        """Background salinity for eqn. of state if salinity not computed."""
        return self._model.get_double("physics.backgroundsalinity")

    @backgroundSalinity.setter
    def backgroundSalinity(self, value: float) -> None:
        self._model.set_double("physics.backgroundsalinity", value)

    @property
    def temperature(self) -> str:
        """Include temperature."""
        return self._model.get_enum_name("physics.temperature")

    @temperature.setter
    def temperature(self, value: str) -> None:
        self._model.set_enum_name("physics.temperature", value)

    @property
    def initialTemperature(self) -> float:
        """Initial temperature."""
        return self._model.get_double("physics.initialtemperature")

    @initialTemperature.setter
    def initialTemperature(self, value: float) -> None:
        self._model.set_double("physics.initialtemperature", value)

    @property
    def backgroundWaterTemperature(self) -> float:
        """Background water temperature for eqn. of state if temperature not computed."""
        return self._model.get_double("physics.backgroundwatertemperature")

    @backgroundWaterTemperature.setter
    def backgroundWaterTemperature(self, value: float) -> None:
        self._model.set_double("physics.backgroundwatertemperature", value)

    @property
    def SecchiDepth(self) -> float:
        """Water clarity parameter."""
        return self._model.get_double("physics.secchidepth")

    @SecchiDepth.setter
    def SecchiDepth(self, value: float) -> None:
        self._model.set_double("physics.secchidepth", value)

    @property
    def SecchiDepth2(self) -> float:
        """Water clarity parameter for non-penetrative radiation."""
        return self._model.get_double("physics.secchidepth2")

    @SecchiDepth2.setter
    def SecchiDepth2(self, value: float) -> None:
        self._model.set_double("physics.secchidepth2", value)

    @property
    def SecchiDepthNonPenetrative(self) -> float:
        """Water clarity parameter for non-penetrative radiation."""
        return self._model.get_double("physics.secchidepthnonpenetrative")

    @SecchiDepthNonPenetrative.setter
    def SecchiDepthNonPenetrative(self, value: float) -> None:
        self._model.set_double("physics.secchidepthnonpenetrative", value)

    @property
    def SecchiDepth2Fraction(self) -> float:
        """Fraction of solar radiation that falls in non-penetrative spectrum."""
        return self._model.get_double("physics.secchidepth2fraction")

    @SecchiDepth2Fraction.setter
    def SecchiDepth2Fraction(self, value: float) -> None:
        self._model.set_double("physics.secchidepth2fraction", value)

    @property
    def SecchiDepthNonPenetrativeFraction(self) -> float:
        """Fraction of solar radiation that falls in non-penetrative spectrum."""
        return self._model.get_double("physics.secchidepthnonpenetrativefraction")

    @SecchiDepthNonPenetrativeFraction.setter
    def SecchiDepthNonPenetrativeFraction(self, value: float) -> None:
        self._model.set_double("physics.secchidepthnonpenetrativefraction", value)

    @property
    def stanton(self) -> float:
        """Coefficient for convective heat flux, if negative, then Cd wind is used."""
        return self._model.get_double("physics.stanton")

    @stanton.setter
    def stanton(self, value: float) -> None:
        self._model.set_double("physics.stanton", value)

    @property
    def dalton(self) -> float:
        """Coefficient for evaporative heat flux, if negative, then Cd wind is used."""
        return self._model.get_double("physics.dalton")

    @dalton.setter
    def dalton(self, value: float) -> None:
        self._model.set_double("physics.dalton", value)

    @property
    def albedo(self) -> float:
        """Albedo coefficient. Fraction of solar radiation reflected by the water surface."""
        return self._model.get_double("physics.albedo")

    @albedo.setter
    def albedo(self, value: float) -> None:
        self._model.set_double("physics.albedo", value)

    @property
    def tempMax(self) -> float:
        """Limit the temperature to max value."""
        return self._model.get_double("physics.tempmax")

    @tempMax.setter
    def tempMax(self, value: float) -> None:
        self._model.set_double("physics.tempmax", value)

    @property
    def tempMin(self) -> float:
        """Limit the temperature to min value."""
        return self._model.get_double("physics.tempmin")

    @tempMin.setter
    def tempMin(self, value: float) -> None:
        self._model.set_double("physics.tempmin", value)

    @property
    def saliMax(self) -> float:
        """Limit for salinity to max value."""
        return self._model.get_double("physics.salimax")

    @saliMax.setter
    def saliMax(self, value: float) -> None:
        self._model.set_double("physics.salimax", value)

    @property
    def saliMin(self) -> float:
        """Limit for salinity to min value."""
        return self._model.get_double("physics.salimin")

    @saliMin.setter
    def saliMin(self, value: float) -> None:
        self._model.set_double("physics.salimin", value)

    @property
    def heat_eachStep(self) -> str:
        """Switch for heat each time step or each user time step."""
        return self._model.get_enum_name("physics.heat_eachstep")

    @heat_eachStep.setter
    def heat_eachStep(self, value: str) -> None:
        self._model.set_enum_name("physics.heat_eachstep", value)

    @property
    def nudgeTimeUni(self) -> float:
        """Uniform nudge relaxation time."""
        return self._model.get_double("physics.nudgetimeuni")

    @nudgeTimeUni.setter
    def nudgeTimeUni(self, value: float) -> None:
        self._model.set_double("physics.nudgetimeuni", value)

    @property
    def iniWithNudge(self) -> str:
        """Initialize salinity and temperature with nudge variables."""
        return self._model.get_enum_name("physics.iniwithnudge")

    @iniWithNudge.setter
    def iniWithNudge(self, value: str) -> None:
        self._model.set_enum_name("physics.iniwithnudge", value)

    @property
    def secondaryFlow(self) -> bool:
        """Secondary flow."""
        return self._model.get_bool("physics.secondaryflow")

    @secondaryFlow.setter
    def secondaryFlow(self, value: bool) -> None:
        self._model.set_bool("physics.secondaryflow", value)

    @property
    def betaSpiral(self) -> float:
        """Weight factor of the spiral flow intensity on flow dispersion stresses. 0.0=disabled."""
        return self._model.get_double("physics.betaspiral")

    @betaSpiral.setter
    def betaSpiral(self, value: float) -> None:
        self._model.set_double("physics.betaspiral", value)

    @property
    def breachGrowth(self) -> str:
        """Method for distributing dam breach width over dam break flow links."""
        return self._model.get_enum_name("physics.breachgrowth")

    @breachGrowth.setter
    def breachGrowth(self, value: str) -> None:
        self._model.set_enum_name("physics.breachgrowth", value)

    @property
    def thermobaricityInPressureGradient(self) -> bool:
        """Apply thermobaricity in computing the baroclinic pressure gradient."""
        return self._model.get_bool("physics.thermobaricityinpressuregradient")

    @thermobaricityInPressureGradient.setter
    def thermobaricityInPressureGradient(self, value: bool) -> None:
        self._model.set_bool("physics.thermobaricityinpressuregradient", value)

    @property
    def surfTempSmoFac(self) -> float:
        """Horizontal smoothing factor for surface water in heatflux computations."""
        return self._model.get_double("physics.surftempsmofac")

    @surfTempSmoFac.setter
    def surfTempSmoFac(self, value: float) -> None:
        self._model.set_double("physics.surftempsmofac", value)

    @property
    def selfAttractionLoading_correct_wl_with_ini(self) -> bool:
        """Correct water level with initial water level in self attraction and loading."""
        return self._model.get_bool("physics.selfattractionloading_correct_wl_with_ini")

    @selfAttractionLoading_correct_wl_with_ini.setter
    def selfAttractionLoading_correct_wl_with_ini(self, value: bool) -> None:
        self._model.set_bool("physics.selfattractionloading_correct_wl_with_ini", value)

    @property
    def nfEntrainmentMomentum(self) -> bool:
        """Use momentum transfer in NearField related entrainment."""
        return self._model.get_bool("physics.nfentrainmentmomentum")

    @nfEntrainmentMomentum.setter
    def nfEntrainmentMomentum(self, value: bool) -> None:
        self._model.set_bool("physics.nfentrainmentmomentum", value)

    @property
    def equili(self) -> bool:
        """Equilibrium spiral flow intensity."""
        return self._model.get_bool("physics.equili")

    @equili.setter
    def equili(self, value: bool) -> None:
        self._model.set_bool("physics.equili", value)

    @property
    def soilTempThick(self) -> float:
        """Use soil temperature buffer if >0.0"""
        return self._model.get_double("physics.soiltempthick")

    @soilTempThick.setter
    def soilTempThick(self, value: float) -> None:
        self._model.set_double("physics.soiltempthick", value)

    @property
    def selfAttractionLoading(self) -> str:
        """Use self attraction and loading."""
        return self._model.get_enum_name("physics.selfattractionloading")

    @selfAttractionLoading.setter
    def selfAttractionLoading(self, value: str) -> None:
        self._model.set_enum_name("physics.selfattractionloading", value)

    @property
    def prandtlNumberTemperature(self) -> float:
        """Turbulent Prandtl number for temperature."""
        return self._model.get_double("physics.prandtlnumbertemperature")

    @prandtlNumberTemperature.setter
    def prandtlNumberTemperature(self, value: float) -> None:
        self._model.set_double("physics.prandtlnumbertemperature", value)

    @property
    def schmidtNumberSalinity(self) -> float:
        """Turbulent Schmidt number for salinity."""
        return self._model.get_double("physics.schmidtnumbersalinity")

    @schmidtNumberSalinity.setter
    def schmidtNumberSalinity(self, value: float) -> None:
        self._model.set_double("physics.schmidtnumbersalinity", value)

    @property
    def schmidtNumberTracer(self) -> float:
        """Turbulent Schmidt number for tracer(s)."""
        return self._model.get_double("physics.schmidtnumbertracer")

    @schmidtNumberTracer.setter
    def schmidtNumberTracer(self, value: float) -> None:
        self._model.set_double("physics.schmidtnumbertracer", value)

    @property
    def umodLin(self) -> float:
        """Linear friction umod."""
        return self._model.get_double("physics.umodlin")

    @umodLin.setter
    def umodLin(self, value: float) -> None:
        self._model.set_double("physics.umodlin", value)

    @property
    def jadelvappos(self) -> bool:
        """Only positive forced evaporation fluxes."""
        return self._model.get_bool("physics.jadelvappos")

    @jadelvappos.setter
    def jadelvappos(self, value: bool) -> None:
        self._model.set_bool("physics.jadelvappos", value)

    @property
    def freeConvectionCoefficient(self) -> float:
        """Free convection turbulence coefficient."""
        return self._model.get_double("physics.freeconvectioncoefficient")

    @freeConvectionCoefficient.setter
    def freeConvectionCoefficient(self, value: float) -> None:
        self._model.set_double("physics.freeconvectioncoefficient", value)

    @property
    def uniffrictcoef1dgrlay(self) -> float:
        """Uniform ground layer friction coefficient for ocean models."""
        return self._model.get_double("physics.uniffrictcoef1dgrlay")

    @uniffrictcoef1dgrlay.setter
    def uniffrictcoef1dgrlay(self, value: float) -> None:
        self._model.set_double("physics.uniffrictcoef1dgrlay", value)

    @property
    def salinityDependentFreezingPoint(self) -> bool:
        """Enable computation of negative temperature values by adjusting freezing point based on salinity levels. `tempMin` should be below 0 degrees Celsius."""
        return self._model.get_bool("physics.salinitydependentfreezingpoint")

    @salinityDependentFreezingPoint.setter
    def salinityDependentFreezingPoint(self, value: bool) -> None:
        self._model.set_bool("physics.salinitydependentfreezingpoint", value)

    @property
    def allowCoolingBelowZero(self) -> str:
        """"""
        return self._model.get_string("physics.allowcoolingbelowzero")

    @allowCoolingBelowZero.setter
    def allowCoolingBelowZero(self, value: str) -> None:
        self._model.set_string("physics.allowcoolingbelowzero", value)

    @property
    def rhoAirRhoWater(self) -> str:
        """"""
        return self._model.get_string("physics.rhoairrhowater")

    @rhoAirRhoWater.setter
    def rhoAirRhoWater(self, value: str) -> None:
        self._model.set_string("physics.rhoairrhowater", value)

    @property
    def effectSpiral(self) -> str:
        """"""
        return self._model.get_string("physics.effectspiral")

    @effectSpiral.setter
    def effectSpiral(self, value: str) -> None:
        self._model.set_string("physics.effectspiral", value)

    @property
    def stericCorrection(self) -> str:
        """"""
        return self._model.get_string("physics.stericcorrection")

    @stericCorrection.setter
    def stericCorrection(self, value: str) -> None:
        self._model.set_string("physics.stericcorrection", value)


class SedimentSection:
    """Typed access to the [sediment] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def sedimentModelNr(self) -> str:
        """Sediment model number."""
        return self._model.get_enum_name("sediment.sedimentmodelnr")

    @sedimentModelNr.setter
    def sedimentModelNr(self, value: str) -> None:
        self._model.set_enum_name("sediment.sedimentmodelnr", value)

    @property
    def morFile(self) -> Path:
        """Morphology settings file (*.mor)."""
        return self._model.get_path("sediment.morfile")

    @morFile.setter
    def morFile(self, value: Path | str) -> None:
        self._model.set_path("sediment.morfile", value)

    @property
    def sedFile(self) -> Path:
        """Sediment characteristics file (*.sed)."""
        return self._model.get_path("sediment.sedfile")

    @sedFile.setter
    def sedFile(self, value: Path | str) -> None:
        self._model.set_path("sediment.sedfile", value)

    @property
    def jaceneqtr(self) -> int:
        """TODO"""
        return self._model.get_int("sediment.jaceneqtr")

    @jaceneqtr.setter
    def jaceneqtr(self, value: int) -> None:
        self._model.set_int("sediment.jaceneqtr", value)

    @property
    def mxgrkrone(self) -> int:
        """Highest fraction index treated by Krone."""
        return self._model.get_int("sediment.mxgrkrone")

    @mxgrkrone.setter
    def mxgrkrone(self, value: int) -> None:
        self._model.set_int("sediment.mxgrkrone", value)

    @property
    def sedDensCoupling(self) -> bool:
        """Sed rho coupling."""
        return self._model.get_bool("sediment.seddenscoupling")

    @sedDensCoupling.setter
    def sedDensCoupling(self, value: bool) -> None:
        self._model.set_bool("sediment.seddenscoupling", value)

    @property
    def implicitFallVelocity(self) -> str:
        """Implicit or explicit fall velocity"""
        return self._model.get_enum_name("sediment.implicitfallvelocity")

    @implicitFallVelocity.setter
    def implicitFallVelocity(self, value: str) -> None:
        self._model.set_enum_name("sediment.implicitfallvelocity", value)

    @property
    def nr_of_sedfractions(self) -> int:
        """Number of sediment fractions."""
        return self._model.get_int("sediment.nr_of_sedfractions")

    @nr_of_sedfractions.setter
    def nr_of_sedfractions(self, value: int) -> None:
        self._model.set_int("sediment.nr_of_sedfractions", value)

    @property
    def dredgeFile(self) -> Path:
        """Dredging/dumping settings file (*.dad)"""
        return self._model.get_path("sediment.dredgefile")

    @dredgeFile.setter
    def dredgeFile(self, value: Path | str) -> None:
        self._model.set_path("sediment.dredgefile", value)

    @property
    def bndTreatment(self) -> bool:
        """Separate treatment boundary links in upwinding transports."""
        return self._model.get_bool("sediment.bndtreatment")

    @bndTreatment.setter
    def bndTreatment(self, value: bool) -> None:
        self._model.set_bool("sediment.bndtreatment", value)

    @property
    def sourSink(self) -> str:
        """Switch off source or sink terms for sed advection."""
        return self._model.get_enum_name("sediment.soursink")

    @sourSink.setter
    def sourSink(self, value: str) -> None:
        self._model.set_enum_name("sediment.soursink", value)

    @property
    def cRefCav(self) -> float:
        """Calibration par only in jased==3, default=20.0 ( )"""
        return self._model.get_double("sediment.crefcav")

    @cRefCav.setter
    def cRefCav(self, value: float) -> None:
        self._model.set_double("sediment.crefcav", value)

    @property
    def d50(self) -> float:
        """Mean Sandgrain diameter (m), e.g. 0.0001"""
        return self._model.get_double("sediment.d50")

    @d50.setter
    def d50(self, value: float) -> None:
        self._model.set_double("sediment.d50", value)

    @property
    def morFac(self) -> float:
        """Morphological acceleration factor (), bottom updates active for morfac > 0, 1.0=realtime, etc"""
        return self._model.get_double("sediment.morfac")

    @morFac.setter
    def morFac(self, value: float) -> None:
        self._model.set_double("sediment.morfac", value)

    @property
    def rhoSed(self) -> float:
        """Mean Sandgrain rho (kg/m3) , e.g. 2650"""
        return self._model.get_double("sediment.rhosed")

    @rhoSed.setter
    def rhoSed(self, value: float) -> None:
        self._model.set_double("sediment.rhosed", value)

    @property
    def numIntVerticalEinstein(self) -> int:
        """Number of vertical intervals in Einstein integrals ( )"""
        return self._model.get_int("sediment.numintverticaleinstein")

    @numIntVerticalEinstein.setter
    def numIntVerticalEinstein(self, value: int) -> None:
        self._model.set_int("sediment.numintverticaleinstein", value)

    @property
    def alfaBed(self) -> float:
        """Calibration par bed load, default=1.0 ( )"""
        return self._model.get_double("sediment.alfabed")

    @alfaBed.setter
    def alfaBed(self, value: float) -> None:
        self._model.set_double("sediment.alfabed", value)

    @property
    def tMorfSpinup(self) -> float:
        """Spin up time for morphological adaptations (s)"""
        return self._model.get_double("sediment.tmorfspinup")

    @tMorfSpinup.setter
    def tMorfSpinup(self, value: float) -> None:
        self._model.set_double("sediment.tmorfspinup", value)

    @property
    def initialSedimentConcentration(self) -> float:
        """Initial Sediment Concentration in jased==3 (kg/m3)"""
        return self._model.get_double("sediment.initialsedimentconcentration")

    @initialSedimentConcentration.setter
    def initialSedimentConcentration(self, value: float) -> None:
        self._model.set_double("sediment.initialsedimentconcentration", value)

    @property
    def uniformErodableThickness(self) -> float:
        """Uniform erodable layer thickness (m)"""
        return self._model.get_double("sediment.uniformerodablethickness")

    @uniformErodableThickness.setter
    def uniformErodableThickness(self, value: float) -> None:
        self._model.set_double("sediment.uniformerodablethickness", value)

    @property
    def alfaSus(self) -> float:
        """Calibration par suspended load, default=1.0 ( )"""
        return self._model.get_double("sediment.alfasus")

    @alfaSus.setter
    def alfaSus(self, value: float) -> None:
        self._model.set_double("sediment.alfasus", value)

    @property
    def morCFL(self) -> bool:
        """Use CFL-like condition for morphologic updating (0=no, 1=yes) (default yes)"""
        return self._model.get_bool("sediment.morcfl")

    @morCFL.setter
    def morCFL(self, value: bool) -> None:
        self._model.set_bool("sediment.morcfl", value)

    @property
    def DzbDtMax(self) -> float:
        """Maximum bed level change (m) per time step for the case MorCFL=1 (default=0.1 m)"""
        return self._model.get_double("sediment.dzbdtmax")

    @DzbDtMax.setter
    def DzbDtMax(self, value: float) -> None:
        self._model.set_double("sediment.dzbdtmax", value)

    @property
    def masBalMinDep(self) -> float:
        """Minimum depth after bottom update for SSC adaptation mass balance."""
        return self._model.get_double("sediment.masbalmindep")

    @masBalMinDep.setter
    def masBalMinDep(self, value: float) -> None:
        self._model.set_double("sediment.masbalmindep", value)

    @property
    def morphoPol(self) -> Path:
        """Only apply bed updating wihtin specified polygon (*.pol)"""
        return self._model.get_path("sediment.morphopol")

    @morphoPol.setter
    def morphoPol(self, value: Path | str) -> None:
        self._model.set_path("sediment.morphopol", value)

    @property
    def ws(self) -> list[float]:
        """Fall velocity (m/s), e.g. 0.0005 m/s"""
        return self._model.get_double_list("sediment.ws")

    @ws.setter
    def ws(self, value: list[float]) -> None:
        self._model.set_double_list("sediment.ws", value)

    @property
    def erosionPar(self) -> list[float]:
        """Krone Partheniades erosion parameter, e.g. 0.0001 (kg/(m2s)"""
        return self._model.get_double_list("sediment.erosionpar")

    @erosionPar.setter
    def erosionPar(self, value: list[float]) -> None:
        self._model.set_double_list("sediment.erosionpar", value)

    @property
    def tauCre(self) -> list[float]:
        """Critical shear stress for erosion (N/m2), e.g. 0.3"""
        return self._model.get_double_list("sediment.taucre")

    @tauCre.setter
    def tauCre(self, value: list[float]) -> None:
        self._model.set_double_list("sediment.taucre", value)

    @property
    def inMorphoPol(self) -> str:
        """Value of the update inside MorphoPol (0=inside polygon no update, 1=inside polygon yes update)"""
        return self._model.get_enum_name("sediment.inmorphopol")

    @inMorphoPol.setter
    def inMorphoPol(self, value: str) -> None:
        self._model.set_enum_name("sediment.inmorphopol", value)

    @property
    def mormergeDtUser(self) -> str:
        """Mormerge operation"""
        return self._model.get_enum_name("sediment.mormergedtuser")

    @mormergeDtUser.setter
    def mormergeDtUser(self, value: str) -> None:
        self._model.set_enum_name("sediment.mormergedtuser", value)

    @property
    def upperLimitSSC(self) -> float:
        """Upper limit of cell centre SSC concentration after transport timestep."""
        return self._model.get_double("sediment.upperlimitssc")

    @upperLimitSSC.setter
    def upperLimitSSC(self, value: float) -> None:
        self._model.set_double("sediment.upperlimitssc", value)


class SedtrailsSection:
    """Typed access to the [sedtrails] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def sedTrailsGrid(self) -> Path:
        """Grid file for sedtrails output locations on corners."""
        return self._model.get_path("sedtrails.sedtrailsgrid")

    @sedTrailsGrid.setter
    def sedTrailsGrid(self, value: Path | str) -> None:
        self._model.set_path("sedtrails.sedtrailsgrid", value)

    @property
    def sedtrailsAnalysis(self) -> str:
        """Sedtrails analysis."""
        return self._model.get_enum_name("sedtrails.sedtrailsanalysis")

    @sedtrailsAnalysis.setter
    def sedtrailsAnalysis(self, value: str) -> None:
        self._model.set_enum_name("sedtrails.sedtrailsanalysis", value)

    @property
    def sedtrailsInterval(self) -> list[float]:
        """Sedtrails output, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("sedtrails.sedtrailsinterval")

    @sedtrailsInterval.setter
    def sedtrailsInterval(self, value: list[float]) -> None:
        self._model.set_double_list("sedtrails.sedtrailsinterval", value)

    @property
    def sedtrailsOutputFile(self) -> Path:
        """Sedtrails time-averaged output file."""
        return self._model.get_path("sedtrails.sedtrailsoutputfile")

    @sedtrailsOutputFile.setter
    def sedtrailsOutputFile(self, value: Path | str) -> None:
        self._model.set_path("sedtrails.sedtrailsoutputfile", value)


class WindSection:
    """Typed access to the [wind] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def icdTyp(self) -> str:
        """Wind drag coefficient type."""
        return self._model.get_enum_name("wind.icdtyp")

    @icdTyp.setter
    def icdTyp(self, value: str) -> None:
        self._model.set_enum_name("wind.icdtyp", value)

    @property
    def cdBreakPoints(self) -> list[float]:
        """Wind drag breakpoints."""
        return self._model.get_double_list("wind.cdbreakpoints")

    @cdBreakPoints.setter
    def cdBreakPoints(self, value: list[float]) -> None:
        self._model.set_double_list("wind.cdbreakpoints", value)

    @property
    def windSpeedBreakpoints(self) -> list[float]:
        """Wind speed breakpoints."""
        return self._model.get_double_list("wind.windspeedbreakpoints")

    @windSpeedBreakpoints.setter
    def windSpeedBreakpoints(self, value: list[float]) -> None:
        self._model.set_double_list("wind.windspeedbreakpoints", value)

    @property
    def rhoAir(self) -> float:
        """Air density."""
        return self._model.get_double("wind.rhoair")

    @rhoAir.setter
    def rhoAir(self, value: float) -> None:
        self._model.set_double("wind.rhoair", value)

    @property
    def computedAirDensity(self) -> bool:
        """Compute air density. Requires quantities airpressure, airtemperature and dewpoint in ext-file."""
        return self._model.get_bool("wind.computedairdensity")

    @computedAirDensity.setter
    def computedAirDensity(self, value: bool) -> None:
        self._model.set_bool("wind.computedairdensity", value)

    @property
    def stressToWind(self) -> float:
        """Switch between wind speed and wind stress approach for wind forcing."""
        return self._model.get_double("wind.stresstowind")

    @stressToWind.setter
    def stressToWind(self, value: float) -> None:
        self._model.set_double("wind.stresstowind", value)

    @property
    def relativeWind(self) -> float:
        """Wind speed factor relative to top-layer water speed*`relativeWind` (0.0=no relative wind, 1.0=using full top layer speed)."""
        return self._model.get_double("wind.relativewind")

    @relativeWind.setter
    def relativeWind(self, value: float) -> None:
        self._model.set_double("wind.relativewind", value)

    @property
    def windPartialDry(self) -> bool:
        """Reduce windstress on water if link partially dry, only for `bedLevType`=3."""
        return self._model.get_bool("wind.windpartialdry")

    @windPartialDry.setter
    def windPartialDry(self, value: bool) -> None:
        self._model.set_bool("wind.windpartialdry", value)

    @property
    def pavBnd(self) -> float:
        """Average air pressure on open boundaries, only applied if value > 0."""
        return self._model.get_double("wind.pavbnd")

    @pavBnd.setter
    def pavBnd(self, value: float) -> None:
        self._model.set_double("wind.pavbnd", value)

    @property
    def pavIni(self) -> float:
        """Initial air pressure, only applied if value > 0."""
        return self._model.get_double("wind.pavini")

    @pavIni.setter
    def pavIni(self, value: float) -> None:
        self._model.set_double("wind.pavini", value)

    @property
    def windHuOrZwsBased(self) -> str:
        """Wind drag hu or zws based."""
        return self._model.get_enum_name("wind.windhuorzwsbased")

    @windHuOrZwsBased.setter
    def windHuOrZwsBased(self, value: str) -> None:
        self._model.set_enum_name("wind.windhuorzwsbased", value)

    @property
    def varyingAirDensity(self) -> bool:
        """Compute air density."""
        return self._model.get_bool("wind.varyingairdensity")

    @varyingAirDensity.setter
    def varyingAirDensity(self, value: bool) -> None:
        self._model.set_bool("wind.varyingairdensity", value)

    @property
    def wind_eachstep(self) -> str:
        """Switch for wind and air pressure each time step or each user time step."""
        return self._model.get_enum_name("wind.wind_eachstep")

    @wind_eachstep.setter
    def wind_eachstep(self, value: str) -> None:
        self._model.set_enum_name("wind.wind_eachstep", value)

    @property
    def rhoWaterInWindStress(self) -> str:
        """Water density used in computation of wind stress."""
        return self._model.get_enum_name("wind.rhowaterinwindstress")

    @rhoWaterInWindStress.setter
    def rhoWaterInWindStress(self, value: str) -> None:
        self._model.set_enum_name("wind.rhowaterinwindstress", value)

    @property
    def gapres(self) -> str:
        """"""
        return self._model.get_string("wind.gapres")

    @gapres.setter
    def gapres(self, value: str) -> None:
        self._model.set_string("wind.gapres", value)


class WavesSection:
    """Typed access to the [waves] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def waveModelNr(self) -> str:
        """Wave model nr."""
        return self._model.get_enum_name("waves.wavemodelnr")

    @waveModelNr.setter
    def waveModelNr(self, value: str) -> None:
        self._model.set_enum_name("waves.wavemodelnr", value)

    @property
    def waveforcing(self) -> int:
        """Wave forcing mode (only supported for waveModelNr = 7)."""
        return self._model.get_int("waves.waveforcing")

    @waveforcing.setter
    def waveforcing(self, value: int) -> None:
        self._model.set_int("waves.waveforcing", value)

    @property
    def _3Dwavebreakerturbulence(self) -> bool:
        """Add wave-induced production terms in turbulence modelling"""
        return self._model.get_bool("waves.3dwavebreakerturbulence")

    @_3Dwavebreakerturbulence.setter
    def _3Dwavebreakerturbulence(self, value: bool) -> None:
        self._model.set_bool("waves.3dwavebreakerturbulence", value)

    @property
    def rouWav(self) -> str:
        """Friction model for wave induced shear stress."""
        return self._model.get_enum_name("waves.rouwav")

    @rouWav.setter
    def rouWav(self, value: str) -> None:
        self._model.set_enum_name("waves.rouwav", value)

    @property
    def gammaX(self) -> float:
        """Maximum wave height/water depth ratio."""
        return self._model.get_double("waves.gammax")

    @gammaX.setter
    def gammaX(self, value: float) -> None:
        self._model.set_double("waves.gammax", value)

    @property
    def flowWithoutWaves(self) -> bool:
        """Exclude Wave data in the flow computations, passing it directly to D-WAQ."""
        return self._model.get_bool("waves.flowwithoutwaves")

    @flowWithoutWaves.setter
    def flowWithoutWaves(self, value: bool) -> None:
        self._model.set_bool("waves.flowwithoutwaves", value)

    @property
    def surfBeatInput(self) -> Path:
        """File with surf beat input conditions."""
        return self._model.get_path("waves.surfbeatinput")

    @surfBeatInput.setter
    def surfBeatInput(self, value: Path | str) -> None:
        self._model.set_path("waves.surfbeatinput", value)

    @property
    def waveSwartDelwaq(self) -> str:
        """If `waveSwartDelwaq`=1 and tiWaq> 0, then increase tauwave to Delwaq with 0.5rhofwuorbuorb."""
        return self._model.get_enum_name("waves.waveswartdelwaq")

    @waveSwartDelwaq.setter
    def waveSwartDelwaq(self, value: str) -> None:
        self._model.set_enum_name("waves.waveswartdelwaq", value)

    @property
    def hwavuni(self) -> float:
        """Root mean square wave height."""
        return self._model.get_double("waves.hwavuni")

    @hwavuni.setter
    def hwavuni(self, value: float) -> None:
        self._model.set_double("waves.hwavuni", value)

    @property
    def tiFetchComp(self) -> float:
        """Time interval fetch comp if `waveModelNr`=1,2."""
        return self._model.get_double("waves.tifetchcomp")

    @tiFetchComp.setter
    def tiFetchComp(self, value: float) -> None:
        self._model.set_double("waves.tifetchcomp", value)

    @property
    def phiwavuni(self) -> float:
        """Root mean square wave direction, math convention."""
        return self._model.get_double("waves.phiwavuni")

    @phiwavuni.setter
    def phiwavuni(self, value: float) -> None:
        self._model.set_double("waves.phiwavuni", value)

    @property
    def _3DWaveStreaming(self) -> int:
        """Influence of wave streaming. 0: no, 1: added to adve."""
        return self._model.get_int("waves.3dwavestreaming")

    @_3DWaveStreaming.setter
    def _3DWaveStreaming(self, value: int) -> None:
        self._model.set_int("waves.3dwavestreaming", value)

    @property
    def _3DWaveBoundaryLayer(self) -> str:
        """Boundary layer formulation."""
        return self._model.get_enum_name("waves.3dwaveboundarylayer")

    @_3DWaveBoundaryLayer.setter
    def _3DWaveBoundaryLayer(self, value: str) -> None:
        self._model.set_enum_name("waves.3dwaveboundarylayer", value)

    @property
    def twavuni(self) -> float:
        """Root mean square wave period."""
        return self._model.get_double("waves.twavuni")

    @twavuni.setter
    def twavuni(self, value: float) -> None:
        self._model.set_double("waves.twavuni", value)

    @property
    def uorbfac(self) -> str:
        """Orbital velocities."""
        return self._model.get_enum_name("waves.uorbfac")

    @uorbfac.setter
    def uorbfac(self, value: str) -> None:
        self._model.set_enum_name("waves.uorbfac", value)

    @property
    def _3DStokesProfile(self) -> str:
        """Stokes profile."""
        return self._model.get_enum_name("waves.3dstokesprofile")

    @_3DStokesProfile.setter
    def _3DStokesProfile(self, value: str) -> None:
        self._model.set_enum_name("waves.3dstokesprofile", value)

    @property
    def jamapsigwav(self) -> str:
        """Wave height on map output"""
        return self._model.get_enum_name("waves.jamapsigwav")

    @jamapsigwav.setter
    def jamapsigwav(self, value: str) -> None:
        self._model.set_enum_name("waves.jamapsigwav", value)

    @property
    def hminlw(self) -> float:
        """Cut-off depth for application of wave forces in momentum balance."""
        return self._model.get_double("waves.hminlw")

    @hminlw.setter
    def hminlw(self, value: float) -> None:
        self._model.set_double("waves.hminlw", value)

    @property
    def jahissigwav(self) -> str:
        """Wave height on his output"""
        return self._model.get_enum_name("waves.jahissigwav")

    @jahissigwav.setter
    def jahissigwav(self, value: str) -> None:
        self._model.set_enum_name("waves.jahissigwav", value)

    @property
    def waveNikuradse(self) -> str:
        """"""
        return self._model.get_string("waves.wavenikuradse")

    @waveNikuradse.setter
    def waveNikuradse(self, value: str) -> None:
        self._model.set_string("waves.wavenikuradse", value)


class GrwSection:
    """Typed access to the [grw] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def groundWater(self) -> bool:
        """Use (horizontal) ground water flow."""
        return self._model.get_bool("grw.groundwater")

    @groundWater.setter
    def groundWater(self, value: bool) -> None:
        self._model.set_bool("grw.groundwater", value)

    @property
    def infiltrationModel(self) -> str:
        """Infiltration method."""
        return self._model.get_enum_name("grw.infiltrationmodel")

    @infiltrationModel.setter
    def infiltrationModel(self, value: str) -> None:
        self._model.set_enum_name("grw.infiltrationmodel", value)

    @property
    def hInterceptionLayer(self) -> float:
        """Intercept this amount of rain."""
        return self._model.get_double("grw.hinterceptionlayer")

    @hInterceptionLayer.setter
    def hInterceptionLayer(self, value: float) -> None:
        self._model.set_double("grw.hinterceptionlayer", value)

    @property
    def unifInfiltrationCapacity(self) -> float:
        """Uniform maximum infiltration capacity."""
        return self._model.get_double("grw.unifinfiltrationcapacity")

    @unifInfiltrationCapacity.setter
    def unifInfiltrationCapacity(self, value: float) -> None:
        self._model.set_double("grw.unifinfiltrationcapacity", value)

    @property
    def conductivity(self) -> float:
        """Non-dimensionless K conductivity saturated, Q = K*A*i (m³/s)."""
        return self._model.get_double("grw.conductivity")

    @conductivity.setter
    def conductivity(self, value: float) -> None:
        self._model.set_double("grw.conductivity", value)

    @property
    def h_aquiferuni(self) -> float:
        """Level of impervious layer is bgrw = bl - `h_aquiferuni`, if negative, bgrw = `bgrwuni`."""
        return self._model.get_double("grw.h_aquiferuni")

    @h_aquiferuni.setter
    def h_aquiferuni(self, value: float) -> None:
        self._model.set_double("grw.h_aquiferuni", value)

    @property
    def bgrwuni(self) -> float:
        """Uniform level of impervious layer, only used if `h_aquiferuni` is negative."""
        return self._model.get_double("grw.bgrwuni")

    @bgrwuni.setter
    def bgrwuni(self, value: float) -> None:
        self._model.set_double("grw.bgrwuni", value)

    @property
    def h_unsatini(self) -> float:
        """Initial level ground water is bedlevel - `h_unsatini`, if negative, sgrw = `sgrwini`."""
        return self._model.get_double("grw.h_unsatini")

    @h_unsatini.setter
    def h_unsatini(self, value: float) -> None:
        self._model.set_double("grw.h_unsatini", value)

    @property
    def sgrwini(self) -> float:
        """Initial ground water level, if `h_unsatini` < 0."""
        return self._model.get_double("grw.sgrwini")

    @sgrwini.setter
    def sgrwini(self, value: float) -> None:
        self._model.set_double("grw.sgrwini", value)


class HydrologySection:
    """Typed access to the [hydrology] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def interceptionModel(self) -> str:
        """Interception model."""
        return self._model.get_enum_name("hydrology.interceptionmodel")

    @interceptionModel.setter
    def interceptionModel(self, value: str) -> None:
        self._model.set_enum_name("hydrology.interceptionmodel", value)


class TimeSection:
    """Typed access to the [time] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def refDate(self) -> datetime:
        """Reference date. By default midnight is taken (00h00m00s)."""
        return self._model.get_datetime("time.refdate")

    @refDate.setter
    def refDate(self, value: datetime) -> None:
        self._model.set_datetime("time.refdate", value)

    @property
    def tZone(self) -> float:
        """Data Sources in GMT are interrogated with time in minutes since `refDate`-`tZone`*60."""
        return self._model.get_double("time.tzone")

    @tZone.setter
    def tZone(self, value: float) -> None:
        self._model.set_double("time.tzone", value)

    @property
    def tUnit(self) -> str:
        """Time units in MDU."""
        return self._model.get_enum_name("time.tunit")

    @tUnit.setter
    def tUnit(self, value: str) -> None:
        self._model.set_enum_name("time.tunit", value)

    @property
    def dtUser(self) -> float:
        """User timestep in seconds (interval for external forcing update & his/map output)."""
        return self._model.get_double("time.dtuser")

    @dtUser.setter
    def dtUser(self, value: float) -> None:
        self._model.set_double("time.dtuser", value)

    @property
    def dtNodal(self) -> float:
        """Time interval for updating nodal factors in astronomical boundary conditions."""
        return self._model.get_double("time.dtnodal")

    @dtNodal.setter
    def dtNodal(self, value: float) -> None:
        self._model.set_double("time.dtnodal", value)

    @property
    def dtMax(self) -> float:
        """Maximum timestep in seconds."""
        return self._model.get_double("time.dtmax")

    @dtMax.setter
    def dtMax(self, value: float) -> None:
        self._model.set_double("time.dtmax", value)

    @property
    def dtInit(self) -> float:
        """Initial timestep in seconds."""
        return self._model.get_double("time.dtinit")

    @dtInit.setter
    def dtInit(self, value: float) -> None:
        self._model.set_double("time.dtinit", value)

    @property
    def tStart(self) -> float:
        """Start time w.r.t. `refDate`."""
        return self._model.get_double("time.tstart")

    @tStart.setter
    def tStart(self, value: float) -> None:
        self._model.set_double("time.tstart", value)

    @property
    def tStop(self) -> float:
        """Stop time w.r.t. `refDate`."""
        return self._model.get_double("time.tstop")

    @tStop.setter
    def tStop(self, value: float) -> None:
        self._model.set_double("time.tstop", value)

    @property
    def startDateTime(self) -> datetime:
        """Computation start datetime, when specified, overrides `tStart`."""
        return self._model.get_datetime("time.startdatetime")

    @startDateTime.setter
    def startDateTime(self, value: datetime) -> None:
        self._model.set_datetime("time.startdatetime", value)

    @property
    def stopDateTime(self) -> datetime:
        """Computation stop datetime, when specified, overrides `tStop`."""
        return self._model.get_datetime("time.stopdatetime")

    @stopDateTime.setter
    def stopDateTime(self, value: datetime) -> None:
        self._model.set_datetime("time.stopdatetime", value)

    @property
    def updateRoughnessInterval(self) -> float:
        """Update interval for time dependent roughness parameters."""
        return self._model.get_double("time.updateroughnessinterval")

    @updateRoughnessInterval.setter
    def updateRoughnessInterval(self, value: float) -> None:
        self._model.set_double("time.updateroughnessinterval", value)

    @property
    def tStartTlfsmo(self) -> float:
        """Start time w.r.t. `refDate` of Fourier smoothing time on water level boundaries."""
        return self._model.get_double("time.tstarttlfsmo")

    @tStartTlfsmo.setter
    def tStartTlfsmo(self, value: float) -> None:
        self._model.set_double("time.tstarttlfsmo", value)

    @property
    def startDateTimeTlfsmo(self) -> datetime:
        """Computation start datetime w.r.t. `refDate` of Fourier smoothing time on water level boundaries, when specified, overrides `tStartTlfsmo`."""
        return self._model.get_datetime("time.startdatetimetlfsmo")

    @startDateTimeTlfsmo.setter
    def startDateTimeTlfsmo(self, value: datetime) -> None:
        self._model.set_datetime("time.startdatetimetlfsmo", value)

    @property
    def autoTimestep(self) -> str:
        """Automatic timestepping limited by the CFL condition. Several options are available controlling which flows are used in the CFL limit. Options 1 and 5 are the default for 2D and 3D models, respectively."""
        return self._model.get_enum_name("time.autotimestep")

    @autoTimestep.setter
    def autoTimestep(self, value: str) -> None:
        self._model.set_enum_name("time.autotimestep", value)

    @property
    def autoTimestepNoStruct(self) -> bool:
        """Exclude structure links (and neighbours) from time step limitation."""
        return self._model.get_bool("time.autotimestepnostruct")

    @autoTimestepNoStruct.setter
    def autoTimestepNoStruct(self, value: bool) -> None:
        self._model.set_bool("time.autotimestepnostruct", value)

    @property
    def autoTimestepNoQOut(self) -> bool:
        """Exclude negative qin terms from time step limitation."""
        return self._model.get_bool("time.autotimestepnoqout")

    @autoTimestepNoQOut.setter
    def autoTimestepNoQOut(self, value: bool) -> None:
        self._model.set_bool("time.autotimestepnoqout", value)

    @property
    def dtFacMax(self) -> float:
        """Max timestep increase factor in successive time steps."""
        return self._model.get_double("time.dtfacmax")

    @dtFacMax.setter
    def dtFacMax(self, value: float) -> None:
        self._model.set_double("time.dtfacmax", value)

    @property
    def timeStepAnalysis(self) -> bool:
        """Write time steps analysis file *.steps."""
        return self._model.get_bool("time.timestepanalysis")

    @timeStepAnalysis.setter
    def timeStepAnalysis(self, value: bool) -> None:
        self._model.set_bool("time.timestepanalysis", value)

    @property
    def autoTimeStepVisc(self) -> bool:
        """Use time step limitation based on explicit diffusive term."""
        return self._model.get_bool("time.autotimestepvisc")

    @autoTimeStepVisc.setter
    def autoTimeStepVisc(self, value: bool) -> None:
        self._model.set_bool("time.autotimestepvisc", value)

    @property
    def autoTimeStepDiff(self) -> str:
        """"""
        return self._model.get_string("time.autotimestepdiff")

    @autoTimeStepDiff.setter
    def autoTimeStepDiff(self, value: str) -> None:
        self._model.set_string("time.autotimestepdiff", value)


class RestartSection:
    """Typed access to the [restart] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def restartFile(self) -> Path:
        """Restart file, only from NetCDF-file, hence: either *_rst.nc or *_map.nc."""
        return self._model.get_path("restart.restartfile")

    @restartFile.setter
    def restartFile(self, value: Path | str) -> None:
        self._model.set_path("restart.restartfile", value)

    @property
    def restartDateTime(self) -> datetime:
        """Restart time, only relevant but obligatory in case of restart from *_map.nc."""
        return self._model.get_datetime("restart.restartdatetime")

    @restartDateTime.setter
    def restartDateTime(self, value: datetime) -> None:
        self._model.set_datetime("restart.restartdatetime", value)

    @property
    def rstIgnoreBl(self) -> bool:
        """Ignore bed level from restart."""
        return self._model.get_bool("restart.rstignorebl")

    @rstIgnoreBl.setter
    def rstIgnoreBl(self, value: bool) -> None:
        self._model.set_bool("restart.rstignorebl", value)


class ExternalForcingSection:
    """Typed access to the [external forcing] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def extForceFile(self) -> Path:
        """Old format for external forcings file *.ext, link with tim/cmp-format boundary conditions specification."""
        return self._model.get_path("external forcing.extforcefile")

    @extForceFile.setter
    def extForceFile(self, value: Path | str) -> None:
        self._model.set_path("external forcing.extforcefile", value)

    @property
    def extForceFileNew(self) -> list[Path]:
        """New format for external forcings file *.ext, link with bc-format boundary conditions specification. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes."""
        return self._model.get_path_list("external forcing.extforcefilenew")

    @extForceFileNew.setter
    def extForceFileNew(self, value: list[Path | str]) -> None:
        self._model.set_path_list("external forcing.extforcefilenew", value)

    @property
    def rainfall(self) -> bool:
        """Include rainfall."""
        return self._model.get_bool("external forcing.rainfall")

    @rainfall.setter
    def rainfall(self, value: bool) -> None:
        self._model.set_bool("external forcing.rainfall", value)

    @property
    def qExt(self) -> bool:
        """Include user Qin/out, externally provided."""
        return self._model.get_bool("external forcing.qext")

    @qExt.setter
    def qExt(self, value: bool) -> None:
        self._model.set_bool("external forcing.qext", value)

    @property
    def evaporation(self) -> bool:
        """Include evaporation in water balance."""
        return self._model.get_bool("external forcing.evaporation")

    @evaporation.setter
    def evaporation(self, value: bool) -> None:
        self._model.set_bool("external forcing.evaporation", value)

    @property
    def windExt(self) -> str:
        """Include wind, externally provided."""
        return self._model.get_enum_name("external forcing.windext")

    @windExt.setter
    def windExt(self, value: str) -> None:
        self._model.set_enum_name("external forcing.windext", value)


class TrachytopesSection:
    """Typed access to the [trachytopes] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def trtRou(self) -> str:
        """Flag for trachytopes."""
        return self._model.get_enum_name("trachytopes.trtrou")

    @trtRou.setter
    def trtRou(self, value: str) -> None:
        self._model.set_enum_name("trachytopes.trtrou", value)

    @property
    def trtDef(self) -> Path:
        """File (*.ttd) including trachytope definitions."""
        return self._model.get_path("trachytopes.trtdef")

    @trtDef.setter
    def trtDef(self, value: Path | str) -> None:
        self._model.set_path("trachytopes.trtdef", value)

    @property
    def trtL(self) -> Path:
        """File (*.arl) including distribution of trachytope definitions."""
        return self._model.get_path("trachytopes.trtl")

    @trtL.setter
    def trtL(self, value: Path | str) -> None:
        self._model.set_path("trachytopes.trtl", value)

    @property
    def dtTrt(self) -> float:
        """Interval for updating of bottom roughness due to trachytopes in seconds."""
        return self._model.get_double("trachytopes.dttrt")

    @dtTrt.setter
    def dtTrt(self, value: float) -> None:
        self._model.set_double("trachytopes.dttrt", value)

    @property
    def trtMxR(self) -> int:
        """Maximum recursion level for composite trachytope definitions."""
        return self._model.get_int("trachytopes.trtmxr")

    @trtMxR.setter
    def trtMxR(self, value: int) -> None:
        self._model.set_int("trachytopes.trtmxr", value)

    @property
    def trtMth(self) -> str:
        """Area averaging method."""
        return self._model.get_enum_name("trachytopes.trtmth")

    @trtMth.setter
    def trtMth(self, value: str) -> None:
        self._model.set_enum_name("trachytopes.trtmth", value)

    @property
    def trtMnh(self) -> float:
        """Minimum water depth for roughness computations."""
        return self._model.get_double("trachytopes.trtmnh")

    @trtMnh.setter
    def trtMnh(self, value: float) -> None:
        self._model.set_double("trachytopes.trtmnh", value)

    @property
    def trtCll(self) -> Path:
        """Calibration factor file for roughness from trachytopes."""
        return self._model.get_path("trachytopes.trtcll")

    @trtCll.setter
    def trtCll(self, value: Path | str) -> None:
        self._model.set_path("trachytopes.trtcll", value)

    @property
    def trtdt(self) -> str:
        """"""
        return self._model.get_string("trachytopes.trtdt")

    @trtdt.setter
    def trtdt(self, value: str) -> None:
        self._model.set_string("trachytopes.trtdt", value)


class OutputSection:
    """Typed access to the [output] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def wrishp_crs(self) -> bool:
        """Writing cross sections to shape file."""
        return self._model.get_bool("output.wrishp_crs")

    @wrishp_crs.setter
    def wrishp_crs(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_crs", value)

    @property
    def wrishp_dambreak(self) -> bool:
        """Writing dambreaks to shape file."""
        return self._model.get_bool("output.wrishp_dambreak")

    @wrishp_dambreak.setter
    def wrishp_dambreak(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_dambreak", value)

    @property
    def wrishp_dryarea(self) -> bool:
        """Writing dry areas to shape file."""
        return self._model.get_bool("output.wrishp_dryarea")

    @wrishp_dryarea.setter
    def wrishp_dryarea(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_dryarea", value)

    @property
    def wrishp_enc(self) -> bool:
        """Writing enclosures to shape file."""
        return self._model.get_bool("output.wrishp_enc")

    @wrishp_enc.setter
    def wrishp_enc(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_enc", value)

    @property
    def wrishp_emb(self) -> bool:
        """Writing embankments to shape file."""
        return self._model.get_bool("output.wrishp_emb")

    @wrishp_emb.setter
    def wrishp_emb(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_emb", value)

    @property
    def wrishp_fxw(self) -> bool:
        """Writing fixed weirs to shape file."""
        return self._model.get_bool("output.wrishp_fxw")

    @wrishp_fxw.setter
    def wrishp_fxw(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_fxw", value)

    @property
    def wrishp_gate(self) -> bool:
        """Writing gates to shape file."""
        return self._model.get_bool("output.wrishp_gate")

    @wrishp_gate.setter
    def wrishp_gate(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_gate", value)

    @property
    def wrishp_genstruc(self) -> bool:
        """Writing general structures to shape file."""
        return self._model.get_bool("output.wrishp_genstruc")

    @wrishp_genstruc.setter
    def wrishp_genstruc(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_genstruc", value)

    @property
    def wrishp_obs(self) -> bool:
        """Writing observation points to shape file."""
        return self._model.get_bool("output.wrishp_obs")

    @wrishp_obs.setter
    def wrishp_obs(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_obs", value)

    @property
    def wrishp_pump(self) -> bool:
        """Writing pumps to shape file."""
        return self._model.get_bool("output.wrishp_pump")

    @wrishp_pump.setter
    def wrishp_pump(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_pump", value)

    @property
    def wrishp_src(self) -> bool:
        """Writing sources and sinks to shape file."""
        return self._model.get_bool("output.wrishp_src")

    @wrishp_src.setter
    def wrishp_src(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_src", value)

    @property
    def wrishp_thd(self) -> bool:
        """Writing thin dams to shape file."""
        return self._model.get_bool("output.wrishp_thd")

    @wrishp_thd.setter
    def wrishp_thd(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_thd", value)

    @property
    def wrishp_weir(self) -> bool:
        """Writing weirs to shape file."""
        return self._model.get_bool("output.wrishp_weir")

    @wrishp_weir.setter
    def wrishp_weir(self, value: bool) -> None:
        self._model.set_bool("output.wrishp_weir", value)

    @property
    def writeSurfaceDataToMapFile(self) -> bool:
        """Only write surface data to map file for 3D quantities."""
        return self._model.get_bool("output.writesurfacedatatomapfile")

    @writeSurfaceDataToMapFile.setter
    def writeSurfaceDataToMapFile(self, value: bool) -> None:
        self._model.set_bool("output.writesurfacedatatomapfile", value)

    @property
    def outputDir(self) -> Path:
        """Output directory of map-, his-, rst-, dat- and timins files, default: DFM_OUTPUT_<modelname>. Set to . for no dir/current dir."""
        return self._model.get_path("output.outputdir")

    @outputDir.setter
    def outputDir(self, value: Path | str) -> None:
        self._model.set_path("output.outputdir", value)

    @property
    def waqOutputDir(self) -> Path:
        """Output directory of Water Quality files."""
        return self._model.get_path("output.waqoutputdir")

    @waqOutputDir.setter
    def waqOutputDir(self, value: Path | str) -> None:
        self._model.set_path("output.waqoutputdir", value)

    @property
    def flowGeomFile(self) -> Path:
        """*_flowgeom.nc Flow geometry file in NetCDF format."""
        return self._model.get_path("output.flowgeomfile")

    @flowGeomFile.setter
    def flowGeomFile(self, value: Path | str) -> None:
        self._model.set_path("output.flowgeomfile", value)

    @property
    def obsFile(self) -> list[Path]:
        """Space separated list of files, containing information about observation points."""
        return self._model.get_path_list("output.obsfile")

    @obsFile.setter
    def obsFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("output.obsfile", value)

    @property
    def deleteObsPointsOutsideGrid(self) -> bool:
        """Delete observation points outside the grid."""
        return self._model.get_bool("output.deleteobspointsoutsidegrid")

    @deleteObsPointsOutsideGrid.setter
    def deleteObsPointsOutsideGrid(self, value: bool) -> None:
        self._model.set_bool("output.deleteobspointsoutsidegrid", value)

    @property
    def crsFile(self) -> list[Path]:
        """Space separated list of files, containing information about observation cross sections."""
        return self._model.get_path_list("output.crsfile")

    @crsFile.setter
    def crsFile(self, value: list[Path | str]) -> None:
        self._model.set_path_list("output.crsfile", value)

    @property
    def fouFile(self) -> Path:
        """Name of attribute file that defines the *_fou.nc Fourier output file in NetCDF format."""
        return self._model.get_path("output.foufile")

    @fouFile.setter
    def fouFile(self, value: Path | str) -> None:
        self._model.set_path("output.foufile", value)

    @property
    def fouUpdateStep(self) -> str:
        """Fourier output type."""
        return self._model.get_enum_name("output.fouupdatestep")

    @fouUpdateStep.setter
    def fouUpdateStep(self, value: str) -> None:
        self._model.set_enum_name("output.fouupdatestep", value)

    @property
    def hisFile(self) -> Path:
        """*_his.nc History file in NetCDF format."""
        return self._model.get_path("output.hisfile")

    @hisFile.setter
    def hisFile(self, value: Path | str) -> None:
        self._model.set_path("output.hisfile", value)

    @property
    def hisInterval(self) -> list[float]:
        """History output, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.hisinterval")

    @hisInterval.setter
    def hisInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.hisinterval", value)

    @property
    def xlsInterval(self) -> float:
        """Interval between XLS history."""
        return self._model.get_double("output.xlsinterval")

    @xlsInterval.setter
    def xlsInterval(self, value: float) -> None:
        self._model.set_double("output.xlsinterval", value)

    @property
    def mapFile(self) -> Path:
        """*_map.nc Map file in NetCDF format."""
        return self._model.get_path("output.mapfile")

    @mapFile.setter
    def mapFile(self, value: Path | str) -> None:
        self._model.set_path("output.mapfile", value)

    @property
    def mapInterval(self) -> list[float]:
        """Map file output, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.mapinterval")

    @mapInterval.setter
    def mapInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.mapinterval", value)

    @property
    def rstInterval(self) -> list[float]:
        """Restart file output, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.rstinterval")

    @rstInterval.setter
    def rstInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.rstinterval", value)

    @property
    def comInterval(self) -> list[float]:
        """Comfile write times, given as 'interval' 'start period' 'end period' w.r.t. `refDate`."""
        return self._model.get_double_list("output.cominterval")

    @comInterval.setter
    def comInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.cominterval", value)

    @property
    def mapFormat(self) -> str:
        """Map file format."""
        return self._model.get_enum_name("output.mapformat")

    @mapFormat.setter
    def mapFormat(self, value: str) -> None:
        self._model.set_enum_name("output.mapformat", value)

    @property
    def ncFormat(self) -> str:
        """Format for all NetCDF output files."""
        return self._model.get_enum_name("output.ncformat")

    @ncFormat.setter
    def ncFormat(self, value: str) -> None:
        self._model.set_enum_name("output.ncformat", value)

    @property
    def ncMapDataPrecision(self) -> str:
        """Precision for NetCDF data in map files (double or single)."""
        return self._model.get_enum_name("output.ncmapdataprecision")

    @ncMapDataPrecision.setter
    def ncMapDataPrecision(self, value: str) -> None:
        self._model.set_enum_name("output.ncmapdataprecision", value)

    @property
    def ncHisDataPrecision(self) -> str:
        """Precision for NetCDF data in his files (double or single)."""
        return self._model.get_enum_name("output.nchisdataprecision")

    @ncHisDataPrecision.setter
    def ncHisDataPrecision(self, value: str) -> None:
        self._model.set_enum_name("output.nchisdataprecision", value)

    @property
    def ncCompression(self) -> bool:
        """Apply compression to NetCDF output files. Only works when `ncFormat`=4."""
        return self._model.get_bool("output.nccompression")

    @ncCompression.setter
    def ncCompression(self, value: bool) -> None:
        self._model.set_bool("output.nccompression", value)

    @property
    def ncNoUnlimited(self) -> bool:
        """Write full-length time-dimension instead of unlimited dimension. Might require `ncFormat`=4."""
        return self._model.get_bool("output.ncnounlimited")

    @ncNoUnlimited.setter
    def ncNoUnlimited(self, value: bool) -> None:
        self._model.set_bool("output.ncnounlimited", value)

    @property
    def ncNoForcedFlush(self) -> bool:
        """Do not force flushing of map-like files every output timestep."""
        return self._model.get_bool("output.ncnoforcedflush")

    @ncNoForcedFlush.setter
    def ncNoForcedFlush(self, value: bool) -> None:
        self._model.set_bool("output.ncnoforcedflush", value)

    @property
    def ncWriteLatLon(self) -> bool:
        """Write extra lat-lon coordinates for all projected coordinate variables in each NetCDF file (for CF-compliancy)."""
        return self._model.get_bool("output.ncwritelatlon")

    @ncWriteLatLon.setter
    def ncWriteLatLon(self, value: bool) -> None:
        self._model.set_bool("output.ncwritelatlon", value)

    @property
    def wriHis_balance(self) -> bool:
        """Write mass balance totals to his file."""
        return self._model.get_bool("output.wrihis_balance")

    @wriHis_balance.setter
    def wriHis_balance(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_balance", value)

    @property
    def wriHis_structure_gen(self) -> bool:
        """Write general structure parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_gen")

    @wriHis_structure_gen.setter
    def wriHis_structure_gen(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_gen", value)

    @property
    def wriHis_structure_dam(self) -> bool:
        """Write dam parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_dam")

    @wriHis_structure_dam.setter
    def wriHis_structure_dam(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_dam", value)

    @property
    def wriHis_structure_pump(self) -> bool:
        """Write pump parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_pump")

    @wriHis_structure_pump.setter
    def wriHis_structure_pump(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_pump", value)

    @property
    def wriHis_structure_gate(self) -> bool:
        """Write gate parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_gate")

    @wriHis_structure_gate.setter
    def wriHis_structure_gate(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_gate", value)

    @property
    def wriHis_structure_weir(self) -> bool:
        """Write weir parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_weir")

    @wriHis_structure_weir.setter
    def wriHis_structure_weir(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_weir", value)

    @property
    def wriHis_structure_orifice(self) -> bool:
        """Write orifice parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_orifice")

    @wriHis_structure_orifice.setter
    def wriHis_structure_orifice(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_orifice", value)

    @property
    def wriHis_structure_bridge(self) -> bool:
        """Write bridge parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_bridge")

    @wriHis_structure_bridge.setter
    def wriHis_structure_bridge(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_bridge", value)

    @property
    def wriHis_structure_culvert(self) -> bool:
        """Write culvert parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_culvert")

    @wriHis_structure_culvert.setter
    def wriHis_structure_culvert(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_culvert", value)

    @property
    def wriHis_structure_longculvert(self) -> bool:
        """Write long culvert parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_longculvert")

    @wriHis_structure_longculvert.setter
    def wriHis_structure_longculvert(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_longculvert", value)

    @property
    def wriHis_structure_damBreak(self) -> bool:
        """Write dam break parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_dambreak")

    @wriHis_structure_damBreak.setter
    def wriHis_structure_damBreak(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_dambreak", value)

    @property
    def wriHis_structure_uniWeir(self) -> bool:
        """Write universal weir parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_uniweir")

    @wriHis_structure_uniWeir.setter
    def wriHis_structure_uniWeir(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_uniweir", value)

    @property
    def wriHis_structure_compound(self) -> bool:
        """Write compound structure parameters to his file."""
        return self._model.get_bool("output.wrihis_structure_compound")

    @wriHis_structure_compound.setter
    def wriHis_structure_compound(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_structure_compound", value)

    @property
    def wriHis_lateral(self) -> bool:
        """Write lateral data."""
        return self._model.get_bool("output.wrihis_lateral")

    @wriHis_lateral.setter
    def wriHis_lateral(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_lateral", value)

    @property
    def wriHis_velocity(self) -> bool:
        """Write velocity magnitude in observation point to his file."""
        return self._model.get_bool("output.wrihis_velocity")

    @wriHis_velocity.setter
    def wriHis_velocity(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_velocity", value)

    @property
    def wriHis_discharge(self) -> bool:
        """Write discharge magnitude in observation point to his file."""
        return self._model.get_bool("output.wrihis_discharge")

    @wriHis_discharge.setter
    def wriHis_discharge(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_discharge", value)

    @property
    def wriHis_sourcesink(self) -> bool:
        """Write sources-sinks data to his file."""
        return self._model.get_bool("output.wrihis_sourcesink")

    @wriHis_sourcesink.setter
    def wriHis_sourcesink(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_sourcesink", value)

    @property
    def wriHis_bubblescreens(self) -> bool:
        """Write bubblescreens data to his file."""
        return self._model.get_bool("output.wrihis_bubblescreens")

    @wriHis_bubblescreens.setter
    def wriHis_bubblescreens(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_bubblescreens", value)

    @property
    def wriHis_turbulence(self) -> bool:
        """Write k, eps and vicww to his file."""
        return self._model.get_bool("output.wrihis_turbulence")

    @wriHis_turbulence.setter
    def wriHis_turbulence(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_turbulence", value)

    @property
    def wriHis_wind(self) -> bool:
        """Write wind velocities to his file."""
        return self._model.get_bool("output.wrihis_wind")

    @wriHis_wind.setter
    def wriHis_wind(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_wind", value)

    @property
    def wriHis_rain(self) -> bool:
        """Write precipitation to his file."""
        return self._model.get_bool("output.wrihis_rain")

    @wriHis_rain.setter
    def wriHis_rain(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_rain", value)

    @property
    def wriHis_airdensity(self) -> bool:
        """Write air density to his file."""
        return self._model.get_bool("output.wrihis_airdensity")

    @wriHis_airdensity.setter
    def wriHis_airdensity(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_airdensity", value)

    @property
    def wriHis_infiltration(self) -> bool:
        """Write infiltration to his file."""
        return self._model.get_bool("output.wrihis_infiltration")

    @wriHis_infiltration.setter
    def wriHis_infiltration(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_infiltration", value)

    @property
    def wriHis_temperature(self) -> bool:
        """Write temperature to his file."""
        return self._model.get_bool("output.wrihis_temperature")

    @wriHis_temperature.setter
    def wriHis_temperature(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_temperature", value)

    @property
    def wriHis_waves(self) -> bool:
        """Write wave data to his file."""
        return self._model.get_bool("output.wrihis_waves")

    @wriHis_waves.setter
    def wriHis_waves(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_waves", value)

    @property
    def wriHis_heat_fluxes(self) -> bool:
        """Write heat fluxes to his file."""
        return self._model.get_bool("output.wrihis_heat_fluxes")

    @wriHis_heat_fluxes.setter
    def wriHis_heat_fluxes(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_heat_fluxes", value)

    @property
    def wriHis_salinity(self) -> bool:
        """Write salinity to his file."""
        return self._model.get_bool("output.wrihis_salinity")

    @wriHis_salinity.setter
    def wriHis_salinity(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_salinity", value)

    @property
    def wriHis_density(self) -> bool:
        """Write density to his file."""
        return self._model.get_bool("output.wrihis_density")

    @wriHis_density.setter
    def wriHis_density(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_density", value)

    @property
    def wriHis_waterlevel_s1(self) -> bool:
        """Write water level to his file."""
        return self._model.get_bool("output.wrihis_waterlevel_s1")

    @wriHis_waterlevel_s1.setter
    def wriHis_waterlevel_s1(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_waterlevel_s1", value)

    @property
    def wriHis_bedlevel(self) -> bool:
        """Write bed level to his file."""
        return self._model.get_bool("output.wrihis_bedlevel")

    @wriHis_bedlevel.setter
    def wriHis_bedlevel(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_bedlevel", value)

    @property
    def wriHis_waterdepth(self) -> bool:
        """Write water depth to his file."""
        return self._model.get_bool("output.wrihis_waterdepth")

    @wriHis_waterdepth.setter
    def wriHis_waterdepth(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_waterdepth", value)

    @property
    def wriHis_velocity_vector(self) -> bool:
        """Write velocity vectors to his file."""
        return self._model.get_bool("output.wrihis_velocity_vector")

    @wriHis_velocity_vector.setter
    def wriHis_velocity_vector(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_velocity_vector", value)

    @property
    def wriHis_upward_velocity_component(self) -> bool:
        """Write upward velocity to his file."""
        return self._model.get_bool("output.wrihis_upward_velocity_component")

    @wriHis_upward_velocity_component.setter
    def wriHis_upward_velocity_component(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_upward_velocity_component", value)

    @property
    def wriHis_sediment(self) -> bool:
        """Write sediment transport to his file."""
        return self._model.get_bool("output.wrihis_sediment")

    @wriHis_sediment.setter
    def wriHis_sediment(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_sediment", value)

    @property
    def wriHis_constituents(self) -> bool:
        """Write tracers to his file."""
        return self._model.get_bool("output.wrihis_constituents")

    @wriHis_constituents.setter
    def wriHis_constituents(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_constituents", value)

    @property
    def wriHis_zcor(self) -> bool:
        """Write vertical coordinates to his file."""
        return self._model.get_bool("output.wrihis_zcor")

    @wriHis_zcor.setter
    def wriHis_zcor(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_zcor", value)

    @property
    def wriHis_taucurrent(self) -> bool:
        """Write mean bed shear stress to his file."""
        return self._model.get_bool("output.wrihis_taucurrent")

    @wriHis_taucurrent.setter
    def wriHis_taucurrent(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_taucurrent", value)

    @property
    def wriHis_wqBot(self) -> bool:
        """Write water quality bottom variables to his file."""
        return self._model.get_bool("output.wrihis_wqbot")

    @wriHis_wqBot.setter
    def wriHis_wqBot(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_wqbot", value)

    @property
    def wriHis_wqBot3d(self) -> bool:
        """Write 3D water quality bottom variables to his file."""
        return self._model.get_bool("output.wrihis_wqbot3d")

    @wriHis_wqBot3d.setter
    def wriHis_wqBot3d(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_wqbot3d", value)

    @property
    def wriMap_waterlevel_s0(self) -> bool:
        """Write water levels at old time level to map file."""
        return self._model.get_bool("output.wrimap_waterlevel_s0")

    @wriMap_waterlevel_s0.setter
    def wriMap_waterlevel_s0(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waterlevel_s0", value)

    @property
    def wriMap_waterlevel_s1(self) -> bool:
        """Write water levels at new time level to map file."""
        return self._model.get_bool("output.wrimap_waterlevel_s1")

    @wriMap_waterlevel_s1.setter
    def wriMap_waterlevel_s1(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waterlevel_s1", value)

    @property
    def wriMap_evaporation(self) -> bool:
        """Write evaporation to map file."""
        return self._model.get_bool("output.wrimap_evaporation")

    @wriMap_evaporation.setter
    def wriMap_evaporation(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_evaporation", value)

    @property
    def wriMap_velocity_component_u0(self) -> bool:
        """Write velocities at old time level to map file."""
        return self._model.get_bool("output.wrimap_velocity_component_u0")

    @wriMap_velocity_component_u0.setter
    def wriMap_velocity_component_u0(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_velocity_component_u0", value)

    @property
    def wriMap_velocity_component_u1(self) -> bool:
        """Write velocities at new time level to map file."""
        return self._model.get_bool("output.wrimap_velocity_component_u1")

    @wriMap_velocity_component_u1.setter
    def wriMap_velocity_component_u1(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_velocity_component_u1", value)

    @property
    def wriMap_velocity_vector(self) -> bool:
        """Write cell-center velocity vectors to map file."""
        return self._model.get_bool("output.wrimap_velocity_vector")

    @wriMap_velocity_vector.setter
    def wriMap_velocity_vector(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_velocity_vector", value)

    @property
    def wriMap_upward_velocity_component(self) -> bool:
        """Write upward velocity component to map file."""
        return self._model.get_bool("output.wrimap_upward_velocity_component")

    @wriMap_upward_velocity_component.setter
    def wriMap_upward_velocity_component(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_upward_velocity_component", value)

    @property
    def wriMap_density_rho(self) -> bool:
        """Write density to map file."""
        return self._model.get_bool("output.wrimap_density_rho")

    @wriMap_density_rho.setter
    def wriMap_density_rho(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_density_rho", value)

    @property
    def wriMap_horizontal_viscosity_viu(self) -> bool:
        """Write horizontal viscosity to map file."""
        return self._model.get_bool("output.wrimap_horizontal_viscosity_viu")

    @wriMap_horizontal_viscosity_viu.setter
    def wriMap_horizontal_viscosity_viu(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_horizontal_viscosity_viu", value)

    @property
    def wriMap_horizontal_diffusivity_diu(self) -> bool:
        """Write horizontal diffusivity to map file."""
        return self._model.get_bool("output.wrimap_horizontal_diffusivity_diu")

    @wriMap_horizontal_diffusivity_diu.setter
    def wriMap_horizontal_diffusivity_diu(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_horizontal_diffusivity_diu", value)

    @property
    def wriMap_flow_flux_q1(self) -> bool:
        """Write fluxes to map file."""
        return self._model.get_bool("output.wrimap_flow_flux_q1")

    @wriMap_flow_flux_q1.setter
    def wriMap_flow_flux_q1(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_flow_flux_q1", value)

    @property
    def wriMap_spiral_flow(self) -> bool:
        """Write spiral flow to map file."""
        return self._model.get_bool("output.wrimap_spiral_flow")

    @wriMap_spiral_flow.setter
    def wriMap_spiral_flow(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_spiral_flow", value)

    @property
    def wriMap_numlimdt(self) -> bool:
        """Write numlimdt to map file."""
        return self._model.get_bool("output.wrimap_numlimdt")

    @wriMap_numlimdt.setter
    def wriMap_numlimdt(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_numlimdt", value)

    @property
    def wriXyz_numlimdt(self) -> bool:
        """Write numlimdt to xyz file This option is useful when a map file is not written."""
        return self._model.get_bool("output.wrixyz_numlimdt")

    @wriXyz_numlimdt.setter
    def wriXyz_numlimdt(self, value: bool) -> None:
        self._model.set_bool("output.wrixyz_numlimdt", value)

    @property
    def wriMap_taucurrent(self) -> bool:
        """Write bottom friction to map file."""
        return self._model.get_bool("output.wrimap_taucurrent")

    @wriMap_taucurrent.setter
    def wriMap_taucurrent(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_taucurrent", value)

    @property
    def wriMap_chezy(self) -> bool:
        """Write chezy roughness in flow elements to map file."""
        return self._model.get_bool("output.wrimap_chezy")

    @wriMap_chezy.setter
    def wriMap_chezy(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_chezy", value)

    @property
    def wriMap_chezy_on_flow_links(self) -> bool:
        """Write chezy roughness on flow links to map file."""
        return self._model.get_bool("output.wrimap_chezy_on_flow_links")

    @wriMap_chezy_on_flow_links.setter
    def wriMap_chezy_on_flow_links(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_chezy_on_flow_links", value)

    @property
    def writePart_domain(self) -> bool:
        """Write interpreted network file (DFM_interpreted_idomain_*.nc) containing domain partition info."""
        return self._model.get_bool("output.writepart_domain")

    @writePart_domain.setter
    def writePart_domain(self, value: bool) -> None:
        self._model.set_bool("output.writepart_domain", value)

    @property
    def velocityDirectionClassesInterval(self) -> float:
        """Class map's step size of class values for velocity direction."""
        return self._model.get_double("output.velocitydirectionclassesinterval")

    @velocityDirectionClassesInterval.setter
    def velocityDirectionClassesInterval(self, value: float) -> None:
        self._model.set_double("output.velocitydirectionclassesinterval", value)

    @property
    def velocityMagnitudeClasses(self) -> list[float]:
        """Class map's list of class values for velocity magnitudes."""
        return self._model.get_double_list("output.velocitymagnitudeclasses")

    @velocityMagnitudeClasses.setter
    def velocityMagnitudeClasses(self, value: list[float]) -> None:
        self._model.set_double_list("output.velocitymagnitudeclasses", value)

    @property
    def wriMap_input_roughness(self) -> bool:
        """Write chezy input roughness on flow links to map file."""
        return self._model.get_bool("output.wrimap_input_roughness")

    @wriMap_input_roughness.setter
    def wriMap_input_roughness(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_input_roughness", value)

    @property
    def wriMap_turbulence(self) -> bool:
        """Write turbulence to map file."""
        return self._model.get_bool("output.wrimap_turbulence")

    @wriMap_turbulence.setter
    def wriMap_turbulence(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_turbulence", value)

    @property
    def wriMap_rain(self) -> bool:
        """Write rainfall rate to map file."""
        return self._model.get_bool("output.wrimap_rain")

    @wriMap_rain.setter
    def wriMap_rain(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_rain", value)

    @property
    def wriMap_wind(self) -> bool:
        """Write winds to map file."""
        return self._model.get_bool("output.wrimap_wind")

    @wriMap_wind.setter
    def wriMap_wind(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_wind", value)

    @property
    def wriMap_airdensity(self) -> bool:
        """Write air density to map file."""
        return self._model.get_bool("output.wrimap_airdensity")

    @wriMap_airdensity.setter
    def wriMap_airdensity(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_airdensity", value)

    @property
    def wriMap_calibration(self) -> bool:
        """Write roughness calibration factors to map file."""
        return self._model.get_bool("output.wrimap_calibration")

    @wriMap_calibration.setter
    def wriMap_calibration(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_calibration", value)

    @property
    def wriMap_salinity(self) -> bool:
        """Write salinity to map file."""
        return self._model.get_bool("output.wrimap_salinity")

    @wriMap_salinity.setter
    def wriMap_salinity(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_salinity", value)

    @property
    def wriMap_temperature(self) -> bool:
        """Write temperature to map file."""
        return self._model.get_bool("output.wrimap_temperature")

    @wriMap_temperature.setter
    def wriMap_temperature(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_temperature", value)

    @property
    def wriMap_constituents(self) -> bool:
        """Write tracers and others constituents to map file."""
        return self._model.get_bool("output.wrimap_constituents")

    @wriMap_constituents.setter
    def wriMap_constituents(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_constituents", value)

    @property
    def wriMap_sediment(self) -> bool:
        """Write sediment transport to map file."""
        return self._model.get_bool("output.wrimap_sediment")

    @wriMap_sediment.setter
    def wriMap_sediment(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_sediment", value)

    @property
    def wriMap_waves(self) -> bool:
        """Write wave variables to map file."""
        return self._model.get_bool("output.wrimap_waves")

    @wriMap_waves.setter
    def wriMap_waves(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waves", value)

    @property
    def wriMap_z0(self) -> bool:
        """Write current-related roughness height to map file."""
        return self._model.get_bool("output.wrimap_z0")

    @wriMap_z0.setter
    def wriMap_z0(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_z0", value)

    @property
    def wriMap_trachytopes(self) -> bool:
        """Write roughness from trachytopes to map file."""
        return self._model.get_bool("output.wrimap_trachytopes")

    @wriMap_trachytopes.setter
    def wriMap_trachytopes(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_trachytopes", value)

    @property
    def wriMap_nudging(self) -> bool:
        """Write nudging to map file."""
        return self._model.get_bool("output.wrimap_nudging")

    @wriMap_nudging.setter
    def wriMap_nudging(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_nudging", value)

    @property
    def wriTek_cdWind(self) -> bool:
        """Write wind friction coefficients to tek file."""
        return self._model.get_bool("output.writek_cdwind")

    @wriTek_cdWind.setter
    def wriTek_cdWind(self, value: bool) -> None:
        self._model.set_bool("output.writek_cdwind", value)

    @property
    def wriMap_heat_fluxes(self) -> bool:
        """Write heat fluxes to map file."""
        return self._model.get_bool("output.wrimap_heat_fluxes")

    @wriMap_heat_fluxes.setter
    def wriMap_heat_fluxes(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_heat_fluxes", value)

    @property
    def wriMap_fixed_weir_energy_loss(self) -> bool:
        """Write energy losses of fixed weirs to map file. `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`."""
        return self._model.get_bool("output.wrimap_fixed_weir_energy_loss")

    @wriMap_fixed_weir_energy_loss.setter
    def wriMap_fixed_weir_energy_loss(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_fixed_weir_energy_loss", value)

    @property
    def wriMap_wet_waterdepth_threshold(self) -> float:
        """Waterdepth threshold above which a grid point counts as 'wet'. Defaults to 0.2*`epshu`. It is used for `wriMap_time_water_on_ground`, `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`."""
        return self._model.get_double("output.wrimap_wet_waterdepth_threshold")

    @wriMap_wet_waterdepth_threshold.setter
    def wriMap_wet_waterdepth_threshold(self, value: float) -> None:
        self._model.set_double("output.wrimap_wet_waterdepth_threshold", value)

    @property
    def wriMap_time_water_on_ground(self) -> bool:
        """Write cumulative time when water is above ground level (only for 1D nodes) to map file."""
        return self._model.get_bool("output.wrimap_time_water_on_ground")

    @wriMap_time_water_on_ground.setter
    def wriMap_time_water_on_ground(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_time_water_on_ground", value)

    @property
    def wriMap_freeboard(self) -> bool:
        """Write freeboard (only for 1D nodes) to map file."""
        return self._model.get_bool("output.wrimap_freeboard")

    @wriMap_freeboard.setter
    def wriMap_freeboard(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_freeboard", value)

    @property
    def wriMap_waterdepth_on_ground(self) -> bool:
        """Write waterdepth that is above ground level to map file (only for 1D nodes)."""
        return self._model.get_bool("output.wrimap_waterdepth_on_ground")

    @wriMap_waterdepth_on_ground.setter
    def wriMap_waterdepth_on_ground(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waterdepth_on_ground", value)

    @property
    def wriMap_volume_on_ground(self) -> bool:
        """Write volume that is above ground level to map file (only for 1D nodes)."""
        return self._model.get_bool("output.wrimap_volume_on_ground")

    @wriMap_volume_on_ground.setter
    def wriMap_volume_on_ground(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_volume_on_ground", value)

    @property
    def wriMap_total_net_inflow_1d2d(self) -> bool:
        """Write current total 1D2D net inflow (discharge) and cumulative total 1D2D net inflow (volume) to map file (only for 1D nodes)."""
        return self._model.get_bool("output.wrimap_total_net_inflow_1d2d")

    @wriMap_total_net_inflow_1d2d.setter
    def wriMap_total_net_inflow_1d2d(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_total_net_inflow_1d2d", value)

    @property
    def wriMap_total_net_inflow_lateral(self) -> bool:
        """Write current total lateral net inflow (discharge) and cumulative total lateral net inflow (volume) to map file (only for 1D nodes)."""
        return self._model.get_bool("output.wrimap_total_net_inflow_lateral")

    @wriMap_total_net_inflow_lateral.setter
    def wriMap_total_net_inflow_lateral(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_total_net_inflow_lateral", value)

    @property
    def wriMap_water_level_gradient(self) -> bool:
        """Write water level gradient to map file (only for 1D links)."""
        return self._model.get_bool("output.wrimap_water_level_gradient")

    @wriMap_water_level_gradient.setter
    def wriMap_water_level_gradient(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_water_level_gradient", value)

    @property
    def wriMap_tidal_potential(self) -> bool:
        """Write tidal potential to map file."""
        return self._model.get_bool("output.wrimap_tidal_potential")

    @wriMap_tidal_potential.setter
    def wriMap_tidal_potential(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_tidal_potential", value)

    @property
    def wriMap_sal_potential(self) -> bool:
        """Write self attraction and loading potential to map file."""
        return self._model.get_bool("output.wrimap_sal_potential")

    @wriMap_sal_potential.setter
    def wriMap_sal_potential(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_sal_potential", value)

    @property
    def wriMap_internal_tides_dissipation(self) -> bool:
        """Write internal tides dissipation to map file."""
        return self._model.get_bool("output.wrimap_internal_tides_dissipation")

    @wriMap_internal_tides_dissipation.setter
    def wriMap_internal_tides_dissipation(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_internal_tides_dissipation", value)

    @property
    def wriMap_flow_analysis(self) -> bool:
        """Write flow analysis data to the map file."""
        return self._model.get_bool("output.wrimap_flow_analysis")

    @wriMap_flow_analysis.setter
    def wriMap_flow_analysis(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_flow_analysis", value)

    @property
    def wriMap_volume1(self) -> bool:
        """Write volumes to map file."""
        return self._model.get_bool("output.wrimap_volume1")

    @wriMap_volume1.setter
    def wriMap_volume1(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_volume1", value)

    @property
    def wriMap_waterdepth(self) -> bool:
        """Write water depths to map file."""
        return self._model.get_bool("output.wrimap_waterdepth")

    @wriMap_waterdepth.setter
    def wriMap_waterdepth(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waterdepth", value)

    @property
    def wriMap_waterdepth_hu(self) -> bool:
        """Write water depths on u-points to map file."""
        return self._model.get_bool("output.wrimap_waterdepth_hu")

    @wriMap_waterdepth_hu.setter
    def wriMap_waterdepth_hu(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_waterdepth_hu", value)

    @property
    def wriMap_ancillary_variables(self) -> bool:
        """Write ancillary variables attributes to map file."""
        return self._model.get_bool("output.wrimap_ancillary_variables")

    @wriMap_ancillary_variables.setter
    def wriMap_ancillary_variables(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_ancillary_variables", value)

    @property
    def wriMap_flowarea_au(self) -> bool:
        """Write low areas au to map file."""
        return self._model.get_bool("output.wrimap_flowarea_au")

    @wriMap_flowarea_au.setter
    def wriMap_flowarea_au(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_flowarea_au", value)

    @property
    def wriMap_velocity_magnitude(self) -> bool:
        """Write cell-center velocity vector magnitude to map file."""
        return self._model.get_bool("output.wrimap_velocity_magnitude")

    @wriMap_velocity_magnitude.setter
    def wriMap_velocity_magnitude(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_velocity_magnitude", value)

    @property
    def wriMap_velocity_vectorq(self) -> bool:
        """Write cell-center velocity vectors (discharge-based) to map file."""
        return self._model.get_bool("output.wrimap_velocity_vectorq")

    @wriMap_velocity_vectorq.setter
    def wriMap_velocity_vectorq(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_velocity_vectorq", value)

    @property
    def wriMap_flow_flux_q1_main(self) -> bool:
        """Write flow flux in main channel to map file."""
        return self._model.get_bool("output.wrimap_flow_flux_q1_main")

    @wriMap_flow_flux_q1_main.setter
    def wriMap_flow_flux_q1_main(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_flow_flux_q1_main", value)

    @property
    def wriMap_interception(self) -> bool:
        """Write interception to map file."""
        return self._model.get_bool("output.wrimap_interception")

    @wriMap_interception.setter
    def wriMap_interception(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_interception", value)

    @property
    def wriMap_windstress(self) -> bool:
        """Write wind stress to map file."""
        return self._model.get_bool("output.wrimap_windstress")

    @wriMap_windstress.setter
    def wriMap_windstress(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_windstress", value)

    @property
    def wriMap_cdWind(self) -> bool:
        """Write wind friction coeffs to map file."""
        return self._model.get_bool("output.wrimap_cdwind")

    @wriMap_cdWind.setter
    def wriMap_cdWind(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_cdwind", value)

    @property
    def wriMap_bnd(self) -> bool:
        """Write boundary points to map file."""
        return self._model.get_bool("output.wrimap_bnd")

    @wriMap_bnd.setter
    def wriMap_bnd(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_bnd", value)

    @property
    def wriMap_Qin(self) -> bool:
        """Write sum of all influxes to map file."""
        return self._model.get_bool("output.wrimap_qin")

    @wriMap_Qin.setter
    def wriMap_Qin(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_qin", value)

    @property
    def wriMap_dtCell(self) -> bool:
        """Write time step per cell based on CFL."""
        return self._model.get_bool("output.wrimap_dtcell")

    @wriMap_dtCell.setter
    def wriMap_dtCell(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_dtcell", value)

    @property
    def wriMap_wqBot3d(self) -> bool:
        """Write 3D water quality bottom variables to map file."""
        return self._model.get_bool("output.wrimap_wqbot3d")

    @wriMap_wqBot3d.setter
    def wriMap_wqBot3d(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_wqbot3d", value)

    @property
    def wriMap_every_dt(self) -> bool:
        """Write output to map file every computational timestep, between start and stop time from `mapInterval`."""
        return self._model.get_bool("output.wrimap_every_dt")

    @wriMap_every_dt.setter
    def wriMap_every_dt(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_every_dt", value)

    @property
    def mapOutputTimeVector(self) -> Path:
        """File (.mpt) containing fixed map output times (s) w.r.t. `refDate`."""
        return self._model.get_path("output.mapoutputtimevector")

    @mapOutputTimeVector.setter
    def mapOutputTimeVector(self, value: Path | str) -> None:
        self._model.set_path("output.mapoutputtimevector", value)

    @property
    def comOutputTimeVector(self) -> Path:
        """File (.ctv) containing fixed comfile write times (s) w.r.t. `refDate`."""
        return self._model.get_path("output.comoutputtimevector")

    @comOutputTimeVector.setter
    def comOutputTimeVector(self, value: Path | str) -> None:
        self._model.set_path("output.comoutputtimevector", value)

    @property
    def fullGridOutput(self) -> str:
        """Full grid output mode for layer positions."""
        return self._model.get_enum_name("output.fullgridoutput")

    @fullGridOutput.setter
    def fullGridOutput(self, value: str) -> None:
        self._model.set_enum_name("output.fullgridoutput", value)

    @property
    def eulerVelocities(self) -> bool:
        """Write Eulerian velocities."""
        return self._model.get_bool("output.eulervelocities")

    @eulerVelocities.setter
    def eulerVelocities(self, value: bool) -> None:
        self._model.set_bool("output.eulervelocities", value)

    @property
    def avgWaveOutputInterval(self) -> list[float]:
        """Average wave output, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.avgwaveoutputinterval")

    @avgWaveOutputInterval.setter
    def avgWaveOutputInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.avgwaveoutputinterval", value)

    @property
    def classMapFile(self) -> Path:
        """Name of class map file."""
        return self._model.get_path("output.classmapfile")

    @classMapFile.setter
    def classMapFile(self, value: Path | str) -> None:
        self._model.set_path("output.classmapfile", value)

    @property
    def waterLevelClasses(self) -> list[float]:
        """Series of values between which water level classes are computed."""
        return self._model.get_double_list("output.waterlevelclasses")

    @waterLevelClasses.setter
    def waterLevelClasses(self, value: list[float]) -> None:
        self._model.set_double_list("output.waterlevelclasses", value)

    @property
    def waterDepthClasses(self) -> list[float]:
        """Series of values between which water depth classes are computed."""
        return self._model.get_double_list("output.waterdepthclasses")

    @waterDepthClasses.setter
    def waterDepthClasses(self, value: list[float]) -> None:
        self._model.set_double_list("output.waterdepthclasses", value)

    @property
    def classMapInterval(self) -> list[float]:
        """Interval between class map file outputs, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.classmapinterval")

    @classMapInterval.setter
    def classMapInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.classmapinterval", value)

    @property
    def waqInterval(self) -> list[float]:
        """Interval between DELWAQ file outputs, given as 'interval' 'start period' 'end period'."""
        return self._model.get_double_list("output.waqinterval")

    @waqInterval.setter
    def waqInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.waqinterval", value)

    @property
    def statsInterval(self) -> float:
        """Interval between screen step outputs in seconds simulation time, if negative in seconds wall clock time."""
        return self._model.get_double("output.statsinterval")

    @statsInterval.setter
    def statsInterval(self, value: float) -> None:
        self._model.set_double("output.statsinterval", value)

    @property
    def timingsInterval(self) -> list[float]:
        """Timings output interval."""
        return self._model.get_double_list("output.timingsinterval")

    @timingsInterval.setter
    def timingsInterval(self, value: list[float]) -> None:
        self._model.set_double_list("output.timingsinterval", value)

    @property
    def richardsonOnOutput(self) -> bool:
        """Write Richardson number."""
        return self._model.get_bool("output.richardsononoutput")

    @richardsonOnOutput.setter
    def richardsonOnOutput(self, value: bool) -> None:
        self._model.set_bool("output.richardsononoutput", value)

    @property
    def mbaLumpSourceSinks(self) -> bool:
        """Lump MBA source/sink mass balance terms."""
        return self._model.get_bool("output.mbalumpsourcesinks")

    @mbaLumpSourceSinks.setter
    def mbaLumpSourceSinks(self, value: bool) -> None:
        self._model.set_bool("output.mbalumpsourcesinks", value)

    @property
    def wrimap_nearfield(self) -> bool:
        """Write NearField parameters."""
        return self._model.get_bool("output.wrimap_nearfield")

    @wrimap_nearfield.setter
    def wrimap_nearfield(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_nearfield", value)

    @property
    def writeDfmInterpretedValues(self) -> bool:
        """Write DFM interpreted values."""
        return self._model.get_bool("output.writedfminterpretedvalues")

    @writeDfmInterpretedValues.setter
    def writeDfmInterpretedValues(self, value: bool) -> None:
        self._model.set_bool("output.writedfminterpretedvalues", value)

    @property
    def mbaLumpBoundaries(self) -> bool:
        """Lump MBA boundary mass balance terms."""
        return self._model.get_bool("output.mbalumpboundaries")

    @mbaLumpBoundaries.setter
    def mbaLumpBoundaries(self, value: bool) -> None:
        self._model.set_bool("output.mbalumpboundaries", value)

    @property
    def waqHorAggr(self) -> Path:
        """DELWAQ output horizontal aggregation file (*.dwq)."""
        return self._model.get_path("output.waqhoraggr")

    @waqHorAggr.setter
    def waqHorAggr(self, value: Path | str) -> None:
        self._model.set_path("output.waqhoraggr", value)

    @property
    def writeDetailedTimers(self) -> bool:
        """Write detailed timers output file."""
        return self._model.get_bool("output.writedetailedtimers")

    @writeDetailedTimers.setter
    def writeDetailedTimers(self, value: bool) -> None:
        self._model.set_bool("output.writedetailedtimers", value)

    @property
    def metadataFile(self) -> Path:
        """Metadata NetCDF file with user-defined global dataset attributes (*_meta.nc)."""
        return self._model.get_path("output.metadatafile")

    @metadataFile.setter
    def metadataFile(self, value: Path | str) -> None:
        self._model.set_path("output.metadatafile", value)

    @property
    def mbaInterval(self) -> float:
        """Mass balance area output interval."""
        return self._model.get_double("output.mbainterval")

    @mbaInterval.setter
    def mbaInterval(self, value: float) -> None:
        self._model.set_double("output.mbainterval", value)

    @property
    def wrirst_bnd(self) -> bool:
        """Write water level."""
        return self._model.get_bool("output.wrirst_bnd")

    @wrirst_bnd.setter
    def wrirst_bnd(self, value: bool) -> None:
        self._model.set_bool("output.wrirst_bnd", value)

    @property
    def generateUuid(self) -> bool:
        """Generate UUID as unique dataset identifier and include in output NetCDF files."""
        return self._model.get_bool("output.generateuuid")

    @generateUuid.setter
    def generateUuid(self, value: bool) -> None:
        self._model.set_bool("output.generateuuid", value)

    @property
    def timeSplitInterval(self) -> str:
        """Time splitting interval after which a new output file is started. Format: value+unit, e.g. '1M'."""
        return self._model.get_string("output.timesplitinterval")

    @timeSplitInterval.setter
    def timeSplitInterval(self, value: str) -> None:
        self._model.set_string("output.timesplitinterval", value)

    @property
    def rugFile(self) -> Path:
        """Polyline file *_rug.pli defining runup gauges."""
        return self._model.get_path("output.rugfile")

    @rugFile.setter
    def rugFile(self, value: Path | str) -> None:
        self._model.set_path("output.rugfile", value)

    @property
    def mbaWriteCsv(self) -> bool:
        """Write mass balance area output to a CSV file."""
        return self._model.get_bool("output.mbawritecsv")

    @mbaWriteCsv.setter
    def mbaWriteCsv(self, value: bool) -> None:
        self._model.set_bool("output.mbawritecsv", value)

    @property
    def mbaLumpFromToMba(self) -> bool:
        """Lump MBA from/to other areas mass balance terms."""
        return self._model.get_bool("output.mbalumpfromtomba")

    @mbaLumpFromToMba.setter
    def mbaLumpFromToMba(self, value: bool) -> None:
        self._model.set_bool("output.mbalumpfromtomba", value)

    @property
    def mbaLumpProcesses(self) -> bool:
        """Lump MBA processes mass balance terms."""
        return self._model.get_bool("output.mbalumpprocesses")

    @mbaLumpProcesses.setter
    def mbaLumpProcesses(self, value: bool) -> None:
        self._model.set_bool("output.mbalumpprocesses", value)

    @property
    def waqVertAggr(self) -> Path:
        """DELWAQ output vertical aggregation file (*.vag)."""
        return self._model.get_path("output.waqvertaggr")

    @waqVertAggr.setter
    def waqVertAggr(self, value: Path | str) -> None:
        self._model.set_path("output.waqvertaggr", value)

    @property
    def mbaWriteNetcdf(self) -> bool:
        """Write mass balance area output to a NetCDF file."""
        return self._model.get_bool("output.mbawritenetcdf")

    @mbaWriteNetcdf.setter
    def mbaWriteNetcdf(self, value: bool) -> None:
        self._model.set_bool("output.mbawritenetcdf", value)

    @property
    def mbaWriteTxt(self) -> bool:
        """Write mass balance area output to a TXT file."""
        return self._model.get_bool("output.mbawritetxt")

    @mbaWriteTxt.setter
    def mbaWriteTxt(self, value: bool) -> None:
        self._model.set_bool("output.mbawritetxt", value)

    @property
    def wrimap_ice(self) -> bool:
        """Write output to map file for ice cover."""
        return self._model.get_bool("output.wrimap_ice")

    @wrimap_ice.setter
    def wrimap_ice(self, value: bool) -> None:
        self._model.set_bool("output.wrimap_ice", value)

    @property
    def writeBalanceFile(self) -> str:
        """"""
        return self._model.get_string("output.writebalancefile")

    @writeBalanceFile.setter
    def writeBalanceFile(self, value: str) -> None:
        self._model.set_string("output.writebalancefile", value)

    @property
    def s1incInterval(self) -> str:
        """"""
        return self._model.get_string("output.s1incinterval")

    @s1incInterval.setter
    def s1incInterval(self, value: str) -> None:
        self._model.set_string("output.s1incinterval", value)

    @property
    def waqFileBase(self) -> str:
        """"""
        return self._model.get_string("output.waqfilebase")

    @waqFileBase.setter
    def waqFileBase(self, value: str) -> None:
        self._model.set_string("output.waqfilebase", value)

    @property
    def snapshotdir(self) -> str:
        """"""
        return self._model.get_string("output.snapshotdir")

    @snapshotdir.setter
    def snapshotdir(self, value: str) -> None:
        self._model.set_string("output.snapshotdir", value)

    @property
    def heatFluxesOnOutput(self) -> str:
        """"""
        return self._model.get_string("output.heatfluxesonoutput")

    @heatFluxesOnOutput.setter
    def heatFluxesOnOutput(self, value: str) -> None:
        self._model.set_string("output.heatfluxesonoutput", value)

    @property
    def wrimap_input_dt(self) -> str:
        """"""
        return self._model.get_string("output.wrimap_input_dt")

    @wrimap_input_dt.setter
    def wrimap_input_dt(self, value: str) -> None:
        self._model.set_string("output.wrimap_input_dt", value)

    @property
    def wrihis_heatflux(self) -> str:
        """"""
        return self._model.get_string("output.wrihis_heatflux")

    @wrihis_heatflux.setter
    def wrihis_heatflux(self, value: str) -> None:
        self._model.set_string("output.wrihis_heatflux", value)

    @property
    def enableDebugArrays(self) -> bool:
        """Enable debug arrays in output."""
        return self._model.get_bool("output.enabledebugarrays")

    @enableDebugArrays.setter
    def enableDebugArrays(self, value: bool) -> None:
        self._model.set_bool("output.enabledebugarrays", value)

    @property
    def wriHis_crs_flow(self) -> bool:
        """Write cross-section flow output to history file."""
        return self._model.get_bool("output.wrihis_crs_flow")

    @wriHis_crs_flow.setter
    def wriHis_crs_flow(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_crs_flow", value)

    @property
    def wriHis_crs_constituents(self) -> bool:
        """Write cross-section constituents output to history file."""
        return self._model.get_bool("output.wrihis_crs_constituents")

    @wriHis_crs_constituents.setter
    def wriHis_crs_constituents(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_crs_constituents", value)

    @property
    def wriHis_water_quality_output(self) -> bool:
        """Write water quality output to history file."""
        return self._model.get_bool("output.wrihis_water_quality_output")

    @wriHis_water_quality_output.setter
    def wriHis_water_quality_output(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_water_quality_output", value)

    @property
    def wriHis_runupgauge(self) -> bool:
        """Write runup gauge output to history file."""
        return self._model.get_bool("output.wrihis_runupgauge")

    @wriHis_runupgauge.setter
    def wriHis_runupgauge(self, value: bool) -> None:
        self._model.set_bool("output.wrihis_runupgauge", value)


class CalibrationSection:
    """Typed access to the [calibration] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def useCalibration(self) -> bool:
        """Activate calibration factor friction multiplier."""
        return self._model.get_bool("calibration.usecalibration")

    @useCalibration.setter
    def useCalibration(self, value: bool) -> None:
        self._model.set_bool("calibration.usecalibration", value)

    @property
    def definitionFile(self) -> Path:
        """File (*.cld) containing calibration definitions."""
        return self._model.get_path("calibration.definitionfile")

    @definitionFile.setter
    def definitionFile(self, value: Path | str) -> None:
        self._model.set_path("calibration.definitionfile", value)

    @property
    def areaFile(self) -> Path:
        """File (*.cll) containing area distribution of calibration definitions."""
        return self._model.get_path("calibration.areafile")

    @areaFile.setter
    def areaFile(self, value: Path | str) -> None:
        self._model.set_path("calibration.areafile", value)


class ProcessesSection:
    """Typed access to the [processes] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def substanceFile(self) -> Path:
        """Substance file name."""
        return self._model.get_path("processes.substancefile")

    @substanceFile.setter
    def substanceFile(self, value: Path | str) -> None:
        self._model.set_path("processes.substancefile", value)

    @property
    def substanceDensityCoupling(self) -> bool:
        """Substance rho coupling."""
        return self._model.get_bool("processes.substancedensitycoupling")

    @substanceDensityCoupling.setter
    def substanceDensityCoupling(self, value: bool) -> None:
        self._model.set_bool("processes.substancedensitycoupling", value)

    @property
    def additionalHistoryOutputFile(self) -> Path:
        """Extra history output filename."""
        return self._model.get_path("processes.additionalhistoryoutputfile")

    @additionalHistoryOutputFile.setter
    def additionalHistoryOutputFile(self, value: Path | str) -> None:
        self._model.set_path("processes.additionalhistoryoutputfile", value)

    @property
    def statisticsFile(self) -> Path:
        """Statistics definition file."""
        return self._model.get_path("processes.statisticsfile")

    @statisticsFile.setter
    def statisticsFile(self, value: Path | str) -> None:
        self._model.set_path("processes.statisticsfile", value)

    @property
    def thetaVertical(self) -> float:
        """Theta value for vertical transport of water quality substances."""
        return self._model.get_double("processes.thetavertical")

    @thetaVertical.setter
    def thetaVertical(self, value: float) -> None:
        self._model.set_double("processes.thetavertical", value)

    @property
    def dtProcesses(self) -> float:
        """Waq processes time step. Must be a multiple of `dtUser`. If `dtProcesses` is negative, water quality processes are calculated with every hydrodynamic time step."""
        return self._model.get_double("processes.dtprocesses")

    @dtProcesses.setter
    def dtProcesses(self, value: float) -> None:
        self._model.set_double("processes.dtprocesses", value)

    @property
    def processFluxIntegration(self) -> str:
        """Process fluxes integration option."""
        return self._model.get_enum_name("processes.processfluxintegration")

    @processFluxIntegration.setter
    def processFluxIntegration(self, value: str) -> None:
        self._model.set_enum_name("processes.processfluxintegration", value)

    @property
    def volumeDryThreshold(self) -> float:
        """Volume below which segments are marked as dry."""
        return self._model.get_double("processes.volumedrythreshold")

    @volumeDryThreshold.setter
    def volumeDryThreshold(self, value: float) -> None:
        self._model.set_double("processes.volumedrythreshold", value)

    @property
    def depthDryThreshold(self) -> float:
        """Water depth below which segments are marked as dry."""
        return self._model.get_double("processes.depthdrythreshold")

    @depthDryThreshold.setter
    def depthDryThreshold(self, value: float) -> None:
        self._model.set_double("processes.depthdrythreshold", value)

    @property
    def wriWaqBot3dOutput(self) -> str:
        """"""
        return self._model.get_string("processes.wriwaqbot3doutput")

    @wriWaqBot3dOutput.setter
    def wriWaqBot3dOutput(self, value: str) -> None:
        self._model.set_string("processes.wriwaqbot3doutput", value)

    @property
    def dtMassBalance(self) -> float:
        """Mass balance area output interval"""
        return self._model.get_double("processes.dtmassbalance")

    @dtMassBalance.setter
    def dtMassBalance(self, value: float) -> None:
        self._model.set_double("processes.dtmassbalance", value)


class ParticlesSection:
    """Typed access to the [particles] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def particlesFile(self) -> Path:
        """Initial particle locations file (*.xyz)."""
        return self._model.get_path("particles.particlesfile")

    @particlesFile.setter
    def particlesFile(self, value: Path | str) -> None:
        self._model.set_path("particles.particlesfile", value)

    @property
    def particlesReleaseFile(self) -> Path:
        """Particles release file (*.tim, 4 column)."""
        return self._model.get_path("particles.particlesreleasefile")

    @particlesReleaseFile.setter
    def particlesReleaseFile(self, value: Path | str) -> None:
        self._model.set_path("particles.particlesreleasefile", value)

    @property
    def addTracer(self) -> bool:
        """Add tracer or not."""
        return self._model.get_bool("particles.addtracer")

    @addTracer.setter
    def addTracer(self, value: bool) -> None:
        self._model.set_bool("particles.addtracer", value)

    @property
    def startTime(self) -> float:
        """Start time (if > 0)."""
        return self._model.get_double("particles.starttime")

    @startTime.setter
    def startTime(self, value: float) -> None:
        self._model.set_double("particles.starttime", value)

    @property
    def timeStep(self) -> float:
        """Time step (if > 0) or every computational time step."""
        return self._model.get_double("particles.timestep")

    @timeStep.setter
    def timeStep(self, value: float) -> None:
        self._model.set_double("particles.timestep", value)

    @property
    def _3DType(self) -> str:
        """3D velocity type."""
        return self._model.get_enum_name("particles.3dtype")

    @_3DType.setter
    def _3DType(self, value: str) -> None:
        self._model.set_enum_name("particles.3dtype", value)


class VegSection:
    """Typed access to the [veg] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def vegetationModelNr(self) -> str:
        """Vegetation model nr."""
        return self._model.get_enum_name("veg.vegetationmodelnr")

    @vegetationModelNr.setter
    def vegetationModelNr(self, value: str) -> None:
        self._model.set_enum_name("veg.vegetationmodelnr", value)

    @property
    def clVeg(self) -> float:
        """Stem distance factor."""
        return self._model.get_double("veg.clveg")

    @clVeg.setter
    def clVeg(self, value: float) -> None:
        self._model.set_double("veg.clveg", value)

    @property
    def cdVeg(self) -> float:
        """Stem Cd coefficient."""
        return self._model.get_double("veg.cdveg")

    @cdVeg.setter
    def cdVeg(self, value: float) -> None:
        self._model.set_double("veg.cdveg", value)

    @property
    def cbVeg(self) -> float:
        """Stem stiffness coefficient."""
        return self._model.get_double("veg.cbveg")

    @cbVeg.setter
    def cbVeg(self, value: float) -> None:
        self._model.set_double("veg.cbveg", value)

    @property
    def rhoVeg(self) -> float:
        """Stem Rho, if > 0, buoyant stick procedure."""
        return self._model.get_double("veg.rhoveg")

    @rhoVeg.setter
    def rhoVeg(self, value: float) -> None:
        self._model.set_double("veg.rhoveg", value)

    @property
    def stemHeightStd(self) -> float:
        """Stem height standard deviation fraction, e.g. 0.1."""
        return self._model.get_double("veg.stemheightstd")

    @stemHeightStd.setter
    def stemHeightStd(self, value: float) -> None:
        self._model.set_double("veg.stemheightstd", value)

    @property
    def stemHeightConvention(self) -> str:
        """Stem height convention."""
        return self._model.get_enum_name("veg.stemheightconvention")

    @stemHeightConvention.setter
    def stemHeightConvention(self, value: str) -> None:
        self._model.set_enum_name("veg.stemheightconvention", value)

    @property
    def densVegMinBap(self) -> float:
        """Minimum vegetation density in Baptist formula. Only in 2D."""
        return self._model.get_double("veg.densvegminbap")

    @densVegMinBap.setter
    def densVegMinBap(self, value: float) -> None:
        self._model.set_double("veg.densvegminbap", value)

    @property
    def expChiStem(self) -> float:
        """TODO."""
        return self._model.get_double("veg.expchistem")

    @expChiStem.setter
    def expChiStem(self, value: float) -> None:
        self._model.set_double("veg.expchistem", value)

    @property
    def expChiLeaf(self) -> float:
        """TODO."""
        return self._model.get_double("veg.expchileaf")

    @expChiLeaf.setter
    def expChiLeaf(self, value: float) -> None:
        self._model.set_double("veg.expchileaf", value)

    @property
    def uChiStem(self) -> float:
        """TODO."""
        return self._model.get_double("veg.uchistem")

    @uChiStem.setter
    def uChiStem(self, value: float) -> None:
        self._model.set_double("veg.uchistem", value)

    @property
    def uChiLeaf(self) -> float:
        """TODO."""
        return self._model.get_double("veg.uchileaf")

    @uChiLeaf.setter
    def uChiLeaf(self, value: float) -> None:
        self._model.set_double("veg.uchileaf", value)

    @property
    def areaLeaf(self) -> float:
        """TODO."""
        return self._model.get_double("veg.arealeaf")

    @areaLeaf.setter
    def areaLeaf(self, value: float) -> None:
        self._model.set_double("veg.arealeaf", value)

    @property
    def cdLeaf(self) -> float:
        """TODO."""
        return self._model.get_double("veg.cdleaf")

    @cdLeaf.setter
    def cdLeaf(self, value: float) -> None:
        self._model.set_double("veg.cdleaf", value)


class IceSection:
    """Typed access to the [ice] MDU section."""

    def __init__(self, model: MduModel):
        self._model = model

    @property
    def wriHis_ice_default(self) -> bool:
        """Default flag for writing ice cover quantities to his-file."""
        return self._model.get_bool("ice.wrihis_ice_default")

    @wriHis_ice_default.setter
    def wriHis_ice_default(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_default", value)

    @property
    def wriHis_ice_open_water_level(self) -> bool:
        """Write water level of open water to his-file."""
        return self._model.get_bool("ice.wrihis_ice_open_water_level")

    @wriHis_ice_open_water_level.setter
    def wriHis_ice_open_water_level(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_open_water_level", value)

    @property
    def wriHis_ice_lower_surface_height(self) -> bool:
        """Write lower surface height of ice cover to his-file."""
        return self._model.get_bool("ice.wrihis_ice_lower_surface_height")

    @wriHis_ice_lower_surface_height.setter
    def wriHis_ice_lower_surface_height(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_lower_surface_height", value)

    @property
    def wriHis_ice_surface_height(self) -> bool:
        """Write upper surface height of ice cover to his-file."""
        return self._model.get_bool("ice.wrihis_ice_surface_height")

    @wriHis_ice_surface_height.setter
    def wriHis_ice_surface_height(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_surface_height", value)

    @property
    def wriHis_ice_area_fraction(self) -> bool:
        """Write area fraction covered by ice to his-file."""
        return self._model.get_bool("ice.wrihis_ice_area_fraction")

    @wriHis_ice_area_fraction.setter
    def wriHis_ice_area_fraction(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_area_fraction", value)

    @property
    def wriHis_ice_thickness(self) -> bool:
        """Write ice thickness to his-file."""
        return self._model.get_bool("ice.wrihis_ice_thickness")

    @wriHis_ice_thickness.setter
    def wriHis_ice_thickness(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_thickness", value)

    @property
    def wriHis_ice_pressure(self) -> bool:
        """Write ice pressure to his-file."""
        return self._model.get_bool("ice.wrihis_ice_pressure")

    @wriHis_ice_pressure.setter
    def wriHis_ice_pressure(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_pressure", value)

    @property
    def wriHis_ice_temperature(self) -> bool:
        """Write ice temperature to his-file."""
        return self._model.get_bool("ice.wrihis_ice_temperature")

    @wriHis_ice_temperature.setter
    def wriHis_ice_temperature(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_ice_temperature", value)

    @property
    def wriHis_snow_thickness(self) -> bool:
        """Write snow thickness to his-file."""
        return self._model.get_bool("ice.wrihis_snow_thickness")

    @wriHis_snow_thickness.setter
    def wriHis_snow_thickness(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_snow_thickness", value)

    @property
    def wriHis_snow_temperature(self) -> bool:
        """Write snow temperature to his-file."""
        return self._model.get_bool("ice.wrihis_snow_temperature")

    @wriHis_snow_temperature.setter
    def wriHis_snow_temperature(self, value: bool) -> None:
        self._model.set_bool("ice.wrihis_snow_temperature", value)


class MduSchema:
    """Typed section access over an MduModel: the sections of an MDU file as typed objects."""

    def __init__(self, model: MduModel):
        self.general = GeneralSection(model)
        self.geometry = GeometrySection(model)
        self.volumetables = VolumeTablesSection(model)
        self.numerics = NumericsSection(model)
        self.physics = PhysicsSection(model)
        self.sediment = SedimentSection(model)
        self.sedtrails = SedtrailsSection(model)
        self.wind = WindSection(model)
        self.waves = WavesSection(model)
        self.grw = GrwSection(model)
        self.hydrology = HydrologySection(model)
        self.time = TimeSection(model)
        self.restart = RestartSection(model)
        self.external_forcing = ExternalForcingSection(model)
        self.trachytopes = TrachytopesSection(model)
        self.output = OutputSection(model)
        self.calibration = CalibrationSection(model)
        self.processes = ProcessesSection(model)
        self.particles = ParticlesSection(model)
        self.veg = VegSection(model)
        self.ice = IceSection(model)

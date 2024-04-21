"""
Description: Xml Configuration parser
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import copy
import re
from typing import Any, Dict, Iterable, List, Optional, Tuple

from lxml import etree

from src.config.credentials import Credentials
from src.config.dependency import Dependency
from src.config.file_check import FileCheck
from src.config.local_paths import LocalPaths
from src.config.location import Location
from src.config.parameter import Parameter
from src.config.program_config import ProgramConfig
from src.config.skip_line import SkipLine
from src.config.test_case_config import TestCaseConfig
from src.config.test_case_path import TestCasePath
from src.config.types.file_type import FileType
from src.config.types.path_type import PathType
from src.config.types.presence_type import PresenceType

def loop(dictionary: Dict[str, Any], key: str) -> List:
    if key in dictionary:
        if isinstance(dictionary[key], list):
            return dictionary[key]
        elif isinstance(dictionary[key], dict):
            return list(dictionary[key].values())
        else:
            return [dictionary[key]]
    else:
        return []


def branch(xml_tree: etree._ElementTree, prefix: str) -> Dict[str, Any]:
    new_tree = {"txt": xml_tree.text}

    for ch in xml_tree.getchildren():
        if type(ch.tag) == str:
            branch_name = ch.tag.replace(prefix, "")

            if branch_name not in new_tree:
                new_tree[branch_name] = []

            new_tree[branch_name].append(branch(ch, prefix))

    for key, val in xml_tree.attrib.items():
        new_tree[key.replace(prefix, "")] = [val]

    return new_tree


# Parse the xml configuration file
class XmlConfigParser(object):
    def load(
        self, path: str, rstr: str, cred: Credentials, server_base_url: str
    ) -> Tuple[LocalPaths, List[ProgramConfig], List[TestCaseConfig]]:
        """load the config file

        Args:
            path (str): (relative or absolute), overwritten roots (if any)
            rstr (str): ??
            cred (Credentials): credential from the command line
            server_base_url (sr): the base URL of the server (SVN, Git, MinIO etc.)

        Returns:
            tuple[LocalPaths, List[ProgramConfig], list]: local paths, program configs and test case configs
        """
        self.__path: str = path
        self.__rstr: str = rstr
        self.__validate__()
        self.__initialize__()
        self.__credentials.append(cred)
        self.__server_base_url: str = server_base_url

        return self.__parse__()

    def __maketree__(self, path):
        global schema
        parser = etree.XMLParser(remove_blank_text=True, attribute_defaults=False)
        parsed_tree: etree._ElementTree = etree.parse(path, parser)

        parsed_tree.xinclude()
        root_node: etree._Element = parsed_tree.getroot()

        schema = root_node.nsmap[None]
        prefix = "{%s}" % schema
        root_name = root_node.tag.replace(prefix, "")

        parsed_tree = branch(root_node, prefix)
        return (parsed_tree, schema, root_name)

    def __validate__(self):
        """validate Xml file format"""
        # parser = make_parser()
        # parser.setContentHandler(ContentHandler())
        # parser.parse(self.__path)
        pass

    def __initialize__(self):
        """initialize defaults"""
        self.__credentials: List[Credentials] = []
        self.__locations: List[Location] = []
        self.__program_configs: List[ProgramConfig] = []
        self.__default_cases: List[TestCaseConfig] = []

    # parse the xml file
    # output: loglevel, local paths, program configs and test case configs
    def __parse__(self):
        xml_doc, schema, root_name = self.__maketree__(self.__path)

        config_tags = xml_doc["config"]

        for config_tag in config_tags:
            #
            # The following for-loop should be deleted (when preparations are finished)
            self.__credentials = self.__credentials + list(self.__parse_credentials(config_tag))
            local_paths = self.__parse_local_paths(config_tag)
            self.__locations = list(self.__parse_locations(config_tag))

        for programs in loop(xml_doc, "programs"):
            if root_name == "deltaresTestbench_v3":
                for program in programs["program"]:
                    programInstance = self.__fillProgram__(program)
                    if programInstance is not None:
                        self.__program_configs.append(programInstance)

        for defaultCases in loop(xml_doc, "defaultTestCases"):
            for case in defaultCases["testCase"]:
                self.__default_cases.append(self.__fillCase__(case))

        result = []
        caseNr = -1
        for cases in loop(xml_doc, "testCases"):
            for case in loop(cases, "testCase"):
                caseNr = caseNr + 1
                try:
                    self.__default_cases.append(self.__fillCase__(case))
                    result.append(self.__fillCase__(case))
                except Exception:
                    print(
                        "Something is wrong with test case: "
                        + str(cases["testCase"][caseNr]["path"][0]["txt"])
                        + ", test case will be ignored"
                    )
        return local_paths, self.__program_configs, result

    def __parse_locations(self, config_tag: Dict[str, Any]) -> Iterable[Location]:
        for locations_tags in loop(config_tag, "locations"):
            for location_tag in locations_tags["location"]:
                yield self.__fillLocation__(location_tag)

    def __parse_local_paths(self, config_tag) -> LocalPaths:
        local_paths = LocalPaths()

        get_text = lambda d: str(d[0]["txt"])

        for lcl in loop(config_tag, "localPaths"):
            local_paths.cases_path = get_text(lcl["testCasesDir"])
            local_paths.engines_path = get_text(lcl["enginesDir"])
            local_paths.reference_path = get_text(lcl["referenceDir"])

        return local_paths

    def __parse_credentials(self, config_tag) -> Iterable[Credentials]:
        for credentials_tag in loop(config_tag, "credentials"):
            for credential_tag in credentials_tag["credential"]:
                new_credentials = Credentials()
                new_credentials.name = str(credential_tag["name"][0])
                new_credentials.username = str(credential_tag["username"][0]["txt"])
                new_credentials.password = str(credential_tag["password"][0]["txt"])
                yield new_credentials

    # fill network path (including default)
    # input: XmlElement
    # output: Location
    def __fillLocation__(self, element: Dict[str, Any]) -> Location:
        if not "ref" in element and not "name" in element:
            return None
        if not "ref" in element:
            new_location = Location()
            new_location.name = str(element["name"][0])
            if "credential" in element:
                c = self.__getCredentials__(str(element["credential"][0]["ref"][0]))
                if not c:
                    raise XmlError("invalid credential reference value in " + new_location.name)
                new_location.credentials = c
            # overwrite roots if specified
            newroot = self.__getOverwritePaths__(self.__rstr, new_location.name, "root")
            if newroot:
                new_location.root = newroot
            elif str(element["root"][0]["txt"].strip()).startswith("{server_base_url}"):
                directory_with_handlebar = str(element["root"][0]["txt"].strip())
                if self.__server_base_url.endswith("/") and directory_with_handlebar.replace("{server_base_url}", "").startswith("/"):
                    new_location.root = directory_with_handlebar.replace("{server_base_url}/", self.__server_base_url)
                    new_location.root = new_location.root.replace("{server_base_url}", self.__server_base_url)
                elif self.__server_base_url.endswith("/"):
                    new_location.root = directory_with_handlebar.replace("{server_base_url}", self.__server_base_url)
                elif directory_with_handlebar.replace("{server_base_url}", "").startswith("/"):
                    new_location.root = directory_with_handlebar.replace("{server_base_url}", self.__server_base_url)
                else:
                    new_location.root = directory_with_handlebar.replace("{server_base_url}", self.__server_base_url + "/")
            else:
                new_location.root = str(element["root"][0]["txt"].strip())
        else:
            new_location = copy.deepcopy(self.__getLocations__(element["ref"][0]))
            if not new_location:
                raise XmlError("invalid network path reference value in " + element["ref"][0])
        if "type" in element:
            if str(element["type"][0]).lower() == "input":
                new_location.type = PathType.INPUT
            if str(element["type"][0]).lower() == "check":
                new_location.type = PathType.CHECK
            if str(element["type"][0]).lower() == "reference":
                new_location.type = PathType.REFERENCE
        if "path" in element:
            #  overwrite paths if specified
            newpath = self.__getOverwritePaths__(self.__rstr, new_location.name, "path")
            if newpath:
                new_location.from_path = newpath
            else:
                new_location.from_path = str(element["path"][0]["txt"])
        if "version" in element:
            new_location.version = str(element["version"][0]["txt"])
        if "from" in element:
            # overwrite from if specified
            newfrom = self.__getOverwritePaths__(self.__rstr, new_location.name, "from")
            if newfrom:
                new_location.from_path = newfrom
            else:
                # Remove leading/trailing slashes; they mess up the building of the path
                new_location.from_path = str(element["from"][0]["txt"]).strip("/\\")
        if "to" in element:
            # overwrite to if specified
            newto = self.__getOverwritePaths__(self.__rstr, new_location.name, "to")
            if newto:
                new_location.to_path = newto
            else:
                new_location.to_path = str(element["to"][0]["txt"])
        return new_location

    # fill program (including defaults)
    # input: XmlElement
    # output: Program
    def __fillProgram__(self, element):
        p = ProgramConfig()
        if "ignore" in element:  # ignore program for this case [RL666]
            if element["ignore"][0].lower() == "true":
                return None
        if "name" in element:
            p.name = str(element["name"][0])
        if "programStringRemoveQuotes" in element and str(element["programStringRemoveQuotes"][0]).lower() == "true":
            p.program_remove_quotes = True
        if "shellStringRemoveQuotes" in element and str(element["shellStringRemoveQuotes"][0]).lower() == "true":
            p.shell_remove_quotes = True
        if "ignoreStandardError" in element and str(element["ignoreStandardError"][0]).lower() == "true":
            p.ignore_standard_error = True
        if "ignoreReturnValue" in element and str(element["ignoreReturnValue"][0]).lower() == "true":
            p.ignore_return_value = True
        if "logOutputToFile" in element and str(element["logOutputToFile"][0]).lower() == "true":
            p.log_output_to_file = True
        if "addSearchPaths" in element and str(element["addSearchPaths"][0]).lower() == "true":
            p.add_search_paths = True
        if "excludeSearchPathsContaining" in element:
            p.exclude_search_paths_containing = str(element["excludeSearchPathsContaining"][0])

        if "ref" in element:
            p.name = str(element["ref"][0])
        if "seq" in element:
            p.sequence = int(element["seq"][0])
        if "delay" in element:
            p.delay = float(element["delay"][0])
        if "maxRunTime" in element:
            p.max_run_time = float(element["maxRunTime"][0]["txt"])
        if "path" in element:
            # overwrite path if specified
            newpath = self.__getOverwritePaths__(self.__rstr, p.name, "path")
            if newpath:
                p.path = newpath
            else:
                p.path = str(element["path"][0]["txt"])
        if "workingDirectory" in element:
            p.working_directory = str(element["workingDirectory"][0]["txt"])
        for e in loop(element, "location"):
            nwp = self.__fillLocation__(e)
            if nwp:
                nwpExists = False
                for enp in p.locations:
                    if enp.name == nwp.name and enp.type == nwp.type:
                        enp = nwp
                        nwpExists = True
                if not nwpExists:
                    p.locations.append(nwp)
        for el in loop(element, "shell"):
            shellProgram = self.__getPrograms__(str(el["ref"][0]))
            if shellProgram == None:
                raise XmlError(
                    "Can not find shell program '"
                    + str(el["ref"][0])
                    + "'. Is this program defined in the config.xml file before being used as shell?"
                )
            else:
                p.shell = shellProgram
        for el in loop(element, "arguments"):
            for package in el["argument"]:
                p.arguments.append(str(package["txt"]))
        for el in loop(element, "modules"):
            for module in el["module"]:
                p.modules.append(str(module["txt"]))
        for el in loop(element, "environments"):
            for env in el["environment"]:
                # append search paths if necessary
                if str(env["name"][0]).lower() == "%path%":
                    p.search_paths.append(str(env["txt"]))
                else:
                    p.environment_variables[str(env["name"][0])] = [
                        str(env["type"][0]),
                        str(env["txt"]),
                    ]
        return p

    # fill file checks
    # input: XmlElement
    # output: list of FileCheck
    def __fillFileCheck__(self, element) -> FileCheck:
        defined_file_types = {
            "ascii": FileType.ASCII,
            "nefis": FileType.NEFIS,
            "his": FileType.HIS,
            "netcdf": FileType.NETCDF,
            "numbertext": FileType.NUMBERTEXT,
            "dseriesregression": FileType.DSERIESREGRESSION,
            "dseriesverification": FileType.DSERIESVERIFICATION,
            "timeseries_pi": FileType.PITIMESERIES,
            "timeseries_csv": FileType.CSVTIMESERIES,
        }
        defined_presence_types = {
            "present": PresenceType.PRESENT,
            "absent": PresenceType.ABSENT,
        }

        fc = FileCheck()
        skiplines = {}
        skipline = []
        i = -1

        fc.name = str(element["name"][0])
        if "ignore" in element and str(element["ignore"][0]).lower() == "true":
            fc.ignore = True
        else:
            if "type" in element:
                typename = str(element["type"][0]).lower()
                if typename in defined_file_types:
                    fc.type = defined_file_types[typename]
                else:
                    fc.type = FileType.NONE

            if "presence" in element:
                presencename = str(element["presence"][0]).lower()
                if presencename in defined_presence_types:
                    fc.presence = defined_presence_types[presencename]
                else:
                    fc.presence = PresenceType.NONE

        parameters = {}
        for el in loop(element, "parameters"):
            params = []
            name = ""
            if "name" in el:
                name = str(el["name"][0])
            for param in loop(el, "parameter"):
                # parameters MUST have a name when this is a nefis file, otherwise it is optional.
                # But name is not allowed to be empty!
                if name == "":
                    if fc.type == FileType.NEFIS:
                        raise OSError(
                            "In config file, checkfile "
                            + fc.name
                            + " has type nefis but field <parameters> has no name attribute"
                        )
                    name = "parameters"
                p = Parameter()
                p.name = str(param["name"][0])
                if "location" in param:
                    p.location = str(param["location"][0])
                if "tolerance" in param:
                    p.set_tolerance(float(param["tolerance"][0]))
                if "toleranceAbsolute" in param:
                    p.tolerance_absolute = float(param["toleranceAbsolute"][0])
                if "toleranceRelative" in param:
                    p.tolerance_relative = float(param["toleranceRelative"][0])
                if "ignore" in param and str(param["ignore"][0]).lower() == "true":
                    p.ignore = True
                params.append(p)
            parameters[name] = params
        for el in loop(element, "skipline"):
            i = i + 1
            p = SkipLine()
            p.name = str(element["skipline"][i]["txt"])
            skipline.append(p)
            skiplines["skipline"] = skipline

        fc.parameters = parameters
        fc.skip_lines = skiplines
        return fc

    # #######################################################################################################################################

    def __fillCase__(self, element) -> Optional[TestCaseConfig]:
        """fill cases (including default)"""
        if "ref" not in element:
            test_case = TestCaseConfig()
            if "maxRunTime" not in element:
                raise XmlError("no maximum run time specified for " + test_case.name)
        else:
            test_case = copy.deepcopy(self.__getCase__(str(element["ref"][0])))
            if test_case is None:
                return None

            if "programs" in element:
                test_case.program_configs = []

        test_case.name = str(element["name"][0])
        if "ignore" in element:
            if str(element["ignore"][0]).lower() == "true":
                test_case.ignore = True
        for e in loop(element, "location"):
            nwp = self.__fillLocation__(e)
            if nwp:
                nwpExists = False
                for enp in test_case.locations:
                    if enp.type == nwp.type:
                        enp = nwp
                        nwpExists = True
                if not nwpExists:
                    test_case.locations.append(nwp)
        # add case path
        if "path" in element:
            # overwrite path if specified
            newpath = self.__getOverwritePaths__(self.__rstr, test_case.name, "path")
            if newpath:
                test_case.path = TestCasePath(newpath, None)
            else:
                tag = element["path"][0]
                version_list = tag.get("version", [])
                version = str(version_list[0]) if version_list else None
                path = tag["txt"]
                test_case.path = TestCasePath(path, version)

        if "dependency" in element:
            tag = element["dependency"][0]
            local_dir = str(tag["local_dir"][0])
            cases_path = str(tag["txt"])
            version_list = tag.get("version", [])
            version = str(version_list[0]) if version_list else None

            test_case.dependency = Dependency(local_dir, cases_path, version)

        if "maxRunTime" in element:
            test_case.max_run_time = float(element["maxRunTime"][0]["txt"])
            for el in element["maxRunTime"]:
                if "OverruleRefMaxRunTime" in el and str(el["OverruleRefMaxRunTime"][0]).lower() == "true":
                    test_case.overrule_ref_max_run_time = True
        for el in loop(element, "programs"):
            for program in loop(el, "program"):
                programInstance = self.__fillProgram__(program)
                if programInstance is not None:
                    test_case.program_configs.append(programInstance)
        for el in loop(element, "errors"):
            for error in el["error"]:
                test_case.errors.append(str(error["txt"]))
        for el in loop(element, "checks"):
            for check in el["file"]:
                test_case.checks.append(self.__fillFileCheck__(check))
        for el in loop(element, "shellarguments"):
            for shellargument in el["shellargument"]:
                test_case.shell_arguments.append(str(shellargument["txt"]))
        if "shell" in element:
            localShellName = str(element["shell"][0]["txt"])
            test_case.shell = self.__getPrograms__(localShellName)

        if "processCount" in element:
            test_case.process_count = int(element["processCount"][0]["txt"])

        return test_case

        # parse roots

    def __getOverwritePaths__(self, rstr, who, what):
        if rstr == None or rstr == "":
            return None
        pathparts = rstr.split(",")
        for part in pathparts:
            actual = PathParts()
            call, path = part.split("=")
            # only check calls for my name (in config)
            name = re.findall(r"(?<=\[)(.*?)(?=\])", call)[0]
            if not who == name:
                continue
            # only check calls with correct name (root, form, to or path)
            if not str(call).startswith(what):
                continue
            # found it
            return path
        # found nothing
        return None

    # get reference value if exists
    def __getCredentials__(self, name) -> Optional[Credentials]:
        for credential in self.__credentials:
            if credential.name == name:
                return credential
        return None

    # get reference value if exists
    def __getLocations__(self, name) -> Optional[Location]:
        for location in self.__locations:
            if location.name == name:
                return location
        return None

    # get reference value if exists
    def __getPrograms__(self, name: str) -> Optional[ProgramConfig]:
        for program in self.__program_configs:
            if program.name == name:
                return program
        return None

    # get reference value if exists
    def __getCase__(self, name: str) -> Optional[TestCaseConfig]:
        for case in self.__default_cases:
            if case.name == name:
                return case
        return None


# Parsable for paths overrides
class PathParts:
    def __init__(self):
        self.__name = None
        self.__root = None
        self.__from = None
        self.__to = None
        self.__path = None

    def getName(self):
        return self.__name

    def setName(self, name):
        self.__name = name

    def getRoot(self):
        return self.__root

    def setRoot(self, root):
        self.__root = root

    def getFrom(self):
        return self.__from

    def setFrom(self, _from):
        self.__from = _from

    def getTo(self):
        return self.__to

    def setTo(self, to):
        self.__to = to

    def getPath(self):
        return self.__path

    def setPath(self, path):
        self.__path = path


# custom error for Xml handler
class XmlError(Exception):
    def __init__(self, value):
        self.__value = value

    def __str__(self):
        return repr(self.__value)

from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import List

from dvc.repo import Repo
from lxml import etree

from src.utils.minio_rewinder import Rewinder
from tools.minio_dvc_migration.s3_url_info import rewind_timestep_2_datetime
from tools.minio_dvc_migration.testcase_data import (
    DependencyKey,
    TestCaseData,
    is_case_with_doc_folder,
    move_doc_folder_to_parent,
    rename_dependency_path_for_dvc,
)


class XmlFileWithTestCaseData:
    """Test case data for XML parsing."""

    TB_NAMESPACE_URI = "http://schemas.deltares.nl/deltaresTestbench_v3"
    NAMESPACE = {"tb": TB_NAMESPACE_URI}

    XI_NAMESPACE_URI = "http://www.w3.org/2001/XInclude"
    XI_NAMESPACE = {"xi": XI_NAMESPACE_URI}

    xml_file: Path
    testcases: list[TestCaseData]
    dependency_version_map: dict[DependencyKey, str]

    def __init__(
        self,
        xml_file: Path,
        testcases: list[TestCaseData],
        dependency_version_map: dict[DependencyKey, str] | None = None,
    ) -> None:
        self.xml_file = xml_file
        self.testcases = testcases
        self.dependency_version_map = dependency_version_map or {}

    def migrate_xml_to_dvc(self) -> None:
        """Update XML file to use DVC and local data paths."""
        self.__migrate_included_xml_to_dvc(self.xml_file)

    def __migrate_included_xml_to_dvc(self, xml_path: Path | None = None) -> None:
        """Migrate a single XML file to use DVC and local data paths."""
        if xml_path is None:
            xml_path = self.xml_file
        print(f"Update xml: {xml_path}")

        if not xml_path.exists():
            raise FileNotFoundError(f"XML file does not exist: {xml_path}")

        try:
            # Read the file to detect original encoding
            with open(xml_path, "rb") as f:
                content = f.read()

            # Preserve whether the file originally had an XML declaration.
            has_xml_declaration = content.startswith(b"<?xml")

            # Detect encoding from XML declaration
            encoding = "utf-8"  # default
            if content.startswith(b"<?xml"):
                xml_decl = content.split(b"?>")[0] + b"?>"
                if b"encoding=" in xml_decl:
                    # Extract encoding from declaration
                    encoding_part = xml_decl.split(b"encoding=")[1]
                    quote_char = encoding_part[0:1]  # ' or "
                    encoding = encoding_part[1:].split(quote_char)[0].decode("ascii")

            parser = etree.XMLParser(remove_blank_text=True)
            tree = etree.parse(xml_path, parser)
            root = tree.getroot()

            self.__update_local_paths(root, self.NAMESPACE)
            self.__update_location_roots(root, self.NAMESPACE)
            self.__update_testcase_version(root, self.NAMESPACE)
            self.__update_dependency_version_and_path(root, self.NAMESPACE)
            self.__process_xi_includes(root, xml_path)

            # Write the modified XML back
            tree.write(
                xml_path,
                encoding=encoding,
                xml_declaration=has_xml_declaration,
                pretty_print=True,
            )
            print(f"Successfully updated XML file: {xml_path}")

        except Exception as e:
            print(f"Error updating XML file {xml_path}: {e}")

    def __update_local_paths(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update local paths to use ./data/cases."""
        # Update testCasesDir
        test_cases_dir = root.find(".//tb:localPaths/tb:testCasesDir", namespace)
        if test_cases_dir is not None:
            test_cases_dir.text = "./data/cases"

        # Update referenceDir
        reference_dir = root.find(".//tb:localPaths/tb:referenceDir", namespace)
        if reference_dir is not None:
            reference_dir.text = "./data/cases"

    def __update_location_roots(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update location roots to use local paths.

        Locations referenced by (default) testcases are always updated.
        When no testcase references are found (e.g. in a standalone configuration file),
        the well-known 'cases' and 'reference_results' locations are updated.
        """
        referenced_location_names: set[str] = set()
        testcase_locations = root.findall(".//tb:testCase/tb:location", namespace)
        for testcase_location in testcase_locations:
            ref = testcase_location.get("ref")
            if ref:
                referenced_location_names.add(ref)

        if not referenced_location_names:
            referenced_location_names = {"cases", "reference_results"}

        config_locations = root.findall(".//tb:locations/tb:location", namespace)
        for config_location in config_locations:
            name = config_location.get("name")
            if not name or name not in referenced_location_names:
                continue

            root_elem = config_location.find("tb:root", namespace)
            if root_elem is not None:
                root_elem.text = "./data/cases"

    def __update_testcase_version(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update the version attribute for migrated testcases to 'DVC'.

        Only versions that are valid rewind timestamps (see `rewind_timestep_2_datetime`) are updated.
        All other versions are preserved.
        """
        testcases = root.findall(".//tb:testCase", namespace)
        for testcase in testcases:
            path_elem = testcase.find("tb:path", namespace)
            if path_elem is not None and path_elem.get("version"):
                current_version = path_elem.get("version") or ""
                if rewind_timestep_2_datetime(current_version.strip()) is not None:
                    path_elem.set("version", "DVC")

    def __update_dependency_version_and_path(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update dependency elements: set version to 'DVC' and rename path to DVC convention.

        A version suffix (e.g. '_v1') is appended when the dependency_version_map contains a mapping
        for the (s3_path, version) pair, which happens when a dependency path has multiple versions.
        Only dependencies with valid rewind timestamps are updated.
        """
        testcases = root.findall(".//tb:testCase", namespace)
        for testcase in testcases:
            dep_elem = testcase.find("tb:dependency", namespace)
            if dep_elem is not None:
                current_version = (dep_elem.get("version") or "").strip()
                if rewind_timestep_2_datetime(current_version) is not None:
                    original_path = (dep_elem.text or "").strip()
                    version_suffix = self.dependency_version_map.get(
                        DependencyKey(f"cases/{original_path}", current_version), ""
                    )
                    dep_elem.set("version", "DVC")
                    if original_path:
                        dep_elem.text = rename_dependency_path_for_dvc(original_path, version_suffix=version_suffix)

    def __process_xi_includes(self, root: etree._Element, current_file: Path) -> None:
        """Process xi:include elements and update the included files recursively."""
        includes = root.findall(".//xi:include", self.XI_NAMESPACE)

        for include in includes:
            href = include.get("href")
            if href:
                # Resolve the path relative to the current file
                include_path = (current_file.parent / href).resolve(strict=False)
                if include_path.exists():
                    print(f"Processing included file: {include_path}")
                    self.__migrate_included_xml_to_dvc(include_path)
                else:
                    print(f"Warning: Included file does not exist: {include_path}")

    def download_from_minio_in_new_folder_structure(self, rewinder: Rewinder) -> None:
        """Download all testcases and their dependencies from MinIO using the new folder structure."""
        print(f"Download {len(self.testcases)} testcases from {self.xml_file}")

        downloaded_dependencies: set[DependencyKey] = set()
        for i, testcase in enumerate(self.testcases, start=1):
            dep_key = testcase.dependency_key
            if testcase.has_dependency and dep_key not in downloaded_dependencies:
                testcase.download_dependency(rewinder=rewinder)
                downloaded_dependencies.add(dep_key)
                print(f"Downloaded dependency for testcase {i}/{len(self.testcases)}: {testcase.name}")

            testcase.download(rewinder=rewinder)
            print(f"Downloaded testcase {i}/{len(self.testcases)}: {testcase.name}")

    def verify_and_filter_testcases(self, rewinder: Rewinder) -> list[TestCaseData]:
        """Remove testcases whose case or reference is missing on MinIO.

        Returns the list of skipped testcases. Remaining testcases are kept in self.testcases.
        """
        skipped: list[TestCaseData] = []
        verified: list[TestCaseData] = []

        for testcase in self.testcases:
            result = testcase.check_minio_presence(rewinder)
            if result["case"] == "found" and result["reference"] == "found":
                verified.append(testcase)
            else:
                skipped.append(testcase)
                print(f"  WARNING: SKIP {testcase.name}: case={result['case']} ref={result['reference']}")

        self.testcases = verified
        return skipped

    def check_minio_presence(self, rewinder: Rewinder) -> dict[str, int]:
        """Check MinIO for file existence for all testcases and their dependencies.

        Returns
        -------
        dict[str, int]
            Counts with keys 'found', 'missing', 'errors', and 'doc_moves'.
        """
        found = 0
        missing = 0
        errors = 0
        doc_moves = 0
        checked_deps: set[DependencyKey] = set()

        print(f"Checking {len(self.testcases)} testcases from {self.xml_file.name} on MinIO")
        for i, testcase in enumerate(self.testcases, start=1):
            result = testcase.check_minio_presence(rewinder)

            case_label = result["case"].upper()
            ref_label = result["reference"].upper()
            print(f"  {i}/{len(self.testcases)} {testcase.name}: case={case_label} ref={ref_label}", end="")

            found += (result["case"] == "found") + (result["reference"] == "found")
            missing += (result["case"] == "missing") + (result["reference"] == "missing")
            errors += (result["case"] == "error") + (result["reference"] == "error")

            doc_status = testcase.check_doc_folder_minio_presence(rewinder)
            if doc_status == "found":
                doc_moves += 1
                print(" doc=MOVE", end="")

            if testcase.has_dependency:
                dep_key = testcase.dependency_key
                if dep_key not in checked_deps:
                    checked_deps.add(dep_key)
                    dep_status = testcase.check_dependency_minio_presence(rewinder)
                    print(f" dep={dep_status.upper()}", end="")
                    found += dep_status == "found"
                    missing += dep_status == "missing"
                    errors += dep_status == "error"

            print()

        return {"found": found, "missing": missing, "errors": errors, "doc_moves": doc_moves}

    def move_testcases_doc_folder_to_parent(self) -> None:
        """Move doc folder to parent folder for all testcases."""
        for testcase in self.testcases:
            local_path = testcase.case.to_local()
            if is_case_with_doc_folder(local_path):
                move_doc_folder_to_parent(local_path)

    def add_to_dvc(self, repo: Repo) -> List[Path]:
        """Add all testcases folders and their dependencies to DVC tracking."""
        dvc_files = []
        added_dependencies: set[DependencyKey] = set()
        for i, testcase in enumerate(self.testcases, start=1):
            print(f"Adding testcase {i}/{len(self.testcases)}: {testcase.name} - {self.xml_file}")

            dep_key = testcase.dependency_key
            if testcase.has_dependency and dep_key not in added_dependencies:
                dvc_files.extend(testcase.add_dependency_to_dvc(repo=repo))
                added_dependencies.add(dep_key)

            dvc_files.extend(testcase.add_to_dvc(repo=repo))

        return dvc_files


def filter_cases_to_migrate(xmls: list[XmlFileWithTestCaseData]) -> list[XmlFileWithTestCaseData]:
    """Filter to testcases that should be migrated from MinIO to DVC."""
    print("Filtering out testcases that are already in DVC...")
    xmls_with_minio_testcases: list[XmlFileWithTestCaseData] = []

    for xml in xmls:
        migrate_cases = [tc for tc in xml.testcases if rewind_timestep_2_datetime(tc.version.strip()) is not None]
        if migrate_cases:
            xmls_with_minio_testcases.append(XmlFileWithTestCaseData(xml_file=xml.xml_file, testcases=migrate_cases))
            print(
                f"XML {xml.xml_file.name} has {len(migrate_cases)} of {len(xml.testcases)} total testcases to migrate"
            )
        else:
            print(f"All testcases in XML {xml.xml_file.name} are not in MinIO or already in DVC")

    return xmls_with_minio_testcases


def build_dependency_version_map(
    xmls: list[XmlFileWithTestCaseData],
) -> dict[DependencyKey, str]:
    """Build a mapping from DependencyKey to a version suffix.

    When a dependency S3 path is referenced with multiple distinct versions across
    the provided XMLs, each version gets a suffix like '_v1', '_v2', etc. sorted
    chronologically. Paths that have only one version across all XMLs are omitted
    from the map (they don't need a suffix). Versions that cannot be parsed as
    timestamps are excluded.
    """
    path_versions: dict[str, set[str]] = defaultdict(set)
    for xml in xmls:
        for tc in xml.testcases:
            if tc.has_dependency and tc.dependency_version:
                if rewind_timestep_2_datetime(tc.dependency_version) is not None:
                    path_versions[tc.dependency_s3_path].add(tc.dependency_version)

    version_map: dict[DependencyKey, str] = {}
    for s3_path, versions in path_versions.items():
        if len(versions) > 1:
            # Use datetime.min as a fallback in the sort key, which satisfies the type checker while being unreachable
            # in practice (since we pre-filter to parseable versions).
            sorted_versions = sorted(versions, key=lambda v: rewind_timestep_2_datetime(v) or datetime.min)
            for i, version in enumerate(sorted_versions, start=1):
                version_map[DependencyKey(s3_path, version)] = f"_v{i}"

    return version_map


def apply_dependency_version_map(
    xmls: list[XmlFileWithTestCaseData],
    version_map: dict[DependencyKey, str],
) -> None:
    """Update testcase dependency paths with version suffixes from the map.

    For each testcase whose dependency key appears in the version map,
    the dependency.path is updated with the suffix appended.
    """
    for xml in xmls:
        xml.dependency_version_map = version_map
        for tc in xml.testcases:
            if not tc.has_dependency:
                continue
            suffix = version_map.get(tc.dependency_key, "")
            if suffix:
                tc.dependency.path += suffix

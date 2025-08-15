import json
import re
from typing import Any, Dict


def pascal_case_to_snake_case(name: str) -> str:
    """
    Convert a PascalCase string to snake_case.

    Args:
        name (str): The PascalCase string.

    # Returns:
        str: The converted snake_case string.
    """
    # Insert underscore before each capital letter (except the first one)
    snake = re.sub(r"(?<!^)(?=[A-Z])", "_", name).lower()
    return snake


def assign_attributes_from_settings(instance: object, settings: Dict[str, Any], attribute_names: list) -> None:
    """Assign values from settings dictionary to object attributes."""
    for attr_name in attribute_names:
        if attr_name in settings:
            print(f"Set '{attr_name}' in {instance.__class__.__name__}")
            setattr(instance, attr_name, settings.get(attr_name, ""))
        else:
            print(f"Setting '{attr_name}' not found in configuration. Using default empty string.")
            setattr(instance, attr_name, "")


class TeamcityIds:
    """Settings related to TeamCity build configurations and identifiers."""

    def __init__(self, settings: Dict[str, str]) -> None:
        self.dimr_publish = "Unassigned"
        self.delft3d_linux_collect_build_type_id = "Unassigned"
        self.delft3d_windows_collect_build_type_id = "Unassigned"
        self.dimr_to_nghs_build_type_id = "Unassigned"
        self.dimr_testbench_release_tests_linux = "Unassigned"
        self.dimr_testbench_release_tests_windows = "Unassigned"
        self.status_of_daily = "Unassigned"

        attribute_names = [
            attr for attr in dir(self) if not attr.startswith("_") and getattr(self, attr) == "Unassigned"
        ]

        assign_attributes_from_settings(self, settings, attribute_names)


class Settings:
    """Settings related to TeamCity build configurations and identifiers."""

    def __init__(self, json_settings_path: str) -> None:
        settings = self.__load_settings(json_settings_path)
        self.teamcity_ids = TeamcityIds(settings.get(pascal_case_to_snake_case(TeamcityIds.__name__), {}))

        self.path_to_windows_version_artifact = "Unassigned"
        self.path_to_linux_version_artifact = "Unassigned"
        self.path_to_release_test_results_artifact = "Unassigned"
        self.name_of_dimr_release_signed_linux_artifact = "Unassigned"
        self.name_of_dimr_release_signed_windows_artifact = "Unassigned"
        self.dimr_space_id = "Unassigned"
        self.dimr_root_page_id = "Unassigned"
        self.dimr_major_page_prefix = "Unassigned"
        self.dimr_minor_page_prefix = "Unassigned"
        self.dimr_patch_page_prefix = "Unassigned"
        self.dimr_subpage_prefix = "Unassigned"
        self.dimr_subpage_suffix = "Unassigned"
        self.network_base_path = "Unassigned"
        self.linux_address = "Unassigned"
        self.relative_path_to_wiki_template = "Unassigned"
        self.delft3d_git_repo = "Unassigned"
        self.relative_path_to_email_template = "Unassigned"
        self.lower_bound_percentage_successful_tests = "Unassigned"
        self.versions_excel_filename = "Unassigned"
        self.sheet_name = "Unassigned"
        self.name_column = "Unassigned"
        self.relative_path_to_output_folder = "Unassigned"
        self.dry_run_prefix = "Unassigned"

        attribute_names = [
            attr for attr in dir(self) if not attr.startswith("_") and getattr(self, attr) == "Unassigned"
        ]

        assign_attributes_from_settings(self, settings, attribute_names)

    def __load_settings(self, json_settings_path: str) -> Dict[str, Any]:
        try:
            with open(json_settings_path, "r") as f:
                return json.load(f)
        except FileNotFoundError as e:
            raise FileNotFoundError(f"Settings file not found: {json_settings_path}") from e
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in settings file '{json_settings_path}': {e}") from e
        except PermissionError as e:
            raise PermissionError(f"Permission denied reading settings file: {json_settings_path}") from e
        except Exception as e:
            raise RuntimeError(f"Unexpected error loading settings from '{json_settings_path}': {e}") from e


class KernelData:
    """Class to hold the various variations of names for a kernel."""

    def __init__(self, name_for_extracting_revision: str, name_for_email: str) -> None:
        """
        Create a new instance of KernelData.

        Args:
            name_for_extracting_revision (str): The name of the kernel used to extract the revision number from
            the build dependencies.
            name_for_email (str): The name to represent the kernel in the email.
        """
        self.name_for_extracting_revision = name_for_extracting_revision
        self.name_for_email = name_for_email


# All kernels for which the versions are set in the configuration parameters of 2.Dimr_collector_release
KERNELS = [
    KernelData(name_for_extracting_revision="DIMRset_ver", name_for_email="DIMRset"),
    KernelData(name_for_extracting_revision="build.vcs.number", name_for_email="OSS"),
]

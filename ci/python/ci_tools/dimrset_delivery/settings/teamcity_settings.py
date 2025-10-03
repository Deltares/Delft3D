import json
import re
from typing import Any, Dict, List, cast


def pascal_case_to_snake_case(name: str) -> str:
    """
    Convert a PascalCase string to snake_case.

    Parameters
    ----------
    name : str
        The PascalCase string to convert.

    Returns
    -------
    str
        The converted snake_case string.
    """
    # Insert underscore before each capital letter (except the first one)
    snake = re.sub(r"(?<!^)(?=[A-Z])", "_", name).lower()
    return snake


def assign_attributes_from_settings(instance: object, settings: Dict[str, Any], attribute_names: list) -> None:
    """
    Assign values from settings dictionary to object attributes.

    Parameters
    ----------
    instance : object
        The object whose attributes will be set.
    settings : Dict[str, Any]
        The dictionary containing settings.
    attribute_names : list
        List of attribute names to assign from settings.
    """
    for attr_name in attribute_names:
        if attr_name in settings:
            print(f"Set '{attr_name}' in {instance.__class__.__name__}")
            setattr(instance, attr_name, settings.get(attr_name, ""))
        else:
            print(f"Setting '{attr_name}' not found in configuration. Using default empty string.")
            setattr(instance, attr_name, "")


INIT_VALUE = ""


class TeamcityIds:
    """
    Holds TeamCity build configuration identifiers.

    This class is used to store and assign TeamCity build configuration IDs from a settings dictionary.
    """

    def __init__(self, settings: Dict[str, str]) -> None:
        """
        Initialize TeamcityIds with settings.

        Parameters
        ----------
        settings : Dict[str, str]
            Dictionary containing TeamCity build configuration IDs.
        """
        self.dimr_publish = INIT_VALUE
        self.delft3d_linux_collect_build_type_id = INIT_VALUE
        self.delft3d_windows_collect_build_type_id = INIT_VALUE
        self.dimr_to_nghs_build_type_id = INIT_VALUE
        self.dimr_testbench_release_tests_linux = INIT_VALUE
        self.dimr_testbench_release_tests_windows = INIT_VALUE
        self.status_of_daily = INIT_VALUE

        attribute_names = [attr for attr in dir(self) if not attr.startswith("_") and getattr(self, attr) == INIT_VALUE]

        assign_attributes_from_settings(self, settings, attribute_names)


class Settings:
    """
    Stores settings related to TeamCity build configurations and identifiers.

    This class loads settings from a JSON file and assigns them to attributes for use in TeamCity operations.
    """

    def __init__(self, json_settings_path: str) -> None:
        """
        Initialize Settings from a JSON settings file.

        Parameters
        ----------
        json_settings_path : str
            Path to the JSON settings file.
        """
        settings = self.__load_settings(json_settings_path)
        self.teamcity_ids = TeamcityIds(settings.get(pascal_case_to_snake_case(TeamcityIds.__name__), {}))
        self.teamcity_project_keys: List[str] = settings.get("teamcity_project_keys", [])

        self.path_to_windows_version_artifact = INIT_VALUE
        self.path_to_linux_version_artifact = INIT_VALUE
        self.path_to_release_test_results_artifact = INIT_VALUE
        self.path_to_release_changelog_artifact = INIT_VALUE
        self.name_of_dimr_release_signed_linux_artifact = INIT_VALUE
        self.name_of_dimr_release_signed_windows_artifact = INIT_VALUE
        self.dimr_space_id = INIT_VALUE
        self.dimr_root_page_id = INIT_VALUE
        self.dimr_major_page_prefix = INIT_VALUE
        self.dimr_minor_page_prefix = INIT_VALUE
        self.dimr_patch_page_prefix = INIT_VALUE
        self.dimr_subpage_prefix = INIT_VALUE
        self.dimr_subpage_suffix = INIT_VALUE
        self.network_base_path = INIT_VALUE
        self.linux_address = INIT_VALUE
        self.relative_path_to_wiki_template = INIT_VALUE
        self.delft3d_git_repo = INIT_VALUE
        self.issuetracker_url = INIT_VALUE
        self.relative_path_to_email_template = INIT_VALUE
        self.lower_bound_percentage_successful_tests = INIT_VALUE
        self.versions_excel_filename = INIT_VALUE
        self.sheet_name = INIT_VALUE
        self.name_column = INIT_VALUE
        self.relative_path_to_output_folder = INIT_VALUE
        self.dry_run_prefix = INIT_VALUE

        attribute_names = [attr for attr in dir(self) if not attr.startswith("_") and getattr(self, attr) == INIT_VALUE]

        assign_attributes_from_settings(self, settings, attribute_names)

    def __load_settings(self, json_settings_path: str) -> Dict[str, Any]:
        """
        Load settings from a JSON file.

        Parameters
        ----------
        json_settings_path : str
            Path to the JSON settings file.

        Returns
        -------
        Dict[str, Any]
            Dictionary containing the loaded settings.

        Raises
        ------
        FileNotFoundError
            If the settings file does not exist.
        ValueError
            If the JSON is invalid.
        PermissionError
            If the file cannot be read due to permissions.
        RuntimeError
            For any other unexpected error.
        """
        try:
            with open(json_settings_path, "r") as f:
                return cast(Dict[str, Any], json.load(f))
        except FileNotFoundError as e:
            raise FileNotFoundError(f"Settings file not found: {json_settings_path}") from e
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in settings file '{json_settings_path}': {e}") from e
        except PermissionError as e:
            raise PermissionError(f"Permission denied reading settings file: {json_settings_path}") from e
        except Exception as e:
            raise RuntimeError(f"Unexpected error loading settings from '{json_settings_path}': {e}") from e


class KernelData:
    """
    Holds variations of names for a kernel.

    This class stores names used for extracting revision information and for email representation.
    """

    def __init__(self, name_for_extracting_revision: str, name_for_email: str) -> None:
        """
        Initialize KernelData with kernel names.

        Parameters
        ----------
        name_for_extracting_revision : str
            Name used to extract the revision number from build dependencies.
        name_for_email : str
            Name to represent the kernel in the email.
        """
        self.name_for_extracting_revision = name_for_extracting_revision
        self.name_for_email = name_for_email


# All kernels for which the versions are set in the configuration parameters of 2.Dimr_collector_release
KERNELS = [
    KernelData(name_for_extracting_revision="DIMRset_ver", name_for_email="DIMRset"),
    KernelData(name_for_extracting_revision="build.vcs.number", name_for_email="OSS"),
]

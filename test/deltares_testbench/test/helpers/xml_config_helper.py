"""Test helper utilities for XML configuration generation."""

import io
import textwrap
from typing import Optional

from src.config.dependency import Dependency
from src.config.test_case_path import TestCasePath


@staticmethod
def make_test_case_config_xml(
    test_case_path: Optional[TestCasePath] = None,
    dependency: Optional[Dependency] = None,
    reference_value: Optional[str] = "0.0",
    include: Optional[str] = "",
    case_root: Optional[str] = "{server_base_url}/cases",
    reference_root: Optional[str] = "{server_base_url}/references",
) -> io.BytesIO:
    """Make config xml with some default values."""
    # Build `path` element.
    test_case_path = test_case_path or TestCasePath("test/case")
    path_elem = "<path"
    if test_case_path.version is not None:
        path_elem += f' version="{test_case_path.version}"'
    path_elem += f">{test_case_path.path}</path>"

    # Build `dependency` element.
    if dependency:
        dependency_elem = f'<dependency local_dir="{dependency.local_dir}"'
        if dependency.version is not None:
            dependency_elem += f' version="{dependency.version}"'
        dependency_elem += f">{dependency.version}</dependency>"
    else:
        dependency_elem = ""

    text = textwrap.dedent(
        rf"""
        <?xml version="1.0" encoding="iso-8859-1"?>
        <deltaresTestbench_v3 xmlns="http://schemas.deltares.nl/deltaresTestbench_v3"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xmlns:xi="http://www.w3.org/2001/XInclude"
            xsi:schemaLocation="http://schemas.deltares.nl/deltaresTestbench_v3 http://content.oss.deltares.nl/schemas/deltaresTestbench_v3-2.00.xsd">
            <config>
                <credentials>
                    <credential name="deltares">
                        <username></username>
                        <password></password>
                    </credential>
                </credentials>
                <localPaths>
                    <testCasesDir>.\data\cases</testCasesDir>
                    <enginesDir>.\data\engines</enginesDir>
                    <referenceDir>.\data\references_results</referenceDir>
                </localPaths>
                <locations>
                    <location name="dsctestbench-cases">
                        <credential ref="deltares" />
                        <root>{case_root}</root>
                    </location>
                    <location name="dsctestbench-references">
                        <credential ref="deltares" />
                        <root>{reference_root}</root>
                    </location>
                </locations>
            </config>
            <programs>
                <program name="foo">
                    <path>foo.exe</path>
                </program>
            </programs>

            <defaultTestCases>
                <testCase name="default_test_case">
                    <location ref="dsctestbench-cases" type="input">
                        <from>.</from>
                    </location>
                    <location ref="dsctestbench-references" type="reference">
                        <from>win64</from>
                    </location>
                    <maxRunTime>60.0</maxRunTime>
                </testCase>
            </defaultTestCases>
            <testCases>
                <testCase name="run_foo" ref="default_test_case">
                    {path_elem}
                    {dependency_elem}
                    <programs>
                        <program ref="foo"></program>
                    </programs>
                    <maxRunTime>60.0</maxRunTime>
                    <checks>
                        <file name="foo.out" type=".out">
                            <parameters>
                                <parameter name="foo" toleranceRelative="{reference_value}" />
                            </parameters>
                        </file>
                    </checks>
                </testCase>
            </testCases>
            {include}
        </deltaresTestbench_v3>
        """
    )
    return io.BytesIO(text.strip().encode("utf-8"))

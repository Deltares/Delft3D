import glob
import os
import xml.etree.ElementTree as ET
from collections import defaultdict


def find_duplicates(directory):
    testcase_versions = defaultdict(set)
    testcase_files = defaultdict(lambda: defaultdict(set))
    files = glob.glob(os.path.join(directory, "**", "*.xml"), recursive=True)
    for xmlfile in files:
        try:
            tree = ET.parse(xmlfile)
            root = tree.getroot()
            # Handle namespace if present
            ns = ""
            if root.tag.startswith("{"):
                ns = root.tag.split("}")[0] + "}"
            for testcase in root.findall(f".//{ns}testCase"):
                name = testcase.attrib.get("name")
                path_elem = testcase.find(f"{ns}path")
                if name and path_elem is not None:
                    version = path_elem.attrib.get("version", "NO_VERSION")
                    testcase_versions[name].add(version)
                    testcase_files[name][version].add(xmlfile)
        except Exception as e:
            print(f"Error parsing {xmlfile}: {e}")

    # Print duplicates with different versions and their files
    print("Duplicate testCase names with different timestamps:")
    for name, versions in testcase_versions.items():
        if len(versions) > 1:
            print(f"  {name}:")
            for version in sorted(versions):
                files = testcase_files[name][version]
                for f in files:
                    print(f"    version={version} in {f}")


if __name__ == "__main__":
    directory = r"c:\checkouts\delft3d\test\deltares_testbench\configs"  # Change as needed
    find_duplicates(directory)

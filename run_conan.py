import argparse
import platform
import subprocess

recipes = [
    ("cmake/binary", "3.31.12"),
    ("zlib/all", "1.3.2"),
    ("hdf5/all", "1.14.6"),
    ("netcdf/all", "4.9.2"),
    ("netcdf-fortran/4.6.2", "4.6.2"),
    ("json-c/all", "0.17"),
    ("libtiff/all", "4.7.1"),
    ("sqlite3/all", "3.53.0"),
    ("nlohmann_json/all", "3.11.3"),
    ("proj/all", "9.3.1"),
    ("libgeotiff/all", "1.7.1"),
    ("gdal/post_3.5.0", "3.12.1"),
    ("expat/all", "2.8.0"),
]

def export_package(recipe_name, version):
    cmd = ["conan", "export", f"conan/recipes/{recipe_name}", f"--version={version}"]
    result = subprocess.run(cmd)
    if result.returncode != 0:
        raise RuntimeError(f"Conan export failed with exit code {result.returncode}")

def clean_conan_cache():
    remove_cmd = ["conan", "remove", "*", "--confirm"]
    remove_result = subprocess.run(remove_cmd)
    if remove_result.returncode != 0:
        raise RuntimeError(f"Conan cache remove failed with exit code {remove_result.returncode}")

    clean_cmd = ["conan", "cache", "clean"]
    clean_result = subprocess.run(clean_cmd)
    if clean_result.returncode != 0:
        raise RuntimeError(f"Conan cache clean failed with exit code {clean_result.returncode}")

def conan_install(profile, output_folder):
    cmd = [
        "conan",
        "install",
        ".",
        f"--profile=conan/profiles/{profile}",
        "--build=missing",
        "--no-remote",
        f"--output-folder={output_folder}",
    ]
    result = subprocess.run(cmd)
    if result.returncode != 0:
        raise RuntimeError(f"Conan install failed with exit code {result.returncode}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Installs conan-managed dependencies for the Delft3D repository.")
    parser.add_argument("-c", "--configuration", choices=["debug", "release"], required=True, help="Build configuration to use for Conan install.")
    parser.add_argument("--clean", action="store_true", help="Clean the local Conan cache before exporting and installing.")
    parser.add_argument("--output-folder", default="build/conan", help="Output folder for Conan install files.")
    args = parser.parse_args()

    os_name = platform.system()
    if os_name == "Windows":
        os_key = "windows"
    elif os_name == "Linux":
        os_key = "linux"
    else:
        raise RuntimeError(f"Unsupported OS: {os_name}")

    profile = f"{os_key}_{args.configuration}"

    # Clean conan cache if requested by the user
    if args.clean:
        clean_conan_cache()

    # Export all recipes before installing to ensure they are available in the local cache
    for recipe_name, version in recipes:
        export_package(recipe_name, version)

    # Install dependencies using the specified profile
    conan_install(profile, args.output_folder)

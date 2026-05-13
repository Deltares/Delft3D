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
    subprocess.run(cmd, check=True)

def clean_conan_cache():
    remove_cmd = ["conan", "remove", "*", "--confirm"]
    subprocess.run(remove_cmd, check=True)

    clean_cmd = ["conan", "cache", "clean"]
    subprocess.run(clean_cmd, check=True)

def conan_install(profile, output_folder, build_type, consumer_build_type=None):
    cmd = [
        "conan",
        "install",
        ".",
        f"--profile:all=./conan/profiles/{profile}",
        "-s:a",
        f"build_type={build_type}",
        "--build=missing",
        "--no-remote",
        f"--output-folder={output_folder}",
    ]

    if consumer_build_type:
        cmd.extend(["-s", f"&:build_type={consumer_build_type}"]) # Odd syntax explained here: https://github.com/conan-io/conan/issues/13478#issuecomment-1475389368

    subprocess.run(cmd, check=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Installs conan-managed dependencies for the Delft3D repository.")
    parser.add_argument("--clean", action="store_true", help="Clean the local Conan cache before exporting and installing.")
    parser.add_argument("--output-folder", default="build/conan", help="Output folder for Conan install files.")
    args = parser.parse_args()

    os_name = platform.system()
    if os_name == "Windows":
        profile = "delft3d_windows"
    elif os_name == "Linux":
        profile = "delft3d_linux"
    else:
        raise RuntimeError(f"Unsupported OS: {os_name}")

    # Clean conan cache if requested by the user
    if args.clean:
        clean_conan_cache()

    # Export all recipes before installing to ensure they are available in the local cache
    for recipe_name, version in recipes:
        export_package(recipe_name, version)

    # Install dependencies and generate CMakeDeps metadata for Debug, Release and RelWithDebInfo
    # Note that they effectively they all use release binaries
    conan_install(profile, args.output_folder, "Release")
    conan_install(profile, args.output_folder, "Release", consumer_build_type="Debug")
    conan_install(profile, args.output_folder, "Release", consumer_build_type="RelWithDebInfo")

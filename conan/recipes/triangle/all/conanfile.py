from conan import ConanFile
from conan.errors import ConanInvalidConfiguration
from conan.tools.cmake import CMake, CMakeToolchain, cmake_layout
from conan.tools.files import copy, load, save
from conan.tools.scm import Git
import os

required_conan_version = ">=2"

class TriangleConan(ConanFile):
    name = "triangle"
    license = "Custom"
    description = "A Two-Dimensional Quality Mesh Generator and Delaunay Triangulator."
    url = "https://github.com/Deltares/Delft3D"
    homepage = "https://www.cs.cmu.edu/~quake/triangle.html"
    topics = ("triangle", "delaunay", "triangulation", "mesh")
    package_type = "library"
    settings = "os", "arch", "compiler", "build_type"
    options = {"shared": [True, False]}
    default_options = {"shared": False}

    def validate(self):
        if self.options.shared:
            raise ConanInvalidConfiguration("triangle does not support shared libraries")

    def layout(self):
        cmake_layout(self)

    def source(self):
        source = self.conan_data["sources"][self.version]
        git = Git(self)
        git.clone(
            source["url"],
            target=self.source_folder,
            args=["--filter=blob:none", "--sparse"],
        )
        git.checkout(source["commit"])
        git.run("sparse-checkout set src/third_party_open/triangle")

    @property
    def _triangle_source_folder(self):
        return os.path.join(self.source_folder, "src", "third_party_open", "triangle")

    def generate(self):
        tc = CMakeToolchain(self)
        tc.generate()

    def build(self):
        cmake = CMake(self)
        cmake.configure(build_script_folder=self._triangle_source_folder)
        cmake.build()

    def _extract_license(self):
        return load(
            self,
            os.path.join(self._triangle_source_folder, "triangle_license_info.txt"),
        )

    def package(self):
        save(self, os.path.join(self.package_folder, "licenses", "LICENSE"), self._extract_license())
        copy(self, "triangle.h", src=self._triangle_source_folder, dst=os.path.join(self.package_folder, "include"))
        copy(self, "*.a", src=self.build_folder, dst=os.path.join(self.package_folder, "lib"), keep_path=False)
        copy(self, "*.lib", src=self.build_folder, dst=os.path.join(self.package_folder, "lib"), keep_path=False)

    def package_info(self):
        self.cpp_info.set_property("cmake_target_name", "triangle::triangle")
        self.cpp_info.libs = ["triangle"]

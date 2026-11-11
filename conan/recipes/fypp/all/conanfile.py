import os

from conan import ConanFile
from conan.tools.files import copy, get, save

required_conan_version = ">=2"


class FyppConan(ConanFile):
    name = "fypp"
    package_type = "application"
    license = "BSD-2-Clause"
    homepage = "https://github.com/aradi/fypp"
    url = "https://pypi.org/project/fypp/"
    description = "Python-powered preprocessor for Fortran"
    topics = ("fortran", "preprocessor")

    settings = "os"

    def source(self):
        get(self, **self.conan_data["sources"][self.version], strip_root=True)

    def package(self):
        bindir = os.path.join(self.package_folder, "bin")
        copy(self, "fypp", src=os.path.join(self.source_folder, "bin"), dst=bindir)
        if self.settings.os == "Windows":
            save(
                self,
                os.path.join(bindir, "fypp.cmd"),
                '@echo off\r\npython "%~dp0fypp" %*\r\n',
            )
        copy(
            self,
            "LICENSE.txt",
            src=self.source_folder,
            dst=os.path.join(self.package_folder, "licenses"),
        )

    def package_info(self):
        self.buildenv_info.prepend_path("PATH", os.path.join(self.package_folder, "bin"))

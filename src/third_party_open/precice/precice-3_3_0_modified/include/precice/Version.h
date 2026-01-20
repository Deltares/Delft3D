#pragma once

// clang-format off

#define PRECICE_VERSION_MAJOR 3
#define PRECICE_VERSION_MINOR 3
#define PRECICE_VERSION_PATCH 0

#define PRECICE_VERSION "3.3.0"

// clang-format on

#define PRECICE_VERSION_GREATER_EQUAL(major, minor, patch) (             \
    (PRECICE_VERSION_MAJOR > major) ||                                   \
    (PRECICE_VERSION_MAJOR == major && PRECICE_VERSION_MINOR > minor) || \
    (PRECICE_VERSION_MAJOR == major && PRECICE_VERSION_MINOR == minor && PRECICE_VERSION_PATCH >= patch))

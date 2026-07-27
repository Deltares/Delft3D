# Build
Download sources (this version: 1.90.0)

Build steps:
    bootstrap vc143
    b2 --prefix=<PREFIX>\src\third_party_open\boost\boost_1_90_0 --build-dir=build variant=release link=shared
    b2 install --prefix=<PREFIX>\src\third_party_open\boost\boost_1_90_0 --build-dir=build variant=release link=shared

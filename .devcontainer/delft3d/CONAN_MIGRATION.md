# Migrating to Conan in devcontainer

If you already have a .devcontainer then migrating to conan is relatively simple.

Here are a migration steps

1. Close the devcontainer VSCode
2. Remove de devcontainer docker image
3. Re-open VSCode. It will take awhile to re-download and rebuild de dev-container
4. By default, CMake extension runs cmake --configure. This will initally fail (with some missing ZLIB)
5. Configure Conan Nexus.
```bash
# One-time Conan setup (Deltares developers)
python run_conan.py initialize deltares
```
Next, visit the [user token page](https://internal-artifacts.deltares.nl/#user/usertoken) on Nexus.
You will be asked to log in with your Deltares credentials.
On this page you will be able to create a "User Token". This token consists of an 'id' and a 'secret'. You will only
be able to see these values right after you create the token. If you lose them you can reset the token, but you will
only be able to login with the new token, and not with the old one anymore. Create a file called `credentials.json` in the
conan cache directory (usually `~/.conan2`) with the following content
(replace `NEXUS_TOKEN_ID` and `NEXUS_TOKEN_SECRET`):
```json
{
    "credentials": [
        {
            "remote": "delft3d-conan-dev",
            "user": "NEXUS_TOKEN_ID",
            "password": "NEXUS_TOKEN_SECRET"
        },
        {
            "remote": "deltares-conan-center-proxy",
            "user": "NEXUS_TOKEN_ID",
            "password": "NEXUS_TOKEN_SECRET"
        }
    ]
}
```
6. Now you are ready to run the buid. First you need to build using the ```build.py``` script.

```bash
python build.py
```
This will invoke ```conan``` and download all dependencies

7. Now everything is built. You can now re-build using only cmake (configure build)

param (
    [string]$source,
    [string]$destination
)

if (-not (Test-Path -Path $source)) {
    Write-Output "Error: The specified source $source '$source' does not exist."
    exit 1
}

if (-not (Get-Item -Path $source).PSIsContainer) {
    Write-Output "Error: The specified source $source '$source' is not a directory."
    exit 1
}

if (-not (Test-Path -Path $destination)) {
    Write-Output "The specified destination path '$destination' does not exist. Creating it."
    New-Item -Path $destination -ItemType Directory | Out-Null
}

if (-not (Get-Item -Path $destination).PSIsContainer) {
    Write-Output "Error: The specified destination path '$destination' is not a directory."
    exit 1
}

$source = (Get-Item -Path $source).FullName
$files = Get-ChildItem -Path $source -Recurse -Include *.exe,*.dll | Where-Object { -not $_.PSIsContainer }

foreach ($file in $files) {
    try {
        # Check if the file is signed
        $signature = Get-AuthenticodeSignature -FilePath $file.FullName
        if ($signature.Status -eq 'Valid') {
            # Calculate the relative path of the file with respect to the source directory
            $relativePath = $file.FullName.Substring($source.Length).TrimStart('\')
            # Create the corresponding directory structure in the destination directory
            $destinationPath = Join-Path -Path $destination -ChildPath $relativePath
            $destinationDir = [System.IO.Path]::GetDirectoryName($destinationPath)
            if (-not (Test-Path -Path $destinationDir)) {
                New-Item -Path $destinationDir -ItemType Directory -Force | Out-Null
            }
            # Move the signed file to the destination directory
            Move-Item -Path $file.FullName -Destination $destinationPath -Force
            Write-Output "Moved signed file: $($file.FullName) to $destinationPath"
        }
    } catch {
        Write-Output "Error processing file: $($file.FullName) - $($_.Exception.Message)"
    }
}
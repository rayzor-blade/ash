# Ash installer for Windows — https://github.com/rayzor-blade/ash
#
#   irm https://raw.githubusercontent.com/rayzor-blade/ash/main/install.ps1 | iex
#
# Downloads the `ash` binary and its bundled DLLs into ~/.ash/bin
# and adds that directory to your User PATH. Ash requires a 64-bit target.

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue' # Speeds up Invoke-WebRequest significantly

if (-not [Environment]::Is64BitOperatingSystem) {
    Write-Error "error: unsupported platform (ash requires a 64-bit target)"
    exit 1
}

$Repo = "rayzor-blade/ash"
$Dest = if ($env:ASH_INSTALL_DIR) { $env:ASH_INSTALL_DIR } else { Join-Path $HOME ".ash\bin" }
$Target = "windows-x86_64"

$Asset = "ash-${Target}.tar.gz"
$Url = "https://github.com/$Repo/releases/latest/download/$Asset"
$Fallback = "https://github.com/$Repo/releases/download/nightly/$Asset"

$TmpDir = Join-Path ([System.IO.Path]::GetTempPath()) ([Guid]::NewGuid().ToString())
New-Item -ItemType Directory -Path $TmpDir | Out-Null
$TmpFile = Join-Path $TmpDir $Asset

try {
    Write-Host "downloading $Asset ..."
    try {
        Invoke-WebRequest -Uri $Url -OutFile $TmpFile -ErrorAction Stop
    } catch {
        Write-Host "latest release has no $Asset; trying the nightly build ..."
        try {
            Invoke-WebRequest -Uri $Fallback -OutFile $TmpFile -ErrorAction Stop
        } catch {
            Write-Error "error: no prebuilt binary available for $Target yet"
            exit 1
        }
    }

    if (-not (Test-Path $Dest)) {
        New-Item -ItemType Directory -Path $Dest -Force | Out-Null
    }

    # Windows 10/11 ships with a native tar.exe, which cleanly unpacks .tar.gz
    & tar.exe xzf "$TmpFile" -C "$Dest"
    if ($LASTEXITCODE -ne 0) {
        throw "tar extraction failed with exit code $LASTEXITCODE"
    }

    $AshExe = Join-Path $Dest "ash.exe"

    # A quick sanity run; missing DLLs show up here, not later.
    try {
        & $AshExe --help 2>&1 | Out-Null
        if ($LASTEXITCODE -ne 0) { throw }
    } catch {
        Write-Host "warning: $AshExe did not run cleanly. Missing DLLs?" -ForegroundColor Yellow
    }

    # Check and update the User PATH
    $UserPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    $PathArray = if ($UserPath) { $UserPath -split ';' } else { @() }
    
    $IsOnPath = $false
    foreach ($p in $PathArray) {
        if ($p.TrimEnd('\') -eq $Dest.TrimEnd('\')) {
            $IsOnPath = $true
            break
        }
    }

    Write-Host ""
    Write-Host "installed: $AshExe"
    & $AshExe --help 2>$null | Select-Object -First 3

    if (-not $IsOnPath) {
        # Update permanent User registry path
        $NewUserPath = if ($UserPath) { "$UserPath;$Dest" } else { $Dest }
        [Environment]::SetEnvironmentVariable("PATH", $NewUserPath, "User")
        
        # Update current session path
        $env:PATH = "$env:PATH;$Dest"

        Write-Host ""
        Write-Host "----------------------------------------------------------------"
        Write-Host "ash has been added to your PATH."
        Write-Host "It is available in this PowerShell session immediately."
        Write-Host "For other terminals (like CMD or existing VS Code windows),"
        Write-Host "you may need to restart them to pick up the new PATH."
        Write-Host ""
        Write-Host "Try:  ash --mode hybrid main.hl"
        Write-Host "----------------------------------------------------------------"
    } else {
        Write-Host ""
        Write-Host "ash is already on your PATH. Try:  ash --mode hybrid main.hl"
    }

} finally {
    if (Test-Path $TmpDir) {
        Remove-Item -Recurse -Force $TmpDir
    }
}
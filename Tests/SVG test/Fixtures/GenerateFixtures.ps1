# GenerateFixtures.ps1
# Iterates over Tests\SVG test\Data\*\*.svg and generates template .test fixture files
# into Tests\SVG test\Fixtures\Stage1\*\*.test relative to the script location.

param (
    [string]$DataPath,
    [string]$FixturesPath
)

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

if (-not $DataPath) {
    $DataPath = [System.IO.Path]::GetFullPath((Join-Path $scriptDir "..\Data"))
} else {
    $DataPath = [System.IO.Path]::GetFullPath($DataPath)
}

if (-not $FixturesPath) {
    $FixturesPath = [System.IO.Path]::GetFullPath($scriptDir)
} else {
    $FixturesPath = [System.IO.Path]::GetFullPath($FixturesPath)
}

if (-not (Test-Path -Path $DataPath)) {
    Write-Host "Data directory not found: $DataPath"
    Exit 0
}

if (-not (Test-Path -Path $FixturesPath)) {
    New-Item -ItemType Directory -Path $FixturesPath -Force | Out-Null
}

$dataUri = New-Object System.Uri(($DataPath.TrimEnd('\', '/') + '\'))

Get-ChildItem -Path $DataPath -Filter "*.svg" -Recurse | ForEach-Object {
    $fileUri = New-Object System.Uri($_.FullName)
    $relativePath = [System.Uri]::UnescapeDataString($dataUri.MakeRelativeUri($fileUri).ToString()).Replace('/', '\')

    $relativeTestPath = [System.IO.Path]::ChangeExtension($relativePath, ".test")
    $targetTestPath = [System.IO.Path]::Combine($FixturesPath, $relativeTestPath)
    $targetDir = [System.IO.Path]::GetDirectoryName($targetTestPath)

    if ($targetDir -and -not (Test-Path -Path $targetDir)) {
        New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
    }

    $svgContent = Get-Content -Path $_.FullName -Raw
    $fixtureContent = "--- SVG ---`n$svgContent`n--- EXPECTED AST ---`n"

    if ($targetTestPath) {
        Set-Content -Path $targetTestPath -Value $fixtureContent -Encoding UTF8
        Write-Host "Created fixture: $targetTestPath"
    }
}

$ErrorActionPreference = 'Stop'
$staging = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot 'legend_staging'))
$project = Split-Path $PSScriptRoot -Parent
$destination = [System.IO.Path]::GetFullPath((Join-Path $project 'output/global_H2_Transport_Global_2611061840_2020_2050_vs_2610281619'))
$files = @(Get-ChildItem -LiteralPath $staging -Recurse -File -Filter '*.png')
if ($files.Count -ne 20) { throw "Expected 20 staged maps; found $($files.Count)." }
$plan = foreach ($file in $files) {
    $relative = [System.IO.Path]::GetRelativePath($staging, $file.FullName)
    $target = [System.IO.Path]::GetFullPath((Join-Path $destination $relative))
    if (-not $target.StartsWith($destination + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)) {
        throw 'Target is outside the figure output directory.'
    }
    if (-not (Test-Path -LiteralPath $target -PathType Leaf)) { throw "Missing original map: $target" }
    [pscustomobject]@{ Source = $file.FullName; Target = $target }
}
foreach ($item in $plan) {
    $backup = Join-Path $staging ('previous_' + [guid]::NewGuid().ToString() + '.png')
    [System.IO.File]::Replace($item.Source, $item.Target, $backup)
    Remove-Item -LiteralPath $backup
}
Write-Output "Atomically replaced $($plan.Count) network maps; temporary PNGs removed."

Write-Host "Start Clean-Build-Clean Process" -ForegroundColor Blue

cls
.\ScriptPSClean.ps1
.\ScriptPSBuild.ps1
.\ScriptPSClean.ps1

Write-Host "End Clean-Build-Clean Process" -ForegroundColor Blue
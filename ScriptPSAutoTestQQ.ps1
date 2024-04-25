cls

Write-Host "Start Testing Process" -ForegroundColor Blue
.\ScriptPSClean.ps1
.\ScriptPSBuild.ps1

Write-Host "Start Testing PR1" -ForegroundColor Blue
.\GQL .\queries\pr1.gql
Write-Host "End Testing PR1" -ForegroundColor Blue

Write-Host "Start Testing PR2" -ForegroundColor Blue
.\GQL .\queries\pr2.gql
Write-Host "End Testing PR2" -ForegroundColor Blue

Write-Host "Start Testing PR3" -ForegroundColor Blue
.\GQL .\queries\pr3.gql
Write-Host "End Testing PR3" -ForegroundColor Blue

Write-Host "Start Testing PR4" -ForegroundColor Blue
.\GQL .\queries\pr4.gql
Write-Host "End Testing PR4" -ForegroundColor Blue

Write-Host "Start Testing PR5" -ForegroundColor Blue
.\GQL .\queries\pr5.gql
Write-Host "End Testing PR5" -ForegroundColor Blue

Write-Host "Start Cleaning Process" -ForegroundColor Blue
.\ScriptPSClean.ps1
Write-Host "End Cleaning Process" -ForegroundColor Blue

Write-Host "End Testing Process" -ForegroundColor Blue
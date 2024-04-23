Write-Host "Start Testing Process" -ForegroundColor Blue
.\ScriptClean.ps1
.\ScriptBuild.ps1

Write-Host "Start Testing PR1" -ForegroundColor Blue
.\GQL .\queries\pr1.gql
Write-Host "End Testing PR1" -ForegroundColor Blue

Write-Host "Start Testing PR2" -ForegroundColor Blue
.\GQL .\queries\pr2.gql
Write-Host "End Testing PR2" -ForegroundColor Blue

Write-Host "Start Testing PR3" -ForegroundColor Blue
.\GQL .\queries\pr3.gql
Write-Host "End Testing PR3" -ForegroundColor Blue

.\ScriptClean.ps1

Write-Host "End Testing Process" -ForegroundColor Blue
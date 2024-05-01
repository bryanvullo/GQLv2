cls

Write-Host "Start Testing Process" -ForegroundColor Blue
.\ScriptPSClean.ps1
.\ScriptPSBuild.ps1

Write-Host "Start Testing PR1" -ForegroundColor Blue
.\GQL pr1.gql
Write-Host "End Testing PR1" -ForegroundColor Blue

Write-Host "Start Testing PR2" -ForegroundColor Blue
.\GQL pr2.gql
Write-Host "End Testing PR2" -ForegroundColor Blue

Write-Host "Start Testing PR3" -ForegroundColor Blue
.\GQL pr3.gql
Write-Host "End Testing PR3" -ForegroundColor Blue

Write-Host "Start Testing PR4" -ForegroundColor Blue
.\GQL pr4.gql
Write-Host "End Testing PR4" -ForegroundColor Blue

Write-Host "Start Testing PR5" -ForegroundColor Blue
.\GQL pr5.gql
Write-Host "End Testing PR5" -ForegroundColor Blue

Write-Host "Start Testing PR6" -ForegroundColor Blue
.\GQL pr6.gql
Write-Host "End Testing PR6" -ForegroundColor Blue

Write-Host "Start Testing PR7" -ForegroundColor Blue
.\GQL pr7.gql
Write-Host "End Testing PR7" -ForegroundColor Blue

Write-Host "Start Testing PR8" -ForegroundColor Blue
.\GQL pr8.gql
Write-Host "End Testing PR8" -ForegroundColor Blue

Write-Host "Start Testing PR9" -ForegroundColor Blue
.\GQL pr9.gql
Write-Host "End Testing PR9" -ForegroundColor Blue

Write-Host "Start Testing PR10" -ForegroundColor Blue
.\GQL pr10.gql
Write-Host "End Testing PR10" -ForegroundColor Blue

Write-Host "Start Testing PR11TEST" -ForegroundColor Blue
.\GQL pr11TEST.gql
Write-Host "End Testing PR11TEST" -ForegroundColor Blue

Write-Host "Start Cleaning Process" -ForegroundColor Blue
.\ScriptPSClean.ps1
Write-Host "End Cleaning Process" -ForegroundColor Blue

Write-Host "End Testing Process" -ForegroundColor Blue
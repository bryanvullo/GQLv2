# Clean script for Haskell project

# Print a header
Write-Host "Starting clean process..." -ForegroundColor Green

# Remove generated Haskell source files
Write-Host "Removing generated Haskell source files..."
Remove-Item -Path .\Lexer.hs -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\Parser.hs -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputLexer.hs -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputParser.hs -Force -ErrorAction SilentlyContinue
Write-Host "Generated Haskell source files removed." -ForegroundColor Yellow

# Remove generated object files
Write-Host "Removing generated object files..."
Remove-Item -Path .\Lexer.o -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\Parser.o -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputLexer.o -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputParser.o -Force -ErrorAction SilentlyContinue
Write-Host "Generated object files removed." -ForegroundColor Yellow

# Remove other generated files (if any)
Write-Host "Removing other generated files..."
Remove-Item -Path .\Lexer.hi -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\Parser.hi -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputLexer.hi -Force -ErrorAction SilentlyContinue
Remove-Item -Path .\InputParser.hi -Force -ErrorAction SilentlyContinue
Write-Host "Other generated files removed." -ForegroundColor Yellow

# Print a footer
Write-Host "Clean process completed." -ForegroundColor Green
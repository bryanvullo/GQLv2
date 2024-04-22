# Build script for Haskell project

# Print a header
Write-Host "Starting build process..." -ForegroundColor Green

# Run alex on Lexer.x
Write-Host "Running alex on Lexer.x..."
stack exec alex -- .\Lexer.x
Write-Host "alex on Lexer.x completed." -ForegroundColor Yellow

# Run happy on Parser.y
Write-Host "Running happy on Parser.y..."
stack exec happy -- .\Parser.y
Write-Host "happy on Parser.y completed." -ForegroundColor Yellow

# Run alex on InputLexer.x
Write-Host "Running alex on InputLexer.x..."
stack exec alex -- .\InputLexer.x
Write-Host "alex on InputLexer.x completed." -ForegroundColor Yellow

# Run happy on InputParser.y
Write-Host "Running happy on InputParser.y..."
stack exec happy -- .\InputParser.y
Write-Host "happy on InputParser.y completed." -ForegroundColor Yellow

# Compile the project
Write-Host "Compiling the project..."
ghc .\GQL.hs
Write-Host "Project compilation completed." -ForegroundColor Yellow

# Print a footer
Write-Host "Build process completed." -ForegroundColor Green
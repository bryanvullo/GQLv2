#!/bin/bash

# Print a header
echo -e "\e[32mStarting clean process...\e[0m"

# Remove generated Haskell source files
echo "Removing generated Haskell source files..."
rm -f Lexer.hs Parser.hs InputLexer.hs InputParser.hs
echo -e "\e[33mGenerated Haskell source files removed.\e[0m"

# Remove generated object files
echo "Removing generated object files..."
rm -f Lexer.o Parser.o InputLexer.o InputParser.o GQL.o
echo -e "\e[33mGenerated object files removed.\e[0m"

# Remove other generated files (if any)
echo "Removing other generated files..."
rm -f Lexer.hi Parser.hi InputLexer.hi InputParser.hi InputParser.info GQL.hi GQL.exe
echo -e "\e[33mOther generated files removed.\e[0m"

# Print a footer
echo -e "\e[32mClean process completed.\e[0m"

# Print a header
echo -e "\e[32mStarting build process...\e[0m"

# Run alex on Lexer.x
echo "Running alex on Lexer.x..."
stack exec alex -- -o Lexer.hs ./Lexer.x
echo -e "\e[33malex on Lexer.x completed.\e[0m"

# Run happy on Parser.y
echo "Running happy on Parser.y..."
stack exec happy -- -o Parser.hs ./Parser.y
echo -e "\e[33mhappy on Parser.y completed.\e[0m"

# Run alex on InputLexer.x
echo "Running alex on InputLexer.x..."
stack exec alex -- -o InputLexer.hs ./InputLexer.x
echo -e "\e[33malex on InputLexer.x completed.\e[0m"

# Run happy on InputParser.y
echo "Running happy on InputParser.y..."
stack exec happy -- -o InputParser.hs ./InputParser.y
echo -e "\e[33mhappy on InputParser.y completed.\e[0m"

# Compile the project
echo "Compiling the project..."
ghc ./GQL.hs
echo -e "\e[33mProject compilation completed.\e[0m"

# Print a footer
echo -e "\e[32mBuild process completed.\e[0m"

echo -e "\e[34mStart Testing Process\e[0m"
./ScriptClean.sh
./ScriptBuild.sh

echo -e "\e[34mStart Testing PR1\e[0m"
./GQL ./queries/pr1.gql
echo -e "\e[34mEnd Testing PR1\e[0m"

echo -e "\e[34mStart Testing PR2\e[0m"
./GQL ./queries/pr2.gql
echo -e "\e[34mEnd Testing PR2\e[0m"

echo -e "\e[34mStart Testing PR3\e[0m"
./GQL ./queries/pr3.gql
echo -e "\e[34mEnd Testing PR3\e[0m"

./ScriptClean.sh

echo -e "\e[34mEnd Testing Process\e[0m"

# Usage:
# chmod +x ScriptBash.sh
# ./ScriptBash.sh
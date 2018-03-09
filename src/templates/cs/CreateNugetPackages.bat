: This script creates .nupkg files for each .nuspec. Ensure that nuget.exe (v3.5 or newer) 
: is either in the PATH or place it in this folder.
for %%i in (*.nuspec) do nuget pack %%i
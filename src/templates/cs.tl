# C# template listing

ONCE            cs/LMCP.sln                         ./LMCP.sln

ONCE            cs/LMCP.csproj                      Library/LMCP.csproj
ONCE            cs/LmcpCore.csproj                  Library/LmcpCore.csproj
ONCE            cs/LibraryAssemblyInfo.cs           Library/Properties/AssemblyInfo.cs
ONCE            cs/ILmcpObject.cs                   Library/Avtas/Lmcp/ILmcpObject.cs
ONCE            cs/LmcpFactory.cs                   Library/Avtas/Lmcp/LmcpFactory.cs
ONCE            cs/LmcpCoreFactory.cs               Library/Avtas/Lmcp/LmcpCoreFactory.cs
ONCE            cs/LmcpFactoryException.cs          Library/Avtas/Lmcp/LmcpFactoryException.cs
ONCE            cs/LmcpBinaryReader.cs              Library/Avtas/Lmcp/LmcpBinaryReader.cs
ONCE            cs/LmcpBinaryWriter.cs              Library/Avtas/Lmcp/LmcpBinaryWriter.cs
ONCE            cs/LmcpBinaryProcessor.cs           Library/Avtas/Lmcp/LmcpBinaryProcessor.cs
ONCE            cs/LmcpObjectReceivedEventArgs.cs   Library/Avtas/Lmcp/LmcpObjectReceivedEventArgs.cs
ONCE            cs/LmcpXmlReader.cs                 Library/Avtas/Lmcp/LmcpXmlReader.cs
ONCE            cs/LmcpCoreXmlReader.cs             Library/Avtas/Lmcp/LmcpCoreXmlReader.cs
ONCE            cs/LmcpXmlWriter.cs                 Library/Avtas/Lmcp/LmcpXmlWriter.cs
ONCE            cs/ISeriesList.cs                   Library/Avtas/Lmcp/ISeriesList.cs
ONCE            cs/CreateNugetPackages.bat          Library/CreateNugetPackages.bat

ONCE            cs/TestClient.csproj                TestClient/TestClient.csproj
ONCE            cs/TestClient.cs                    TestClient/Program.cs
ONCE            cs/TestClientAssemblyInfo.cs        TestClient/Properties/AssemblyInfo.cs
ONCE            cs/TestServer.csproj                TestServer/TestServer.csproj
ONCE            cs/TestServer.cs                    TestServer/Program.cs
ONCE            cs/TestServerAssemblyInfo.cs        TestServer/Properties/AssemblyInfo.cs

PER_MDM         cs/SeriesList.cs                    Library/-<namespace>-/SeriesList.cs
PER_MDM         cs/SeriesEnum.cs                    Library/-<namespace>-/SeriesEnum.cs
PER_MDM         cs/SeriesXmlReader.cs               Library/-<namespace>-/SeriesXmlReader.cs
PER_MDM         cs/SeriesProj.csproj                Library/Lmcp-<cs_series_name>-.csproj
PER_MDM         cs/SeriesNuget.nuspec               Library/-<cs_series_name>-.nuspec

PER_STRUCT      cs/SeriesObject.cs                  Library/-<namespace>-/-<datatype_name>-.cs

PER_ENUM        cs/Enum.cs                          Library/-<namespace>-/-<enum_name>-.cs
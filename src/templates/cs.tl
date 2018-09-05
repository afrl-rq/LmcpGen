# C# template listing

ONCE            cs/LMCP.sln                         ./LMCP.sln

ONCE            cs/LmcpCore.csproj                  Library/LMCP.csproj
ONCE            cs/LmcpCore.csproj                  Library/LmcpCore/LmcpCore.csproj
ONCE            cs/LibraryAssemblyInfo.cs           Library/LmcpCore/Properties/AssemblyInfo.cs
ONCE            cs/ILmcpObject.cs                   Library/LmcpCore/Avtas/Lmcp/ILmcpObject.cs
ONCE            cs/LmcpCoreFactory.cs               Library/LmcpCore/Avtas/Lmcp/LmcpCoreFactory.cs
ONCE            cs/LmcpCoreFactory.cs               Library/LmcpCore/Avtas/Lmcp/LmcpFactory.cs
ONCE            cs/LmcpFactoryException.cs          Library/LmcpCore/Avtas/Lmcp/LmcpFactoryException.cs
ONCE            cs/LmcpBinaryReader.cs              Library/LmcpCore/Avtas/Lmcp/LmcpBinaryReader.cs
ONCE            cs/LmcpBinaryWriter.cs              Library/LmcpCore/Avtas/Lmcp/LmcpBinaryWriter.cs
ONCE            cs/LmcpBinaryProcessor.cs           Library/LmcpCore/Avtas/Lmcp/LmcpBinaryProcessor.cs
ONCE            cs/LmcpObjectReceivedEventArgs.cs   Library/LmcpCore/Avtas/Lmcp/LmcpObjectReceivedEventArgs.cs
ONCE            cs/LmcpCoreXmlReader.cs             Library/LmcpCore/Avtas/Lmcp/LmcpCoreXmlReader.cs
ONCE            cs/LmcpCoreXmlReader.cs             Library/LmcpCore/Avtas/Lmcp/LmcpXmlReader.cs
ONCE            cs/LmcpXmlWriter.cs                 Library/LmcpCore/Avtas/Lmcp/LmcpXmlWriter.cs
ONCE            cs/ISeriesList.cs                   Library/LmcpCore/Avtas/Lmcp/ISeriesList.cs
ONCE            cs/CreateNugetPackages.bat          Library/CreateNugetPackages.bat

ONCE            cs/TestClient.csproj                TestClient/TestClient.csproj
ONCE            cs/TestClient.cs                    TestClient/Program.cs
ONCE            cs/TestClientAssemblyInfo.cs        TestClient/Properties/AssemblyInfo.cs
ONCE            cs/TestServer.csproj                TestServer/TestServer.csproj
ONCE            cs/TestServer.cs                    TestServer/Program.cs
ONCE            cs/TestServerAssemblyInfo.cs        TestServer/Properties/AssemblyInfo.cs

PER_MDM         cs/SeriesList.cs                    Library/-<cs_series_name>-/-<namespace>-/SeriesList.cs
PER_MDM         cs/SeriesEnum.cs                    Library/-<cs_series_name>-/-<namespace>-/SeriesEnum.cs
PER_MDM         cs/SeriesXmlReader.cs               Library/-<cs_series_name>-/-<namespace>-/SeriesXmlReader.cs
PER_MDM         cs/SeriesProj.csproj                Library/-<cs_series_name>-/Lmcp-<cs_series_name>-.csproj
PER_MDM         cs/SeriesNuget.nuspec               Library/-<cs_series_name>-/-<cs_series_name>-.nuspec

PER_STRUCT      cs/SeriesObject.cs                  Library/-<cs_series_name>-/-<namespace>-/-<datatype_name>-.cs

PER_ENUM        cs/Enum.cs                          Library/-<cs_series_name>-/-<namespace>-/-<enum_name>-.cs
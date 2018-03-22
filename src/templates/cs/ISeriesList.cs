using Avtas.Lmcp;

namespace Avtas.Lmcp
{
  public interface ISeriesList
  {
    long SeriesId { get; }
    ILmcpObject GetInstance( uint object_type, ushort series_version );
  }
}

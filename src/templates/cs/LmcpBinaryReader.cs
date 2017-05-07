// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

using System;
using System.IO;
using System.Text;

namespace Avtas.Lmcp
{
  /// <summary>
  /// A <see cref="BinaryWriter"/> that writes integer values in either big- or little-endian format
  /// and handles LMCP-style strings.
  /// </summary>
  public class LmcpBinaryReader : BinaryReader
  {
    #region Fields

    private Endianness endianness;

    #endregion

    #region Constructors

    /// <summary>
    /// Initializes a new instance of <c>LmcpBinaryReader</c> with the supplied stream.
    /// </summary>
    /// <param name="input">A stream.</param>
    public LmcpBinaryReader(Stream input)
      : base(input)
    {
      this.endianness = Endianness.BigEndian;
    }

    /// <summary>
    /// Initializes a new instance of <c>LmcpBinaryReader</c> with the supplied stream.
    /// </summary>
    /// <param name="input">A byte array.</param>
    public LmcpBinaryReader(byte[] input)
      : base(new MemoryStream(input))
    {
      this.endianness = Endianness.BigEndian;
    }

    #endregion

    #region ReadIntXX Methods

    /// <summary>
    /// Reads a 2-byte signed integer from the current stream and advances the current position of the stream by two bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadInt16"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 2-byte signed integer read from the current stream.</returns>
    public override short ReadInt16()
    {
      short val = base.ReadInt16();
      if (this.endianness == Endianness.BigEndian)
      {
        // reverse the byte order
        return (short)((short)(val << 8) | (short)((val >> 8) & 0x00FF));
      }
      else
      {
        return val;
      }
    }

    /// <summary>
    /// Reads a 4-byte signed integer from the current stream and advances the current position of the stream by four bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadInt32"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte signed integer read from the current stream.</returns>
    public override int ReadInt32()
    {
      int val = base.ReadInt32();
      // reverse the byte order
      if (this.endianness == Endianness.BigEndian)
      {
        return
          (val << 24) | ((val >> 24) & 0x000000FF) |
          ((val << 8) & 0x00FF0000) | ((val >> 8) & 0x0000FF00);
      }
      else
      {
        return val;
      }
    }

    /// <summary>
    /// Reads an 8-byte signed integer from the current stream and advances the current position of the stream by eight bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadInt64"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte signed integer read from the current stream.</returns>
    public override long ReadInt64()
    {
      long val = base.ReadInt64();
      if (this.endianness == Endianness.BigEndian)
      {
        // reverse the byte order
        return
          (val << 56) | ((val >> 56) & 0x00000000000000FF) |
          ((val << 40) & 0x00FF000000000000) | ((val >> 40) & 0x000000000000FF00) |
          ((val << 24) & 0x0000FF0000000000) | ((val >> 24) & 0x0000000000FF0000) |
          ((val << 8) & 0x000000FF00000000) | ((val >> 8) & 0x00000000FF000000);
      }
      else
      {
        return val;
      }
    }

    #endregion

    #region ReadUIntXX Methods

    /// <summary>
    /// Reads a 2-byte unsigned integer from the current stream using big endian encoding and advances the position of the stream by two bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in big endian format.
    /// <seealso cref="BinaryReader.ReadUInt16"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 2-byte unsigned integer read from this stream.</returns>
    public override ushort ReadUInt16()
    {
      ushort val = base.ReadUInt16();
      if (this.endianness == Endianness.BigEndian)
      {
        // reverse the byte order
        return (ushort)((ushort)(val << 8) | (ushort)(val >> 8));
      }
      else
      {
        return val;
      }
    }

    /// <summary>
    /// Reads a 4-byte unsigned integer from the current stream using big endian encoding and advances the position of the stream by four bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in big endian format.
    /// <seealso cref="BinaryReader.ReadUInt32"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte unsigned integer read from this stream.</returns>
    public override uint ReadUInt32()
    {
      uint val = base.ReadUInt32();
      // reverse the byte order
      if (this.endianness == Endianness.BigEndian)
      {
        return
          (val << 24) | (val >> 24) |
          ((val << 8) & 0x00FF0000) | ((val >> 8) & 0x0000FF00);
      }
      else
      {
        return val;
      }
    }

    /// <summary>
    /// Reads an 8-byte unsigned integer from the current stream using big endian encoding and advances the position of the stream by eight bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadUInt64"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte unsigned integer read from this stream.</returns>
    public override ulong ReadUInt64()
    {
      ulong val = base.ReadUInt64();
      if (this.endianness == Endianness.BigEndian)
      {
        // reverse the byte order
        return
          (val << 56) | (val >> 56) |
          ((val << 40) & 0x00FF000000000000) | ((val >> 40) & 0x000000000000FF00) |
          ((val << 24) & 0x0000FF0000000000) | ((val >> 24) & 0x0000000000FF0000) |
          ((val << 8) & 0x000000FF00000000) | ((val >> 8) & 0x00000000FF000000);
      }
      else
      {
        return val;
      }
    }

    #endregion

    #region ReadSingle, ReadDouble, ReadDecimal Methods

    /// <summary>
    /// Reads a 4-byte floating point value from the current stream and advances the current position of the stream by four bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadSingle"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte floating point value read from the current stream.</returns>
    public override float ReadSingle()
    {
      if (this.endianness == Endianness.BigEndian)
      {
        byte[] originalBytes = BitConverter.GetBytes(base.ReadSingle());
        byte[] newBytes = new byte[] {
					originalBytes[3], originalBytes[2], originalBytes[1], originalBytes[0]};
        return BitConverter.ToSingle(newBytes, 0);
      }
      else
      {
        return base.ReadSingle();
      }
    }

    /// <summary>
    /// Reads an 8-byte floating point value from the current stream and advances the current position of the stream by eight bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> returns this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryReader.ReadDouble"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte floating point value read from the current stream.</returns>
    public override double ReadDouble()
    {
      if (this.endianness == Endianness.BigEndian)
      {
        byte[] originalBytes = BitConverter.GetBytes(base.ReadDouble());
        byte[] newBytes = new byte[] {
					originalBytes[7], originalBytes[6], originalBytes[5], originalBytes[4],
					originalBytes[3], originalBytes[2], originalBytes[1], originalBytes[0]};
        return BitConverter.ToDouble(newBytes, 0);
      }
      else
      {
        return base.ReadDouble();
      }
    }

    /// <summary>
    /// Reads a 16-byte floating point value from the current stream and advances the current position of the stream by sixteen bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> can only read <c>decimal</c> values in little-endian format.
    /// <seealso cref="BinaryReader.ReadDecimal"/>
    /// </remarks>
    /// /// <exception cref="NotSupportedException">The endianness of the reader is <see cref="Endianness.BigEndian"/>.</exception>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 16-byte floating point value read from the current stream.</returns>
    public override decimal ReadDecimal()
    {
      if (this.endianness == Endianness.BigEndian)
      {
        throw new NotSupportedException("Cannot read a decimal value in big-endian format.");
      }
      else
      {
        return base.ReadDecimal();
      }
    }

    #endregion


    /// <summary>
    /// Reads an LMCP-style string.
    /// </summary>
    public override string ReadString()
    {
      byte[] charArray = new byte[this.ReadUInt16()];
      this.Read(charArray, 0, charArray.Length);
      return ASCIIEncoding.ASCII.GetString(charArray);
    }

    /// <summary>
    /// Reads an LMCP-object.
    /// </summary>
    public ILmcpObject ReadObject()
    {
      bool exists = ReadBoolean();
      if (exists)
      {
        ILmcpObject o = LmcpFactory.CreateObject(ReadInt64(), ReadUInt32(), ReadUInt16());
        if (o != null)
        {
          o.Unpack(this.BaseStream);
          return o;
        }
      }
      return null;
    }
  }
}

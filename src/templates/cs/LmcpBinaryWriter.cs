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
  internal enum Endianness
  {
    LittleEndian,
    BigEndian
  }

  // <summary>
  // A <see cref="BinaryReader"/> that stores integer values in either big- or little-endian format
  // and handles LMCP-style strings.
  // </summary>
  public class LmcpBinaryWriter : BinaryWriter
  {
    #region Fields

    private Endianness endianness;

    #endregion

    #region Constructors

    /// <summary>
    /// Initializes a new instance of <c>LmcpBinaryWriter</c> with the supplied stream and endianness.
    /// </summary>
    /// <param name="output">A stream.</param>
    public LmcpBinaryWriter(Stream output)
      : base(output)
    {
      this.endianness = Endianness.BigEndian;
    }

    /// <summary>
    /// Initializes a new instance of <c>LmcpBinaryWriter</c> with the supplied stream and endianness.
    /// </summary>
    /// <param name="output">A byte array to output to.</param>
    public LmcpBinaryWriter(byte[] output)
      : base(new MemoryStream(output))
    {
      this.endianness = Endianness.BigEndian;
    }


    #endregion

    #region Write Int Methods

    /// <summary>
    /// Writes a 2-byte signed integer to the current stream..
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(short)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 2-byte signed integer read to the current stream.</returns>
    public override void Write(short value)
    {
      base.Write((short)((short)(value << 8) | (short)((value >> 8) & 0x00FF)));
    }

    /// <summary>
    /// Writes a 4-byte signed integer to the current stream and advances the current position of the stream by four bytes.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(int)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte signed integer read to the current stream.</returns>
    public override void Write(int value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        base.Write((value << 24) | ((value >> 24) & 0x000000FF) |
          ((value << 8) & 0x00FF0000) | ((value >> 8) & 0x0000FF00));
      }
      else
      {
        base.Write(value);
      }
    }

    /// <summary>
    /// Writes an 8-byte signed integer to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(long)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte signed integer read to the current stream.</returns>
    public override void Write(long value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        base.Write((value << 56) | ((value >> 56) & 0x00000000000000FF) |
          ((value << 40) & 0x00FF000000000000) | ((value >> 40) & 0x000000000000FF00) |
          ((value << 24) & 0x0000FF0000000000) | ((value >> 24) & 0x0000000000FF0000) |
          ((value << 8) & 0x000000FF00000000) | ((value >> 8) & 0x00000000FF000000));
      }
      else
      {
        base.Write(value);
      }
    }

    #endregion

    #region Write UInt Methods

    /// <summary>
    /// Writes a 2-byte unsigned integer to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(ushort)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 2-byte unsigned integer read from this stream.</returns>
    public override void Write(ushort value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        base.Write((ushort)((ushort)(value << 8) | (ushort)(value >> 8)));
      }
      else
      {
        base.Write(value);
      }
    }

    /// <summary>
    /// Writes a 4-byte unsigned integer to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(uint)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte unsigned integer read from this stream.</returns>
    public override void Write(uint value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        base.Write((value << 24) | (value >> 24) |
          ((value << 8) & 0x00FF0000) | ((value >> 8) & 0x0000FF00));
      }
      else
      {
        base.Write(value);
      }
    }

    /// <summary>
    /// Writes an 8-byte unsigned integer to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(ulong)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte unsigned integer read from this stream.</returns>
    public override void Write(ulong value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        base.Write((value << 56) | (value >> 56) |
          ((value << 40) & 0x00FF000000000000) | ((value >> 40) & 0x000000000000FF00) |
          ((value << 24) & 0x0000FF0000000000) | ((value >> 24) & 0x0000000000FF0000) |
          ((value << 8) & 0x000000FF00000000) | ((value >> 8) & 0x00000000FF000000));
      }
      else
      {
        base.Write(value);
      }
    }

    #endregion

    #region Write Floating-Point Methods

    /// <summary>
    /// Writes a 4-byte floating point value to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(float)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 4-byte floating point value read to the current stream.</returns>
    public override void Write(float value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        byte[] originalBytes = BitConverter.GetBytes(value);
        this.Write(new byte[] {
					originalBytes[3], originalBytes[2], originalBytes[1], originalBytes[0]});
      }
      else
      {
        base.Write(value);
      }
    }

    /// <summary>
    /// Writes an 8-byte floating point value to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> writes this struct in the format specified by the <see cref="Endianness"/> value supplied at instantiation.
    /// <seealso cref="BinaryWriter.Write(double)"/>
    /// </remarks>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>An 8-byte floating point value read to the current stream.</returns>
    public override void Write(double value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        byte[] originalBytes = BitConverter.GetBytes(value);
        this.Write(new byte[] {
					originalBytes[7], originalBytes[6], originalBytes[5], originalBytes[4],
					originalBytes[3], originalBytes[2], originalBytes[1], originalBytes[0]});
      }
      else
      {
        base.Write(value);
      }
    }

    /// <summary>
    /// Writes a 16-byte floating point value to the current stream.
    /// </summary>
    /// <remarks>
    /// <c>MultiEndianBinaryReader</c> can only write a <c>decimal</c> value in little-endian format.
    /// <seealso cref="BinaryWriter.Write(decimal)"/>
    /// </remarks>
    /// <exception cref="NotSupportedException">The endianness of the writer is <see cref="Endianness.BigEndian"/>.</exception>
    /// <exception cref="EndOfStreamException">The end of the stream is reached.</exception>
    /// <exception cref="ObjectDisposedException">The stream is closed.</exception>
    /// <exception cref="IOException">An I/O error occurs.</exception>
    /// <returns>A 16-byte floating point value read to the current stream.</returns>
    public override void Write(decimal value)
    {
      if (this.endianness == Endianness.BigEndian)
      {
        throw new NotSupportedException("Cannot write a decimal value in big-endian format.");
      }
      else
      {
        base.Write(value);
      }
    }

    #endregion

    /// <summary>
    /// Placeholder
    /// </summary>
    public override void Write(string value)
    {
      if (value == null)
        this.Write((short)0);
      else
      {
        this.Write((ushort)value.Length);
        this.Write(value.ToCharArray());
      }
    }

    /// <summary>
    /// Writes an LMCP object to this writer using LMCP rules
    /// </summary>
    public void Write(ILmcpObject obj)
    {
      if (obj == null)
      {
        Write(false);
      }
      else
      {
        Write(true);
        Write(obj.SeriesNameAsLong);
        Write(obj.LmcpType);
        Write(obj.SeriesVersion);
        obj.Pack(this.BaseStream);
      }
    }
  }
}

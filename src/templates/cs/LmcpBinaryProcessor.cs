
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
using System.Diagnostics;

namespace Avtas.Lmcp
{
  /// <summary>
  /// Reads binary data and raises events when complete
  /// LMCP object are detected.
  /// </summary>
  public class LmcpBinaryProcessor
  {
    /// <summary>
    /// Occurs when a complete LMCP object is received.
    /// </summary>
    public event EventHandler<LmcpObjectReceivedEventArgs> ObjectReceived;

    /// <summary>
    /// Occurs when there is an error in the binary data.
    /// </summary>
    public event EventHandler Error;

    private byte[] _buffer = new byte[65536];
    private int _bufferSize;
    private const int _headerPlusChecksum = LmcpFactory.HEADER_SIZE + LmcpFactory.CHECKSUM_SIZE;

    /// <summary>
    /// Initializes a new instance of the <see cref="LmcpBinaryProcessor"/> class.
    /// </summary>
    public LmcpBinaryProcessor()
    {
    }

    /// <summary>
    /// Adds bytes to the processor.
    /// </summary>
    /// <param name="bytes">The bytes to add.</param>
    /// <param name="length">The length of valid data in <paramref name="bytes"/>.</param>
    /// <exception cref="ArgumentNullException"><paramref name="bytes"/> is <c>null</c>.</exception>
    /// <exception cref="ArgumentException"><paramref name="length"/> is greater than <c>bytes.Length</c>.</exception>
    public void AddBytes(byte[] bytes, int length)
    {
      if (bytes == null)
        throw new ArgumentNullException("byte");
      if (length > bytes.Length)
        throw new ArgumentException("length must be less than or equal to bytes.Length.", "length");

      if (_bufferSize + length > _buffer.Length)
      {
        Debug.WriteLine("Buffer overflow in LMCP processor.");
        if (Error != null)
          Error(this, EventArgs.Empty);
      }

      if (_bufferSize + length > _buffer.Length)
        throw new OverflowException(String.Format("Buffer exceed max size of {0}.", _buffer.Length));

      Array.Copy(bytes, 0, _buffer, _bufferSize, length);

      _bufferSize += length;

      while (_bufferSize > 8)
      {
        int nextObjectSize = (int)LmcpFactory.GetSize(_buffer);
        nextObjectSize += _headerPlusChecksum;

        if (_bufferSize >= nextObjectSize)
        {
          // Make array holding just the new object.
          byte[] nextObjectBuffer = new byte[nextObjectSize];
          Array.Copy(_buffer, 0, nextObjectBuffer, 0, nextObjectSize);

          // Move the remaining bytes from _buffer to the front.
          _bufferSize -= nextObjectSize;
          if (_bufferSize > 0)
            Array.Copy(_buffer, nextObjectSize, _buffer, 0, _bufferSize);

          try
          {
            Lmcp.ILmcpObject o = Lmcp.LmcpFactory.GetObject(nextObjectBuffer);

            if (ObjectReceived != null)
              ObjectReceived(this, new LmcpObjectReceivedEventArgs(o));
          }
          catch (InvalidOperationException ex)
          {
            Debug.WriteLine(ex.ToString());
            if (Error != null)
              Error(this, EventArgs.Empty);
          }
        }
        else
        {
          break;
        }
      }
    }
  }
}

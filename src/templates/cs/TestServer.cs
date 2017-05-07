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
using System.Net;
using System.Net.Sockets;
using Avtas.Lmcp;

namespace TestServer
{
  /// <summary>
  /// This program acts as a test server showing extremly basic
  /// LMCP connectivity over a TCP connection.
  /// </summary>
  /// <remarks>
  /// This implementation allows only a single client, is not robust
  /// to disconnects, and 
  /// </remarks>
  internal class Program
  {
    private static int _port = 11041;

    internal static void Main( string[] args )
    {
      try
      {
        if ( args.Length != 0 )
          Int32.TryParse( args[0], out _port );

        Console.WriteLine( "Listening for connections on port {0}.", _port );

        LmcpBinaryProcessor processor = new LmcpBinaryProcessor();
        processor.ObjectReceived += OnLmcpObjectReceived;
        processor.Error += OnError;

        byte[] buffer = new byte[1024];

        TcpListener socket = new TcpListener( IPAddress.Loopback, _port );
        socket.Start();

        for ( ;;)
        {
          TcpClient s = socket.AcceptTcpClient();
          Console.WriteLine( "Connection established." );
          NetworkStream stream = s.GetStream();

          for ( ;;)
          {
            try
            {
              ILmcpObject obj = LmcpFactory.GetObject( new BinaryReader( stream ) );
              if ( obj != null )
              {
                Console.WriteLine( "Received " + obj.GetType() );
              }
            }
            catch ( IOException )
            {
              Console.WriteLine( "Connection closed." );
              s.Close();
              break;
            }
          }
        }
      }
      catch ( Exception ex )
      {
        Console.WriteLine( "Unexpected error: {0}.", ex );
      }

      Console.WriteLine( "Press any key..." );
      Console.ReadKey();
    }

    private static void OnLmcpObjectReceived( object sender, LmcpObjectReceivedEventArgs e )
    {
      Console.WriteLine( e.LmcpObject.ToXml( "" ) );
    }

    private static void OnError( object sender, EventArgs e )
    {
      Console.WriteLine( "Error decoding LMCP object." );
    }
  }
}


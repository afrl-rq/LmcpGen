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
using System.Net.Sockets;

namespace TestClient
{
  /// <summary>
  /// This program acts as a test client showing extremely basic
  /// LMCP connectivity over a TCP connection, transmitted
  /// every type of LMCP object in the package.
  /// </summary>
  internal class Program
  {
    private static string _host = "localhost";
    private static int _port = 11041;

    internal static void Main( string[] args )
    {
      TcpClient client = null;
      try
      {
        if ( args.Length > 0 )
          _host = args[0];
        if ( args.Length > 1 )
          Int32.TryParse( args[1], out _port );

        Console.WriteLine( "Connecting to {0}:{1}.", _host, _port );

        for ( ;;)
        {
          try
          {
            client = new TcpClient();
            client.Connect( _host, _port );
            break;
          }
          catch ( ArgumentOutOfRangeException )
          {
            Console.WriteLine( "Port {0} is invalid.", _port );
            break;
          }
          catch ( SocketException )
          {
            continue;
          }
        }

        if ( client != null )
          SendKitchenSink( new BinaryWriter( client.GetStream() ) );
      }
      catch ( Exception ex )
      {
        Console.WriteLine( "Unexpected error: {0}.", ex );
      }
      finally
      {
        if ( client != null )
          client.Close();
      }

      Console.WriteLine( "Press any key..." );
      Console.ReadKey();
    }

    private static void SendKitchenSink( BinaryWriter writer )
    {
      Avtas.Lmcp.ILmcpObject o = null;
      -<send_all_messages>-
    }
  }
}

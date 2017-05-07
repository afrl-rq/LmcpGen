// ===============================================================================
// Authors: AFRL/RQQA
// Organization: Air Force Research Laboratory, Aerospace Systems Directorate, Power and Control Division
// 
// Copyright (c) 2017 Government of the United State of America, as represented by
// the Secretary of the Air Force.  No copyright is claimed in the United States under
// Title 17, U.S. Code.  All Other Rights Reserved.
// ===============================================================================

// This file was auto-created by LmcpGen. Modifications will be overwritten.

﻿using System;

namespace Avtas.Lmcp
{
  /// <summary>
  /// Contains data for the <see cref="E:LmcpReader.ObjectReceived"/> event.
  /// </summary>
  public class LmcpObjectReceivedEventArgs : EventArgs
  {
    /// <summary>
    /// Initializes a new instance of the <see cref="LmcpObjectReceivedEventArgs"/> class.
    /// </summary>
    /// <param name="lmcpObject">The LMCP object.</param>
    public LmcpObjectReceivedEventArgs(ILmcpObject lmcpObject)
    {
      LmcpObject = lmcpObject;
    }

    /// <summary>
    /// Gets the LMCP object.
    /// </summary>
    public ILmcpObject LmcpObject { get; private set; }
  }
}

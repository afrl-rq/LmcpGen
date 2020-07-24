with Ada.Text_IO; use Ada.Text_IO;
with ZMQ.Sockets;
with ZMQ.Contexts;
with ZMQ.Messages;

procedure LMCP.Client is
   Ctx : ZMQ.Contexts.Context;
   Sub : ZMQ.Sockets.Socket;
begin
   Ctx.Set_number_of_IO_threads (1);

   Sub.Initialize (Ctx, ZMQ.Sockets.SUB);

   -- LmcpObjectNetworkPublishPullBridge
   --   'AddressPUB' attribute specifies PUB address (defaults to 'tcp://*:5556')
   --   PUB socket is zeromq server (i.e. binds)
   Sub.Connect ("tcp://127.0.0.1:5556");

   -- Accept all forwarded messages (filtering on PUB side via 'SubscribeToMessage' child elements)
   Sub.Establish_Message_Filter ("");

   for i in  1 .. 10 loop
      declare
         ZmqMsg : ZMQ.Messages.Message;
      begin
         ZmqMsg.Initialize (0);

         Sub.Recv (ZmqMsg);
         Put_Line (ZmqMsg.GetData);

         -- ZMQ.Messages.Message is finalized automatically as controlled types (i.e. ensures
         -- call to underlying zmq_msg_close() via its Finalize subprogram)
      end;
   end loop;

   -- Socket is closed (i.e. zmq_close()) automatically via Finalize as a controlled type
end LMCP.Client;

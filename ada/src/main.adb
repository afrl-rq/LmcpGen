with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with avtas.lmcp; use avtas.lmcp;
with avtas.lmcp.types; use avtas.lmcp.types;
with afrl.cmasi.enumerations; use afrl.cmasi.enumerations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with afrl.cmasi.keyValuePair; use afrl.cmasi.keyValuePair;
with afrl.cmasi.location3D; use afrl.cmasi.location3D;
with afrl.cmasi.AbstractGeometry; use afrl.cmasi.AbstractGeometry;
with afrl.cmasi.abstractZone; use afrl.cmasi.abstractZone;
with afrl.cmasi.keepInZone; use afrl.cmasi.keepInZone;
with afrl.cmasi.circle; use afrl.cmasi.circle;
with afrl.cmasi.entityConfiguration; use afrl.cmasi.entityConfiguration;
with afrl.cmasi.payloadConfiguration; use afrl.cmasi.payloadConfiguration;
with afrl.cmasi.cameraConfiguration; use afrl.cmasi.cameraConfiguration;
with afrl.cmasi.gimbalconfiguration; use afrl.cmasi.gimbalconfiguration;

-- Notes (LRH, 14 Dec):
--
-- For each class, made the package name the same as the class.
-- This means you have to use the qualified name in some contexts, e.g.
--    type Circle is new AbstractGeometry.AbstractGeometry with private;
--
-- Every series has a base Object basepackage.object.Object that inherits from
-- avtas.lmcp.object.Object. This is because there are a few functions that
-- should return the same result for every object in the series. The C/C++
-- implemention just hardcodes these same values for each object in the series.
-- The Ada implementation could go either way.
--
-- In avtas.lmcp.types, I defined what I hope correspond to C/C++ compatible
-- types. I used the Interfaces package. If these types need to change, then we
-- only have to change them in one place (and we get short names for the types).
--
-- (1) Within an LMCP object, if the object has a field that itself corresponds
-- to an LMCP object, the field for that object is an access type (to support
-- polymorphism). By default, the accessed object is uninitialized
-- (access type is null). Are there any issues here?
--
-- (2) When an LMCP object has a field that corresponds to a vector of any type,
-- the field is an access type (to avoid copying long vectors). Unlike (1), by
-- default, the vector is initialized to a new (empty) vector. Only a get method
-- is provided (no set), because once you get the access type,
-- you can manipulate the vector. It is not clear that this is the best way to
-- implement this, but we're unsure how memory management of objects works in
-- Ada.
--
-- Right now, all access types are with the 'all' keyword to support aliasing.
-- We can likely remove this, but for now it might be convenient.
--
-- I use Unbounded_Strings for fields that are Strings. Internally, I think this
-- is necessary. However, set/get methods take/return Unbounded_Strings when in
-- reality they could easily be modified to work over Strings. So, we may want
-- to change that, but for now, it's convenient.
--
-- Many methods are still unimplemented.
--
-- The underlying mechanism for serialization/deserialization is not
-- implemented (ByteBuffer in C++). Derek says the C++ implementation is bad
-- (methods have unexpected side effects -- search for rewind(); could probably
-- do more with 'native' types). Need to consider a better implementation for
-- Ada.
--
-- I want to investigate the ZMQ binds for Ada to send/receive LMCP messages.
-- Interfaces to SPARK code will likely have to assume things about the LMCP
-- messages. That seems fine to me.

procedure Main is

   tempUnboundedString : Unbounded_String;

   kz : KeepInZone;
   c : Circle_Acc := new Circle;
   l3d : Location3D;
   l3d_Acc : Location3D_Acc;
   az : AbstractZone;
   kvp : KeyValuePair;
   ec : EntityConfiguration;
   cc_Acc : PayloadConfiguration_Class_Acc;
   gc_Acc : PayloadConfiguration_Class_Acc;

begin

   l3d.setLatitude(30.43258611);
   l3d.setLongitude(-87.17408333);
   l3d.setAltitude(600.0);
   l3d.setAltitudeType(AltitudeTypeEnum'(MSL));

   l3d_Acc := new Location3D;
   l3d_Acc.setLatitude(30.43258611);
   l3d_Acc.setLongitude(-87.17408333);
   l3d_Acc.setAltitude(600.0);

   -- What if I had aliased l3d instead?
   -- How is it different here, since both l3d_Acc and c.getCenterPoint
   -- refer to the same thing?
   c.setRadius(100.0);
   c.setCenterPoint(l3d_Acc);
   New_Line;
   -- How to control how many digits are output for a float? See toString code
   -- in afrl.cmasi.circle adb.
   Put("Print an afrl.cmasi.Circle:");
   New_Line;
   Put(c.toString(3));
   New_Line;

   az.getAffectedAircraft.Append(10);
   az.getAffectedAircraft.Append(11);
   az.getAffectedAircraft.Append(12);
   Put("Print the values inside an afrl.cmasi.AbstractZone's AffectedAircraft[] field");
   New_Line;
   -- What is the "best" way of looping over the values stored in a Vector
   -- pointed to by an access type? This took me awhile to figure out; is it
   -- making a copy here?
   for i of az.getAffectedAircraft.all loop
      Put(i'Image);
   end loop;
   New_Line(2);
   -- This is where we might change the interface to use a String, even
   -- if internally it's using an Unbounded_String
   az.setLabel(Unbounded_String'(To_Unbounded_String("AzLabel")));
   Put("Print a test label:");
   Put(To_String(az.getLabel));
   New_Line(2);

   Put("Print a test FullLmcpTypeName: ");
   Put(kvp.getFullLmcpTypeName);
   New_Line(2);

   -- Set a KeepInZone's Boundary by pointing to a Circle created here.
   -- Any issues with this?
   kz.setZoneId(10);
   kz.setMinAltitude(100.0);
   kz.setMaxAltitude(750.0);
   kz.setBoundary(AbstractGeometry_Acc(c));

   -- Experiment with vectors of tagged types. EntityConfiguration has a vector
   -- PayloadConfigurationList with several types. Implemented
   -- CameraConfiguration and GimbalConfiguration. Mix and match.
   --
   -- Eww, I had to define a Class access type PayloadConfiguration_Class_Acc in
   -- afrl.cmasi.payloadConfiguration. Then afrl.cmasi.entityConfiguration
   -- has a vector accessed through Vect_PayloadConfiguration_Class_Acc_Acc
   -- Is that right? What is the convention? It looks like it works though!
   -- I would need to go back and implement "class accessor" types for all
   -- classes to support this situation.
   gc_Acc := new GimbalConfiguration;
   cc_Acc := new CameraConfiguration;
   ec.getPayloadConfigurationList.Append(gc_Acc);
   ec.getPayloadConfigurationList.Append(cc_Acc);

end Main;

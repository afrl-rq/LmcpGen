package Conversion_Equality_Lemmas with
   SPARK_Mode
is

   generic
      type Numeric is private;
      type Byte is private;
      type Index is range <>;
      type Bytes is array (Index range <>) of Byte;
      with function To_Numeric (Input : Bytes) return Numeric;
      with function To_Bytes (Input : Numeric) return Bytes;
   procedure Lemma_Conversion_Equality
     (This_Numeric : Numeric;
      These_Bytes  : Bytes)
   with
     Ghost,
     Post => These_Bytes = To_Bytes (This_Numeric)   and  -- this conversion
             To_Bytes (This_Numeric) = These_Bytes   and  -- and the other order too
             This_Numeric = To_Numeric (These_Bytes) and  -- and this conversion
             To_Numeric (These_Bytes) = This_Numeric;     -- and the other order too

end Conversion_Equality_Lemmas;

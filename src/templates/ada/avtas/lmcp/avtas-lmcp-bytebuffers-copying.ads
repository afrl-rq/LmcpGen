package AVTAS.LMCP.ByteBuffers.Copying with
   SPARK_Mode
is
   pragma Unevaluated_Use_Of_Old (Allow);

   procedure Copy_Buffer_To_String
     (This        : in out ByteBuffer'Class;
      Length      : Index;
      Destination : in out String)
   with
     Pre  => Remaining (This) >= Length   and then
             Length <= Max_String_length  and then
             Length <= Destination'Length and then
             Position (This) + Length <= High_Water_Mark (This),
     Post => Position (This) = Position (This)'Old + Length      and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) = Remaining (This)'Old - Length    and then
             Equal (Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1),
                    Destination (Destination'First .. Destination'First + Integer (Length - 1))) and then
             Prior_Content_Unchanged (This, Old_Value => This'Old),
     Inline;


   procedure Copy_Buffer_To_Unbounded_String
     (This        : in out ByteBuffer'Class;
      Length      : Index;
      Destination : out Unbounded_String)
   with
     Pre  => Remaining (This) >= Length  and then
             Length <= Max_String_length and then
             Position (This) + Length <= High_Water_Mark (This),
     Post => Position (This) = Position (This)'Old + Length      and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old and then
             Remaining (This) = Remaining (This)'Old - Length    and then
             Equal (Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1),
                    To_String (Destination))                  and then
             Prior_Content_Unchanged (This, Old_Value => This'Old),
     Inline;


   procedure Copy_String_To_Buffer
     (This : in out ByteBuffer'Class;
      From : String)
   with
     Pre  => Remaining (This) >= From'Length  and then
             From'Length <= Max_String_Length and then
             High_Water_Mark (This) + From'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + From'Length               and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + From'Length and then
             Remaining (This) = Remaining (This)'Old - From'Length             and then
             New_Content_Equal (This, This.Position'Old, From)                 and then
             Prior_Content_Unchanged (This, Old_Value => This'Old),
     Inline;

   procedure Copy_Bytes_To_Buffer
     (This : in out ByteBuffer'Class;
      From : Byte_Array)
   with
     Pre  => Remaining (This) >= From'Length and then
             High_Water_Mark (This) + From'Length <= This.Capacity,
     Post => Position (This) = Position (This)'Old + From'Length                   and then
             High_Water_Mark (This) = High_Water_Mark (This)'Old + From'Length     and then
             Raw_Bytes (This) (Position (This)'Old .. Position (This) - 1) = From  and then
             Prior_Content_Unchanged (This, Old_Value => This'Old),
     Inline;

end AVTAS.LMCP.ByteBuffers.Copying;

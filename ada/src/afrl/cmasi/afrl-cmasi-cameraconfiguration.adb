package body afrl.cmasi.cameraConfiguration is

   function getFullLmcpTypeName(this : CameraConfiguration'Class) return String is ("afrl.cmasi.cameraConfiguration.CameraConfiguration");

   function getLmcpTypeName (this : CameraConfiguration'Class) return String is ("CameraConfiguration");

   function getLmcpType (this : CameraConfiguration'Class) return UInt32_t is (CmasiEnum'Pos(CAMERACONFIGURATION_ENUM));

   function getSupportedWavelengthBand(this : CameraConfiguration'Class) return WavelengthBandEnum is (this.SupportedWavelengthBand);

   procedure setSupportedWavelengthBand(this : out CameraConfiguration'Class; SupportedWavelengthBand : in WavelengthBandEnum) is
   begin
      this.SupportedWavelengthBand := SupportedWavelengthBand;
   end setSupportedWavelengthBand;

   function getFieldOfViewMode(this : CameraConfiguration'Class) return FOVOperationModeEnum is (this.FieldOfViewMode);

   procedure setFieldOfViewMode(this : out CameraConfiguration'Class; FieldOfViewMode : in FOVOperationModeEnum) is
   begin
      this.FieldOfViewMode := FieldOfViewMode;
   end setFieldOfViewMode;

   function getMinHorizontalFieldOfView(this : CameraConfiguration'Class) return Float_t is (this.MinHorizontalFieldOfView);

   procedure setMinHorizontalFieldOfView(this : out CameraConfiguration'Class; MinHorizontalFieldOfView : in Float_t) is
   begin
      this.MinHorizontalFieldOfView := MinHorizontalFieldOfView;
   end setMinHorizontalFieldOfView;

   function getMaxHorizontalFieldOfView(this : CameraConfiguration'Class) return Float_t is (this.MaxHorizontalFieldOfView);

   procedure setMaxHorizontalFieldOfView(this : out CameraConfiguration'Class; MaxHorizontalFieldOfView : in Float_t) is
   begin
      this.MaxHorizontalFieldOfView := MaxHorizontalFieldOfView;
   end setMaxHorizontalFieldOfView;

   function getDiscreteHorizontalFieldOfViewList(this : CameraConfiguration'Class) return Vect_Float_t_Acc is (this.DiscreteHorizontalFieldOfViewList);

   function getVideoStreamHorizontalResolution(this : CameraConfiguration'Class) return UInt32_t is (this.VideoStreamHorizontalResolution);

   procedure setVideoStreamHorizontalResolution(this : out CameraConfiguration'Class; VideoStreamHorizontalResolution : in UInt32_t) is
   begin
      this.VideoStreamHorizontalResolution := VideoStreamHorizontalResolution;
   end setVideoStreamHorizontalResolution;

   function getVideoStreamVerticalResolution(this : CameraConfiguration'Class) return UInt32_t is (this.VideoStreamVerticalResolution);

   procedure setVideoStreamVerticalResolution(this : out CameraConfiguration'Class; VideoStreamVerticalResolution : in UInt32_t) is
   begin
      this.VideoStreamVerticalResolution := VideoStreamVerticalResolution;
   end setVideoStreamVerticalResolution;

end afrl.cmasi.cameraConfiguration;

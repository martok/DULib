unit DUVideo;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls, VFW, mmsystem;

type
  TVideoFrame = class;
  
  TFrameStreamEvent = procedure (Frame: TVideoFrame) of object;

  TVideoFrame = class
  private
    FDib: PBitmapInfoHeader;
    FBits: Pointer;
    procedure SetDIB(const Value: PBitmapInfoHeader);
    function GetHeight: integer;
    function GetWidth: integer;
  public
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property DibSect: PBitmapInfoHeader read FDib write SetDIB;
    function ConvertBitmap(Bitmap: TBitmap): boolean;
  end;

  TVideoSource = class
  private
    FStreamed: TFrameStreamEvent;
  protected
    FrameNum: integer;
    function GetFrameRate: single; virtual;
    function GetLength: integer; virtual;
    procedure DoFrameStreamed(fr:TVideoFrame); 
    function DoGetFrame(Index: integer; Frame:TVideoFrame):boolean; virtual;
  public
    constructor Create;
    function GrabFrame(Index: integer; Frame:TVideoFrame):boolean;
    procedure StartStream; virtual;
    procedure EndStream; virtual;
    property OnFrameStreamed: TFrameStreamEvent read FStreamed write FStreamed;
    property StreamLength: integer read GetLength;
    property FrameRate: single read GetFrameRate;
  end;

  TAVIVideoSource = class(TVideoSource)
  protected
    pAvi: IAVIFile;
    pAviS: IAVIStream;
    pFrame: IGetFrame;
    Info: TAVIFileInfo;
    function GetFrameRate: Single; override;
    function GetLength: Integer; override;
    function DoGetFrame(Index: Integer; Frame: TVideoFrame): Boolean; override;
  public
    constructor Create(Filename: string);
    destructor Destroy; override;
  end;

  TAVICAPCaptureSource = class(TVideoSource)
  private
    FScale: boolean;
    FCanvas: TCanvas;
    procedure SetScale(const Value: boolean);
    function EnumerateAndAskVideoCodecs: Boolean;
    procedure CloseCapture;
    procedure OpenCapture;
  protected
    FHost: TWinControl;
    FWnd: HWND;
    FFrame: TVideoFrame;
    FCapBitmapInfo,                // BitmapInfo des PREVIEW FRAMES
    FBitmapInfo2 : TBitmapInfo;    // BitmapInfo von FDecoBitmap
    FDecoBitmap: TBitmap;
    FDecoDIB : Pointer;               // ^Bit-Buffer von FBitmap2
    FfccType, FfccHandler : DWord; // 'fcc-Kennung' des Codecs
    FICHandle : THandle;           // Handle des Codecs
    function DoGetFrame(Index: Integer; Frame: TVideoFrame): Boolean; override;
    function FrameCallback(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD;
  public
    constructor Create(DriverId: integer; HostWin: TWinControl);
    destructor Destroy; override;
    class function ListSources(List: TStrings): boolean;
    property Canvas: TCanvas read FCanvas;
    property PreviewScale: boolean read FScale write SetScale;
    procedure StartStream; override;
    procedure EndStream; override;
    function DlgVideoFormat: BOOL;
    function DlgVideoSource: BOOL;
    function DlgVideoDisplay: BOOL;
    function DlgVideoCompression: BOOL;
  end;

procedure DUVideoInit;
procedure DUVideoDone;

implementation

var
  DibDraw: THandle;

procedure DUVideoInit;
begin
  AVIFileInit;
  DibDraw:= DrawDibOpen;
end;

procedure DUVideoDone;
begin
  DrawDibClose(DibDraw);
  AVIFileExit;
end;

{ TVideoFrame }

function TVideoFrame.ConvertBitmap(Bitmap: TBitmap): boolean;
begin
  Result:= FDib<>nil;
  if not Result then exit;
  with Bitmap do begin
    Width:= FDib.biWidth;             // Setze die Größe der Bitmap
    Height:= FDib.biHeight;
                                      // Zeichne das DIB ins BMP
    Result:= DrawDibDraw(DibDraw, Canvas.handle, 0, 0, FDib.biwidth, FDib.biheight,
                FDib, FBits, 0, 0, FDib.biWidth, FDib.biheight, 0);
  end;
end;

function TVideoFrame.GetHeight: integer;
begin
  Result:= FDib.biHeight;
end;

function TVideoFrame.GetWidth: integer;
begin
  Result:= FDib.biWidth;
end;

procedure TVideoFrame.SetDIB(const Value: PBitmapInfoHeader);
begin
  FDib := Value;
  if Assigned(Value) then
    FBits:= Pointer(DWORD(FDib) + (4 * FDib^.biClrUsed) + FDib^.biSize)
  else
    FBits:= nil;
end;

{ TVideoSource }

procedure TVideoSource.DoFrameStreamed(fr: TVideoFrame);
begin
  if Assigned(FStreamed) then FStreamed(fr);
end;

procedure TVideoSource.EndStream;
begin
end;

procedure TVideoSource.StartStream;
begin
end;

function TVideoSource.GrabFrame(Index: integer; Frame:TVideoFrame):boolean;
begin
  if Index<0 then
    Index:= FrameNum+1;
  Result:= DoGetFrame(Index, Frame);
  FrameNum:= Index;
end;

function TVideoSource.GetFrameRate: single;
begin
  Result:= 0;
end;

function TVideoSource.GetLength: integer;
begin
  Result:= 0;
end;

function TVideoSource.DoGetFrame(Index: integer; Frame: TVideoFrame): boolean;
begin
  Result:= false;
end;

constructor TVideoSource.Create;
begin
  inherited Create;
  FrameNum:= 0;
end;

{ TAVIVideoSource }

constructor TAVIVideoSource.Create(Filename: string);
var first,last:integer;
begin
  inherited Create;
  if AVIFileOpen(pavi, Pchar(Filename), OF_READ, nil) <> 0 then
    RaiseLastOSError
  else begin
    AVIFileInfo(pAvi, Info, sizeof(Info));
    if AVIFileGetStream(pAvi, pAviS, streamtypeVIDEO, 0) = LongInt(AVIERR_NODATA) then
      RaiseLastOSError;
    first:= AVIStreamStart(pAviS);
    last:= AVIStreamEnd(pAviS);
    pFrame:= AVIStreamGetFrameOpen(pAviS, nil);
    if AVIStreamBeginStreaming(pAviS, first, last, 1000) <> 0 then
      RaiseLastOSError;
  end;
end;

destructor TAVIVideoSource.Destroy;
begin
  if pavi <> nil then begin
    if pavis <> nil then begin
      AVIStreamEndStreaming(pAviS);
      AVIStreamGetFrameClose(pFrame);
      AVIStreamRelease(pAviS);
      Pointer(pFrame):= nil;
      Pointer(pAviS):= nil;
    end;
    AVIFileRelease(pavi);
    Pointer(pavi):= nil;
  end;
  inherited;
end;

function TAVIVideoSource.GetFrameRate: Single;
begin
  if pAvi <> nil then
    Result:= Info.dwRate / Info.dwScale
  else
    Result:= 0;
end;

function TAVIVideoSource.GetLength: Integer;
begin
  if pAvi <> nil then
    Result:= Info.dwLength
  else
    Result:= 0;
end;

function TAVIVideoSource.DoGetFrame(Index: Integer; Frame: TVideoFrame): Boolean;
var
  pbmi: PBitmapInfoHeader;
begin
  pbmi:= AVIStreamGetFrame(pFrame, Index);
  Result:= pbmi<>nil;
  Frame.DibSect:= pbmi;
end;

{ TAVICAPCaptureSource }

function GlobFrameCallback(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD; stdcall;
var P:Pointer;
begin
  P:= Pointer(capGetUserData(hWnd));
  Result:= TAVICAPCaptureSource(P).FrameCallback(hWnd, lpVHdr);
end;

constructor TAVICAPCaptureSource.Create(DriverId: integer; HostWin: TWinControl);
begin
  inherited Create;
  DriverId:= 0;
  FHost:= HostWin;
  FWnd:=  capCreateCaptureWindow( PChar('TAVICAPCaptureSource'),
              WS_CHILD or WS_VISIBLE, 0, 0,
              FHost.Width, FHost.Height,
              FHost.Handle, DriverID);
  capSetUserData(FWnd, LongInt(Self));
  capDriverConnect(FWnd, DriverID);
  FFrame:= TVideoFrame.Create;
  FCanvas:= TCanvas.Create;
  FCanvas.Handle:= GetDC(FWnd);
end;

destructor TAVICAPCaptureSource.Destroy;
begin
  FCanvas.Free;
  FFrame.Free;
  EndStream;
  capDriverDisconnect(FWnd);
  DestroyWindow(FWnd);
  inherited;
end;

function TAVICAPCaptureSource.DoGetFrame(Index: Integer;
  Frame: TVideoFrame): Boolean;
begin
  OpenCapture;
  try
    capSetCallbackOnFrame(FWnd, GlobFrameCallback);
    capGrabFrameNoStop(FWnd);
    capSetCallbackOnFrame(FWnd,nil);
    Frame.FDib:= FFrame.FDib;
    Frame.FBits:= FFrame.FBits;
  finally
    CloseCapture;
  end;
  Result:= true;
end;

class function TAVICAPCaptureSource.ListSources(List: TStrings): boolean;
var i:integer;
    szName,
    szVersion: array[0..MAX_PATH] of char;
begin
  List.Clear;
  for i:= 0 to 9 do
    if capGetDriverDescription(I,
                               @szName,
                               sizeof(szName),
                               @szVersion,
                               sizeof(szVersion)) then
    List.AddObject(szName+' ('+szVersion+')', Pointer(I));
  Result:= List.Count>0;
end;

procedure TAVICAPCaptureSource.SetScale(const Value: boolean);
begin
  FScale := Value;
  capPreviewScale(FWnd, Value);
end;

// befragt alle auffindbaren Video-Codecs
function TAVICAPCaptureSource.EnumerateAndAskVideoCodecs : Boolean;
var
  Counter   : integer;
  ICInfoRec : TICInfo;
  Ahic      : THandle;
begin
  result := false;
  Counter := 0;
  // solange noch kein passender Video-Codec gefunden wurde, wird weitergesucht
  while (result = false) and ICInfo(ICTYPE_VIDEO, counter, @ICInfoRec) do begin
    // Versuch den Codec zur Befragung zu öffnen
    Ahic := ICOpen(ICInfoRec.fccType, ICInfoRec.fccHandler, ICMODE_QUERY);
    if Ahic<>0 then begin
      // befragen des Codecs ob er die Dekomprimierung ausführen kann
      if ICSendMessage(Ahic, ICM_DECOMPRESS_QUERY, integer(@FCapBitmapInfo), integer(@FBitmapInfo2)) = ICERR_OK then begin
        // wenn ja, dann: Speichern der Codec-"fcc-Kennung" + Abbruch der Codec-Suche
        FfccType := ICInfoRec.fccType;
        FfccHandler := ICInfoRec.fccHandler;
        result := true;
      end;
      ICClose(Ahic);
    end;
    inc(counter);
  end;
end;

procedure TAVICAPCaptureSource.OpenCapture;
var ImgHdrSize,ImgSize : DWord;
begin
  // das BitmapInfo der PREVIEW FRAMES muss beschafft werden
  FillChar(FCapBitmapInfo, SizeOf(FCapBitmapInfo), 0);
  capGetVideoFormat(FWnd, @FCapBitmapInfo, SizeOf(FCapBitmapInfo));
  // nach dem Format der PREVIEW FRAMES richtet sich weiteres

  // ein TBitmap, welches gleich in ein
  // DIB umgewandelt wird. Das BitmapInfo, das dadurch anfallen wird, ist
  // dann für die Suche eines Codecs und die Dekomprimierung essentiell.
  FDecoBitmap := TBitmap.Create;
  FDecoBitmap.Width := FCapBitmapInfo.bmiHeader.biWidth;
  FDecoBitmap.Height := FCapBitmapInfo.bmiHeader.biHeight;
  FDecoBitmap.PixelFormat := pf24Bit;
  // Umwandlung von FBitmap2 in ein DIB
  GetDIBSizes(FDecoBitmap.Handle,ImgHdrSize,ImgSize);
  GetMem(FDecoDIB,ImgSize);
  GetDIB(FDecoBitmap.Handle, FDecoBitmap.Palette, FBitmapInfo2, FDecoDIB^);

  if EnumerateAndAskVideoCodecs = false // ist die Codec-Suche erfolglos...
  then raise Exception.Create('Kein passender Codec gefunden') // Prog-Ende
  else begin // sonst wird alles fürs Frame-Grabben & Decompressen eingeschaltet
    FICHandle := ICOpen(FfccType, FfccHandler, ICMODE_FASTDECOMPRESS);
  end;
end;

procedure TAVICAPCaptureSource.CloseCapture;
begin
  if FDecoDIB=nil then exit;
  ICClose(FICHandle);
  FreeMem(FDecoDIB);
  FDecoBitmap.Free;
  FDecoDIB:= nil;
end;


procedure TAVICAPCaptureSource.StartStream;
begin
  OpenCapture;
  capSetCallbackOnFrame(FWnd, GlobFrameCallback);
  capPreview(FWnd, true);
  capPreviewRate(FWnd, 1000 div 20);
end;

procedure TAVICAPCaptureSource.EndStream;
begin
  capPreview(FWnd, false);
  capSetCallbackOnFrame(FWnd,nil);
  CloseCapture;
end;

function TAVICAPCaptureSource.FrameCallback(hWnd: HWND; lpVHdr: PVIDEOHDR): DWORD;
begin
  Result:= 1;
  // dekomprimieren der Frame-Rohdaten durch den schon im Creater geöffneten Codec
  ICSendMessage(FICHandle, ICM_DECOMPRESS_BEGIN, integer(@FCapBitmapInfo), integer(@FBitmapInfo2));
  ICDecompress(FICHandle,ICDECOMPRESS_UPDATE, @FCapBitmapInfo,lpVHdr^.lpData,
                                                  @FBitmapInfo2,FDecoDIB);
  ICSendMessage(FICHandle, ICM_DECOMPRESS_END, 0, 0);
  FFrame.DibSect:= @FBitmapInfo2;
  FFrame.FBits:= FDecoDIB;
  DoFrameStreamed(FFrame);
end;

function TAVICAPCaptureSource.DlgVideoCompression: BOOL;
begin
  Result:= capDlgVideoCompression(FWnd);
end;

function TAVICAPCaptureSource.DlgVideoDisplay: BOOL;
begin
  Result:= capDlgVideoDisplay(FWnd);
end;

function TAVICAPCaptureSource.DlgVideoFormat: BOOL;
begin
  Result:= capDlgVideoFormat(FWnd);
end;

function TAVICAPCaptureSource.DlgVideoSource: BOOL;
begin
  Result:= capDlgVideoSource(FWnd);
end;

end.

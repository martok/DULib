unit DUFilePackage;

interface

  uses
    Windows, Classes, SysUtils;

type
{$IFDEF VER130}            // This is a bit awkward
    // 8-byte integer
    TInteger8 = Int64;     // Delphi 5
{$ELSE}
  {$IFDEF VER120}
    TInteger8 = Int64;     // Delphi 4
  {$ELSE}
    TInteger8 = COMP;      // Delphi  2 or 3
  {$ENDIF}
{$ENDIF}

  PROCEDURE CalcCRC32 (p: pointer; ByteCount:  DWORD; VAR CRCvalue:  DWORD);
  PROCEDURE CalcFileCRC32 (FromName:  STRING; VAR CRCvalue:  DWORD;
              VAR TotalBytes:  TInteger8;
              VAR error:  WORD);

type
  String50 = string[50];

  PFileInfo = ^TFileInfo;
  TFileInfo = record
                Name:String50;
                Start,Size:integer;
                CRC:DWORD;
              end;
  TFileList = class
  private
    FList:TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(FI:TFileInfo);
    function GetItem(Index:integer):TFileInfo;
    procedure Delete(Index:integer);
    procedure Clear;
    function Count:integer;
  end;

  TPackageFile = record
                   FileStream,                   IndexStream :TFileStream;                   Name,                   IndexName: string;                   Files:TFileList;                   Opened:boolean;                 end;

  PackageFile = TPackageFile;

  procedure InitPackFile(var p:PackageFile);

  procedure OpenPackFile(var p:PackageFile; FileName:String; Mode:integer=fmOpenReadWrite);
  procedure ClosePackFile(var p:PackageFile);

  procedure ReadToStream(var p:PackageFile; Index:integer; Stream:TStream);
  procedure ReadToFile(var p:PackageFile; Index:integer; FileName:string='');

  procedure AddFileFromStream(var p:PackageFile; Stream:TStream; NameInPack:String50='');
  procedure AddFileFromFile(var p:PackageFile; FileName:String; NameInPack:String50='');

  procedure WriteIndexFile(var p:PackageFile);
  procedure IndexToStrList(var p:PackageFile; List:TStrings);

const
  sIndexDoesNotExist = 'Indexdatei %s existiert nicht!';
  sFileNotOpened = 'Datei %s ist nicht geöffnet!';
  sIndexOutOfRange = 'Ungültiger Index %d';

  IndexExt = '.ind';
  FileExt  = '.pck';

implementation

USES
  Dialogs;

CONST
  table:  ARRAY[0..255] OF DWORD =
 ($00000000, $77073096, $EE0E612C, $990951BA,
  $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116,
  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

  $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086,
  $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
  $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

  $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

// Use CalcCRC32 as a procedure so CRCValue can be passed in but
// also returned.  This allows multiple calls to CalcCRC32 for
// the "same" CRC-32 calculation.
PROCEDURE CalcCRC32 (p:  pointer; ByteCount:  DWORD; VAR CRCValue:  DWORD);
  // The following is a little cryptic (but executes very quickly).
  // The algorithm is as follows:
  //  1.  exclusive-or the input byte with the low-order byte of
  //      the CRC register to get an INDEX
  //  2.  shift the CRC register eight bits to the right
  //  3.  exclusive-or the CRC register with the contents of
  //      Table[INDEX]
  //  4.  repeat steps 1 through 3 for all bytes

 VAR
  i:  DWORD;
  q:  ^BYTE;
BEGIN
  q := p;
  FOR   i := 0 TO ByteCount-1 DO BEGIN
    CRCvalue := (CRCvalue SHR 8)  XOR
                Table[ q^ XOR (CRCvalue AND $000000FF) ];
    INC(q)
  END
END {CalcCRC32};


// The CRC-32 value calculated here matches the one from the PKZIP program.
// Use MemoryStream to read file in binary mode.
PROCEDURE CalcFileCRC32 (FromName:  STRING; VAR CRCvalue:  DWORD;
          VAR TotalBytes:  TInteger8;
          VAR error:  WORD);
  VAR
    Stream:  TMemoryStream;
BEGIN
  error := 0;
  CRCValue := $FFFFFFFF;
  Stream := TMemoryStream.Create;
  TRY
    TRY
      Stream.LoadFromFile(FromName);
      IF   Stream.Size > 0
      THEN CalcCRC32 (Stream.Memory, Stream.Size, CRCvalue)
    EXCEPT
      ON E: EReadError DO
       error := 1
    END;

    CRCvalue := NOT CRCvalue;
    TotalBytes := Stream.Size
  FINALLY
    Stream.Free
  END;
END {CalcFileCRC32};


//------------------------------------------------------------------------------


{ TFileList }

constructor TFileList.Create;
begin
  inherited Create;
  FList:= TList.Create;
end;

destructor TFileList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TFileList.Add(FI: TFileInfo);
var tmp:PFileInfo;
begin
  if not Assigned(FList) then exit;
  New(tmp);
  with tmp^ do begin
    Name := FI.Name;
    Start:= FI.Start;
    Size := FI.Size;
    CRC  := FI.CRC;
  end;
  FList.Add(tmp);
end;

procedure TFileList.Delete(Index: integer);
begin
  if not Assigned(FList) then exit;
  if (Index>Count-1) or
     (Index<0) then
    Exception.CreateFmt(sIndexOutOfRange,[Index]);
  FList.Delete(Index);
end;

function TFileList.GetItem(Index: integer): TFileInfo;
begin
  if not Assigned(FList) then exit;
  if (Index>Count-1) or
     (Index<0) then
    Exception.CreateFmt(sIndexOutOfRange,[Index]);
  Result:= TFileInfo(FList.Items[Index]^);
end;

procedure TFileList.Clear;
begin
  if not Assigned(FList) then exit;
  FList.Clear;
end;

function TFileList.Count: integer;
begin
  Result:= 0;
  if not Assigned(FList) then exit;
  Result:= FList.Count;
end;


procedure ReadIndexFile(var p:PackageFile);
var tmp:TFileInfo;
begin
  p.Files.Clear;
  p.IndexStream.Seek(soFromBeginning,0);
  FillChar(tmp,sizeof(tmp),#0);
  while p.IndexStream.Read(tmp,sizeof(tmp))=sizeof(tmp) do begin
    p.Files.Add(tmp);
    FillChar(tmp,sizeof(tmp),#0);
  end;
end;

procedure InitPackFile(var p:PackageFile);
begin
  ClosePackFile(p);
  FillChar(p,SizeOf(p),0);
end;

procedure TestOpened(var p:PackageFile);
begin
  if not p.Opened then
    raise Exception.CreateFmt(sFileNotOpened,[p.Name]);
end;

procedure OpenPackFile(var p:PackageFile; FileName:String; Mode:integer=fmOpenReadWrite);
begin
  with p do begin
    if Opened then ClosePackFile(p);
    try
      if FileName = '' then exit;
      Name:= FileName;
      IndexName:= ChangeFileExt(Name,IndexExt);
      if not FileExists(FileName) then Mode := fmCreate;
      FileStream:= TFileStream.Create(FileName,Mode);
      IndexStream:= TFileStream.Create(IndexName,Mode);
      Files:= TFileList.Create;
      ReadIndexFile(p);
      Opened:= true;
    except
      Opened:= false;
    end;
  end;
end;

procedure ClosePackFile(var p:PackageFile);
begin
  with p do begin
    if Opened then begin
      Opened:= false;
      FileStream.Free;
      IndexStream.Free;
      Files.Free;
      Name:= '';
      p.Files.Clear;
    end;
  end;
end;

procedure ReadToStream(var p:PackageFile; Index:integer; Stream:TStream);
var fi:TFileInfo;
    buf:array of byte;
    crc:DWord;
begin
  TestOpened(p);
  if (Index>p.Files.Count-1) or
     (Index<0) then
    Exception.CreateFmt(sIndexOutOfRange,[Index]);
  fi:= p.Files.GetItem(Index);
  GetMem(buf,fi.Size);
  try
    p.FileStream.Seek(soFromBeginning,fi.Start);
    p.FileStream.Read(buf,fi.Size);
    CalcCRC32(buf,SizeOf(buf),crc);
    if crc = fi.CRC then
      Stream.Write(buf,SizeOf(buf));
  finally
    FreeMem(buf);
  end;
end;

procedure ReadToFile(var p:PackageFile; Index:integer; FileName:string='');
var m:TMemoryStream;
    n:string;
begin
  TestOpened(p);
  m:= TMemoryStream.Create;
  try
    ReadToStream(p,Index,m);
    if FileName='' then n:= ExtractFilePath(ParamStr(0))+
                            p.Files.GetItem(Index).Name
                   else n:= FileName;
    m.SaveToFile(n);
  finally
    m.Free;
  end;
end;

procedure AddFileFromStream(var p:PackageFile; Stream:TStream; NameInPack:String50);
var fi:TFileInfo;
    buf:array of char;
    crc:DWord;
    i:integer;
begin
  TestOpened(p);                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
  FillChar(fi,SizeOf(fi),#0);
  i:= Stream.Size;
  fi.Name:= NameInPack;
  fi.Start:= p.FileStream.Size;
  fi.Size:= i;
  SetLength(buf,i);
  GetMem(buf,i);
  try
    FillChar(buf,i,0);
    Stream.Seek(soFromBeginning,0);
    Stream.Read(buf,SizeOf(buf));
    CalcCRC32(buf,SizeOf(Buf),Crc);
    fi.CRC:= crc;
    p.FileStream.Seek(soFromEnd,0);
    p.FileStream.Write(buf,SizeOf(buf));
  finally
    FreeMem(buf);
  end;
end;

procedure AddFileFromFile(var p:PackageFile; FileName:String; NameInPack:String50='');
var m:TMemoryStream;
    n:string;
begin
  TestOpened(p);
  m:= TMemoryStream.Create;
  try
    m.LoadFromFile(FileName);
    if NameInPack='' then n:= ExtractFileName(FileName)
                     else n:= NameInPack;
    AddFileFromStream(p,m,n);
  finally
    m.Free;
  end;
end;

procedure WriteIndexFile(var p:PackageFile);
var i:integer;
    fi:TFileInfo;
begin
  TestOpened(p);
  with p do begin
    IndexStream.Size:= 0;
    for i:= 0 to Files.Count-1 do begin
      fi:= Files.GetItem(i);
      IndexStream.Write(fi,sizeof(fi));
    end;
  end;
end;

procedure IndexToStrList(var p:PackageFile; List:TStrings);
var i:integer;
begin
  TestOpened(p);
  for i:=0 to p.Files.Count-1 do
    List.Add(p.Files.GetItem(i).Name);
end;

end.

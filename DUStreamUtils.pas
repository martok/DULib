unit DUStreamUtils;

interface

uses Classes, SysUtils;

procedure CreateStream(var stream:TFileStream; FileName:TFileName; Mode:integer);
procedure FreeStream  (var stream:TFileStream);

procedure Write       (stream:TFileStream; const Buffer; Count: Longint);
procedure WriteString (stream:TFileStream; s:String);
procedure WriteInteger(stream:TFileStream; i:Integer);
procedure WriteBool   (stream:TFileStream; b:Boolean);
procedure WriteReal   (stream:TFileStream; r:Real);
procedure WriteRecord (stream:TFileStream; const rec; Size:integer);

procedure Read       (stream:TFileStream; var Buffer; Count: Longint);
function ReadString (stream:TFileStream):String;
function ReadInteger(stream:TFileStream):Integer;
function ReadBool   (stream:TFileStream):Boolean;
function ReadReal   (stream:TFileStream):Real;
procedure ReadRecord (stream:TFileStream;var rec; Size:integer);

implementation

procedure CreateStream(var stream:TFileStream; FileName:TFileName; Mode:integer);
begin
  if assigned(stream) then FreeStream(stream);
  stream:= TFileStream.Create(FileName,Mode);
end;

procedure FreeStream  (var stream:TFileStream);
begin
  if not assigned(stream) then exit;
  stream.Free;
  stream := nil;
end;

procedure Write(stream:TFileStream; const Buffer; Count: Longint);
begin
  if not assigned(stream) then exit;
  stream.Write(Buffer, Count);
end;

procedure WriteString(stream:TFileStream; s:String);
var tmp:ShortString;
begin
  FillChar(tmp,255,#0);
  tmp[0]:=#0;
  tmp:= copy(s,1,255);
  Write(stream, tmp, ord(tmp[0])+1);
end;

procedure WriteInteger(stream:TFileStream; i:Integer);
begin
  Write(stream, i, sizeof(i));
end;

procedure WriteBool(stream:TFileStream; b:Boolean);
begin
  Write(stream, b, sizeof(b));
end;

procedure WriteReal(stream:TFileStream; r:Real);
begin
  Write(stream, r, sizeof(r));
end;

procedure WriteRecord (stream:TFileStream; const rec; Size:integer);
begin
  Write(stream, rec, Size);
end;

////////////////////////////////////////////////////////////////////////////////

procedure Read       (stream:TFileStream; var Buffer; Count: Longint);
begin
  if not assigned(stream) then exit;
  stream.Read(buffer,Count);
end;

function ReadString (stream:TFileStream):String;
var tmp:ShortString;
begin
  Read(stream,tmp[0],1);
  Read(stream,tmp[1],Byte(tmp[0]));
  Result:= tmp;
end;

function ReadInteger(stream:TFileStream):Integer;
begin
  Read(stream,Result,sizeof(result));
end;

function ReadBool   (stream:TFileStream):Boolean;
begin
  Read(stream,Result,sizeof(result));
end;

function ReadReal   (stream:TFileStream):Real;
begin
  Read(stream,Result,sizeof(result));
end;

procedure ReadRecord (stream:TFileStream;var rec; Size:integer);
begin
  Read(stream,rec,size);
end;

end.

unit DUFileUtils;

interface

uses Windows, SysUtils, Classes;

  function ChangeExt(const FileName, NewExt: string):string;
  function GetFileDate(const FileName:string):TDateTime;
  function Delete(FileName:string):boolean;
  function GetDriveNumber(drive:Char):byte;
  function GetDiskFree(DriveNr:byte):integer;
  function GetDiskSize(DriveNr:byte):integer;
  function ExractExt(const FileName:string): string;
  function ShortName(const FileName:string): string;
  function ExtractPath(const FileName:string): string;
  function CreateFile(const FileName:string; Mode:integer): integer;
  procedure CloseFile(const FileName:string);
  procedure Read(Var ReadTo; count:integer);
  procedure Write(WriteFrom; Count:integer);


implementation

var Fil:TFileStream;



function ChangeExt(const FileName, NewExt: string):string;
var Ext:string;
begin
 if NewExt[0]<>'.' then ext:= '.' + newExt
   else ext:=NewExt;
 Result:= ChangeFileExt(FileName,Ext);
end;

function GetFileDate(const FileName:string):TDateTime;
begin
 result:= FileDateToDateTime(FileAge(FileName));
end;

function Delete(FileName:string):boolean;
begin
 Result:=DeleteFile(FileName);
end;

function GetDriveNumber(drive:Char):byte;
begin
 Case LowerCase(Char(drive)) of
    a: result:= 1;
    b: result:= 2;
    c: result:= 3;
    d: result:= 4;
    e: result:= 5;
    f: result:= 6;
    g: result:= 7;
    h: result:= 8;
    i: result:= 9;
    j: result:= 10;
    k: result:= 11;
    l: result:= 12;
    m: result:= 13;
    n: result:= 14;
    o: result:= 15;
    p: result:= 16;
    q: result:= 17;
    r: result:= 18;
    s: result:= 19;
    t: result:= 20;
    u: result:= 21;
    v: result:= 22;
    w: result:= 23;
    x: result:= 24;
    y: result:= 25;
    z: result:= 26;
 end;
end;


function GetFreeDisk(DriveNr:byte):integer;
begin
  result:= DiskFree(DriveNr);
end;

function GetDiskSize(DriveNr:byte):integer;
begin
  Result:=DiskSize(DriveNr);
end;

function ExractExt(const FileName:string): string;
begin
  result:= ExtractFileExt(FileName);
end;

function ShortName(const FileName:string): string;
begin
  Result:= ExtractFileName(FileName);
end;

function ExtractPath(const FileName:string): string;
begin
   result:= ExtractFilePath(FileName);
end;

function CreateFile(const FileName:string; Mode:word): integer;
begin
 Fil:=TFileStream.Create(FileName,Mode);
 Result:=Fil;
end;

procedure CloseFile;
begin
 Fil.Free;
end;

procedure Read(Var ReadTo; Count:integer): string;
var Buffer:variant;
begin
 Fil.Read(buffer,Count);
 ReadTo:=buffer;
end;

procedure Write(WriteFrom:variant): string;
begin
 Fil.Write(WriteFrom,count);
end;

end.

{-----------------------------------------------------------------------------
 Unit Name: DUMiscUtils
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   iif routines, Version information

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUMiscUtils;

interface

uses SysUtils, Windows, DUStrings;

type
  TVersionInfo = record
                   Version : ShortString;
                   Description,
                   CopyRight,
                   OrigName,
                   Company,
                   ProductName,
                   Language : string;
                   Major,Minor,Rev,Build:Word;
                   Flags: DWORD;
                 end;

function iif(Condition:boolean; IfTrue:string;  IfFalse:string=''):string; overload;
function iif(Condition:boolean; IfTrue:TObject; IfFalse:TObject=nil):TObject; overload;
function iif(Condition:boolean; IfTrue:integer; IfFalse:integer=0):integer; overload;
function iif(Condition:boolean; IfTrue:int64;   IfFalse:int64=0):int64; overload;
function iif(Condition:boolean; IfTrue:double;  IfFalse:double=0):double; overload;

function GetErrText(Err: Integer): String;
function GetVersionInfo(AFilename:String=''):TVersionInfo;

implementation

function iif(Condition:boolean; IfTrue, IfFalse:string):string;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function iif(Condition:boolean; IfTrue, IfFalse:TObject):TObject;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function iif(Condition:boolean; IfTrue, IfFalse:integer):integer;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function iif(Condition:boolean; IfTrue, IfFalse:int64):int64;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function iif(Condition:boolean; IfTrue, IfFalse:double):double;
begin
  if Condition then Result:= IfTrue else Result:= IfFalse;
end;

function GetErrText(Err: Integer): String;
begin
  Result:= SysErrorMessage(Err);
end;

function GetVersionInfo(AFilename:String):TVersionInfo;
type
  PLanguageCharSet = ^TLanguageCharSet;
  TLanguageCharSet = packed record L,C : Word end;

var
  VerInfoSize     : Integer;
  VerInfo         : Pointer;
  BufLen          : DWord;
  LanguageCharSet : PLanguageCharSet;
  s               : String;
  pFixFInfo: PVSFixedFileInfo;
  nFixFInfo: DWORD;

  function GetStringValue(const AIdent:String):string;
  var
    BufLen : DWord;
    p      : Pointer;
  begin
    if VerQueryValue(VerInfo,
                     PChar(Format('\StringFileInfo\%.4x%.4x\%s',
                                  [LanguageCharSet^.L,LanguageCharSet^.C,AIdent])),
                     p,BufLen) and (BufLen>0) then begin
      SetString(Result,PChar(p),BufLen-1);
    end
    else begin
      Result:='';
    end;
  end;

begin
  if AFilename='' then AFilename:= ParamStr(0);
  FillChar(Result,Sizeof(Result),0);
  VerInfoSize:=GetFileVersionInfoSize(PChar(AFilename),BufLen);
  if VerInfoSize>0 then begin
    GetMem(VerInfo,VerInfoSize);
    try
      if GetFileVersionInfo(PChar(AFilename),0,VerInfoSize,VerInfo) then begin
        if VerQueryValue(VerInfo, '\', Pointer(pFixFInfo), nFixFInfo) then
        begin
          Result.Flags:= pFixFInfo^.dwFileFlags;
        end;
        // Sprach und Zeichensatz-ID ermitteln
        if VerQueryValue(VerInfo,
                         '\VarFileInfo\Translation',
                         Pointer(LanguageCharSet),BufLen) then begin
          // Stringfelder auslesen
          s:= GetStringValue('FileVersion');
          Result.Version:= s;
          Result.Major:= StrToIntDef(fetchToken(s,'.'),0);
          Result.Minor:= StrToIntDef(fetchToken(s,'.'),0);
          Result.Rev:= StrToIntDef(fetchToken(s,'.'),0);
          Result.Build:= StrToIntDef(s,0);

          Result.Description:=GetStringValue('FileDescription');
          Result.CopyRight:=GetStringValue('LegalCopyright');
          Result.OrigName:=GetStringValue('OriginalName');
          Result.Company:=GetStringValue('CompanyName');
          Result.ProductName:=GetStringValue('ProductName');
          // Sprache auslesen
          SetLength(s,100);
          BufLen:=VerLanguageName(LanguageCharSet.L,PChar(s),100);
          if BufLen>0 then begin
            SetLength(s,StrLen(PChar(s)));
            Result.Language:= s;
          end;
        end;
      end;
    finally
      FreeMem(VerInfo,VerInfoSize);
    end;
  end;
end;

end.
 

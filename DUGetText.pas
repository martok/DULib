{-----------------------------------------------------------------------------
 Unit Name: DUGetText
 Author:    Sebastian H�tter
 Date:      2006-08-01
 Purpose:   Translation module

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUGetText;

interface

uses Windows, Classes, SysUtils, CRC32, Controls, typinfo;

type
  String50 = string[50];

  TLangHeader = packed record
                  ProgramID:DWORD;
                  LangID,SubLangID:WORD;
                  LangName,
                  Translator,Comments: String50;
                  ItemCount:DWORD;
                end;
  TLangItemHead = packed record
                    StringID,
                    Length:DWORD;
                  end;
  TLangItem = record
                Head:TLangItemHead;
                Data:String;
              end;

  TTranslationTable = record
                        Header:TLangHeader;
                        Items: array of TLangItem;
                      end;

const UNKNOWN = $FFFF;
      FilterStr = '*.lang';

procedure ListLanguages(ProgID:DWORD; List:TStrings; Path:string='');
function FindLanguage(ProgID:DWORD; LangID, SubLangID:WORD; Path:string=''):boolean;
procedure ReadLanguage(var Data:TTranslationTable; Stream:TStream);
procedure WriteLanguage(Data:TTranslationTable; Stream:TStream);
function _T(Name:string):string;
procedure TranslateForm(Form:TComponent; Prefix:string);
function ToStrID(Str:String):DWORD;
function GetRaw:TTranslationTable;

implementation

var TransTbl:TTranslationTable;

procedure ListLanguages(ProgID:DWORD; List:TStrings; Path:string='');
var sr:TSearchRec;
    st:TFileStream;
    tbl:TTranslationTable;
begin
  if Path='' then Path:= ExtractFilePath(ParamStr(0))+FilterStr;
  if FindFirst(Path,faAnyFile,sr)=0 then
    repeat
      if ((sr.Attr and faDirectory)=0) and(copy(sr.name,1,1)<>'.') then begin
        st:= TFileStream.Create(ExtractFilePath(Path)+sr.Name,fmOpenRead or fmShareDenyNone);
        try
          ReadLanguage(tbl,st);
          if ((ProgID   =UNKNOWN) or (tbl.Header.ProgramID=ProgID)) then begin
            List.Add(format('%s|%.8x|%.8x|%s',
                  [tbl.Header.LangName,tbl.header.LangID,tbl.header.SubLangID,sr.Name]));
          end;
        finally
          st.Free;
        end;
      end;
    until FindNext(sr)<>0;
  FindClose(sr);
end;

function FindLanguage(ProgID:DWORD; LangID, SubLangID:WORD; Path:string=''):boolean;
var sr:TSearchRec;
    st:TFileStream;
    tbl:TTranslationTable;
begin
  Result:= false;
  if Path='' then Path:= ExtractFilePath(ParamStr(0))+FilterStr;
  if FindFirst(Path,faAnyFile,sr)=0 then
    repeat
      if ((sr.Attr and faDirectory)=0) and (copy(sr.name,1,1)<>'.') then begin
        st:= TFileStream.Create(ExtractFilePath(Path)+sr.Name,fmOpenRead or fmShareDenyNone);
        try
          ReadLanguage(tbl,st);
          if ((ProgID   =UNKNOWN) or (tbl.Header.ProgramID=ProgID)) and
             ((LangID   =UNKNOWN) or (tbl.Header.LangID   =LangID)) and
             ((SubLangID=UNKNOWN) or (tbl.Header.SubLangID=SubLangID)) then begin
            TransTbl:= tbl;
            Result:= true;
            break;
          end;
        finally
          st.Free;
        end;
      end;
    until FindNext(sr)<>0;
  FindClose(sr);
end;

procedure ReadLanguage(var Data:TTranslationTable; Stream:TStream);
var i:integer;
begin
  Stream.Read(Data.Header,Sizeof(Data.Header));
  SetLength(Data.Items,Data.Header.ItemCount);
  for i:= 0 to Data.Header.ItemCount -1 do begin
    Stream.Read(Data.Items[i].Head,Sizeof(Data.Items[i].Head));
    SetLength(Data.Items[i].Data,Data.Items[i].Head.Length);
    Stream.Read(Data.Items[i].Data[1],Data.Items[i].Head.Length);
  end;
end;


procedure WriteLanguage(Data:TTranslationTable; Stream:TStream);
var i:integer;
begin
  Stream.Write(Data.Header,Sizeof(Data.Header));
  for i:= 0 to Data.Header.ItemCount -1 do begin
    Stream.Write(Data.Items[i].Head,Sizeof(Data.Items[i].Head));
    Stream.Write(Data.Items[i].Data[1],Data.Items[i].Head.Length);
  end;
end;

function _T(Name:string):string;
var i:integer;
    v:DWORD;
begin
  v:= ToStrID(Name);
  Result:= Name;
  for i:= 0 to TransTbl.Header.ItemCount-1 do begin
    if TransTbl.Items[i].Head.StringID=v then begin
      Result:= TransTbl.Items[i].Data;
      break;
    end;
  end;
end;

procedure TranslateForm(Form:TComponent; Prefix:string);
var i:integer;
  procedure Tran(C:TComponent; Prop:string; suffix:string='');
  var
    p:string;
  begin
    if not IsPublishedProp(C,Prop) then exit;
    p:= GetPropValue(C,Prop);
    if (copy(p,1,1)='$') then
      SetPropValue(C,Prop,_T(Prefix+'_'+copy(p,2,maxint)));
  end;

begin
  Tran(Form,'Caption');
  Tran(Form,'Hint','_Hint');
  for i:= 0 to Form.ComponentCount-1 do begin
    Tran(Form.Components[i],'Caption');
    Tran(Form.Components[i],'Hint','_Hint');
    if Form.Components[i] is TWinControl then
      TranslateForm(Form.Components[i],Prefix+'_'+Form.Components[i].Name);
  end;
end;

function ToStrID(Str:String):DWORD;
begin
  CalcStringCRC32(lowercase(Str), Result);
end;

function GetRaw:TTranslationTable;
begin
  Result:= TransTbl;
end;


end.

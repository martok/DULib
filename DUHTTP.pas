{-----------------------------------------------------------------------------
 Unit Name: DUHTTP
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Get files via HTTP. Special routines for Text and Graphics

 History:   2006-08-01 initial release
-----------------------------------------------------------------------------}
unit DUHTTP;

interface

uses HttpProt, Graphics, graphicEx, Classes, Windows, Forms;

function GetHTTPIcs(const AURL: string; var str: TStream; TimeOut:integer=10000): boolean;
function GetHTTPText(url:string):string;
procedure GetHTTPGraphic(url: string; var Gr: TGraphic);

implementation

type

TEvClass = class
procedure HTTPDone(Sender  : TObject; RqType  : THttpRequest; ErrCode : Word);
end;

procedure TEvClass.HTTPDone(Sender  : TObject; RqType  : THttpRequest; ErrCode : Word);
begin
  THttpCli(Sender).Tag:= 1;
end;


function GetHTTPIcs(const AURL:string; var str:TStream; TimeOut:integer):boolean;
var httpcli:THttpCli;
    start:DWORD;
    evc:TEvClass;
begin
  if not Assigned(str) then
    str := TMemoryStream.Create;
  httpcli:= THttpCli.Create(nil);
  evc:= TEvClass.Create;
  try
    httpcli.URL        := AURL;
    httpcli.RcvdStream := str;
    httpcli.OnRequestDone:= evc.HTTPDone;
    start:= GetTickCount;
    httpcli.GetASync;
    while (httpcli.Tag=0) and (GetTickCount-start<DWORD(TimeOut)) do
      application.ProcessMessages;
    Result:= httpcli.StatusCode=200;
  finally
    httpcli.Free;
    evc.Free;
  end;
end;

function GetHTTPText(url: string): string;
var s:TStringStream;
begin
  s:= TStringStream.Create('');
  try
    if GetHTTPIcs(url,tstream(s)) then Result:= s.DataString;
  finally
    s.Free;
  end;
end;

procedure GetHTTPGraphic(url: string; var Gr:TGraphic);
var s:TMemoryStream;
    gc:TGraphicExGraphicClass;
begin
  s:= TMemoryStream.Create;
  try
    if GetHTTPIcs(url,tstream(s)) then begin
      s.Position:= 0;
      gc:= FileFormatList.GraphicFromContent(s);
      if assigned(gc) and not (gr is gc) then begin
        if Assigned(gr) then gr.Free;
        Gr:= gc.Create;
      end;
      if assigned(gr) then
        Gr.LoadFromStream(s);
    end;
  finally
    s.Free;
  end;
end;

end.
 
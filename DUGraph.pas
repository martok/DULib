unit DUGraph;

interface

uses Windows, SysUtils;

type TDCRec=record
              wnd:HWND;
              dc :HDC;
            end;

procedure OpenDC(Window:HWND);

procedure MoveTo(x,y:integer);
procedure LineTo(x,y:integer);
procedure OutTextXY(x,y:integer;s:string);

procedure CloseDC;


var Using:TDCRec;

const EmptyDCRec:TDCRec=(wnd:INVALID_HANDLE_VALUE; dc:INVALID_HANDLE_VALUE);

implementation

procedure OpenDC(Window:HWND);
begin
  if (Using.wnd=EmptyDCRec.wnd) and
     (Using.dc =EmptyDCRec.dc) then CloseDc;
  Using.DC:= GetDC(Window);
  Using.wnd:= Window;
end;

procedure MoveTo(x,y:integer);
begin
  windows.MoveToEx(Using.dc,x,y,nil);
end;

procedure LineTo(x,y:integer);
begin
  windows.LineTo(Using.dc,x,y);
end;

procedure OutTextXY(x,y:integer;s:string);
var p:PChar;
begin
  p:= strNew(pchar(s));
  windows.TextOut(using.dc,x,y,p,length(s)+1);
end;

procedure CloseDC;
begin
  ReleaseDC(Using.wnd,Using.dc);
  Using:=EmptyDCRec;
end;


end.

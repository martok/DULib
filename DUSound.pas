unit DUSound;


interface

uses Windows, Forms;

{Einmal: }
procedure BeepSingle(Duration:integer);
procedure BeepSingleEx(Duration,Frequency:integer);

{Mehrmals: }
procedure BeepMultiple(Duration,Count:integer);
procedure BeepMultipleEx(Duration,Count,Frequency:integer);

{Pause mit ProcessMassages: }
procedure Delay(Msec:Integer);


implementation

procedure delay(Msec:integer);
var i:integer;
begin
 i:=GetTickCount;
 repeat
   Application.ProcessMessages;
 until
   GetTickCount >= i + msec;
end;

{
procedure Delay(ATime:Integer);
var Start : Integer;
begin
  Start:=GetTickCount;
  repeat
     Application.ProcessMessages;
  until GetTickCount-Start > ATime;
end;
}

procedure DoSound(Hz: Word);  //Win9x
var
faktor: Word;
begin
  faktor := round(1192840 / Hz);
  asm
    push    ax
    push    bx
    in      al,61h
    or      al,3
    out     61h,al
    mov     al,0B6h
    out     43h,al
    mov     bx,faktor
    mov     al,bl
    out     42h,al
    mov     al,bh
    out     42h,al
    pop     bx
    pop     ax
  end;
end;

procedure NoSound;    //Win9x
asm
  push    ax
  in      al,61h
  and     al,0Dh
  out     61h,al
  pop     ax
  ret
end;

// fuer Windows 95
procedure BeepWin9x(frequency,duration:integer);
begin
   DoSound(frequency);
   Delay(duration);
   NoSound;
end;


{   VALUE                   |  PLATFORM
----------------------------|----------------------------------
VER_PLATFORM_WIN32s         |  Win32s on Windows 3.1.
VER_PLATFORM_WIN32_WINDOWS  |  Windows 95, Windows 98, or Windows Me.
VER_PLATFORM_WIN32_NT       |  Windows NT 3.51, Windows NT 4.0,
                            |  Windows 2000, Windows XP, or Windows .NET Server.
}

function IsWindowsNT:boolean;
var osv:OSVERSIONINFO;
begin
  osv.dwOSVersionInfoSize:= sizeof(osv);
  GetVersionex(osv);
  Result:= osv.dwPlatformId = VER_PLATFORM_WIN32_NT;
end;

procedure BeepSingle(duration:integer);
begin
  if IsWindowsNT then
    Windows.Beep(1000,duration)
  else
    BeepWin9x(1000,duration);
end;

procedure BeepSingleEx(duration,Frequency:integer);
begin
  if IsWindowsNT then
    Windows.Beep(Frequency,duration)
  else
    BeepWin9x(Frequency,duration);
end;

procedure BeepMultiple(duration,Count:integer);
var i:integer;
begin
  for i:=1 to Count do begin
    if IsWindowsNT then
      Windows.Beep(1000,duration)
    else
      BeepWin9x(1000,duration);
    Delay(duration div 5);
  end;
end;

procedure BeepMultipleEx(duration,Count,Frequency:integer);
var i:integer;
begin
  for i:=1 to Count do begin
    if IsWindowsNT then
      Windows.Beep(Frequency,duration)
    else
      BeepWin9x(Frequency,duration);
    Delay(duration div 5);
  end;
end;

end.
 

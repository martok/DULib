unit DUDirsApps;

interface

uses Windows, SysUtils, Consts,ShlObj, ShellAPI, ComObj, ActiveX, Classes,Forms;

const // Windows Constants:
      DIR_DESKTOP                  = CSIDL_DESKTOP;
      DIR_INTERNET                 = CSIDL_INTERNET;
      DIR_PROGRAMS                 = CSIDL_PROGRAMS;
      DIR_CONTROLS                 = CSIDL_CONTROLS;
      DIR_PRINTERS                 = CSIDL_PRINTERS;
      DIR_PERSONAL                 = CSIDL_PERSONAL;
      DIR_FAVORITES                = CSIDL_FAVORITES;
      DIR_STARTUP                  = CSIDL_STARTUP;
      DIR_RECENT                   = CSIDL_RECENT;
      DIR_SENDTO                   = CSIDL_SENDTO;
      DIR_BITBUCKET                = CSIDL_BITBUCKET;
      DIR_STARTMENU                = CSIDL_STARTMENU;
      DIR_DESKTOPDIRECTORY         = CSIDL_DESKTOPDIRECTORY;
      DIR_DRIVES                   = CSIDL_DRIVES;
      DIR_NETWORK                  = CSIDL_NETWORK;
      DIR_NETHOOD                  = CSIDL_NETHOOD;
      DIR_FONTS                    = CSIDL_FONTS;
      DIR_TEMPLATES                = CSIDL_TEMPLATES;
      DIR_COMMON_STARTMENU         = CSIDL_COMMON_STARTMENU;
      DIR_COMMON_PROGRAMS          = CSIDL_COMMON_PROGRAMS;
      DIR_COMMON_STARTUP           = CSIDL_COMMON_STARTUP;
      DIR_COMMON_DESKTOPDIRECTORY  = CSIDL_COMMON_DESKTOPDIRECTORY;
      DIR_APPDATA                  = CSIDL_APPDATA;
      DIR_PRINTHOOD                = CSIDL_PRINTHOOD;
      DIR_ALTSTARTUP               = CSIDL_ALTSTARTUP;
      DIR_COMMON_ALTSTARTUP        = CSIDL_COMMON_ALTSTARTUP;
      DIR_COMMON_FAVORITES         = CSIDL_COMMON_FAVORITES;
      DIR_INTERNET_CACHE           = CSIDL_INTERNET_CACHE;
      DIR_COOKIES                  = CSIDL_COOKIES;
      DIR_HISTORY                  = CSIDL_HISTORY;

      DIR_WINDIR  = 1001;
      DIR_SYSTEM  = 1002;
      DIR_TEMP    = 1003;

function AppendBackSlash(var Dir:string; GetAsResult:boolean=true):string;
function RemoveBackSlash(var Dir:string; GetAsResult:boolean=true):string;

// Another browse-for-folder function with the ability to select an intial directory
// (other SelectDirectory functions are in FileCtrl.pas).
function SelectDirectory(const Caption, InitialDir: String; const Root: WideString;
                         ShowStatus: Boolean; out Directory: String): Boolean;
function DirectoryExists(const Name: string): Boolean;
procedure ForceDirectories(Dir: string);

function GetSpecialPath(Dir:integer):string;
procedure StartApp(App:string;Params:string='');
function ExecAndWait(const Filename, Params: string;
                     WindowState: word): boolean;
function StartAndWaitForReady(FN:string):HWND;
function FindWindowPartitial(Parent:HWND; WndName,WndClass:string):HWND;
function GetExeForFile(const Ext: String; Execute:boolean): String;
procedure MakeShellLink(LinkFile,Target:string);

implementation

// Code by: 1999-2003 Dipl. Ing. Mike Lischke. All Rights Reserved.
function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;

// callback function used in SelectDirectory to set the status text and choose an initial dir

var
  Path: array[0..MAX_PATH] of Char;
  X, Y: Integer;
  R: TRect;

begin
  case uMsg of
    BFFM_INITIALIZED:
      begin
        // Initialization has been done, now set our initial directory which is passed in lpData
        // (and set btw. the status text too).
        // Note: There's no need to cast lpData to a PChar since the following call needs a
        //       LPARAM parameter anyway.
        SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, lpData);

        // place the dialog screen centered
        GetWindowRect(hwnd, R);
        X := (Screen.Width - (R.Right - R.Left)) div 2;
        Y := (Screen.Height - (R.Bottom - R.Top)) div 2;
        SetWindowPos(hwnd, 0, X, Y, 0, 0, SWP_NOSIZE or SWP_NOZORDER);
      end;
    BFFM_SELCHANGED:
      begin
        // Set the status window to the currently selected path.
        if SHGetPathFromIDList(Pointer(lParam), Path) then SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Integer(@Path));
      end;
  end;
  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function SelectDirectory(const Caption, InitialDir: String; const Root: WideString;
                         ShowStatus: Boolean; out Directory: String): Boolean;

// Another browse-for-folder function with the ability to select an intial directory
// (other SelectDirectory functions are in FileCtrl.pas).

var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList,
  ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
  Windows: Pointer;
  Path: String;

begin
  Result := False;
  Directory := '';
  Path := InitialDir;
  if (Length(Path) > 0) and (Path[Length(Path)] = '\') then Delete(Path, Length(Path), 1);
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      IDesktopFolder.ParseDisplayName(Application.Handle, nil, PWideChar(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if ShowStatus then ulFlags := ulFlags or BIF_STATUSTEXT;
        lParam := Integer(PChar(Path));
        lpfn := BrowseCallbackProc;
      end;

      // make the browser dialog modal
      Windows := DisableTaskWindows(Application.Handle);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(Windows);
      end;

      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;
// End of Code by: 1999-2003 Dipl. Ing. Mike Lischke

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

procedure ForceDirectories(Dir: string);
begin
  if Length(Dir) = 0 then
    raise Exception.Create(SCannotCreateDir);
  while (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '\') do
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

function AppendBackSlash(var Dir:string; GetAsResult:boolean=true):string;
begin
  if (length(Dir)>0) and (Dir[Length(dir)]<>'\') then begin
    if GetAsResult then Result:= Dir+'\' else
    Dir:= Dir+'\';
  end;
end;

function RemoveBackSlash(var Dir:string; GetAsResult:boolean=true):string;
begin
  if (length(Dir)>0) and (Dir[Length(dir)]='\') then begin
    if GetAsResult then Result:= copy(Dir,1,Length(dir)-1) else
    Delete(Dir,length(dir)-1,1);
  end;
end;

function GetSpecialPath(Dir:integer):string;
var pidl: PItemIDList;
    Path: array[0..max_path] of char;
    p:string;
begin
  case Dir of
    DIR_WINDIR : GetWindowsDirectory(Path, Length(Path));
    DIR_SYSTEM : GetSystemDirectory(Path, Length(Path));
    DIR_TEMP   : GetTempPath(Length(Path),Path);
  else
    begin
      SHGetSpecialFolderLocation(0, Dir, pidl);
      SHGetPathFromIDList(pidl, path);
    end;
  end;
  p:= Path;
  Result:=AppendBackSlash(p);
end;

procedure StartApp(App:string;Params:string='');
begin
  ShellExecute(0,'open',PCHAR(App),PCHAR(params),PCHAR(ExtractFileDir(App)),SW_SHOWNORMAL);
end;

function ExecAndWait(const Filename, Params: string;
                     WindowState: word): boolean;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := '"' + Filename + '" ' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, 
                          CREATE_NEW_CONSOLE or 
                          NORMAL_PRIORITY_CLASS, NIL, 
                          PChar(ExtractFilePath(Filename)), 
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

var 
  WL: TList;
  ProcInfo: TProcessInformation;

function GetAllWindows(Handle: HWND; lParam: LPARAM): boolean; stdcall;
begin
  Result := True;
  WL.add(pointer(Handle));
end;

function GetWndClassName(Wnd:THandle):string;
var b:array[0..max_Path-1] of char;
begin
  fillchar(b,max_path,0);
  Windows.GetClassName(Wnd,b,max_Path);
  Result:= StrPas(b);
end;

function GetWndName(Wnd:THandle):string;
var b:array[0..max_Path-1] of char;
begin
  fillchar(b,max_path,0);
  Windows.GetWindowText(Wnd,b,max_Path);
  Result:= StrPas(b);
end;

function StartAndWaitForReady(FN:string):HWND;
var
  i: integer;
  tmpPid: THandle;
  tmp2: dword;
  StartInfo: TStartupInfo;
  Created:boolean;
begin
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  Result:= INVALID_HANDLE_VALUE;
  StartInfo.cb := SizeOf(TStartupInfo);
  Created := CreateProcess(nil, PChar(fn), nil, nil, False,
    CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
    nil, nil, StartInfo, ProcInfo);

  // how hard is it to find your process's hWnd? pretty fucking hard!
  if created then
  begin
    repeat
      sleep(200);

      try
        WL := TList.Create;

        if (not EnumWindows(@GetAllWindows, 0)) then Exit;

        for i := 0 to WL.Count - 1 do
        begin
          tmp2 := GetWindowThreadProcessId(HWND(WL[i]), @tmpPid);
          if (tmp2 = ProcInfo.dwThreadId) and (tmppid = ProcInfo.dwProcessId)
             and (GetWndClassName(HWND(WL[i]))='TApplication')  then begin
            Result := HWND(WL[i]);
          end;
        end;
      finally
        WL.Free;
      end;

    until Result <> INVALID_HANDLE_VALUE;
    while not IsWindowVisible(Result) do sleep(200);
  end;
end;

function FindWindowPartitial(Parent:HWND; WndName,WndClass:string):HWND;
var
  i: integer;
begin
  Result:= INVALID_HANDLE_VALUE;
  try
    WL := TList.Create;

    if (not EnumWindows(@GetAllWindows, 0)) then Exit;

    for i := 0 to WL.Count - 1 do
    begin
      if ((WndClass='') or (pos(WndClass,GetWndClassName(HWND(WL[i])))>0)) and
         ((WndName='') or (pos(WndName,GetWndName(HWND(WL[i])))>0)) and
         ((Parent=0) or (GetWindowLong(HWND(WL[i]),GWL_HWNDPARENT)=Integer(Parent))) then begin
        Result := HWND(WL[i]);
      end;
    end;
  finally
    WL.Free;
  end;
end;

function GetExeForFile(const Ext: String; Execute:boolean): String;
var x: Integer;
    f:File;
    fn:string;
    r:array[0..MAX_PATH+1]of char;
begin
  fn:= ExtractFilePath(ParamStr(0))+'file'+ext;
  Assign(f,fn);
  Rewrite(f);
  SetLength(Result, MAX_PATH);
  FillChar(r,sizeof(r),0);
  x:= FindExecutable(PChar(fn), nil, r);
  if x<32
//  then SetLength(Result, StrLen(PChar(Result)))
  then Result:=inttostr(x)
  else Result:= StrPas(r);
  if Execute and FileExists(Result) then
    StartApp(Result);
  CloseFile(f);
  Erase(f);
end;

procedure MakeShellLink(LinkFile,Target:string);
var IObject: IUnknown;
  ILink: IShellLink;
  IFile: IPersistFile;
begin
  Target := ExpandFileName(Target); //Name des verknüpften Programms
  Target:= AnsiQuotedStr(Target,'"');
  LinkFile:= AnsiQuotedStr(LinkFile,'"');
  IObject := CreateComObject(CLSID_ShellLink);
  ILink := IObject as IShellLink;
  IFile := IObject as IPersistFile;
  with ILink do begin
    SetPath(PChar(Target));
    SetWorkingDirectory(PChar(ExtractFilePath(Target)));
  end;
  IFile.Save(PWChar(LinkFile),false);
end;

end.

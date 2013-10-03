unit DUThreads;

interface

uses Windows, SyncObjs;

type
  TLockedInteger = record
    Lock:TCriticalSection;
    Value:integer;
  end;


  Psth_ThreadInfo = ^Tsth_ThreadInfo;
  Tsth_ThreadInfo= record
    Handle:THandle;
    ThreadID:Cardinal;
    Parameter:Pointer;

    Func:Pointer;

    ReturnData:Pointer;
    ReturnSize:integer;

    StopCounter: TLockedInteger;
  end;
  Tsth_Func= function(Parameter:pointer; var TI:Tsth_ThreadInfo):integer;


procedure sth_Start(ThFunc:Tsth_Func; Parameter:Pointer; var ThreadInfo:Tsth_ThreadInfo);
procedure sth_Suspend(TI:Tsth_ThreadInfo);
procedure sth_Resume(TI:Tsth_ThreadInfo);
procedure sth_StopThread(TI:Tsth_ThreadInfo);
function sth_CheckStop(TI:Tsth_ThreadInfo):boolean;
procedure sth_WaitThread(TI:Tsth_ThreadInfo; Seconds:Cardinal);
procedure sth_WaitThreads(TIs:Array of Tsth_ThreadInfo; Seconds:Cardinal);

function sth_InitInteger(Initval:integer):TLockedInteger;
procedure sth_SafeInc(var li:TLockedInteger; Delta:integer);
procedure sth_SafeSet(var li:TLockedInteger; Value:integer);
function sth_SafeGet(var li:TLockedInteger):integer;

implementation

function ThreadInternal(TI:Pointer):integer;
begin
  Result:= Tsth_Func(Psth_ThreadInfo(TI).Func)(Psth_ThreadInfo(TI).Parameter,Psth_ThreadInfo(TI)^);
end;

procedure sth_Start(ThFunc:Tsth_Func; Parameter:Pointer; var ThreadInfo:Tsth_ThreadInfo);
var ti:Psth_ThreadInfo;
begin
  New(ti);
  ThreadInfo.Parameter:= Parameter;
  ThreadInfo.Func:= @ThFunc;
  ti^:= ThreadInfo;
  ThreadInfo.Handle:= BeginThread(nil,0,ThreadInternal,ti,CREATE_SUSPENDED,ThreadInfo.ThreadID);
  ti^:= ThreadInfo;
  sth_Resume(ThreadInfo);
end;

procedure sth_Suspend(TI:Tsth_ThreadInfo);
begin
  SuspendThread(TI.Handle);
end;

procedure sth_Resume(TI:Tsth_ThreadInfo);
begin
  ResumeThread(TI.Handle);
end;

procedure sth_StopThread(TI:Tsth_ThreadInfo);
begin
  sth_SafeInc(TI.StopCounter,-1);
end;

function sth_CheckStop(TI:Tsth_ThreadInfo):boolean;
begin
  Result:= sth_SafeGet(TI.StopCounter)<0;
end;

procedure sth_WaitThread(TI:Tsth_ThreadInfo; Seconds:Cardinal);
begin
  WaitForSingleObject(TI.Handle,Seconds);
end;

procedure sth_WaitThreads(TIs:Array of Tsth_ThreadInfo; Seconds:Cardinal);
var a:array of THandle;
    I:integer;
begin
  SetLength(a,length(TIs));
  for i:= 0 to high(TIs) do
    a[i]:= TIs[i].Handle;
  WaitForMultipleObjects(length(a),@a[0],true,Seconds);
end;

function sth_InitInteger(Initval:integer):TLockedInteger;
begin
  Result.Lock:= TCriticalSection.Create;
  sth_SafeSet(Result,InitVal);
end;

procedure sth_SafeInc(var li:TLockedInteger; Delta:integer);
begin
  li.Lock.Enter;
  inc(li.Value,Delta);
  li.Lock.Leave;
end;

procedure sth_SafeSet(var li:TLockedInteger; Value:integer);
begin
  li.Lock.Enter;
  li.Value:= Value;
  li.Lock.Leave;
end;

function sth_SafeGet(var li:TLockedInteger):integer;
begin
  li.Lock.Enter;
  Result:= li.Value;
  li.Lock.Leave;
end;

end.
 
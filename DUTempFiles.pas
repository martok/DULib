{-----------------------------------------------------------------------------
 Unit Name: DUTempFiles
 Author:    Sebastian Hütter
 Date:      2006-10-01
 Purpose:   Provides a simple list for temporary files and auto cleanup.

 History:   2006-10-01 initial release
 
------------------------------------------------------------------------------
 Copyright Notice: If you copy or modify this file, this block
   _must_ be preserved! If you are using this code in your projects,
   I would like to see a word of thanks in the About-Box or a similar place.
-----------------------------------------------------------------------------}
unit DUTempFiles;

interface

uses DUDirsApps, Classes, StrSearch;

type
  TTempFileList = class
  private
    FAutoCleanUp: boolean;

  protected
    FReservedNames,
      FTempFiles: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTempFileName(Prefix: string; Ext: string = '.tmp'; MustUnique:boolean=true): string;
    procedure AddTempFile(F: string);
    procedure Cleanup;
    procedure CleanupFilter(Filter: string);
  published
    property AutoCleanup: boolean read FAutoCleanUp write FAutoCleanUp;
  end;

var
  TempFiles: TTempFileList;

implementation

uses SysUtils;

{ TTempFileList }

constructor TTempFileList.Create;
begin
  inherited;
  FAutoCleanUp := true;
  FReservedNames := TStringList.Create;
  FTempFiles := TStringList.Create;
  FTempFiles.Sorted:=true;
  FTempFiles.Duplicates:= dupIgnore;
end;

destructor TTempFileList.Destroy;
begin
  if FAutoCleanUp then
    Cleanup;
  FReservedNames.Free;
  FTempFiles.Free;
  inherited;
end;

procedure TTempFileList.AddTempFile(F: string);
begin
  FTempFiles.Add(F);
end;

procedure TTempFileList.Cleanup;
begin
  CleanupFilter('*');
end;

procedure TTempFileList.CleanupFilter(Filter: string);
var
  i: integer;
begin
  i := 0;
  while i < FTempFiles.Count do begin
    if Like(FTempFiles[i], Filter) then begin
      DeleteFile(FTempFiles[i]);
      FTempFiles.Delete(i);
    end
    else
      Inc(i);
  end;
end;

function TTempFileList.GetTempFileName(Prefix, Ext: string; MustUnique:boolean): string;
var s:String;
begin
  s:= GetSpecialPath(DIR_TEMP)+Prefix+Ext;
  if MustUnique then
    while (FReservedNames.IndexOf(S)<>-1) and
          FileExists(S) do
      s:= GetSpecialPath(DIR_TEMP)+Prefix+IntToHex(Random(Maxint),8)+Ext;
  FReservedNames.Add(S);
  AddTempFile(S);
  Result:= S;
end;

initialization
  Tempfiles := TTempFileList.Create;
  Randomize;
finalization
  Tempfiles.Free;
end.


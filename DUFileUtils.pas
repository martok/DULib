{-----------------------------------------------------------------------------
 Unit Name: DUFileUtils
 Author:    Sebastian Hütter
 Date:      2006-08-01
 Purpose:   Routines for file information

 History:   2006-08-01 initial release
            2006-10-08 fixed GetOpen/SaveFileName
-----------------------------------------------------------------------------}
unit DUFileUtils;

interface

uses Windows, SysUtils, Classes, Dialogs;

function GetDriveNumber(drive: Char): byte;
function GetDiskFree(DriveNr: byte): int64;
function GetDiskSize(DriveNr: byte): int64;

function GetFileSize(FileName: string): int64;

function GetOpenFileName(ACaption, AFilter: string; var AFilename: string): boolean;
function GetSaveFileName(ACaption, AFilter: string; AskOverwrite: boolean; var AFilename: string): boolean;

implementation

function GetFileDate(const FileName: string): TDateTime;
begin
  result := FileDateToDateTime(FileAge(FileName));
end;

function GetDriveNumber(drive: Char): byte;
begin
  Result := ord(UpCase(drive)) - ord('A');
  if (Result < 1) or (Result > 26) then
    Result := 0;
end;

function GetDiskFree(DriveNr: byte): int64;
begin
  result := DiskFree(DriveNr);
end;

function GetDiskSize(DriveNr: byte): int64;
begin
  Result := DiskSize(DriveNr);
end;

function GetFileSize(FileName: string): Int64;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SearchRec) = 0 then // if found
{$WARNINGS OFF}
    Result := Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) + // calculate the size
      Int64(SearchREc.FindData.nFileSizeLow)
{$WARNINGS ON}
  else
    Result := -1;
  FindClose(SearchRec);
end;

function GetOpenFileName(ACaption, AFilter: string; var AFilename: string): boolean;
begin
  with TOpenDialog.Create(nil) do try
    Title := ACaption;
    Filter := AFilter;
    FileName := AFilename;
    Result := Execute;
    if Result then
      AFilename := FileName;
  finally
    Free;
  end;
end;

function GetSaveFileName(ACaption, AFilter: string; AskOverwrite: boolean; var AFilename: string): boolean;
begin
  with TSaveDialog.Create(nil) do try
    Title := ACaption;
    Filter := AFilter;
    if AskOverwrite then
      Options := Options + [ofOverwritePrompt];
    FileName := AFilename;
    Result := Execute;
    if Result then
      AFilename := FileName;
  finally
    Free;
  end;
end;

end.


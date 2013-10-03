unit DUTemplater;

interface

uses SysUtils, Classes;

type
  TTemplater = class
    FTemplate, FResults:string;
  public
    procedure LoadTemplate(FileName:string);
    procedure Restart;
    procedure PutSymbol(Sym,Value:string);
    procedure SaveResults(FileName:string);
    property Template : string read FTemplate write FTemplate;
    property Results : string read FResults;
  end;


implementation

{ TTemplater }

procedure TTemplater.LoadTemplate(FileName: string);
var sl:TStringList;
begin
  sl:= TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    FTemplate:= sl.Text;
    Restart;
  finally
    sl.Free;
  end;
end;

procedure TTemplater.PutSymbol(Sym, Value: string);
begin
  FResults:= StringReplace(FResults,Sym,Value,[rfReplaceAll]);
end;

procedure TTemplater.Restart;
begin
  FResults:= FTemplate;
end;

procedure TTemplater.SaveResults(FileName: string);
var sl:TStringList;
begin
  sl:= TStringList.Create;
  try
    sl.Text:= FResults;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

end.

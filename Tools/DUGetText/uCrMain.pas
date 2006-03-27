unit uCrMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IniFiles;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edInFile: TEdit;
    Bevel1: TBevel;
    btnBrowseInFile: TButton;
    opndlg: TOpenDialog;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbLangName: TLabel;
    lbTranslator: TLabel;
    lbComment: TLabel;
    Label5: TLabel;
    edOutFile: TEdit;
    btnBrowseOut: TButton;
    btnCompile: TButton;
    Button1: TButton;
    procedure btnBrowseInFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseOutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
  private
    { Private-Deklarationen }
    Curr:TMemIniFile;
    procedure OpenFile(FN:String);
    function GetStr(Sect,Key,Def:String):string;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses DUGetText, DUStrings, CRC32;

{$R *.DFM}

resourcestring
    InFilter  = 'LangRaw(*.lraw)|*.lraw|Alle Dateien(*.*)|*.*';
    OutFilter = 'Lang(*.lang)|*.lang|Alle Dateien(*.*)|*.*';
    AllWritten= 'Datei erzeugt!';

const
  Sig  = '## DUGETTEXT LRAW FILE ##';
  Info = 'Info';


procedure TForm1.FormCreate(Sender: TObject);
begin
  Curr:= TMemIniFile.Create('');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Curr.Free;
end;

function TForm1.GetStr(Sect, Key, Def: String): string;
begin
  Result:= Curr.ReadString(Sect,Key,Def);
  Result:= ExtractQuotedString(Result,'"');
end;

procedure TForm1.OpenFile(FN: String);
var l:TStringList;
begin
  if not FileExists(Fn) then exit;
  Curr.Clear;
  l:= TStringList.Create;
  try
    l.LoadFromFile(FN);
    if L.Count<2 then exit;
    if l[0]<>Sig then exit;
    l.Delete(0);
    Curr.SetStrings(l);
    lbLangName.Caption:= GetStr(Info,'LangName','');
    lbTranslator.Caption:= GetStr(Info,'Translator','');
    lbComment.Caption:= GetStr(Info,'Comments','');
  finally
    l.Free;
  end;
end;

procedure TForm1.btnBrowseInFileClick(Sender: TObject);
begin
  opndlg.Filter:= InFilter;
  opndlg.FileName:= edInFile.Text;
  if opndlg.Execute then
    edInFile.Text:= opndlg.FileName;
  lbLangName.Caption:='';
  lbTranslator.Caption:= '';
  lbComment.Caption:= '';
  OpenFile(edInFile.Text);
end;

procedure TForm1.btnBrowseOutClick(Sender: TObject);
begin
  opndlg.Filter:= OutFilter;
  opndlg.FileName:= edOutFile.Text;
  if opndlg.Execute then
    edOutFile.Text:= opndlg.FileName;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnCompileClick(Sender: TObject);
var tbl:TTranslationTable;
    l:TStringList;
    i:integer;
    s:string;
    st:TFileStream;
begin
  FillChar(tbl.Header,sizeof(tbl.Header),0);
  tbl.Header.ProgramID:= Curr.ReadInteger(Info,'ProgramID',0);
  tbl.Header.LangID:= Curr.ReadInteger(Info,'LangID',LANG_NEUTRAL);
  tbl.Header.SubLangID:= Curr.ReadInteger(Info,'SubLangID',SUBLANG_NEUTRAL);
  tbl.Header.LangName:= GetStr(Info,'LangName','');
  tbl.Header.Translator:= GetStr(Info,'Translator','');
  tbl.Header.Comments:= GetStr(Info,'Comments','');

  l:= TStringList.Create;
  try
    Curr.ReadSection('Items',l);
    tbl.Header.ItemCount:= l.Count;
    SetLength(Tbl.Items,tbl.Header.ItemCount);
    for i:= 0 to l.Count-1 do begin
      FillChar(Tbl.Items[i],sizeof(Tbl.Items[i]),0);
      Tbl.Items[i].Head.StringID:= ToStrID(l[i]);
      s:= GetStr('Items',l[i],'');
      ConvertSpecials(S);
      Tbl.Items[i].Head.Length:= length(s);
      tbl.Items[i].Data:= s;
    end;
    st:= TFileStream.Create(edOutFile.Text,fmCreate);
    try
      WriteLanguage(tbl,st);
    finally
      st.Free;
    end;
  finally
    l.Free;
  end;
  MessageDlg(AllWritten,mtInformation,[mbOk],0);
end;

end.

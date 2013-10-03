{-----------------------------------------------------------------------------
 Unit Name: DUXMLParser
 Author:    Sebastian Hütter
 Date:      01-08-2006
 Purpose:   Simple Parser for XML files, w/o MSXML

 History:   2006-08-01 Initial release
            2006-09-07 Rewritten Parser
            2007-03-03 Successfully passed my xml-killer test,
                       so I can call it somehow "final" 
-----------------------------------------------------------------------------}
unit DUXMLParser;

interface

uses Classes, SysUtils, Contnrs;

const
  TagStart  = '<';
  TagEnd    = '>';
  TagClose  = '/';
  TagComment= '!--';
  TagCommentEnd = '--';
  TagQuote  = '"';
  TagXMLDef = '?';
  TagSpecDef= '!';
  AttrSep   = ' ';
  ValueSep  = '=';

  NodeDelim = '/';

type
  EXMLParseError = class(Exception);

  TEleProps = (epUnk, epClose, epAutoClose, epXML, epComment, epSpecial);
  TSaveOptions = set of (soComments, soOnlyChilds);

  XMLData= Variant;

  TXMLNode = class;
  TXMLElement = class;
  TCBUserData = record
    pParam1,pParam2:Pointer;
    wParam1,wParam2:Integer;
    dwParam1,dwParam2:Cardinal;
  end;
  TCallbackProc = procedure (Node:TXMLNode; var Done, ContinueSub:boolean; var UserData:TCBUserData);

  TXMLElement = class
  private
    FName: string;
    FAttrs:TStringList;
    FText: string;
    FParent: TXMLElement;
    FSaveComments: boolean;
    FKind: TEleProps;
    FData: Pointer;
    FSaveOnlyChilds: boolean;
    function GetItem(Name: string): TXMLNode;
    procedure SetItem(Name: string; const Value: TXMLNode);
    function GetItemCount: integer;
    function GetAttr(Name: string): string;
    procedure SetAttr(Name: string; const Value: string);
    function GetAttrCount: integer;
    procedure SetParent(const Value: TXMLElement);
    function GetLevel: integer;
    function GetItemAt(Index: Integer): TXMLNode;
    procedure SetItemAt(Index: Integer; const Value: TXMLNode);
    procedure SplitTag(Tag: String; var Props: TEleProps; var Nm,
      Args: string);
    function ForceNode(Node: string): TXMLNode;
  protected
    FItems:TObjectList;
    function FindItem(Name:string; var Item:TXMLNode):boolean;

    procedure InternalParse(XML: String; var P: integer);
    procedure InternalSave(var XML:String; Lvl: integer; SaveOpts:TSaveOptions);
    function FindTag(s: String; var P: integer; var Tag: string): string;

    property Name:string read FName write FName;
    property TextData:string read FText write FText;
    property Level:integer read GetLevel;
    property Kind : TEleProps read FKind write FKind;
    property AttrCount:integer read GetAttrCount;
    property Attributes[Name:string] : string read GetAttr write SetAttr;
    property AttrList : TStringList read FAttrs;
    property SaveComments : boolean read FSaveComments write FSaveComments;
    property SaveOnlyChilds : boolean read FSaveOnlyChilds write FSaveOnlyChilds;
    property Data:Pointer read FData write FData;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property ParentNode:TXMLElement read FParent write SetParent;

    procedure LoadFromString(XML:String);
    procedure SaveToString(var XML:string);

    property ItemCount:integer read GetItemCount;
    property Items[Name:string] : TXMLNode read GetItem write SetItem; default;
    property ItemAt[Index:Integer] : TXMLNode read GetItemAt write SetItemAt;
    function AddItem(AName:string):TXMLNode;
    procedure RemoveItem(Item:TXMLElement; PreserveObject:boolean=false);
    procedure InsertAfter(Parent,Sibling:TXMLElement);
    procedure InsertBefore(Parent,Sibling:TXMLElement);
    procedure ClearItems;

    function GetNode(Node: string): TXMLNode;
    function GetAttribute(Node, Attr: string; Default:XMLData): XMLData; overload;
    procedure SetAttribute(Node, Attr: string; Value:XMLData); overload;
    function GetText(Node: string; Default:XMLData): XMLData;  overload;
    procedure SetText(Node: string; Value:XMLData);

    function GetAttribute(Attr: string; Default:XMLData): XMLData; overload;
    procedure SetAttribute(Attr: string; Value:XMLData);     overload;
    function GetText(Node: string): XMLData; overload;

    procedure WalkItems(Callback: TCallbackProc; var UserData:TCBUserData);
  end;





  TXMLNode = class(TXMLElement)
  public
    property Level;
    property Name;
    property TextData;
    property AttrCount;
    property Attributes;
    property AttrList;
    property Data;
    property Kind;
    property SaveOnlyChilds;
  end;


  TXMLFile = class(TXMLElement)
  private

  protected

  public
    procedure LoadFromFile(FileName:String);
    procedure LoadFromStream(Stream:TStream);
    procedure SaveToFile(FileName:String);
    procedure SaveToStream(Stream:TStream);
    property SaveComments;
  end;



implementation

uses DUStrings, StrUtils;

const
  AutoCloseProps = [epAutoClose, epXML, epComment, epSpecial];
  SubItemProps   = [epUnk];
  OnlyTextData   = [epSpecial, epComment];

{ TXMLElement }

constructor TXMLElement.Create;
begin
  inherited;
  FItems:= TObjectList.Create;
  FAttrs:= TStringList.Create;
  FSaveComments:= true;
  FSaveOnlyChilds:= true;
  Data:= nil;
end;

destructor TXMLElement.Destroy;
begin
  FItems.Free;
  FAttrs.Free;
  inherited;
end;

function TXMLElement.FindItem(Name: string; var Item: TXMLNode): boolean;
var i:integer;
begin
  for i:= 0 to ItemCount-1 do begin
    if IsSameText(name, TXMLElement(FItems[i]).Name) then begin
      Item:= TXMLNode(FItems[i]);
      Result:= true;
      exit;
    end;
  end;
  Item:= nil;
  Result:= false;
end;

function TXMLElement.GetItemCount: integer;
begin
  Result:= FItems.Count;
end;

function TXMLElement.GetItem(Name: string): TXMLNode;
begin
  FindItem(Name,Result);
end;

procedure TXMLElement.SetItem(Name: string; const Value: TXMLNode);
var i:integer;
begin
  for i:= 0 to ItemCount-1 do begin
    if IsSameText(name, TXMLElement(FItems[i]).Name) then begin
      FItems[i].Free;
      FItems[i]:= Value;
      exit;
    end;
  end;
end;

function TXMLElement.GetItemAt(Index: Integer): TXMLNode;
begin
  Result:= TXMLNode(FItems[Index]);
end;

procedure TXMLElement.SetItemAt(Index: Integer; const Value: TXMLNode);
begin
  FItems[Index]:= Value;
end;


function TXMLElement.FindTag(s: String; var P:integer; var Tag:string):string;
var b:boolean;
begin
  Tag:= '';
  Result:= '';
  if s[p]<>TagStart then
    repeat
      Result:= Result+s[p]; inc(p);
    until (p>=length(s)) or (s[p]=TagStart);
  repeat
    Tag:= Tag+s[p]; inc(p);
    if StartsWith(Tag,'<!--') then
      b:= EndsWith(Tag,'-->')
    else
      b:= (s[p]=TagEnd);
  until (p>=length(s)) or b;
  if (s[p]=TagEnd) then Tag:= Tag+TagEnd;
  inc(p);
end;

procedure TXMLElement.SplitTag(Tag: String; var Props:TEleProps; var Nm,Args:string);
begin
  Props:= epUnk;
  Nm:= '';
  Args:= '';
  Nm:= trim(copy(Tag,2,length(Tag)-2));
  if StartsWith(Tag,'<!--') and EndsWith(Tag,'-->') then begin
    Nm:= Copy(Tag,5,length(Tag)-4-3);
    Props:= epComment;
    exit;
  end;

  case Nm[1] of
    TagClose:begin
               Props:=epClose;
               Delete(nm,1,1);
             end;
    TagXMLDef:begin
               Props:=epXML;
               Delete(nm,1,1);
             end;
    TagSpecDef: begin
                  Props:= epSpecial;
                  Nm:= Copy(Tag,3,length(Tag)-3);
                  exit;
                end;
  end;
  if EndsWith(Tag,TagClose+TagEnd) then begin
    Props:= epAutoClose;
    DeleteRight(Nm,PosRightNth(TagClose,Nm,1));
  end;
  Nm:= trim(Nm);
  Args:= trim(Args);
  if pos(AttrSep,Nm)>0 then begin
    Args:= CopyFrom(Nm,AttrSep);
    Nm:= CopyTo(Nm,AttrSep);
  end;
end;

procedure TXMLElement.InternalParse(XML: String; var P:integer);
var text,tag,Nm,args,a_nm,a_val:string;
    prp:TEleProps;
    nn:TXMLNode;
    i:integer;
    a_st:(sName,sVal,sQuot);

    procedure Start_args;
    begin
      a_nm:= ''; a_val:= ''; a_st:= sName;
    end;
begin
  while p<length(xml) do begin
    text:= FindTag(XML,P,Tag);
    SplitTag(Tag,prp,Nm,args);

    if (prp=epClose) and IsSameText(Name,nm) then begin
      FText:= trim(text);
      exit;
    end;
    nn:= AddItem(Nm);
    nn.Kind:= prp;
    if prp in OnlyTextData then begin
      nn.Name:= '';
      nn.TextData:= Nm;
    end;

    Start_args;
    for i:= 1 to length(args) do begin
      case a_st of
        sName: case args[i] of
                 ValueSep : a_st:= sVal;
                 AttrSep: if a_nm>'' then begin
                            nn.FAttrs.Add(a_nm);
                            Start_args;
                          end;
                 else a_nm:= a_nm+args[i];
               end;
        sVal: case args[i] of
                 TagQuote : if a_val='' then a_st:= sQuot;
                 AttrSep: begin
                            nn.FAttrs.Add(a_nm+ValueSep+a_val);
                            Start_args;
                          end;
                 else a_val:= a_val+args[i];
               end;
        sQuot: case args[i] of
                 TagQuote: begin
                            nn.FAttrs.Add(a_nm+ValueSep+a_val);
                            Start_args;
                          end;
                 else a_val:= a_val+args[i];
               end;
      end;
    end;

    if prp in SubItemProps then
      nn.InternalParse(XML,p);
  end;
end;


procedure TXMLElement.LoadFromString(XML: String);
var p:integer;
begin
  FItems.Clear;
  FAttrs.Clear;
  //XML:= KillChars(XML,[#13,#10]);
  XML:= trim(XML);
  if length(XML)=0 then exit;
  p:= 1;
  InternalParse(XML,p);
end;

function TXMLElement.GetAttr(Name: string): string;
begin
  Result:= ExtractQuotedString(FAttrs.Values[Name],TagQuote);
end;

function TXMLElement.GetAttrCount: integer;
begin
  Result:= FAttrs.Count;
end;

procedure TXMLElement.SetAttr(Name: string; const Value: string);
begin
  FAttrs.Values[Name]:= Value;
end;

procedure TXMLElement.SetParent(const Value: TXMLElement);
begin
  if Assigned(FParent) then
    FParent.RemoveItem(Self,true);
  FParent := Value;
  if Assigned(Value) then
    Value.FItems.Add(Self);
end;

function TXMLElement.GetLevel: integer;
var n:TXMLElement;
begin
  Result:= -1;
  n:= self;
  while assigned(n) do begin
    inc(Result);
    n:= n.ParentNode;
  end;
end;

function TXMLElement.GetText(Node: string; Default:XMLData): XMLData;
var n:TXMLElement;
begin
  n:= GetNode(Node);
  if assigned(n) then
    Result:= n.TextData
  else
    Result:= Default;
end;

function TXMLElement.GetAttribute(Node, Attr: string; Default:XMLData): XMLData;
var n:TXMLElement;
begin
  n:= GetNode(Node);
  if assigned(n) and (n.Attributes[Attr]>'') then
    Result:= n.Attributes[Attr]
  else
    Result:= Default;
end;

function TXMLElement.GetNode(Node: string): TXMLNode;
var p:TStringArray;
    i:integer;
    n:TXMLElement;
begin
  p:= SplitString(Node,NodeDelim);
  n:= self;
  i:= 0;
  for i:= i to high(p) do begin     //a bit of useless code, but the compiler wants it...
    n:= n.Items[p[i]];
    if not Assigned(n) then break;
  end;
  if (i=length(p)) then 
    Result:= TXMLNode(n)
  else Result:= nil;
end;

function TXMLElement.ForceNode(Node: string): TXMLNode;
var p:TStringArray;
    i:integer;
    n:TXMLElement;
begin
  p:= SplitString(Node,NodeDelim);
  n:= self;
  for i:= 0 to high(p) do begin
    if Assigned(n.Items[p[i]]) then
      n:= n.Items[p[i]]
    else
      n:= n.AddItem(p[i]);
  end;
  Result:= TXMLNode(n);
end;


procedure TXMLElement.InternalSave(var XML: String; Lvl: integer; SaveOpts:TSaveOptions);
var pref,attrs:string;
    i:integer;
  function _TagStart:string;
  begin
    case FKind of
      epXML     :Result:=pref+TagStart+TagXMLDef+Name;
      epSpecial :Result:=pref+TagStart+TagSpecDef+Name;
      epClose   :Result:=pref+TagStart+TagClose+Name;
      epComment :Result:=pref+TagStart+TagComment;
      else       Result:=pref+TagStart+Name;
    end;
  end;
  function _TagClose:string;
  begin
    Result:= TagStart+TagClose+FName+TagEnd;
  end;

begin
  pref:= DupeString('  ',Lvl);
  if (Level=0) or (soOnlyChilds in SaveOpts) then begin
    if (TextData>'') then begin
      XML:= XML+TextData;
      exit;
    end else
      for i:= 0 to GetItemCount-1 do
        GetItemAt(i).InternalSave(xml,Lvl,SaveOpts - [soOnlyChilds])
  end
  else begin
    if (FKind=epComment) and not (soComments in SaveOpts) then exit;

    XML:= XML+_TagStart;
    if (FKind=epComment) then begin
      XML:= XML+TextData+TagCommentEnd+tagEnd;
      XML:= XML+#13#10;
      exit;
    end;

    for i:= 0 to AttrCount-1 do
      attrs:= attrs+' '+FAttrs.Names[i]+ValueSep+QuotedString(FAttrs.ValueFromIndex[i],TagQuote);
    if attrCount>0 then XML:= XML+attrs;

    case FKind of
      epXML      : XML:= XML + TagXMLDef+TagEnd;
      epSpecial  : XML:= XML + TextData +TagEnd;
      else begin
        if TextData>'' then
          XML:= XML+TagEnd+FText+_TagClose
        else if GetItemCount>0 then  begin
          XML:= XML+tagEnd+#13#10;
          for i:= 0 to GetItemCount-1 do
            GetItemAt(i).InternalSave(xml,Lvl+1,SaveOpts);
          XML:= XML+pref+_TagClose;
        end else begin
          XML:= XML+TagClose+TagEnd;
          FKind:= epAutoClose;
        end;
      end;
    end;

    XML:= XML+#13#10;
  end;
end;

procedure TXMLElement.SaveToString(var XML: string);
var so:TSaveOptions;
begin
  so:= [];
  if FSaveComments then
    include(so,soComments);
  if FSaveOnlyChilds then
    include(so,soOnlyChilds);
  InternalSave(XML,0, so);
end;

function TXMLElement.AddItem(AName: string): TXMLNode;
begin
  Result:= TXMLNode.Create;
  Result.Name:= AName;
  Result.ParentNode:= self;
end;

procedure TXMLElement.RemoveItem(Item: TXMLElement; PreserveObject:boolean=false);
var b:boolean;
begin
  b:= FItems.OwnsObjects;
  FItems.OwnsObjects:= not PreserveObject;
  FItems.Remove(Item);
  FItems.OwnsObjects:= b;
end;

procedure TXMLElement.WalkItems(Callback: TCallbackProc; var UserData:TCBUserData);
var stop:boolean;
  procedure Walker(Node:TXMLElement);
  var i:integer;
      sub:boolean;
  begin
    for i:= 0 to Node.ItemCount-1 do begin
      if Stop then exit;
      sub:= true;
      Callback(Node.ItemAt[i],Stop,sub,UserData);
      if Sub then
        Walker(Node.ItemAt[i]);
    end;
  end;
begin
  stop:= false;
  Walker(Self);
end;

procedure TXMLElement.ClearItems;
begin
  FItems.Clear;
end;

procedure TXMLElement.SetAttribute(Node, Attr: string; Value: XMLData);
var n:TXMLElement;
begin
  n:= ForceNode(Node);
  n.Attributes[Attr]:= Value;
end;

procedure TXMLElement.SetText(Node: string; Value: XMLData);
var n:TXMLElement;
begin
  n:= ForceNode(Node);
  n.TextData:= Value;
end;

procedure TXMLElement.InsertAfter(Parent, Sibling: TXMLElement);
begin
  ParentNode:= Parent;
  with Parent.FItems do
    Move(IndexOf(Self),IndexOf(Sibling)+1);
end;

procedure TXMLElement.InsertBefore(Parent, Sibling: TXMLElement);
begin
  ParentNode:= Parent;
  with Parent.FItems do
    Move(IndexOf(Self),IndexOf(Sibling));
end;

function TXMLElement.GetAttribute(Attr: string; Default: XMLData): XMLData;
begin
  Result:= GetAttribute('',Attr,Default);
end;

procedure TXMLElement.SetAttribute(Attr: string; Value: XMLData);
begin
  SetAttribute('',Attr,Value);
end;

function TXMLElement.GetText(Node: string): XMLData;
begin
  Result:= GetText(Node,'');
end;

{ TXMLFile }

procedure TXMLFile.LoadFromFile(FileName: String);
var fs:TMemoryStream;
begin
  fs:= TMemoryStream.Create;
  try
    fs.LoadFromFile(FileName);
    LoadFromStream(fs);
  finally
    fs.free;
  end;
end;

procedure TXMLFile.LoadFromStream(Stream: TStream);
var str:TStringStream;
begin
  str:= TStringStream.Create('');
  try
    str.CopyFrom(Stream,0);
    LoadFromString(str.DataString);
  finally
    str.Free;
  end;
end;

procedure TXMLFile.SaveToFile(FileName: String);
var fs:TMemoryStream;
begin
  fs:= TMemoryStream.Create;
  try
    SaveToStream(fs);
    fs.SaveToFile(FileName);
  finally
    fs.free;
  end;
end;

procedure TXMLFile.SaveToStream(Stream: TStream);
var x:String;
begin
  x:= '';
  SaveToString(x);
  Stream.Write(x[1],length(x));
end;

end.

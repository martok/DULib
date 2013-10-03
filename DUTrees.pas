unit DUTrees;

interface

{$DEFINE USE_TREENT}

uses {$IFDEF USE_TREENT}TreeNT{$ELSE}ComCtrls{$ENDIF}, SysUtils;

{$IFDEF USE_TREENT}
  type
    TTreeNode=TTreeNTNode;
    TTreeView=TTreeNT;
{$ENDIF}


procedure DirToTree(const ADirectory: string; var Tree: TTreeView; Start: TTreeNode);
Function GetTreeNodePath(ANode: TTreenode; ADelimiter: Char='\'): String;

implementation

procedure DirToTree(const ADirectory: string; var Tree: TTreeView; Start: TTreeNode);

  function SlashSep(const Path, S: string): string;
  begin
   if AnsiLastChar(Path)^ <> '\' then
     Result := Path + '\' + S    else
     Result := Path + S;
  end;

var SearchRec: TSearchRec;
   NewNode: TTreeNode;

begin
  if FindFirst(SlashSep(ADirectory, '*.*'), faAnyFile, SearchRec) = 0 then begin
    try
      repeat
        if ((SearchRec.Attr and faDirectory) <> 0) then begin
          if ((SearchRec.Name <> '.') and (SearchRec.Name <> '..')) then begin
            NewNode:=Tree.Items.AddChild(Start, SearchRec.Name);
            DirToTree(SlashSep(ADirectory, SearchRec.Name), Tree, NewNode);
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

Function GetTreeNodePath(ANode: TTreenode; ADelimiter: Char='\'): String;
Begin
  Result := '';

  while assigned(ANode) do
  begin
    Result := ADelimiter + aNode.Text + Result;
    ANode := ANode.Parent;
  end;

  if Result <> '' then
  Delete(Result,1,1);
End;

end.

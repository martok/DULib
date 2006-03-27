unit DUMessageBox;

interface

uses Forms, Graphics, ExtCtrls, StdCtrls, Controls, Windows, Classes;

// Gibt gedrückten Button-Index im Array zurück:
// Buttons sind die Beschriftungen der Schalter
function MessageDialog(ACaption,AMsg:string;Buttons:Array of string):integer;

// Gibt gedrückten Button-Index im Array zurück:
// Img ist das links angezeigte Bild
function MessageDialogEx(ACaption,AMsg:string; Buttons:Array of string; Img:TGraphic):integer;



implementation

function MessageDialog(ACaption,AMsg:string;Buttons:Array of string):integer;
var form1:TForm;
    Btn:TButton;
    Label1: TLabel;
    Panel1,Panel2:TPanel;
    i, OldRight, x1, x2:integer;
begin
  Form1:= TForm.Create(Application);
  try
    with Form1 do begin
      Position := poDesktopCenter;
      BorderStyle:= bsDialog;
      Left := 235;
      Top := 109;
      Height := 173;
      Caption := ACaption;
      Color := clBtnFace;
      Font.Charset := DEFAULT_CHARSET;
      Font.Color := clWindowText;
      Font.Height := -11;
      Font.Name := 'MS Sans Serif';
      Font.Style := [];
      OldCreateOrder := False;
      PixelsPerInch := 96;
      Label1:= TLabel.Create(Form1);
      with Label1 do begin
        Autosize:=true;
        Parent:= Form1;
        Left := 0;
        Top := 0;
        Width := 395;
        Height := 102;
        Align := alClient;
        Alignment := taCenter;
        Caption := AMsg;
        Font.Charset := DEFAULT_CHARSET;
        Font.Color := clWindowText;
        Font.Height := -13;
        Font.Name := 'MS Sans Serif';
        Font.Style := [];
        ParentFont := False;
        Layout := tlCenter;
      end;
      Panel1:= TPanel.Create(Form1);
      with Panel1 do begin
        Parent:= Form1;
        Left := 0;
        Top := 102;
        Width := 395;
        Height := 45;
        Align := alBottom;
        BevelOuter := bvRaised;
        TabOrder := 0;
        Panel2:= TPanel.Create(Panel1);
        with Panel2 do begin
          Autosize:= true;
          Parent:= Panel1;
          Left := 4;
          Top := 5;
          Width := 390;
          Height := 35;
          AutoSize := True;
          BevelOuter := bvLowered;
          Anchors := [];
          TabOrder := 0;

          OldRight:= 0;
          for i:= Low(Buttons) to High(Buttons) do begin
            Btn:= TButton.Create(Panel2);
            with Btn do begin
              Parent:= Panel2;
              Top:=5;
              Left:= OldRight+10;
              Caption:= Buttons[i];
              Modalresult:= TModalResult(i+1);
              if Width< canvas.TextWidth(Caption)+10 then
                Width:= Canvas.TextWidth(Caption)+20;
              OldRight:= BoundsRect.Right;
            end;
          end;
          Width:= OldRight;
        end;
      end;

    end;
    form1.Width:=Panel2.Width+20;
    x1 := Form1.ClientWidth div 2;
    x2 := Panel2.ClientWidth div 2;
    Panel2.Left:= (x1 - x2);
    Result:= form1.ShowModal;
  finally
    Form1.Release;
  end;
end;

function MessageDialogEx(ACaption,AMsg:string; Buttons:Array of string; Img:TGraphic):integer;
var form1:TForm;
    Btn:TButton;
    Label1: TLabel;
    Panel1,Panel2:TPanel;
    Image1:TImage;
    i, OldRight, x1, x2:integer;
begin
  Form1:= TForm.Create(Application);
  try
    with Form1 do begin
      Position := poDesktopCenter;
      BorderStyle:= bsDialog;
      Left := 235;
      Top := 109;
      Height := 173;
      Caption := ACaption;
      Color := clBtnFace;
      Font.Charset := DEFAULT_CHARSET;
      Font.Color := clWindowText;
      Font.Height := -11;
      Font.Name := 'MS Sans Serif';
      Font.Style := [];
      OldCreateOrder := False;
      PixelsPerInch := 96;
      Image1:=TImage.Create(Form1);
      with Image1 do begin
        Parent:= Form1;
        Left := 0;
        Top := 0;
        Height := 102;
        Align := alLeft;
        Font.Charset := DEFAULT_CHARSET;
        Font.Color := clWindowText;
        Font.Height := -13;
        Font.Name := 'MS Sans Serif';
        Font.Style := [];
        ParentFont := False;
        Center:=true;

        if Assigned(Img) then begin
          Picture.Graphic:= Img;
          if (Img.Width>Width) and (Img.Height>Height) then
            Stretch:=true;
        end else begin
          Width:=100;
          Picture.Icon:= Application.Icon;
        end;
      end;
      Label1:= TLabel.Create(Form1);
      with Label1 do begin
        Autosize:=true;
        Parent:= Form1;
        Left := 0;
        Top := 0;
        Width := 395;
        Height := 102;
        Align := alClient;
        Alignment := taCenter;
        Caption := AMsg;
        Font.Charset := DEFAULT_CHARSET;
        Font.Color := clWindowText;
        Font.Height := -13;
        Font.Name := 'MS Sans Serif';
        Font.Style := [];
        ParentFont := False;
        Layout := tlCenter;
      end;
      Panel1:= TPanel.Create(Form1);
      with Panel1 do begin
        Parent:= Form1;
        Left := 0;
        Top := 102;
        Width := 395;
        Height := 45;
        Align := alBottom;
        BevelOuter := bvNone;
        TabOrder := 0;
        Panel2:= TPanel.Create(Panel1);
        with Panel2 do begin
          Autosize:= true;
          Parent:= Panel1;
          Left := 4;
          Top := 5;
          Width := 390;
          Height := 35;
          AutoSize := True;
          BevelOuter := bvNone;
          Anchors := [];
          TabOrder := 0;

          OldRight:= 0;
          for i:= Low(Buttons) to High(Buttons) do begin
            Btn:= TButton.Create(Panel2);
            with Btn do begin
              Parent:= Panel2;
              Top:=5;
              Left:= OldRight+10;
              Caption:= Buttons[i];
              Font.Size:= 8;
              Modalresult:= TModalResult(i+1);
              if Width< canvas.TextWidth(Caption)+10 then
                Width:= Canvas.TextWidth(Caption)+20;
              OldRight:= BoundsRect.Right;
            end;
          end;
          Width:= OldRight;
        end;
      end;

    end;
    form1.Width:=Panel2.Width+20;
    x1 := Form1.ClientWidth div 2;
    x2 := Panel2.ClientWidth div 2;
    Panel2.Left:= (x1 - x2);
    Result:= form1.ShowModal;
  finally
    Form1.Release;
  end;
end;

end.

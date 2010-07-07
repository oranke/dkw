unit StdCtrls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL StdCtrls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TButton+TCheckBox+TEdit+TLabel+TMemo
   - for TMemo: use global Text property, as there's no Lines[] property;
     don't set anything in Lines property in IDE 
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup
     (no Anchor, e.g.)

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are Copyright (C) 2001 Paul Toth. http://tothpaul.free.fr
  All Rights Reserved.

}

{

  2010-04-13
    TScrollBar 추가.
    http://www.functionx.com/win32/controls/scrollbars.htm
    
}

{$WARNINGS OFF}
{$HINTS ON}

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, Graphics;

type
  TLabel = class(TGraphicControl)
  protected
    procedure SetCaption(const Value: string);
  public
    procedure Paint; override;
    property Caption: string read fCaption write SetCaption;
  end;

  TButton = class(TWinControl)
  protected
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(TWinControl)
  private
    //fText: string;
    fPassWordChar: char;
    fReadOnly: boolean;
  protected
    CreateFlags: cardinal;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure CreateHandle; override;
    function GetText: string;
    procedure SetText(const Value: string); override;
    procedure SetReadOnly(Value: Boolean);
    procedure SetPassWordChar(const Value: Char);
  public
    procedure SelectAll;
    property Text: string read GetText write SetText;
    property ReadOnly: boolean read fReadOnly write SetReadOnly;
    property PassWordChar: Char read fPassWordChar write SetPassWordChar;
  end;

  TMemo = class(TEdit)
  protected
    procedure CreateHandle; override;
  end;

  TCheckBox = class(TWinControl)
  private
    fChecked: boolean;
    procedure SetChecked(const Value: boolean);
    function GetChecked: boolean;
  protected
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
  public
    property Checked: boolean read GetChecked write SetChecked;
  end;


  TScrollBarKind = (sbHorizontal, sbVertical);

  TScrollBar = class(TWinControl)
  private
    fKind: TScrollBarKind;
    fMin, fMax, fPosition: Integer;
    fSmallChange, fLargeChange: Integer;
    fOnChange: TNotifyEvent;
    procedure SetMin(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetPosition(const Value: Integer);
  private
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
  protected
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Min: Integer read fMin write SetMin;
    property Max: Integer read fMax write SetMax;
    property Position: Integer read fPosition write SetPosition;
    property SmallChange: Integer read fSmallChange write fSmallChange;
    property LargeChange: Integer read fLargeChange write fLargeChange;
  end;



implementation


{ TButton }

constructor TButton.Create(AOwner:TComponent);
begin
  inherited;
  fTransparent := True; // do not use color

  //WriteLn('Button Create ', FVisible);
end;

procedure TButton.CreateHandle;
begin
  Handle := CreateWindow(
    'BUTTON',
    pointer(fCaption),
    WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP,
    fLeft,fTop,fWidth,fHeight,
    Parent.Handle,
    0,
    hInstance,
    nil
  );


  if not fVisible then ShowWindow(Handle, SW_HIDE);

  //WriteLn('Button CreateHandle ', IsWindowVisible(Handle));
end;


procedure TButton.ReadProperty(const Name: string; Reader: TReader);
begin
  inherited;
  //WriteLn('Button ReadProperty');
end;

{ TEdit }

procedure TEdit.CreateHandle;
begin
  Color := clWhite;
  if CreateFlags=0 then
  begin
   CreateFlags :=
    WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP or
    ES_AUTOHSCROLL or ES_AUTOVSCROLL;
   if Length(fPassWordChar) > 0 then
     CreateFlags := CreateFlags or ES_PASSWORD;
  end;

  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT or WS_EX_CLIENTEDGE,
   'EDIT',
   pointer(fCaption),
   CreateFlags,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );


  // 패스워드가 제대로 동작하게 한다.
  SendMessage(fHandle, EM_SETPASSWORDCHAR, Ord(FPasswordChar), 0);
  //SendMessage(Handle,WM_SETTEXT,0,integer(fCaption));
  //SendMessage(Handle,WM_SETTEXT,0,integer(fCaption));

  if not fVisible then ShowWindow(Handle,SW_HIDE);
  if fReadOnly then SendMessage(fHandle, EM_SETREADONLY, 1, 0);
end;

function TEdit.GetText: string;
var i: integer;
begin
  i := SendMessage(fHandle,WM_GETTEXTLENGTH,0,0);
  if i=0 then
    result := ''
  else
  begin
    SetLength(result,i);
    SetLength(result,SendMessage(fHandle,WM_GETTEXT,i+1,integer(result)));
  end;
end;

procedure TEdit.ReadProperty(const Name: string; Reader: TReader);
const
  Properties: array[0..2] of PChar=(
    'PasswordChar','ReadOnly', 'ImeName'
  );
var
  tmp: string;
begin
  case StringIndex(Name, Properties) of
    0: begin
      tmp := Reader.StringProperty;
      if tmp<>'' then
        fPassWordChar := tmp[1];
    end;
    1: fReadOnly := Reader.BooleanProperty;
    // 2010-04-13. ImeName 속성 읽어올 때 에러 제거. 
    2: Reader.StringProperty; //MessageBox(0, PChar(Reader.StringProperty), '', 0);
  else
    inherited;
  end;
end;

procedure TEdit.SelectAll;
begin
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETSEL, 0, -1);
end;

procedure TEdit.SetReadOnly(Value: Boolean);
begin
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETREADONLY, Ord(Value), 0);
  fReadOnly := Value;
end;

procedure TEdit.SetPassWordChar(const Value: Char);
begin
  if FPasswordChar <> Value then
  begin
    FPasswordChar := Value;
    if fHandle <> 0 then
    begin
      SendMessage(fHandle, EM_SETPASSWORDCHAR, Ord(FPasswordChar), 0);
      SendMessage(fHandle, WM_SETTEXT, 0, Longint(Text));
    end;
  end;
end;


procedure TEdit.SetText(const Value: string);
begin
  HandleNeeded;
  //fText := Value;
  if fHandle<>0 then
    SendMessage(fHandle, WM_SETTEXT, 0, LongWord(Value));
end;


{ TMemo }

procedure TMemo.CreateHandle;
begin
  CreateFlags := WS_VISIBLE or WS_CHILD or WS_VSCROLL or WS_HSCROLL or
    ES_LEFT or ES_MULTILINE or ES_WANTRETURN or ES_AUTOVSCROLL;
  inherited;
end;


{ TCheckBox }

procedure TCheckBox.CreateHandle;
begin
  inherited;
  Handle := CreateWindowEx(
    WS_EX_CONTROLPARENT,
    'BUTTON',
    pointer(fCaption),
    WS_VISIBLE or WS_CHILD or BS_AUTOCHECKBOX or WS_TABSTOP,
    fLeft,fTop,fWidth,fHeight,
    Parent.Handle,
    0,
    hInstance,
    nil
  );

  SendMessage(Handle, BM_SETCHECK, integer(fChecked), 0);
  if not fVisible then ShowWindow(Handle, SW_HIDE);
end;

function TCheckBox.GetChecked: boolean;
begin
  result := boolean(SendMessage(Handle,BM_GETCHECK,0,0));
end;

procedure TCheckBox.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..0] of PChar=(
    'Checked');
//type TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
begin
  case StringIndex(Name,Properties) of
  0: fChecked := Reader.BooleanProperty;
  //1: Reader.IdentProperty(fState,TypeInfo(TCheckBoxState));
    // no cbGrayed implementation yet
  else
    inherited;
  end;
end;

procedure TCheckBox.SetChecked(const Value: boolean);
begin
  SendMessage(Handle, BM_SETCHECK, integer(Value), 0);
end;


{ TLabel }

procedure TLabel.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  with Canvas do begin
    Font.Assign(Self.Font);
    if fTransparent then
      Brush.Style := bsClear else begin
      Brush.Color := Parent.Color;
      FillRect(R);
    end;
    PrepareText;
    DrawText(Handle, pointer(fCaption), length(fCaption), R,
      DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  end;
end;

procedure TLabel.SetCaption(const Value: string);
begin
  fCaption := Value;
  Invalidate;
end;

const
  Classes: array[0..5] of TPersistentClass =
    (TLabel, TButton, TEdit, TCheckBox, TMemo, TScrollBar);

{ TScrollBar }

constructor TScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  fKind := sbHorizontal;
  fTransparent := True; // do not use color

  fMin := 0;
  fMax := 100;
  fPosition := 0;

  fSmallChange := 1;
  fLargeChange := 1;
end;

procedure TScrollBar.CreateHandle;
const
  Kinds: array[TScrollBarKind] of DWORD = (SBS_HORZ, SBS_VERT);
var
  ScrollInfo: TScrollInfo;
begin
  inherited;

  Handle := CreateWindowEx(
    WS_EX_CONTROLPARENT,
    'SCROLLBAR',
    nil,
    WS_CHILD or WS_VISIBLE or WS_TABSTOP or Kinds[fKind],
    fLeft,fTop,fWidth,fHeight,
    Parent.Handle,
    0,
    hInstance,
    nil
  );

  SetScrollRange(Handle, SB_CTL, fMin, fMax, false);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPage := 0;//FPageSize;
  ScrollInfo.fMask := SIF_PAGE;// or SIF_RANGE or SIF_POS;
  SetScrollInfo(Handle, SB_CTL, ScrollInfo, false);
  SetScrollPos(Handle, SB_CTL, fPosition, false);

  if not fVisible then ShowWindow(Handle, SW_HIDE);
end;

procedure TScrollBar.ReadProperty(const Name: string; Reader: TReader);
const
  Properties: array[0..6] of PChar=(
    'Kind','Min', 'Max', 'Position', 'SmallChange', 'LargeChange',
    'OnChange'

  );
begin
  case StringIndex(Name, Properties) of
    0: Reader.IdentProperty(fKind, TypeInfo(TScrollBarKind));
    1: fMin := Reader.IntegerProperty;
    2: fMax := Reader.IntegerProperty;
    3: fPosition := Reader.IntegerProperty;
    4: fSmallChange := Reader.IntegerProperty;
    5: fLargeChange := Reader.IntegerProperty;
    6: TMethod(fOnChange) := FindMethod(Reader);
  else
    inherited;
  end
end;

procedure TScrollBar.SetMin(const Value: Integer);
begin
  if fMin = Value then Exit;
  fMin := Value;
  //if IsWindow(Handle) then
    SetScrollRange(Handle, SB_CTL, fMin, fMax, True);
end;

procedure TScrollBar.SetMax(const Value: Integer);
begin
  if fMax = Value then Exit;
  fMax := Value;
  //if IsWindow(Handle) then
    SetScrollRange(Handle, SB_CTL, fMin, fMax, True);
end;

procedure TScrollBar.SetPosition(const Value: Integer);
begin
  if fPosition = Value then Exit;
  fPosition := Value;
  //if IsWindow(Handle) then
    SetScrollPos(Handle, SB_CTL, fPosition, True);

  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TScrollBar.WMVScroll(var Message: TWMVScroll);
var
  CurPos: Integer;
begin
  CurPos := GetScrollPos(Handle, SB_CTL);

  case Message.ScrollCode of
    SB_TOP            : CurPos := fMin;
    SB_BOTTOM         : CurPos := fMax;
    SB_LINEUP         : Dec(CurPos, fSmallChange);
    SB_LINEDOWN       : Inc(CurPos, fSmallChange);
    SB_THUMBPOSITION,
    SB_THUMBTRACK     : CurPos := Message.Pos;
    SB_PAGEUP         : Dec(CurPos, fLargeChange);
    SB_PAGEDOWN       : Inc(CurPos, fLargeChange);
    SB_ENDSCROLL      : Exit;
  end;

  if CurPos < fMin then CurPos := fMin;
  if CurPos > fMax then CurPos := fMax;

  Position := CurPos;

  //WriteLn('VScroll ', CurPos);
end;

procedure TScrollBar.WMHScroll(var Message: TWMHScroll);
var
  CurPos: Integer;
begin
  CurPos := GetScrollPos(Handle, SB_CTL);

  case Message.ScrollCode of
    SB_LEFT           : CurPos := fMin;
    SB_RIGHT          : CurPos := fMax;
    SB_LINELEFT       : Dec(CurPos, fSmallChange);
    SB_LINERIGHT      : Inc(CurPos, fSmallChange);
    SB_THUMBPOSITION,
    SB_THUMBTRACK     : CurPos := Message.Pos;
    SB_PAGELEFT       : Dec(CurPos, fLargeChange);
    SB_PAGERIGHT      : Inc(CurPos, fLargeChange);
    SB_ENDSCROLL      : Exit;
  end;

  if CurPos < fMin then CurPos := fMin;
  if CurPos > fMax then CurPos := fMax;

  Position := CurPos;

  //WriteLn('HScroll ', CurPos);
end;


initialization
  RegisterClasses(Classes);

end.

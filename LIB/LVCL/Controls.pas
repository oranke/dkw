unit Controls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Controls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TControl+TCustomControl+TGraphicControl+TWinControl
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup

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
  // oranke

  2010-01-04
    TMouseButton 타입 정의.
    윈컨트롤의 배경채우기 오류 수정

  2010-04-13
    마우스 우측버튼 메시지 처리. 
    ClientWidth, ClientHeight 관련 코드 추가.
    코드로 컨트롤의 바운더리 설정시 채움부분 오류 수정.
    HandleNeeded 를 Public으로 옮김.

    fVisible 기본값을 true로 변경. Visible 속성이 제대로 처리되도록 하기 위함.
     
    
}

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics;

type
  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y: Integer) of object;

  TKeyPressEvent = procedure (Sender: TObject; var Key: Char) of object;

  TControlStyle = set of (csAcceptsControl, csCaptureMouse, csClickEvents,
    csFramed, csSetCaption, csOpaque, cdDoubleClicks);
  TControlState = set of (csLButtonDown, csClicked, csPalette, csReadingState,
    csAlignmentNeeded, csFocusing, csCreating, csPaintCopy, csCustomPaint,
    csDestroyingHandle, csDocking);

  TBorderStyle = (bsNone,bsSingle);

  TWinControl = class;

  TControl = class(TComponent)
  private
    FParent: TWinControl;
    procedure SetParent(AParent: TWinControl);
  private
    FOnClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
  private
    function GetClientWidth(): Integer;
    function GetClientHeight(): Integer;
    procedure SetClientWidth(Value: Integer);
    procedure SetClientHeight(Value: Integer);
  protected
    FVisible,
    FEnabled: Boolean;
    FLeft, FTop: integer;
    FWidth, FHeight: integer;
    FTransparent: boolean;
    FControlStyle: TControlStyle;
    FControlState: TControlState;
  protected
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(Value: Boolean); virtual;

    function GetClientOrigin: TPoint; virtual;
    function GetClientRect: TRect; virtual;

    function GetBoundsRect: TRect;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure SetBoundsRect(const Rect: TRect);

  protected
    procedure ReadProperty(const Name:string; Reader: TReader); override;
    function FindMethod(Reader: TReader): TMethod;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    function ClientToScreen(const Point: TPoint): TPoint;
    function ScreenToClient(const Point: TPoint): TPoint;
  public
    property Parent: TWinControl read FParent write SetParent;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property Visible: boolean read fVisible;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    
    property ClientWidth: Integer read GetClientWidth write SetClientWidth;
    property ClientHeight: Integer read GetClientHeight write SetClientHeight;
  end;


  TCustomControl = class(TControl)
  private
    fCanvas: TCanvas;
    fFont: TFont;
    fColor: integer;
    fParentFont: boolean;
  protected
    FOnPaint: TNotifyEvent;
    FOnShow: TNotifyEvent;
    fCaption: string;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function SubProperty(const Name: string): TPersistent; override;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Invalidate;
    property Font: TFont read GetFont write SetFont;
    property Canvas: TCanvas read GetCanvas;
    property Color: integer read fColor write fColor;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    property Left: integer read fLeft;
    property Top: integer read fTop;
    property Transparent: boolean read fTransparent;
    property Caption: string read fCaption;
  end;

  
  TGraphicControl = class(TCustomControl)
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
  end;


  TWinControl = class(TCustomControl)
  private
    procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMWindowPosChanged(var Msg: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSizing(var Msg: TMessage); message WM_SIZING;
    procedure WMSize(var Msg: TWMSize); message wm_size;
    procedure WMLButtonDown(var msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var msg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMLButtonUp(var msg: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonUp(var msg: TWMLButtonUp); message WM_RBUTTONUP;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure SetCaption(const Value: string);
  private
  public
    procedure DefaultHandler(var Message); override;
  protected
    FOnKeyPress: TKeyPressEvent;
    fHandle: integer;
    fOldProc: integer;
    fTabOrder: integer;
    fGraphics: TList;
    fControls: TList;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure SetParentComponent(Value: TComponent); override;
    function GetParentComponent: TComponent; override;
    procedure CreateHandle; virtual; abstract;
    procedure SetHandle(Value: integer);
    procedure SetText(const Value: string); virtual;
    function GetClientRect: TRect; override;
    procedure AddChild(AChild: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleNeeded;
    procedure Paint; override;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure SetFocus;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    property Handle: integer read fHandle write SetHandle;
    property Caption: string read fCaption write SetCaption;
  end;


implementation

uses
  Forms, Dialogs;

function WndProc(Hwnd,Msg,wParam,lParam: integer): integer; stdcall;
var obj: TObject;
    dsp: TMessage;
begin
  obj := TObject(GetWindowLong(HWnd,GWL_USERDATA)); // faster than GetProp()
  if not Assigned(obj) then
    result := DefWindowProc(HWnd,Msg,wParam,lParam)
  else
  begin
    dsp.msg := msg;
    dsp.wParam := WParam;
    dsp.lParam := lParam;
    dsp.result := 0;
    obj.Dispatch(dsp);
    result := dsp.result;
  end;
end;


constructor TControl.Create(aOwner: TComponent);
begin
  inherited;

  FVisible := true;
  FEnabled := true;
  
  if aOwner is TWinControl then
    FParent := TWinControl(aOwner);
end;


procedure TControl.ReadProperty(const Name: string; Reader:TReader);
const
  TControlProperties:array[0..3] of PChar=(
   'OnClick', 'OnMouseDown', 'OnMouseMove', 'OnMouseUp'
  );
begin
  case StringIndex(Name,TControlProperties) of
    0 : TMethod(FOnClick) := FindMethod(Reader);
    1 : TMethod(FOnMouseDown) := FindMethod(Reader);
    2 : TMethod(FOnMouseMove) := FindMethod(Reader);
    3 : TMethod(FOnMouseUp) := FindMethod(Reader);
  else
    inherited;
  end;
end;



procedure TControl.SetParent(AParent: TWinControl);
begin
  if FParent <> AParent then
  begin
    if AParent = Self then
      raise EInvalidOperation.Create('Control can''t parent set to self');
    if FParent <> nil then
    begin
      //FParent.RemoveControl(Self);
    end;
    if AParent <> nil then
    begin
      //AParent.InsertControl(Self);
      //UpdateAnchorRules;
    end;
  end;
  //ShowMessage('aaa');
end;

function TControl.GetClientWidth: Integer;
var
  Rt: TRect;
begin
  Rt := GetClientRect;
  Result := Rt.Right - Rt.Left;
end;


function TControl.GetClientHeight: Integer;
var
  Rt: TRect;
begin
  Rt := GetClientRect;
  Result := Rt.Bottom - Rt.Top;
end;

procedure TControl.SetClientWidth(Value: Integer);
var
  Rt : TRect;
begin
  Rt := GetClientRect;
  SetBounds(fLeft, fTop, fWidth - Rt.Right + Value, fHeight);
end;

procedure TControl.SetClientHeight(Value: Integer);
var
  Rt : TRect;
begin
  Rt := GetClientRect;
  SetBounds(fLeft, fTop, fWidth, fHeight - Rt.Bottom + Value);
end;

function TControl.FindMethod(Reader: TReader): TMethod;
var AComponent: TComponent;
    Name: shortstring;
begin
  if Reader.ReadValueType in [vaString,vaIdent] then begin
    Name := Reader.ReadShortString;
    AComponent := self;
    while AComponent<>nil do begin
      result.Data := AComponent;
      result.Code := AComponent.MethodAddress(Name);
      if result.Code<>nil then
        exit;
      AComponent := AComponent.Owner;
    end;
  end;
  raise EClassesError.Create('method?');
end;

procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Include(fControlState,csClicked);
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if csClicked in fControlState then
  begin
    if Assigned(FOnClick) then
      if (X <= fWidth) and (Y <= fHeight) then
        FOnClick(Self);
    Exclude(fControlState,csClicked);
  end;
end;


function TControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;


procedure TControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    //Perform(CM_ENABLEDCHANGED, 0, 0);
  end;
end;



function TControl.GetClientOrigin: TPoint;
begin
  if Parent = nil then
  begin
    //FillChar(Result, SizeOf(TPoint), #0);
    //raise EInvalidOperation.Create('Parent required');
    Result.X := 0;
    Result.Y := 0;
  end else
    Result := Parent.ClientOrigin;

  //WriteLn('ParentPos : ', Result.X, ', ', Result.Y, ', ', FLeft, ', ', FTop);
  
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
end;

function TControl.GetClientRect: TRect;
begin
  result.Left := fLeft;
  result.Right := result.Left+fWidth;
  result.Top := fTop;
  result.Bottom := result.Top+fHeight;
end;

function TControl.GetBoundsRect: TRect;
begin
  result.Left := fLeft;
  result.Right := result.Left+fWidth;
  result.Top := fTop;
  result.Bottom := result.Top+fHeight;
end;

procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if ((ALeft <> FLeft) or (ATop <> FTop) or
    (AWidth <> FWidth) or (AHeight <> FHeight)) then
  begin
    //InvalidateControl(Visible, False);
    FLeft := ALeft;
    FTop := ATop;
    FWidth := AWidth;
    FHeight := AHeight;
    //UpdateAnchorRules;
    //UpdateExplicitBounds;
    //Invalidate;
    //Perform(WM_WINDOWPOSCHANGED, 0, 0);
    //RequestAlign;
    //if not (csLoading in ComponentState) then Resize;
  end;
end;

procedure TControl.SetBoundsRect(const Rect: TRect);
begin
  with Rect do
    SetBounds(Left, Top, Right - Left, Bottom - Top);
end;




function TControl.ClientToScreen(const Point: TPoint): TPoint;
var
  Origin: TPoint;
begin
  Origin := ClientOrigin;
  Result.X := Point.X + Origin.X;
  Result.Y := Point.Y + Origin.Y;
end;

function TControl.ScreenToClient(const Point: TPoint): TPoint;
var
  Origin: TPoint;
begin
  Origin := ClientOrigin;
  Result.X := Point.X - Origin.X;
  Result.Y := Point.Y - Origin.Y;
end;

{TCustomControl}

constructor TCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  fParentFont := true; // default value
end;

destructor TCustomControl.Destroy;
begin
  fFont.Free;
  fCanvas.Free;
  inherited;
end;

function TCustomControl.GetCanvas: TCanvas;
begin
  if fCanvas=nil then
    fCanvas := TCanvas.Create;
  result := fCanvas;
end;

procedure TCustomControl.ReadProperty(const Name: string; Reader: TReader);
const
  TWinControlProperties: array[0..11] of PChar=(
   'Left','Top',
   'Width','Height',
   'Color',
   'Transparent',
   'Caption',
   'OnPaint',
   'ParentFont',
   'OnShow',
   'Enabled',
   'Visible'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : fLeft := Reader.IntegerProperty;
    1 : fTop := Reader.IntegerProperty;
    2 : fWidth := Reader.IntegerProperty;
    3 : fHeight := Reader.IntegerProperty;
    4 : fColor := Reader.ColorProperty;
    5 : fTransparent := Reader.BooleanProperty;
    6 : fCaption := Reader.StringProperty;
    7 : TMethod(FOnPaint) := FindMethod(Reader);
    8 : fParentFont := Reader.BooleanProperty;
    9 : TMethod(FOnShow) := FindMethod(Reader);
    10: fEnabled := Reader.BooleanProperty;
    11: fVisible := Reader.BooleanProperty;
  else
    inherited;
  end;
end;

function TCustomControl.SubProperty(const Name:string): TPersistent;
const
  TControlSubProperties:array[0..0] of PChar=(
   'Font'
  );
begin
  case StringIndex(Name,TControlSubProperties) of
   0 : begin
     if fFont=nil then
       fFont := TFont.Create;
     result := fFont;
   end;
   else result := nil;
  end;
end;

function TCustomControl.GetFont: TFont;
begin
  if fFont=nil then begin
    if fParentFont and (Parent<>nil) then begin
      result := Parent.Font;
      exit;
    end;
    fFont := TFont.Create;
  end;
  result := fFont;
end;

procedure TCustomControl.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TCustomControl.Paint;
begin
  if not fTransparent then
   with Canvas do
    if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TGraphicControl.SetParentComponent(Value: TComponent);
begin
  while (Value<>nil) and not Value.InheritsFrom(TWinControl) do
    Value := Value.ParentComponent;
  //if Assigned(Value) and (Value is TWinControl)  then
  if Value <> nil then
  begin
    fParent := TWinControl(Value);
    Font.Assign(fParent.Font);
    fParent.fGraphics.Add(Self);
  end;
end;

constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited;
  fControls := TList.Create;
  fGraphics := TList.Create;
end;

destructor TWinControl.Destroy;
begin
  if fColor<>0 then
    DeleteObject(fColor);
  Handle := 0;
  fControls.Free;
  fGraphics.Free;
  inherited;
end;

procedure TWinControl.ReadProperty(const Name:string; Reader:TReader);
const
  TWinControlProperties:array[0..2] of PChar=(
   'Text',
   'TabOrder',
   'OnKeyPress'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : SetText(Reader.StringProperty);
    1 : fTabOrder := Reader.IntegerProperty;
    2 : TMethod(FOnKeyPress) := FindMethod(Reader);
   else inherited;
  end;
end;

procedure TWinControl.SetParentComponent(Value:TComponent);
begin
  while (Value<>nil) and not(Value.InheritsFrom(TWinControl)) do
    Value := Value.ParentComponent;
  if Value<>nil then begin
    fParent := TWinControl(Value);
    Canvas.Font.Assign(fParent.Font);
    fParent.AddChild(Self);
  end;
end;

function TWinControl.GetParentComponent:TComponent;
begin
  result := fParent;
end;

procedure TWinControl.HandleNeeded;
begin
  if fParent<>nil then
    fParent.HandleNeeded;
  if fHandle=0 then
    CreateHandle;
end;

procedure TWinControl.SetHandle(Value: integer);
begin
  if fHandle<>0 then begin
    SetWindowLong(fHandle,GWL_WNDPROC,fOldProc);
    DestroyWindow(fHandle);
  end;
  fHandle := Value;
  if fHandle<>0 then begin
    fOldProc := GetWindowLong(fHandle,GWL_WNDPROC);
    SetWindowLong(fHandle,GWL_USERDATA,integer(self)); // faster than SetProp()
    SetWindowLong(fHandle,GWL_WNDPROC,integer(@WndProc));
    SendMessage(fHandle,WM_SETFONT,Font.Handle,0);
  end;
end;

procedure TWinControl.SetText(const Value:string);
begin
  fCaption := Value;
end;

function TWinControl.GetClientRect: TRect;
begin
  Windows.GetClientRect(Handle, result);
end;

procedure TWinControl.AddChild(AChild:TWinControl);
begin
  fControls.Add(AChild);
end;


procedure TWinControl.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
  inherited;

//  fLeft := Msg.WindowPos.x;
//  fTop  := Msg.WindowPos.y;

  //WriteLn('WMPosChanging: ', FLeft, ', ', FTop, ', ', FWidth, ', ', FHeight)
end;

procedure TWinControl.WMWindowPosChanged(var Msg: TWMWindowPosChanged);
begin
  inherited;
  fLeft  := Msg.WindowPos.x;
  fTop   := Msg.WindowPos.y;
  fWidth := Msg.WindowPos.cx;
  fHeight := Msg.WindowPos.cy;
  //WriteLn('WMPosChanged: ', FLeft, ', ', FTop, ', ', FWidth, ', ', FHeight);
end;

// oranke: 2010-01-04 컨트롤의 배경채우기 오류 수정
procedure TWinControl.WMSizing(var Msg: TMessage);
begin
  inherited;
  //fLeft := PRect(Msg.LParam).Left;
  //fTop  := PRect(Msg.LParam).Top;
  fWidth := PRect(Msg.LParam).Right - PRect(Msg.LParam).Left;
  fHeight:= PRect(Msg.LParam).Bottom - PRect(Msg.LParam).Top;
  //WriteLn('WMSizing : ', FLeft, ', ', FTop, ', ', FWidth, ', ', FHeight)
end;


procedure TWinControl.WMSize(var Msg: TWMSize);
begin
  inherited;
end;


procedure TWinControl.WMLButtonDown(var msg: TWMLButtonDown);
var
  CtrlPt: TPoint;
  i : Integer;
begin
  inherited;

  CtrlPt.X := msg.XPos;
  CtrlPt.Y := msg.YPos;

  for i := fGraphics.Count-1 downto 0 do
  with TGraphicControl(fGraphics[i]) do
  if Enabled and Visible and PtInRect(BoundsRect, CtrlPt) then
  begin
    TGraphicControl(fGraphics[i]).MouseDown(
      mbLeft,
      KeysToShiftState(msg.Keys),
      msg.XPos - Left,
      msg.YPos - Top
    );
    Exit;
  end;

  MouseDown(mbLeft, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMRButtonDown(var msg: TWMRButtonDown);
var
  CtrlPt: TPoint;
  i : Integer;
begin
  inherited;

  CtrlPt.X := msg.XPos;
  CtrlPt.Y := msg.YPos;

  for i := fGraphics.Count-1 downto 0 do
  with TGraphicControl(fGraphics[i]) do
  if Enabled and Visible and PtInRect(BoundsRect, CtrlPt) then
  begin
    TGraphicControl(fGraphics[i]).MouseDown(
      mbRight,
      KeysToShiftState(msg.Keys),
      msg.XPos - Left,
      msg.YPos - Top
    );
    Exit;
  end;

  MouseDown(mbRight, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMLButtonUp(var msg: TWMLButtonUp);
begin
  inherited;
  MouseUp(mbLeft, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMRButtonUp(var msg: TWMLButtonUp);
begin
  inherited;
  MouseUp(mbRight, KeysToShiftState(msg.Keys), msg.XPos, msg.YPos);
end;

procedure TWinControl.WMChar(var Msg: TWMChar);
var Key: Char;
begin
  Key := chr(Msg.CharCode);
  if Assigned(FOnKeyPress) then
  begin
    FOnKeyPress(Self, Key);
    if Key=#0 then exit;
  end;
  inherited;
end;

procedure TWinControl.WMEraseBkGnd(var msg: TWMEraseBkGnd);
var
  R: TRect;
begin
  msg.result := 1;
  if not fTransparent then
  with Canvas do
  begin
    //WriteLn('윈컨트롤 EraseBKGnd');
    R := ClientRect;

    Handle := Msg.DC;
    Brush.Color := Self.Color;
    //FillRect(Rect(0,0,fWidth,fHeight));
    FillRect(Rect(0,0, R.Right-R.Left,R.Bottom-R.Top));

  end;
end;

procedure TWinControl.WMPaint(var Msg: TWMPaint);
begin
  with Canvas do begin
    Handle := Msg.DC;
    if Handle=0 then
      Handle := GetDC(self.fHandle);
    Paint;
    if Msg.DC=0 then
      ReleaseDC(self.fHandle,Handle);
  end;
  inherited;
end;


procedure TWinControl.WMDestroy(var Msg: TWMDestroy);
begin
  inherited;
  PostQuitMessage(0);
end;

procedure TWinControl.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    result := CallWindowProc(pointer(fOldProc), fHandle, Msg, wParam, lParam)
end;

procedure TWinControl.Paint;
var H, i: integer;
begin
  inherited;
  H := Self.Canvas.Handle;
  for i := 0 to fGraphics.Count-1 do
    with TGraphicControl(fGraphics.List[i]) do begin
      Canvas.Handle := H;
      Paint;
    end;
end;

procedure TWinControl.SetFocus;
begin
  Windows.SetFocus(fHandle);
end;

procedure TWinControl.SetCaption(const Value: string);
begin
  fCaption := Value;
  if fHandle <> 0 then
  begin
    //WriteLn(IntToHex(fHandle, 8), ', ', Caption);
    SendMessage(fHandle,WM_SETTEXT,0, integer(Value));
    //SendMessage(fHandle,WM_SETTEXT,0, integer(PChar(Value)));
  end;
end;

procedure TWinControl.Hide;
var i: integer;
begin
  HandleNeeded;
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if fVisible then
        Hide;
  fVisible := false;
  ShowWindow(fHandle,SW_HIDE);
end;

procedure TWinControl.Show;
var
  i: integer;
  //Str: String;
begin
  HandleNeeded;

  {
  //WriteLn(Handle);
  SetLength(Str, MAX_PATH);
  GetClassName(Handle, PChar(Str), MAX_PATH);
  SetLength(Str, StrLen(PChar(Str)));
  WriteLn('Show ', Str);
  }
  
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if not fVisible then
        Show;
  fVisible := true;
  ShowWindow(fHandle,SW_SHOW);
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TCustomControl.Invalidate;
var
  R: TRect;
begin
  if Parent=nil then
    exit;
  R := ClientRect;
  InvalidateRect(Parent.Handle,@R, false);
end;


procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
//var
  //R: TRect;
begin
  //MoveWindow(Handle,ALeft,Atop,AWidth,AHeight,false);
  MoveWindow(Handle,ALeft,Atop,AWidth,AHeight,true); //false);

  //WriteLn('윈컨트롤 바운드 재설정');
  //R := ClientRect;
  //InvalidateRect(Handle, @R, false);
end;

end.

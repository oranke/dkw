{-----------------------------------------------------------------------------
 Unit Name: selection
 Author:    oranke_f
 Date:      2010-07-01
 Purpose:
  selection.c 포팅.
  
 History:

-----------------------------------------------------------------------------}


unit selection;

interface

uses
  Windows, SysUtils;

implementation

uses
  dkw_h;

var
  gSelectMode : Integer = 0;
  gSelectPos  : COORD	= ( // { -1, -1 }; // pick point
    x : -1; y: -1
  );
  gSelectRect  : SMALL_RECT = ( //{ -1, -1, -1, -1 }; // expanded selection area
    Left   : -1;
    Top    : -1;
    Right  : -1;
    Bottom : -1;
  );

const
  WORD_BREAK_CHARS : array [0..27-1] of WideChar = (
    ' ',  '"',  '&',  '''',  '(',  ')', '*',
    ',',  ';',  '<',  '=',   '>',  '?', '@',
    '[',  '\',  ']',  '^',  '`',   '{',  '}',
    '~', #9,

    #$3000,
    #$3001,
    #$3002,
    #0
  );

function IsWordBreakChar(wc: WideChar): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := Low(WORD_BREAK_CHARS) to High(WORD_BREAK_CHARS) do
    if WORD_BREAK_CHARS[i] = wc then
    begin
      Result := true;
      Exit;
    end;
end;  

function SCRN_InvalidArea(x, y: SmallInt): BOOL;
begin
  Result :=
    (y < gCSI^.srWindow.Top)    or
    (y > gCSI^.srWindow.Bottom) or
    (x < gCSI^.srWindow.Left)   or
    (x > gCSI^.srWindow.Right)
end;

function SELECT_GetScrn(x, y: SmallInt): PCharInfo;
begin
  Result := gScreen;
  Inc(Result, CSI_WndCols(gCSI) * (y - gCSI^.srWindow.Top) + x);
end;

procedure __select_word_expand_left();
var
  base, ptr: PCharInfo;
  c: Integer;
begin
	if SCRN_InvalidArea(gSelectRect.Left, gSelectRect.Top) then Exit;

  base := SELECT_GetScrn(gSelectRect.Left, gSelectRect.Top);
  ptr  := base;
  c    := gSelectRect.Left;

  // todo: 조건 체크해!!
	//for( ; c >= gCSI->srWindow.Left ; c--, ptr--)
  while c >= gCSI^.srWindow.Left do
  begin
		if IsWordBreakChar(ptr^.UnicodeChar) then
    begin
      Inc(c);
      Break;
    end;

    Dec(c);
    Dec(ptr);
  end;

	if (c < 0) then c := 0;
	if (gSelectRect.Left > c) then gSelectRect.Left := c;
end;


procedure __select_word_expand_right();
var
  base, ptr: PCharInfo;
  c: Integer;
begin
	if SCRN_InvalidArea(gSelectRect.Right, gSelectRect.Bottom) then Exit;

  base := SELECT_GetScrn(gSelectRect.Right, gSelectRect.Bottom);
  ptr  := base;
  c    := gSelectRect.Right;
    
  // todo: 조건 체크해!!
	//for( ; c <= gCSI->srWindow.Right ; c++, ptr++)
  while c <= gCSI^.srWindow.Right do
  begin
    if IsWordBreakChar(ptr^.UnicodeChar) then
      Break;

    Inc(c);
    Inc(ptr);  
  end;

	if (gSelectRect.Right < c) then
    gSelectRect.Right := c; 
end;

procedure __select_char_expand();
var
  base: PCharInfo;
begin
	if SCRN_InvalidArea(gSelectRect.Left, gSelectRect.Top) then
  begin
  end else
  begin
		base  := SELECT_GetScrn(gSelectRect.Left, gSelectRect.Top);
		if BOOL(base^.Attributes and COMMON_LVB_TRAILING_BYTE) then
			Dec(gSelectRect.Left);
  end;

	if SCRN_InvalidArea(gSelectRect.Right, gSelectRect.Bottom) then
  begin
  end else
  begin
		base  := SELECT_GetScrn(gSelectRect.Right, gSelectRect.Bottom);
		if BOOL(base^.Attributes and COMMON_LVB_TRAILING_BYTE) then
			Inc(gSelectRect.Right);
  end;
end;

procedure __select_expand();
begin
	if (gSelectMode = 0)  then
		__select_char_expand()
  else
  if (gSelectMode = 1) then
  begin
		__select_word_expand_left();
		__select_word_expand_right();
  end else
  if (gSelectMode = 2) then
  begin
		gSelectRect.Left  := gCSI^.srWindow.Left;
		gSelectRect.Right := gCSI^.srWindow.Right+1;
  end;
end;

//static void copy_char(wchar_t*& p, CHAR_INFO* src, SHORT start, SHORT end, bool ret = true)
procedure copy_char(var p: PWideChar; src: PCharInfo; _start, _end: SHORT; ret: BOOL = true);
var
  pend, test, last: PCharInfo;
begin
	pend := src;
  Inc(pend, _end);
	test := src;
  Inc(test, _start);
	last := test;
  Dec(last, 1);

	//* search last char */
  while DWORD(test) <= DWORD(pend) do
  begin
    if (test^.UnicodeChar > Char($20)) then
      last := test;
    Inc(test);
  end;
	//* copy */
  test := src;
  Inc(test, _start);
  while DWORD(test) <= DWORD(last) do
  begin
		if not BOOL(test^.Attributes and COMMON_LVB_TRAILING_BYTE) then
    begin
      p^ := test^.UnicodeChar;
      Inc(p);
    end;
    Inc(test);
  end;

	if ret and (DWORD(last) < DWORD(pend)) then
  begin
    p^ := #13;
    Inc(p);
    p^ := #10;
    Inc(p);
	end;

	p^ := #0;
end;


procedure window_to_charpos(var x, y: Integer);
begin
  Dec(x, gBorderSize);
  Dec(y, gBorderSize);
	if (x < 0) then x := 0;
	if (y < 0) then y := 0;
	x := x div Integer(gFontW);
	y := y div Integer(gFontH);
  Inc(x, gCSI^.srWindow.Left);
  Inc(y, gCSI^.srWindow.Top);
	if(x > gCSI^.srWindow.Right)  then x := gCSI^.srWindow.Right+1;
	if(y > gCSI^.srWindow.Bottom) then y := gCSI^.srWindow.Bottom;
end;

function SELECT_Invalid(): BOOL;
begin
  Result :=
 	 (gSelectRect.Top > gSelectRect.Bottom) or
	 ((gSelectRect.Top = gSelectRect.Bottom) and
  	(gSelectRect.Left >= gSelectRect.Right))
end;

//*----------*/
function selectionGetArea(var sr: SMALL_RECT): BOOL;
begin
  Result := false;
	if SELECT_Invalid then Exit;
	sr := gSelectRect;
  Result := true;
end;

//*----------*/
procedure selectionClear(hWnd: HWND);
begin
  if SELECT_Invalid then Exit;

	gSelectRect.Left   := 0;
  gSelectRect.Right  := 0;
	gSelectRect.Top    := 0;
  gSelectRect.Bottom := 0;
	InvalidateRect(hWnd, nil, FALSE);
end;


//*----------*/
function selectionGetString(): PWideChar;
var
  nb, y: Integer;

	size: COORD;
	work: PCharInfo;
	buffer: PWideChar;
	wp: PWideChar;
	pos : COORD;
	sr: SMALL_RECT;
begin
  Result := nil;
	if SELECT_Invalid then Exit;


	if (gSelectRect.Top = gSelectRect.Bottom) then
		nb := gSelectRect.Right - gSelectRect.Left
	else
  begin
		nb := gCSI^.srWindow.Right - gSelectRect.Left+1;
		for y := gSelectRect.Top+1 to gSelectRect.Bottom-1 do
			Inc(nb, CSI_WndCols(gCSI));
		Inc(nb, gSelectRect.Right - gCSI^.srWindow.Left);
	end;

  size.X := CSI_WndCols(gCSI);
  size.Y := 1;

  GetMem(work, SizeOf(CHAR_INFO) * size.X);
  GetMem(buffer, SizeOf(WideChar) * (nb + 32));
  wp := buffer;
  pos.X := 0;
  pos.Y := 0;
  sr.Left := gCSI^.srWindow.Left;
  sr.Top  := 0;
  sr.Right  := gCSI^.srWindow.Right;
  sr.Bottom := 0;

  wp^ := #0;

	if (gSelectRect.Top = gSelectRect.Bottom) then
  begin
		sr.Top    := gSelectRect.Top;
    sr.Bottom := gSelectRect.Top;
		ReadConsoleOutput_Unicode(gStdOut, work, size, pos, @sr);
		copy_char(wp, work, gSelectRect.Left, gSelectRect.Right-1, false);
  end else
  begin
		sr.Top    := gSelectRect.Top;
    sr.Bottom := gSelectRect.Top;
		ReadConsoleOutput_Unicode(gStdOut, work, size, pos, @sr);
		copy_char(wp, work, gSelectRect.Left, gCSI^.srWindow.Right);
		for y := gSelectRect.Top+1 to gSelectRect.Bottom-1 do
    begin
			sr.Top := y;
      sr.Bottom := y;
			ReadConsoleOutput_Unicode(gStdOut, work, size, pos, @sr);
			copy_char(wp, work, gCSI^.srWindow.Left, gCSI^.srWindow.Right);
    end;
		sr.Top    := gSelectRect.Bottom;
    sr.Bottom := gSelectRect.Bottom;
		ReadConsoleOutput_Unicode(gStdOut, work, size, pos, @sr);
		copy_char(wp, work, gCSI^.srWindow.Left, gSelectRect.Right-1, false);
  end;

  FreeMem(work);
  Result := buffer;
end;

//*----------*/
{$WRITEABLECONST ON}
procedure onLBtnDown(hWnd: HWND; x, y: Integer);
const
  prev_time: DWORD = 0;
  prevX : Integer = -100;
  prevY : Integer = -100;
var
  now_time, stime: DWORD;
  sx, sy: Integer;  
begin
  //* calc click count */
  begin
    now_time := GetTickCount();
    if (prev_time > now_time) then
      stime := now_time + (not prev_time)+1
    else
      stime := now_time - prev_time;
    if (stime <= GetDoubleClickTime()) then
    begin
      if prevX > x then sx := prevX-x else sx := x-prevX;
      if prevY > y then sy := prevY-y else sy := y-prevY;

      if (sx <= GetSystemMetrics(SM_CXDOUBLECLK)) and 
         (sy <= GetSystemMetrics(SM_CYDOUBLECLK)) then
      begin
        Inc(gSelectMode);
        if (gSelectMode > 2) then gSelectMode := 0;
      end else
        gSelectMode := 0;

    end else
      gSelectMode := 0;

    prev_time := now_time;
    prevX := x;
    prevY := y;
  end;

	if (not Assigned(gScreen)) or (not Assigned(gCSI)) then Exit;

	window_to_charpos(x, y);
	SetCapture(hWnd);

	gSelectPos.X := x;
	gSelectPos.Y := y;
	gSelectRect.Left  := x;
  gSelectRect.Right := x;
	gSelectRect.Top    := y;
  gSelectRect.Bottom := y;

	__select_expand();
	InvalidateRect(hWnd, nil, FALSE);
end;
{$WRITEABLECONST OFF}

//*----------*/
procedure onLBtnUp(hWnd: HWND; x, y: Integer);
var
  str: PWideChar;
  _length: Integer;
  hMem: THandle;
  ptr: PWideChar;
  _result: BOOL;
begin
	if (hWnd <> GetCapture()) then Exit;
	ReleaseCapture();

	if (not Assigned(gScreen)) or (not Assigned(gCSI)) then Exit;
	//window_to_charpos(x, y);

	str := selectionGetString();
	if not Assigned(str) then Exit;

  _length := Length(str) + 1;
	_result := true;

	hMem := GlobalAlloc(GMEM_MOVEABLE, sizeof(WideChar) * _length);
	if not BOOL(hMem) then _result := false;

  ptr := GlobalLock(hMem);
	if (_result and   (not Assigned(ptr))) then
		_result := false;
	if (_result) then
  begin
		CopyMemory(ptr, str, sizeof(WideChar) * _length);
		GlobalUnlock(hMem);
  end;
	if (_result and (not OpenClipboard(hWnd))) then
  begin
		Sleep(10);
		if (not OpenClipboard(hWnd)) then
			_result := false;
	end;
	if (_result) then
  begin
		if (not EmptyClipboard()) or
		   (not BOOL(SetClipboardData(CF_UNICODETEXT, hMem))) then
			_result := false;
		CloseClipboard();
	end;
	if (not _result) and BOOL(hMem) then
		GlobalFree(hMem);
    
	FreeMem(str);
end;


//*----------*/
procedure onMouseMove(hWnd: HWND; x, y: Integer);
var
  bak: SMALL_RECT;
begin
	if (hWnd <> GetCapture()) then Exit;
	if (not Assigned(gScreen)) or (not Assigned(gCSI)) then Exit;
	window_to_charpos(x, y);

  bak := gSelectRect;

	if (y < gSelectPos.Y) or ((y = gSelectPos.Y) and (x < gSelectPos.X)) then
  begin
		gSelectRect.Left   := x;
		gSelectRect.Top    := y;
		gSelectRect.Right  := gSelectPos.X;
		gSelectRect.Bottom := gSelectPos.Y;
  end else
  begin
		gSelectRect.Left   := gSelectPos.X;
		gSelectRect.Top    := gSelectPos.Y;
		gSelectRect.Right  := x;
		gSelectRect.Bottom := y;
  end;

	__select_expand();

  if not Comparemem(@bak, @gSelectRect, sizeof(bak)) then
		InvalidateRect(hWnd, nil, FALSE);
end;


initialization
  dkw_h.selectionGetArea := selectionGetArea;
  dkw_h.selectionClear   := selectionClear;

  dkw_h.onLBtnDown:= onLBtnDown;
  dkw_h.onLBtnUp:= onLBtnUp;
  dkw_h.onMouseMove:= onMouseMove;

finalization

end.


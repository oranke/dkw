{-----------------------------------------------------------------------------
 Unit Name: main
 Author:    oranke_f
 Date:      2010-7-01
 Purpose:
  main.c 파일의 포팅.

 History:
  2010-07-01
    작업 시작.

  2010-07-08
    윈도7, 비스타에서 초기구동시 나타나는 작은 콘솔창 오류 수정.
    gSelectOverScreen 옵션 추가.

    __draw_screen 에서 불필요한 work_width 를 사용하지 않도록
    BlockWorkWidth 지시자 추가.  

  2010-07-11
    알트와 휠, 또는 알트와 +/- 키로 투명도 조절할 수 있게 함.
    
-----------------------------------------------------------------------------}

{$DEFINE HideConsole}

unit main;

interface

uses
  Windows, Messages, SysUtils;

procedure WinMain();

implementation

uses
  {dialogs, }dkw_h, IMM, option;

var
{
  gStdIn      : THandle = 0;
  gStdOut     : THandle = 0;
  gFontW      : DWORD;
  gFontH      : DWORD;
  gBorderSize : DWORD;
  gCSI        : PConsoleScreenBufferInfo;
  gScreen     : PCharInfo;
}

  gStdErr : THandle = 0;
  gConWnd : THandle = 0;
  gChild : THandle = 0;	//* child process */

  gFontLog  : LOGFONT;	//* font IME */
  gFont     : THandle;		//* font */
  //gFontW    : DWORD;		//* char width */
  //gFontH    : DWORD;		//* char height */

  gWinW     : DWORD;		//* window columns */
  gWinH     : DWORD;		///* window rows */

  gFrame    : TRect;		  //* window frame size */
  gBgBmp    : THandle = 0;	//* background image */
  gBgBrush  : THandle = 0;  //* background brush */
  //gBorderSize : DWORD	= 0;  //* internal border */
  gLineSpace : DWORD	= 0;	//* line space */
  gVScrollHide : BOOL	 = FALSE;

  gImeOn : BOOL	= FALSE; //* IME-status */

  gTitle : PWideChar = nil;

  gFontSizeCtrl : BOOL;
  gFontSizeCtrlStep: Integer;

  gIsTranspColor: BOOL;
  gTransp: Integer;
  gTranspCtrl : BOOL;
  gTranspCtrlStep: Integer;
  
  gSelectOverScreen: BOOL = true;


const
  //* setConsoleFont */
  MAX_FONTS = 128;

type
  _CONSOLE_FONT = record
    index: DWORD;
    dim: COORD;
  end;

  CONSOLE_FONT = _CONSOLE_FONT;
  TConsoleFont = _CONSOLE_FONT;
  PConsoleFont = ^TConsoleFont;


function GetConsoleFontInfo(a: THandle; b: BOOL; c: DWORD; d: PConsoleFont): BOOL; stdcall; external 'KERNEL32.DLL' name 'GetConsoleFontInfo';
function GetNumberOfConsoleFonts: DWORD; stdcall; external 'KERNEL32.DLL' name 'GetNumberOfConsoleFonts';
function SetConsoleFont(a: THandle; b: DWORD): BOOL; stdcall; external 'KERNEL32.DLL' name 'SetConsoleFont';
function GetConsoleWindow(): HWND; stdcall; external 'KERNEL32.DLL' name 'GetConsoleWindow';

const
  kColor0                =  0;
  kColor1                =  1;
  kColor2                =  2;
  kColor3                =  3;
  kColor4                =  4;
  kColor5                =  5;
  kColor6                =  6;
  kColor7                =  7;
	kColor8                =  8;
  kColor9                =  9;
  kColor10               =  10;
  kColor11               =  11;
  kColor12               =  12;
  kColor13               =  13;
  kColor14               =  14;
  kColor15               =  15;
	kColorCursorFg         =  16;
  kColorCursorBg         =  17;
  kColorCursorImeFg      =  18;
  kColorCursorImeBg      =  19;
  kColorMax              =  20;

var
  gColorTable: Array [ 0..kColorMax-1 ] of COLORREF;



function ReadConsoleOutput_Unicode(con: THandle; buffer: PCharInfo;
	size, pos: COORD; sr: PSmallRect): BOOL; stdcall;
var
  s, s2, e: PCharInfo;
  codepage: DWORD;
  ch: Array [0..2-1] of Char;
  wch: WideChar;
begin
  Result := FALSE;

  {
	if not ReadConsoleOutputW(con, buffer, size, pos, sr^) then Exit;
  Result := true;
  Exit;
  {}

	if not ReadConsoleOutputA(con, buffer, size, pos, sr^) then Exit;

  // 시작버퍼 설정.
  s := buffer;
  // 끝버퍼 설정. 
  e := buffer;
  Inc(e, size.X * size.Y);

  codepage := GetConsoleOutputCP();

  while DWORD(s) < DWORD(e) do
  begin
    ch[0] := s^.AsciiChar;

		if BOOL(s^.Attributes and COMMON_LVB_LEADING_BYTE) then
    begin
      s2 := s;
      Inc(s2);

			if (DWORD(s2) < DWORD(e)) and BOOL(s2^.Attributes and COMMON_LVB_TRAILING_BYTE) then
      begin
				ch[1] := s2^.AsciiChar;
        wch := #0;
				if BOOL(MultiByteToWideChar(codepage, 0, @ch, 2, @wch, 1)) then
        begin
          s^.UnicodeChar := wch;
          Inc(s);
          s^.UnicodeChar := wch;
          Inc(s);
          Continue;
        end;
      end;
    end;

    wch := #0;
		if BOOL(MultiByteToWideChar(codepage, 0, @ch, 1, @wch, 1)) then
			s^.UnicodeChar := wch;

		s^.Attributes := s^.Attributes and not (COMMON_LVB_LEADING_BYTE or COMMON_LVB_TRAILING_BYTE);
		Inc(s);
  end;

  Result := true;
end;

//*****************************************************************************/

//*----------*/
procedure __draw_invert_char_rect(hDC: THandle; var rc: TRect);
begin
  Inc(rc.Right);
  Inc(rc.Bottom);
  rc.Left  := rc.Left * Integer(gFontW);
  rc.Right := rc.Right * Integer(gFontW);
  rc.Top    := rc.Top * Integer(gFontH);
  rc.Bottom := rc.Bottom * Integer(gFontH);
	BitBlt(hDC, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, 0, 0, 0, DSTINVERT);
end;

//*----------*/
procedure __draw_selection(hDC: THandle);
var
	sel: SMALL_RECT;
  rc: TRect;
begin
  if not selectionGetArea(sel) then Exit;

	if (gCSI^.srWindow.Top <= sel.Top) and (sel.Top <= gCSI^.srWindow.Bottom) then
  begin
  end else
  if (gCSI^.srWindow.Top <= sel.Bottom) and (sel.Bottom <= gCSI^.srWindow.Bottom) then
  begin

  end else
  if (sel.Top < gCSI^.srWindow.Top) and (gCSI^.srWindow.Bottom < sel.Bottom) then
  begin
  end else
    Exit;

	if (sel.Top = sel.Bottom) then
  begin
		//* single line */
		rc.left  := sel.Left - gCSI^.srWindow.Left;
		rc.right := sel.Right-1 - gCSI^.srWindow.Left;
		rc.bottom := sel.Top - gCSI^.srWindow.Top;
		rc.top    := rc.bottom;
		__draw_invert_char_rect(hDC, rc);
		Exit;
  end;

	//* multi line */
	if (gCSI^.srWindow.Top <= sel.Top) and (sel.Top <= gCSI^.srWindow.Bottom) then
  begin
		//* top */
		rc.left := sel.Left - gCSI^.srWindow.Left;
		rc.right := gCSI^.srWindow.Right - gCSI^.srWindow.Left;
		rc.bottom := sel.Top - gCSI^.srWindow.Top;
		rc.top := rc.bottom;
		__draw_invert_char_rect(hDC, rc);
  end;

	if (sel.Top+1 <= sel.Bottom-1) then
  begin
		//* center */
		rc.left := 0;
		rc.right := gCSI^.srWindow.Right - gCSI^.srWindow.Left;

		if (gCSI^.srWindow.Top <= sel.Top+1) then
			rc.top := sel.Top+1 - gCSI^.srWindow.Top
		else
			rc.top := 0;

		if (gCSI^.srWindow.Bottom >= sel.Bottom-1) then
			rc.bottom := sel.Bottom-1 - gCSI^.srWindow.Top
		else
			rc.bottom := gCSI^.srWindow.Bottom - gCSI^.srWindow.Top;
		__draw_invert_char_rect(hDC, rc);
	end;
  
	if (gCSI^.srWindow.Top <= sel.Bottom) and (sel.Bottom <= gCSI^.srWindow.Bottom) then
  begin
		//* bottom */
		rc.left := 0;
		rc.right := sel.Right-1 - gCSI^.srWindow.Left;
		rc.bottom := sel.Bottom - gCSI^.srWindow.Top;
		rc.top := rc.bottom;
		__draw_invert_char_rect(hDC, rc);
  end;

end;

//*----------*/
{$DEFINE BlockWorkWidth}
procedure __draw_screen(hDC: THandle);
var
	pntX, pntY,
	x, y: Integer;
	color_fg,
	color_bg: Integer;
	ptr: PCharInfo;
	work_color_fg,
	work_color_bg: Integer;
	work_text,
	work_text_ptr: PWideChar;
	work_pntX: Integer;
{$IFNDEF BlockWorkWidth}
	work_width: PInteger;
	work_width_ptr: PInteger;
{$ENDIF}  

begin
	ptr := gScreen;
	work_color_fg := -1;
	work_color_bg := -1;
  GetMem(work_text, SizeOf(WideChar) * CSI_WndCols(gCSI));
{$IFNDEF BlockWorkWidth}
  GetMem(work_width, SizeOf(Integer) * CSI_WndCols(gCSI));
{$ENDIF}
	pntY := 0;
	for y := gCSI^.srWindow.Top to gCSI^.srWindow.Bottom do
  begin
		pntX := 0;
		work_pntX := 0;
		work_text_ptr := work_text;
{$IFNDEF BlockWorkWidth}
		work_width_ptr := work_width;
{$ENDIF}    
		for x := gCSI^.srWindow.Left to gCSI^.srWindow.Right do
    begin
			if BOOL(ptr^.Attributes and COMMON_LVB_TRAILING_BYTE) then
      begin
				Inc(pntX, gFontW);
				Inc(ptr);
				continue;
      end;

			color_fg := ptr^.Attributes and $F;
			color_bg := (ptr^.Attributes shr 4) and $F;

			if (color_fg <> work_color_fg) or
			   (color_bg <> work_color_bg) then
      begin
				if (work_text_ptr > work_text) then
				//if DWORD(work_text_ptr) > DWORD(work_text) then
        begin
					ExtTextOutW(hDC, work_pntX, pntY, 0, nil,
						work_text,
						work_text_ptr - work_text,
        {$IFNDEF BlockWorkWidth}
						work_width
        {$ELSE}
            nil
        {$ENDIF}
          );
        end;

				work_text_ptr := work_text;
{$IFNDEF BlockWorkWidth}
				work_width_ptr := work_width;
{$ENDIF}        
				work_pntX := pntX;
				work_color_fg := color_fg;
				work_color_bg := color_bg;
				SetTextColor(hDC, gColorTable[work_color_fg]);
				SetBkColor(  hDC, gColorTable[work_color_bg]);
        if BOOL(work_color_bg) then
          SetBkMode(hDC, OPAQUE)
        else
          SetBkMode(hDC, TRANSPARENT);
      end;

			if BOOL(ptr^.Attributes and COMMON_LVB_LEADING_BYTE) then
      begin
        work_text_ptr^ := ptr^.UnicodeChar;
        Inc(work_text_ptr);
{$IFNDEF BlockWorkWidth}
				work_width_ptr^ := gFontW * 2;
        Inc(work_width_ptr);
{$ENDIF}
      end else
      begin
				work_text_ptr^ := ptr^.UnicodeChar;
        Inc(work_text_ptr);
{$IFNDEF BlockWorkWidth}
				work_width_ptr^ := gFontW;
        Inc(work_width_ptr);
{$ENDIF}        
      end;

			Inc(pntX, gFontW);
			Inc(ptr);
    end;

		if (work_text_ptr > work_text) then
		//if DWORD(work_text_ptr) > DWORD(work_text) then
    begin
			ExtTextOutW(hDC, work_pntX, pntY, 0, nil,
				work_text,
				work_text_ptr - work_text,
    {$IFNDEF BlockWorkWidth}
				work_width
    {$ELSE}
        nil
    {$ENDIF}
      );
    end;

		Inc(pntY, gFontH);

  end;

	//* draw selection */
	__draw_selection(hDC);
  
	//* draw cursor */
	if (gCSI^.srWindow.Top    <= gCSI^.dwCursorPosition.Y) and
	   (gCSI^.srWindow.Bottom >= gCSI^.dwCursorPosition.Y) and
	   (gCSI^.srWindow.Left   <= gCSI^.dwCursorPosition.X) and
	   (gCSI^.srWindow.Right  >= gCSI^.dwCursorPosition.X) then
  begin
    if gImeOn then
    begin
      color_fg := kColorCursorImeFg;
      color_bg := kColorCursorImeBg;
    end else
    begin
      color_fg := kColorCursorFg;
      color_bg := kColorCursorBg;
    end;
		SetTextColor(hDC, gColorTable[ color_fg ]);
		SetBkColor(  hDC, gColorTable[ color_bg ]);
		SetBkMode(hDC, OPAQUE);
		pntX := gCSI^.dwCursorPosition.X - gCSI^.srWindow.Left;
		pntY := gCSI^.dwCursorPosition.Y - gCSI^.srWindow.Top;
    ptr  := gScreen;
		Inc(ptr, CSI_WndCols(gCSI) * pntY + pntX);
		pntX := pntX * Integer(gFontW);
		pntY := pntY * Integer(gFontH);

{$IFNDEF BlockWorkWidth}
    if BOOL(ptr^.Attributes and COMMON_LVB_LEADING_BYTE) then
  		work_width^ :=  gFontW*2
    else
     work_width^ :=  gFontW;
{$ENDIF}

		ExtTextOutW(hDC, pntX, pntY, 0, nil,
			@ptr^.UnicodeChar, 1,
{$IFNDEF BlockWorkWidth}
      work_width
{$ELSE}
      nil
{$ENDIF}
    );
  end;

{$IFNDEF BlockWorkWidth}
	FreeMem(work_width);
{$ENDIF}  
	FreeMem(work_text);
end;

//*----------*/
procedure onPaint(hWnd: THandle);
var
	ps: PAINTSTRUCT;
	hDC: THandle;
	rc: TRect;

	hMemDC,
	hBmp,
	oldfont,
	oldbmp: THandle;
begin
	hDC := BeginPaint(hWnd, ps);
	GetClientRect(hWnd, rc);

	hMemDC := CreateCompatibleDC(hDC);
	hBmp := CreateCompatibleBitmap(hDC, rc.right-rc.left, rc.bottom-rc.top);
	oldfont := SelectObject(hMemDC, gFont);
	oldbmp  := SelectObject(hMemDC, hBmp);

	FillRect(hMemDC, rc, gBgBrush);

	if Assigned(gScreen) and Assigned(gCSI) then
  begin
		SetWindowOrgEx(hMemDC, -Integer(gBorderSize), -Integer(gBorderSize), nil);
		__draw_screen(hMemDC);
		SetWindowOrgEx(hMemDC, 0, 0, nil);
  end;

	BitBlt(hDC,rc.left,rc.top, rc.right-rc.left, rc.bottom-rc.top, hMemDC,0,0, SRCCOPY);

	SelectObject(hMemDC, oldfont);
	SelectObject(hMemDC, oldbmp);
	DeleteObject(hBmp);
	DeleteDC(hMemDC);

	EndPaint(hWnd, ps);
end;

//*----------*/
procedure __set_console_window_size(cols, rows: LongInt);
var
	csi: CONSOLE_SCREEN_BUFFER_INFO;
  tmp: SMALL_RECT;
begin
	GetConsoleScreenBufferInfo(gStdOut, csi);

	gWinW := cols;
	gWinH := rows;

	if (cols = CSI_WndCols(@csi)) and (rows = CSI_WndRows(@csi)) then Exit;

	tmp.Left := 0;
  tmp.Right := 0;
  tmp.Top   := 0;
  tmp.Bottom := 0;
	SetConsoleWindowInfo(gStdOut, TRUE, tmp);

	csi.dwSize.X := SHORT(cols);
	csi.srWindow.Left := 0;
	csi.srWindow.Right := SHORT(cols -1);

	if (csi.dwSize.Y < rows) or (csi.dwSize.Y = CSI_WndRows(@csi)) then
		csi.dwSize.Y := SHORT(rows);

	Inc(csi.srWindow.Bottom, SHORT((rows - CSI_WndRows(@csi))));
	if (csi.dwSize.Y <= csi.srWindow.Bottom) then
  begin
		Dec(csi.srWindow.Top, csi.srWindow.Bottom - csi.dwSize.Y +1);
		csi.srWindow.Bottom := csi.dwSize.Y -1;
  end;

	SetConsoleScreenBufferSize(gStdOut, csi.dwSize);
	SetConsoleWindowInfo(gStdOut, TRUE, csi.srWindow);
end;

//*----------*/
procedure onSizing(hWnd: THandle; side: DWORD; rc: PRect);
var
  fw, fh, width, height: LongInt;
begin
	//trace("onSizing\n");
	fw := (gFrame.right - gFrame.left) + (Integer(gBorderSize) * 2);
	fh := (gFrame.bottom - gFrame.top) + (Integer(gBorderSize) * 2);
	width  := rc^.right - rc^.left;
	height := rc^.bottom - rc^.top;

	Dec(width, fw);
	Dec(width, width mod Integer(gFontW));
	Inc(width, fw);

	Dec(height, fh);
	Dec(height, height mod Integer(gFontH));
	Inc(height, fh);

	if (side = WMSZ_LEFT) or (side =WMSZ_TOPLEFT) or (side = WMSZ_BOTTOMLEFT) then
		rc^.left := rc^.right - width
	else
		rc^.right := rc^.left + width;

	if (side = WMSZ_TOP) or (side = WMSZ_TOPLEFT) or (side = WMSZ_TOPRIGHT) then
		rc^.top := rc^.bottom - height
	else
		rc^.bottom := rc^.top + height;
end;

//*----------*/
procedure onWindowPosChange(hWnd: THandle; wndpos: PWindowPos);
var
  fw, fh, width, height: LongInt;
begin
	//trace("onWindowPosChange\n");
	if(not BOOL(wndpos^.flags and SWP_NOSIZE)) and (not IsIconic(hWnd)) then
  begin
		fw := (gFrame.right - gFrame.left) + (Integer(gBorderSize) * 2);
		fh := (gFrame.bottom - gFrame.top) + (Integer(gBorderSize) * 2);
		width  := wndpos^.cx;
		height := wndpos^.cy;
		width  := (width - fw) div Integer(gFontW);
		height := (height - fh) div Integer(gFontH);

		__set_console_window_size(width, height);

		wndpos^.cx := width  * Integer(gFontW) + fw;
		wndpos^.cy := height * Integer(gFontH) + fh;
  end;
end;


procedure __set_ime_position(hWnd: THandle);
var
  imc: THandle;
  px, py: LongInt;
	cf: COMPOSITIONFORM;
begin
	if not gImeOn or not Assigned(gCSI) then Exit;
	imc := ImmGetContext(hWnd);
	px := gCSI^.dwCursorPosition.X - gCSI^.srWindow.Left;
	py := gCSI^.dwCursorPosition.Y - gCSI^.srWindow.Top;
	cf.dwStyle := CFS_POINT;
	cf.ptCurrentPos.x := px * Integer(gFontW) + Integer(gBorderSize);
	cf.ptCurrentPos.y := py * Integer(gFontH) + Integer(gBorderSize);
	ImmSetCompositionWindow(imc, @cf);
	ImmReleaseContext(hWnd, imc);
end;

//*----------*/


{$WRITEABLECONST ON}
procedure onTimer(hWnd: THandle);
const
  timer_count: Integer = 0;
var
  str: PWideChar;
  csi: PConsoleScreenBufferInfo;
	size: COORD;
  nb: DWORD;
	buffer,
	ptr: PCharInfo;
	sr: SMALL_RECT;
	pos : COORD;

	si: SCROLLINFO;
  w, h: Integer;
begin

	if (WaitForSingleObject(gChild, 0) <> WAIT_TIMEOUT) then
  begin
		PostMessageW(hWnd, WM_CLOSE, 0,0);
		Exit;
  end;

	//* refresh handle */
	if BOOL(gStdOut) then CloseHandle(gStdOut);
	gStdOut :=
    CreateFileW(
      'CONOUT$', GENERIC_READ or GENERIC_WRITE,
			FILE_SHARE_READ or FILE_SHARE_WRITE,
			nil, OPEN_EXISTING, 0, 0
    );

	//* title update */
  Inc(timer_count);
	if (timer_count and $F) = 1 then
  begin
		GetMem(str, SizeOf(WideChar) * 256);
		GetConsoleTitleW(str, 256);

		//if Assigned(gTitle) and  not BOOL(WideCompareStr(gTitle^, str^)) then
		if Assigned(gTitle) and
      not BOOL(CompareStringW(LOCALE_USER_DEFAULT, 0, gTitle, Length(gTitle),
           str, Length(str)) - 2) then
			FreeMem(str)
		else
    begin
			FreeMem(gTitle);
			gTitle := str;
			SetWindowTextW(hWnd, gTitle);
    end;
  end;

	GetMem(csi, SizeOf(CONSOLE_SCREEN_BUFFER_INFO));

	GetConsoleScreenBufferInfo(gStdOut, csi^);
	size.X := CSI_WndCols(csi);
	size.Y := CSI_WndRows(csi);

	///* copy screen buffer */
	nb := size.X * size.Y;
	GetMem(buffer, SizeOf(CHAR_INFO) * nb);
	ptr := buffer;
	pos.X := 0;
  pos.Y := 0;

	//* ReadConsoleOuput - maximum read size 64kByte?? */
	size.Y := Round($8000 / sizeof(CHAR_INFO) / size.X);
	sr.Left  := csi^.srWindow.Left;
	sr.Right := csi^.srWindow.Right;
	sr.Top   := csi^.srWindow.Top;

	repeat
		sr.Bottom := sr.Top + size.Y -1;
		if (sr.Bottom > csi^.srWindow.Bottom) then
    begin
			sr.Bottom := csi^.srWindow.Bottom;
			size.Y := sr.Bottom - sr.Top +1;
    end;
		ReadConsoleOutput_Unicode(gStdOut, ptr, size, pos, @sr);
		Inc(ptr, size.X * size.Y);
		sr.Top := sr.Bottom +1;
	until (sr.Top > csi^.srWindow.Bottom);

	//* compare */
	if Assigned(gScreen) and Assigned(gCSI) and
	   CompareMem(csi, gCSI, sizeof(CONSOLE_SCREEN_BUFFER_INFO)) and
	   CompareMem(buffer, gScreen, sizeof(CHAR_INFO) * nb) then
  begin
		//* no modified */
		FreeMem(buffer);
		FreeMem(csi);
		Exit;
  end;

	//* swap buffer */
	if Assigned(gScreen) then
  begin
    FreeMem(gScreen);
    gScreen := nil;
  end;
	if Assigned(gCSI) then
  begin
    FreeMem(gCSI);
    gCSI := nil;
  end;
	gScreen := buffer;
	gCSI := csi;

	//* redraw request */
	InvalidateRect(hWnd, nil, TRUE);

	//* set vertical scrollbar status */
	if not gVScrollHide then
  begin
		si.cbSize := sizeof(si);
		si.fMask := SIF_DISABLENOSCROLL or SIF_POS or SIF_PAGE or SIF_RANGE;
		si.nPos := gCSI^.srWindow.Top;
		si.nPage := CSI_WndRows(gCSI);
		si.nMin := 0;
		si.nMax := gCSI^.dwSize.Y-1;
		SetScrollInfo(hWnd, SB_VERT, si, TRUE);
  end;

	if gImeOn then __set_ime_position(hWnd);

	w := CSI_WndCols(gCSI);
	h := CSI_WndRows(gCSI);
	if (Integer(gWinW) <> w) or (Integer(gWinH) <> h) then
  begin
		w := (w * Integer(gFontW)) + (Integer(gBorderSize) * 2) + (gFrame.right - gFrame.left);
		h := (h * Integer(gFontH)) + (Integer(gBorderSize) * 2) + (gFrame.bottom - gFrame.top);
		SetWindowPos(hWnd, 0, 0,0,w,h, SWP_NOMOVE or SWP_NOZORDER);
  end;
end;
{$WRITEABLECONST OFF}


//*****************************************************************************/

//var
  //DefWindowProc: function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall = DefWindowProcW;
//function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall; external 'user32.dll' name 'DefWindowProcW';

function create_font(name: string; height: Integer): BOOL; forward;

// 폰트의 크기 조절.
procedure ResizeFont(hwnd: HWND; const aValue: Integer);
var
  fw, fh, width, height: LongInt;
  NewSize: Integer;
begin
  if not gFontSizeCtrl then Exit;

  NewSize := -gFontLog.lfHeight + aValue;
  if NewSize <= 0  then Exit;

  create_font(string(gFontLog.lfFaceName), -gFontLog.lfHeight + aValue);

  fw := (gFrame.right - gFrame.left) + (Integer(gBorderSize) * 2);
  fh := (gFrame.bottom - gFrame.top) + (Integer(gBorderSize) * 2);

  width := Integer(gWinW) * Integer(gFontW) + fw;
  height := Integer(gWinH) * Integer(gFontH) + fh;

  SetWindowPos(hwnd, 0, 0, 0, width, height, SWP_NOMOVE);

  InvalidateRect(hWnd, nil, TRUE);

  WriteFontSize(-gFontLog.lfHeight);
end;

// 투명도 조절.
procedure ResetAlpha(hwnd: HWND; const aValue: Integer);
var
  NewTransp: Integer;
begin
  if gIsTranspColor then Exit;
  if not gTranspCtrl then Exit;

  NewTransp := gTransp + aValue;

  if NewTransp > 255 then NewTransp := 255;
  if NewTransp < 0 then NewTransp := 0;

  if NewTransp = gTransp then Exit;

  gTransp := NewTransp;

  if gTransp in [1..254] then
  begin
    SetWindowLong(hwnd, GWL_EXSTYLE, GetWindowLong(hwnd, GWL_EXSTYLE) or WS_EX_LAYERED);
		SetLayeredWindowAttributes(hWnd, 0, gTransp, LWA_ALPHA)
  end else
  begin
    SetWindowLong(hwnd, GWL_EXSTYLE, GetWindowLong(hwnd, GWL_EXSTYLE) and not WS_EX_LAYERED);
  end;

  WriteTransp(gTransp);

end;

function WndProc(hWnd: HWND; msg: UINT; wp: WPARAM; lp: LPARAM): LRESULT; stdcall;
const
  AltMask = $20000000;
var
  imc: THandle;
  sb: WPARAM;

  x, y: Short;
  rc: TRect;
begin
  Result := 1;

  case msg of
    WM_CREATE:
    begin
			imc := ImmGetContext(hWnd);
			ImmSetCompositionFontW(imc, @gFontLog);
			ImmReleaseContext(hWnd, imc);

		  SetTimer(hWnd, $3571, 10, nil);
    end;

    WM_DESTROY:
    begin
		  KillTimer(hWnd, $3571);
  		PostQuitMessage(0);
  		if (WaitForSingleObject(gChild, 0) = WAIT_TIMEOUT) then
	  		TerminateProcess(gChild, 0);
    end;


	  WM_TIMER: onTimer(hWnd);

  	WM_ERASEBKGND:;

  	WM_PAINT: onPaint(hWnd);

  	WM_SIZING: onSizing(hWnd, DWORD(wp), PRect(lp));
    
	  WM_WINDOWPOSCHANGING,
  	WM_WINDOWPOSCHANGED:
    begin
  		onWindowPosChange(hWnd, PWindowPos(lp));
	  	selectionClear(hWnd);
    end;
    
  	WM_LBUTTONDOWN: onLBtnDown(hWnd, short(LOWORD(lp)), short(HIWORD(lp)));

	  WM_LBUTTONUP: onLBtnUp(hWnd, short(LOWORD(lp)), short(HIWORD(lp)));

  	WM_MOUSEMOVE:
    begin
      onMouseMove(hWnd, short(LOWORD(lp)), short(HIWORD(lp)));

      if gSelectOverScreen then
      begin
        x := Short(LOWORD(lp));
        y := Short(HIWORD(lp));
        GetClientRect(hWnd, rc);

        if ( y < 0 ) then
          PostMessage(gConWnd, WM_MOUSEWHEEL, WHEEL_DELTA shl 16, (y shl 16) or x )
        else
        if (y >= rc.bottom) then
          PostMessage(gConWnd, WM_MOUSEWHEEL, -WHEEL_DELTA shl 16, (y shl 16) or x );

      end;
    end;

	  WM_MBUTTONDOWN,
	  WM_RBUTTONDOWN: onPasteFromClipboard(hWnd);

	  WM_DROPFILES: onDropFile(THandle(wp));
    
	  WM_IME_STARTCOMPOSITION:
    begin
  		__set_ime_position(hWnd);
	  	Result := DefWindowProcW(hWnd, msg, wp, lp);
      Exit;
    end;

  	WM_IME_NOTIFY:
    begin
      if (wp = IMN_SETOPENSTATUS) then
      begin
        imc := ImmGetContext(hWnd);
        gImeOn := ImmGetOpenStatus(imc);
        ImmReleaseContext(hWnd, imc);
        InvalidateRect(hWnd, nil, TRUE);
      end;
      Result := DefWindowProcW(hWnd, msg, wp, lp);
      Exit;
    end;

	  WM_IME_CHAR:
    begin
      PostMessageW(gConWnd, msg, wp, lp);
	  	selectionClear(hWnd);
    end;

	  WM_CHAR:
    begin
      //PostMessage(gConWnd, msg, wp, lp);
	  	selectionClear(hWnd);
    end;

	  WM_SYSCOMMAND:
		if not onSysCommand(hWnd, DWORD(wp)) then
    begin
      Result := DefWindowProcW(hWnd, msg, wp, lp);
      Exit;
    end;

	  WM_VSCROLL: PostMessageW(gConWnd, msg, wp, lp);

	  WM_MOUSEWHEEL:
    begin
      if gFontSizeCtrl and BOOL(LOWORD(wp) and MK_CONTROL) then
      begin
        ResizeFont(
          hwnd,
          gFontSizeCtrlStep * Short(HIWORD(wp)) div ABS(Short(HIWORD(wp)))
        );
      end else
      if (not gIsTranspColor) and gTranspCtrl and BOOL(GetKeyState(VK_MENU) and $8000) then
      begin
        ResetAlpha(
          hwnd,
          gTranspCtrlStep * Short(HIWORD(wp)) div ABS(Short(HIWORD(wp))) 
        );
      end else
        //* throw console window */
        PostMessageW(gConWnd, msg, wp, lp);
    end;

	  WM_SYSKEYDOWN,
	  WM_SYSKEYUP:
    // 알파 재설정
    if (msg = WM_SYSKEYDOWN) and (wp=VK_ADD) and BOOL(lp and AltMask) then
    begin
      ResetAlpha(hwnd, gTranspCtrlStep);
    end else
    if (msg = WM_SYSKEYDOWN) and (wp=VK_SUBTRACT) and BOOL(lp and AltMask) then
    begin
      ResetAlpha(hwnd, -gTranspCtrlStep);
    end else
    //* alt+enter */
		if (wp <> VK_RETURN) then
			PostMessageW(gConWnd, msg, wp, lp);

	  WM_KEYDOWN,
	  WM_KEYUP:
    if gUseCtrl_C_Copy and (msg = WM_KEYDOWN) and (wp=ord('C')) and BOOL(GetKeyState(VK_CONTROL) and $8000) then
    begin
      selectionToClipBoard(hwnd);
    end else
    if gUseCtrl_V_Paste and (msg = WM_KEYDOWN) and (wp=ord('V')) and BOOL(GetKeyState(VK_CONTROL) and $8000) then
    begin
      onPasteFromClipboard(hWnd);
    end else
    // 폰트 리사이즈.
    if (msg = WM_KEYDOWN) and (wp=VK_ADD) and BOOL(GetKeyState(VK_CONTROL) and $8000) then
    begin
      ResizeFont(hwnd, gFontSizeCtrlStep);
    end else
    if (msg = WM_KEYDOWN) and (wp=VK_SUBTRACT) and BOOL(GetKeyState(VK_CONTROL) and $8000) then
    begin
      ResizeFont(hwnd, -gFontSizeCtrlStep);
    end else

		if ((wp = VK_NEXT) or (wp = VK_PRIOR) or
  	    (wp = VK_HOME) or (wp = VK_END)) and
		   BOOL(GetKeyState(VK_SHIFT) and $8000) then
    begin
			if (msg = WM_KEYDOWN) then
      begin
				sb := SB_PAGEDOWN;
				if (wp = VK_PRIOR)    then sb := SB_PAGEUP
				else if(wp = VK_HOME) then sb := SB_TOP
				else if(wp = VK_END)  then sb := SB_BOTTOM;
				PostMessageW(gConWnd, WM_VSCROLL, sb, 0);
      end;
    end else
    if (msg = WM_KEYDOWN) and (wp = VK_INSERT) and
  		 BOOL(GetKeyState(VK_SHIFT) and $8000) then
    begin
			onPasteFromClipboard(hWnd);
    end else
    begin
			PostMessageW(gConWnd, msg, wp, lp);
    end;

  else
    Result := DefWindowProcW(hwnd, msg, wp, lp);
  end;
end;

//*****************************************************************************/

procedure SAFE_CloseHandle(var handle: THandle); forward;
procedure SAFE_DeleteObject(var handle: THandle); forward;

//*----------*/
function create_window(opt: TDkOpt): BOOL;
var
  hInstance: THandle;
  classname: WideString;
  conf_title: string;
  title: WideString;
  wc: WNDCLASSEXW;
	style, exstyle: DWORD;
  width, height, posx, posy: LongInt;
	csi: CONSOLE_SCREEN_BUFFER_INFO;
	rc: TRect;

  hwnd: THandle;
begin
	//trace("create_window\n");

	hInstance := GetModuleHandle(nil);
	className := 'DkwWindowClass';
	style := WS_OVERLAPPEDWINDOW;
	exstyle := WS_EX_ACCEPTFILES;

	if opt.isTranspColor or
	   ((0 < opt.getTransp) and (opt.getTransp < 255)) then
		exstyle :=  exstyle or WS_EX_LAYERED;
    //or WS_EX_COMPOSITED;

	if opt.isScrollRight then
		exstyle := exstyle or WS_EX_RIGHTSCROLLBAR
	else
		exstyle := exstyle or WS_EX_LEFTSCROLLBAR;

	if opt.isTopMost then
		exstyle := exstyle or WS_EX_TOPMOST;

	if opt.isScrollHide or (opt.getSaveLines < 1) then
		gVScrollHide := TRUE
	else
		style := style or WS_VSCROLL;

	if opt.isIconic then style := style or WS_MINIMIZE;

	conf_title := opt.getTitle;
  if Length(conf_title) = 0 then
    title := 'dkw'
  else
  begin
    title := conf_title;
    {
    title = new wchar_t[ strlen(conf_title)+1 ];
    ZeroMemory(title, sizeof(wchar_t) * (strlen(conf_title)+1));
    MultiByteToWideChar(CP_ACP, 0, conf_title, strlen(conf_title), title, sizeof(wchar_t) * (strlen(conf_title)+1));
    }
  end;

	//* calc window size */
	GetConsoleScreenBufferInfo(gStdOut, csi);

	AdjustWindowRectEx(gFrame, style, FALSE, exstyle);
	if not gVScrollHide then
		Inc(gFrame.right, GetSystemMetrics(SM_CXVSCROLL));

	gWinW := csi.srWindow.Right  - csi.srWindow.Left + 1;
  width := gWinW;
	gWinH := csi.srWindow.Bottom - csi.srWindow.Top  + 1;
  height := gWinH;
	width  := width * Integer(gFontW);
	height := height * Integer(gFontH);
	Inc(width, gBorderSize * 2);
	Inc(height, gBorderSize * 2);
	Inc(width, gFrame.right  - gFrame.left);
	Inc(height, gFrame.bottom - gFrame.top);

	if opt.isWinPos then
  begin
		SystemParametersInfo(SPI_GETWORKAREA,0, @rc,0);
		posx := opt.getWinPosX;
		if (posx < 0) then
      posx := rc.right - (width - posx -1)
		else
      Inc(posx, rc.left);
		if (posx < rc.left) then posx := rc.left;
		if (posx > rc.right-5) then posx := rc.right -5;
		posy := opt.getWinPosY;
		if(posy < 0) then
      posy := rc.bottom - (height - posy -1)
		else
      Inc(posy, rc.top);
		if(posy < rc.top) then posy := rc.top;
		if(posy > rc.bottom-5) then posy := rc.bottom -5;
  end else
  begin
		posx := Integer(CW_USEDEFAULT);
		posy := Integer(CW_USEDEFAULT);
  end;

	//**/
  FillChar(wc, SizeOf(wc), 0);
	wc.cbSize := sizeof(wc);
	wc.style := 0;
	wc.lpfnWndProc := @WndProc;
	wc.cbClsExtra := 0;
	wc.cbWndExtra := 0;
	wc.hInstance := hInstance;
	wc.hIcon := LoadIcon(hInstance, 'MAINICON');
	wc.hCursor := LoadCursor(0, IDC_ARROW);
	wc.hbrBackground := CreateSolidBrush(gColorTable[0]);
	wc.lpszMenuName := nil;
	wc.lpszClassName := PWideChar(className);
	wc.hIconSm := wc.hIcon;
	if not BOOL(RegisterClassExW(wc)) then
  begin
    Result := false;
    Exit;
  end;

	hWnd :=
    CreateWindowExW(exstyle, PWideChar(className), PWideChar(title),
      style, posx, posy, width, height,
			0, 0, hInstance, nil
    );
	if not BOOL(hWnd) then
  begin
		Result := false;
    Exit;
  end;

	sysmenu_init(hWnd);

	if (0 < opt.getTransp) and (opt.getTransp < 255) then
		SetLayeredWindowAttributes(hWnd, 0, opt.getTransp, LWA_ALPHA)
	else
  if opt.isTranspColor then
		SetLayeredWindowAttributes(hWnd, opt.getTranspColor, 255, LWA_COLORKEY);

	ShowWindow(hWnd, SW_SHOW);
  Result := true;
end;

//*----------*/
// todo: 유니코드 기준으로 변경할 것. 
function create_child_process(cmd, curdir: string): BOOL;
var
  buf: PChar;
	pi: PROCESS_INFORMATION;
	si: STARTUPINFOA ;

  Dir: PAnsiChar;
begin
	//trace("create_child_process\n");

  Result := false;
  if Length(cmd) = 0 then
  begin
		GetMem(buf, 32768);
		buf[0] := #0;
		if not BOOL(GetEnvironmentVariableA('COMSPEC', buf, 32768)) then
      StrPCopy(buf, 'cmd.exe');
  end else
  begin
		GetMem(buf, Length(cmd) + 1);
		StrPCopy(buf, cmd);
	end;

	FillChar(si, sizeof(si), 0);
	si.cb := sizeof(si);
	si.dwFlags := STARTF_USESTDHANDLES;
	si.hStdInput  := gStdIn;
	si.hStdOutput := gStdOut;
	si.hStdError  := gStdErr;

  if Length(curdir) = 0 then
    Dir := nil
  else
    Dir := PAnsiChar(curdir);
    
	if not CreateProcessA(nil, buf, nil, nil, TRUE,
			    0, nil, Dir, si, pi) then
  begin
		FreeMem(buf);
    Exit;
  end;

	FreeMem(buf);
	CloseHandle(pi.hThread);
	gChild := pi.hProcess;
  Result := true;
end;

//*----------*/
function create_font(name: string; height: Integer): BOOL;
var
  hDC, oldfont: THandle;
  met: TEXTMETRIC;
  width1: array[0..26-1] of Integer;
  width2: array[0..26-1] of Integer;
  i, width: Integer;

begin
	//trace("create_font\n");

  //name := 'FixedSys';

	FillChar(gFontLog, sizeof(gFontLog), 0);
	gFontLog.lfHeight := -height;
	gFontLog.lfWidth := 0;
	gFontLog.lfEscapement := 0;
	gFontLog.lfOrientation := 0;
	gFontLog.lfWeight := FW_NORMAL;
	gFontLog.lfItalic := 0;
	gFontLog.lfUnderline := 0;
	gFontLog.lfStrikeOut := 0;
	gFontLog.lfCharSet := DEFAULT_CHARSET;
	gFontLog.lfOutPrecision := OUT_DEFAULT_PRECIS;
	gFontLog.lfClipPrecision := CLIP_DEFAULT_PRECIS;
	gFontLog.lfQuality := DEFAULT_QUALITY;
	gFontLog.lfPitchAndFamily := FIXED_PITCH or FF_DONTCARE;

  StrPCopy(@gFontLog.lfFaceName[0], name);

	//if(name) {
		//MultiByteToWideChar(CP_ACP,0, name, -1, gFontLog.lfFaceName, LF_FACESIZE);
	//}

	SAFE_DeleteObject(gFont);
	gFont := CreateFontIndirect(gFontLog);

	//* calc font size */
	hDC := GetDC(0);
	oldfont := SelectObject(hDC, gFont);
  width := 0;

	GetTextMetrics(hDC, met);
	GetCharWidth32(hDC, $41, $5A, width1);
	GetCharWidth32(hDC, $61, $7A, width2);
	SelectObject(hDC, oldfont);
	ReleaseDC(0, hDC);

	for i:= 0 to 26-1 do
  begin
		Inc(width, width1[i]);
		Inc(width, width2[i]);
  end;

	width  := Width div (26 * 2);
	gFontW := width; //* met.tmAveCharWidth; */
	gFontH := DWORD(met.tmHeight) + gLineSpace;

	Result := TRUE;
end;

//*----------*/
// JCL 의 Hardlinks.pas 에서 "NtpGetProcessHeap" 함수를 참고함. 
// http://www.koders.com/delphi/fid007F6DDAB2DC02A8F2B83EF53A28A90F7E03C6B2.aspx?s=thread
// prupp 구조체는 ntapi.h 파일을 참조할 것.
function Get_prupp(): Pointer;
asm
  // get PEB
  MOV EAX, FS:[$30]
  // get PRUPP (PRTL_USER_PROCESS_PARAMETERS)
  MOV EAX, [EAX+$10]
end;

procedure __hide_alloc_console();
var
  param: PInteger;
  pflags: PDWORD;
  pshow: PWORD;
  backup_flags: DWORD;
  backup_show : WORD;
	si: STARTUPINFO;
begin
	{*
	 * Open Console Window
	 * hack StartupInfo.wShowWindow flag
	 *}

  {
    윈도 비스타와 7에서 제대로 동작하도록 수정.
  }

	//pflags := PDWORD($00020068); //* private memory */
	//pshow  :=  PWORD($0002006C);

  param := Get_prupp();
  pflags := @PByteArray(param)^[$68];
  pshow  := @PByteArray(param)^[$6C];

	backup_flags := pflags^;
	backup_show  := pshow^;

	GetStartupInfo(si);

  //* check */
	if (si.dwFlags = backup_flags) and (si.wShowWindow = backup_show) then
  begin
		pflags^ := pflags^ or STARTF_USESHOWWINDOW;
		pshow^  := SW_HIDE;
	end;

	AllocConsole();

	///* restore */
	pflags^ := backup_flags;
	pshow^  := backup_show;
end;


//*----------*/
function sig_handler(n: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
end;

function create_console(opt: TDkOpt): BOOL;
var
  conf_title: string;
  title: WideString;
	sa: SECURITY_ATTRIBUTES;

	font: Array [0..MAX_FONTS-1] of CONSOLE_FONT;
  fonts: DWORD;
  minimalFont: CONSOLE_FONT;
  i : Integer;

	size: COORD;
	sr: SMALL_RECT;
begin
	conf_title := opt.getTitle;
  if Length(conf_title) = 0 then
    title := 'dkw'
  else
  begin
    title := conf_title;
    {
    title = new wchar_t[ strlen(conf_title)+1 ];
    ZeroMemory(title, sizeof(wchar_t) * (strlen(conf_title)+1));
    MultiByteToWideChar(CP_ACP, 0, conf_title, strlen(conf_title), title, sizeof(wchar_t) * (strlen(conf_title)+1));
    }
  end;

{$IFDEF HideConsole}
	__hide_alloc_console();
{$ELSE}
  AllocConsole();
{$ENDIF}

  gConWnd := GetConsoleWindow();
	while (gConWnd = 0) do
  begin
		Sleep(10);
    gConWnd := GetConsoleWindow();
  end;

{$IFDEF HideConsole}
	while IsWindowVisible(gConWnd) do
  begin
		ShowWindow(gConWnd, SW_HIDE);
		Sleep(10);
  end;
{$ENDIF}  

	SetConsoleTitleW(PWideChar(title));
  //SetConsoleTitleW('abcde');
  //SetConsoleTitle('abcde');

	SetConsoleCtrlHandler(@sig_handler, TRUE);

	sa.nLength := sizeof(sa);
	sa.lpSecurityDescriptor := nil;
	sa.bInheritHandle := TRUE;

	gStdIn := CreateFile('CONIN$',  GENERIC_READ or GENERIC_WRITE,
			     FILE_SHARE_READ or FILE_SHARE_WRITE,
			     @sa, OPEN_EXISTING, 0, 0);
	gStdOut := CreateFile('CONOUT$',  GENERIC_READ or GENERIC_WRITE,
			     FILE_SHARE_READ or FILE_SHARE_WRITE,
			     @sa, OPEN_EXISTING, 0, 0);
	gStdErr := CreateFile('CONOUT$',  GENERIC_READ or GENERIC_WRITE,
			     FILE_SHARE_READ or FILE_SHARE_WRITE,
			     @sa, OPEN_EXISTING, 0, 0);

	if (not BOOL(gConWnd)) or (not BOOL(gStdIn)) or (not BOOL(gStdOut)) or (not BOOL(gStdErr)) then
  begin
    Result := false;
    Exit;
  end;


  fonts := GetNumberOfConsoleFonts();
  if (fonts > MAX_FONTS) then fonts := MAX_FONTS;

  GetConsoleFontInfo(gStdOut, FALSE, fonts, @font[0]);
  minimalFont.index := 0;
  minimalFont.dim.X := 0;
  minimalFont.dim.Y := 0;

  for i := 0 to fonts - 1 do
    if (minimalFont.dim.X < font[i].dim.X) and (minimalFont.dim.Y < font[i].dim.Y) then
      minimalFont := font[i];

{$IFDEF HideConsole}
  SetConsoleFont(gStdOut, minimalFont.index);
{$ENDIF}  
  

	//* set buffer & window size */
  FillChar(sr, SizeOf(SMALL_RECT), 0);
	SetConsoleWindowInfo(gStdOut, TRUE, sr);
	size.X := opt.getWinCharW;
	size.Y := opt.getWinCharH + opt.getSaveLines;
	SetConsoleScreenBufferSize(gStdOut, size);
	sr.Left := 0;
	sr.Right := opt.getWinCharW-1;
	sr.Top := size.Y - opt.getWinCharH;
	sr.Bottom := size.Y-1;
	SetConsoleWindowInfo(gStdOut, TRUE, sr);
	size.X := sr.Left;
	size.Y := sr.Top;
	SetConsoleCursorPosition(gStdOut, size);
  Result := true;
end;


//*----------*/
function init_options(opt: TDkOpt): BOOL;
var
  _result: BOOL;
  i: Integer;
begin
	//* create argv */

  Result := false;

	opt.loadXdefaults();
	_result := opt.setArgs;

  if not _result then Exit;

	//* set */
	for i := kColor0 to kColor15 do
		gColorTable[i] := opt.getColor(i);
	gColorTable[kColor7] := opt.getColorFg;
	gColorTable[kColor0] := opt.getColorBg;

	gColorTable[kColorCursorBg] := opt.getColorCursor;
	gColorTable[kColorCursorFg] := (not gColorTable[kColorCursorBg]) and $FFFFFF;
	gColorTable[kColorCursorImeBg] := opt.getColorCursorIme;
	gColorTable[kColorCursorImeFg] := (not gColorTable[kColorCursorImeBg]) and $FFFFFF;

	gBorderSize := opt.getBorderSize;
	gLineSpace := opt.getLineSpace;

  gFontSizeCtrl     := opt.getFontSizeCtrl;
  gFontSizeCtrlStep := opt.getFontSizeCtrlStep;

  gIsTranspColor  := opt.isTranspColor;
  gTransp         := opt.getTransp;
  gTranspCtrl     := opt.getTranspCtrl;
  gTranspCtrlStep := opt.getTranspCtrlStep;

  gUseCtrl_C_Copy   := opt.getUseCtrl_C_Copy;
  gUseCtrl_V_Paste  := opt.getUseCtrl_V_Paste;
  gSelectOverScreen := opt.getSelectOverScreen;

	if Length(opt.getBgBmp) <> 0 then
		gBgBmp := LoadImageA(0, PAnsiChar(opt.getBgBmp), IMAGE_BITMAP, 0,0, LR_LOADFROMFILE);

	if BOOL(gBgBmp) then gBgBrush := CreatePatternBrush(gBgBmp);
	if not BOOL(gBgBrush) then gBgBrush := CreateSolidBrush(gColorTable[0]);

	Result := true;
end;


//*----------*/
function initialize(): BOOL;
var
	opt: TDkOpt;

begin
  Result := false;

	opt:= TDkOpt.Create;
  try

    if not init_options(opt) then Exit;
    if not create_console(opt) then Exit;
    if not create_font(opt.getFont, opt.getFontSize) then Exit;
    if not create_child_process(opt.getCmd, opt.getCurDir) then Exit; 
    if not create_window(opt) then Exit;
    
  finally
    opt.Free;
  end;

  Result := true;
end;

//*----------*/

procedure SAFE_CloseHandle(var handle: THandle);
begin
	if BOOL(handle) then
  begin
    CloseHandle(handle);
    handle := 0;
  end;
end;

procedure SAFE_DeleteObject(var handle: THandle);
begin
	if BOOL(handle) then
  begin
    DeleteObject(handle);
    handle := 0;
  end;
end;

procedure _terminate();
begin
	if Assigned(gTitle) then
  begin
		FreeMem(gTitle);
		gTitle := nil;
  end;

	if Assigned(gScreen) then
  begin
		FreeMem(gScreen);
		gScreen := nil;
  end;

	if Assigned(gCSI) then
  begin
		FreeMem(gCSI);
		gCSI := nil;
  end;
	gConWnd := 0;
	SAFE_CloseHandle(gStdIn);
	SAFE_CloseHandle(gStdOut);
	SAFE_CloseHandle(gStdErr);
	SAFE_CloseHandle(gChild);
	SAFE_DeleteObject(gFont);
	SAFE_DeleteObject(gBgBrush);
	SAFE_DeleteObject(gBgBmp);

end;


//*----------*/
procedure WinMain();
var
  msg : TMsg;
begin
	if initialize() then
  begin
		while (GetMessage(msg, 0, 0,0)) do
    begin
			TranslateMessage(msg);
			DispatchMessage(msg);
    end;
	end;
	_terminate();

end;


initialization
  dkw_h.ReadConsoleOutput_Unicode := ReadConsoleOutput_Unicode;

finalization

end.






{-----------------------------------------------------------------------------
 Unit Name: misc
 Author:    oranke_f
 Date:      2010-07-02
 Purpose:
  misc.cpp 파일 번역.

 History:
  2010-07-02
    작업 시작.

  2010-07-08
    CopyAll 기능 추가.
    ckw-0.8.10-mod4-20100517 에서 참고함.
    http://sites.google.com/site/craftware/ckw/download

-----------------------------------------------------------------------------}


unit misc;

interface

uses
  Windows, Messages, ShellAPI;
  
implementation

uses
  dkw_h;

//-- from rsrc.h
const
  //IDR_ICON	101
  IDD_DIALOG1	= 105;
  IDC_STATIC1	= 10001;
  IDC_EDIT1	= 10002;

  IDM_ABOUT	  = 40001;
  IDM_COPYALL	= 40002;


procedure __write_console_input(str: PWideChar; length: DWORD);
var
  p, buf: PInputRecord;
  i: DWORD;
begin
  if not Assigned(str) then Exit;
  if length = 0 then Exit;

  GetMem(buf, SizeOf(INPUT_RECORD) * length);
	p := buf;
  i := 0;

  while i < length do
  begin
		p^.EventType := KEY_EVENT;
 		p^.Event.KeyEvent.bKeyDown := TRUE;
		p^.Event.KeyEvent.wRepeatCount := 1;
		p^.Event.KeyEvent.wVirtualKeyCode := 0;
		p^.Event.KeyEvent.wVirtualScanCode := 0;
		p^.Event.KeyEvent.UnicodeChar := str^;
		p^.Event.KeyEvent.dwControlKeyState := 0;

    Inc(str);
    Inc(i);
    Inc(p);
  end;

	WriteConsoleInputW(gStdIn, buf^, length, length);

  FreeMem(buf);
end;

//*----------*/
procedure onPasteFromClipboard(hWnd: HWND);
var
  _result: BOOL;
  hMem: THandle;
  ptr: PWideChar;
begin
  _result := true;

	if not IsClipboardFormatAvailable(CF_UNICODETEXT) then Exit;

	if not OpenClipboard(hWnd) then
  begin
		Sleep(10);
		if not OpenClipboard(hWnd) then Exit;
  end;
  
	hMem := GetClipboardData(CF_UNICODETEXT);
	if not BOOL(hMem) then _result := false;

  ptr := GlobalLock(hMem);
	if _result and (not Assigned(ptr)) then _result := false;

	if _result then
  begin
		__write_console_input(ptr, length(ptr));
		GlobalUnlock(hMem);
  end;
	CloseClipboard();
  
end;



//*----------*/
procedure onDropFile(hDrop: THandle);
var
	i, nb, len: DWORD;
  wbuf: array [0..MAX_PATH+32-1] of WideChar;
  wp: PWideChar;
begin
	nb := DragQueryFileW(hDrop, DWORD(-1), nil, 0);

  for i := 0 to nb - 1 do
  begin
		len := DragQueryFileW(hDrop, i, nil, 0);
		if (len < 1) or (len > MAX_PATH) then continue;

		wp := @wbuf[1];
		if not BOOL(DragQueryFileW(hDrop, i, wp, MAX_PATH)) then continue;

		wp[len] := #0;

		while ( Ord(wp^) > $20) do Inc(wp);

		if BOOL(wp^) then
    begin
			wp := wbuf;
      Inc(len);
      wp[len] := '"';
      wp[0] := wp[len];
      Inc(len);
    end else
    begin
			wp := @wbuf[1];
    end;

		wp[len] := ' ';
    Inc(len);

		__write_console_input(wp, len);
  end;

	DragFinish(hDrop);
end;


//*----------*/
function AboutDlgProc(hWnd: HWND; msg: UINT; wp: WPARAM; lp: LPARAM): BOOL; stdcall;
var
  hEdit: THandle;
begin
  Result := false;
  case msg of
    WM_INITDIALOG:
    begin
      hEdit := GetDlgItem(hWnd, IDC_EDIT1);
      SetWindowTextW(hEdit,
        'This program is free software; you can redistribute it and/or'#13#10+
        'modify it under the terms of the GNU General Public License'#13#10+
        'as published by the Free Software Foundation; either version 2'#13#10+
        'of the License, or (at your option) any later version.'#13#10+
        ''#13#10+
        'This program is distributed in the hope that it will be useful,'#13#10+
        'but WITHOUT ANY WARRANTY; without even the implied warranty of'#13#10+
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'#13#10+
        'See the GNU General Public License for more details.'#13#10+
        ''#13#10+
        'You should have received a copy of the GNU General Public License'#13#10+
        'along with this program; if not, write to the Free Software Foundation, Inc.,'#13#10+
        ' 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.'
      );

      Result := TRUE;
    end;

    WM_COMMAND:
    begin
      //WriteLn(LOWORD(wp));
      case LOWORD(wp) of
        IDOK, 
        IDCANCEL:
        begin
          EndDialog(hwnd, 0);
        
          Result := TRUE;
        end;
      end;

    end;
  end;
end;

//*----------*/
procedure sysmenu_init(hWnd: HWND);
var
	mii: MENUITEMINFO;
	hMenu : THandle;
  MenuStr: String;
begin
	hMenu := GetSystemMenu(hWnd, FALSE);

  FillChar(mii, SizeOf(MENUITEMINFO), 0);

  MenuStr := '&Copy All';
	mii.cbSize := sizeof(mii);
	mii.fMask := MIIM_TYPE or MIIM_ID;
	mii.fType := MFT_STRING;
	mii.wID := IDM_COPYALL;
	mii.dwTypeData := PChar(MenuStr);
	mii.cch := Length(MenuStr);
	InsertMenuItem(hMenu, SC_CLOSE, FALSE, mii);

	mii.fType := MFT_SEPARATOR;
	mii.wID := 0;
	mii.dwTypeData := nil;
	mii.cch := 0;
	InsertMenuItem(hMenu, SC_CLOSE, FALSE, mii);

  MenuStr := '&About';
	mii.cbSize := sizeof(mii);
	mii.fMask := MIIM_TYPE or MIIM_ID;
	mii.fType := MFT_STRING;
	mii.wID := IDM_ABOUT;
	mii.dwTypeData := PChar(MenuStr);
	mii.cch := Length(MenuStr);
	InsertMenuItem(hMenu, SC_CLOSE, FALSE, mii);

	mii.fType := MFT_SEPARATOR;
	mii.wID := 0;
	mii.dwTypeData := nil;
	mii.cch := 0;
	InsertMenuItem(hMenu, SC_CLOSE, FALSE, mii);
  
end;


//*----------*/
function onSysCommand(hWnd: HWND; id: DWORD): BOOL;
begin
  Result := false;
  case id of
    IDM_COPYALL:
    begin
      copyAllStringToClipBoard(hwnd);
    end;
    
    IDM_ABOUT:
    begin
  		DialogBoxW(
        GetModuleHandle(nil),
        'IDD_DIALOG1',
			  hWnd,
			  @AboutDlgProc
      );

      Result := true;
    end;  
  end;
end;


initialization
  dkw_h.onPasteFromClipboard:= onPasteFromClipboard;
  dkw_h.onDropFile:= onDropFile;
  dkw_h.sysmenu_init:= sysmenu_init;
  dkw_h.onSysCommand:= onSysCommand;



finalization

end.

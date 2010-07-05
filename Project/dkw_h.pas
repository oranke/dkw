{-----------------------------------------------------------------------------
 Unit Name: dkw_h
 Author:    oranke_f
 Date:      2010-07-01
 Purpose:
  ckw.h 파일의 포팅.
  델파이 구조상 모든 정보를 다 옮길 수는 없지만 비슷하게...
  함수 포인터를 두고 다른 유니트에서 매핑해보자. 


 History:
  2010-07-01
    작업 시작.
    
-----------------------------------------------------------------------------}



unit dkw_h;

interface

uses
  Windows;

const
  COMMON_LVB_LEADING_BYTE  = $0100;
  COMMON_LVB_TRAILING_BYTE = $0200;

function CSI_WndCols(csi: PConsoleScreenBufferInfo): Integer;
function CSI_WndRows(csi: PConsoleScreenBufferInfo): Integer;

//* main.cpp */
var
  gStdIn      : THandle = 0;
  gStdOut     : THandle = 0;
  gFontW      : DWORD;
  gFontH      : DWORD;
  gBorderSize : DWORD;
  gCSI        : PConsoleScreenBufferInfo;
  gScreen     : PCharInfo;

  ReadConsoleOutput_Unicode: function (con: THandle; buffer: PCharInfo;
	  size, pos: COORD; sr: PSmallRect): BOOL; stdcall;

//* selection.cpp */
  selectionGetArea: function (var sr: SMALL_RECT): BOOL;
  selectionClear: procedure (hWnd: HWND);
  onLBtnDown: procedure (hWnd: HWND; x, y: Integer);
  onLBtnUp: procedure (hWnd: HWND; x, y: Integer);
  onMouseMove: procedure (hWnd: HWND; x, y: Integer);

//* misc.cpp */
  onPasteFromClipboard: procedure (hWnd: HWND);
  onDropFile: procedure (hDrop: THandle);
  sysmenu_init: procedure (hWnd: HWND);
  onSysCommand: function (hWnd: HWND; id: DWORD): BOOL;

implementation

function CSI_WndCols(csi: PConsoleScreenBufferInfo): Integer;
begin
  Result := csi^.srWindow.Right - csi^.srWindow.Left +1;
end;

function CSI_WndRows(csi: PConsoleScreenBufferInfo): Integer;
begin
  Result := csi^.srWindow.Bottom - csi^.srWindow.Top +1;
end;

initialization

finalization

end.

{-----------------------------------------------------------------------------
 Unit Name: option
 Author:    oranke_f
 Date:      2010-07-02
 Purpose:
    option.h, option.cpp 파일의 번역.
    ckOpt 클래스를 번역한 TDkOpt 객체 구현.

    실행파일명과 동일하면서 확장자가 opt 인 파일에서 정보 읽음.

 History:
  2010-07-02
    제작 시작.

-----------------------------------------------------------------------------}


unit option;

interface

uses
  Windows, Messages, SysUtils, Classes;//, Graphics;

type
  TDkOpt = class
  private
    fIsWinPos: bool;
    fWinPosX: integer;
    fWinPosY: integer;
    fWinCharW: integer;
    fWinCharH: integer;
    fIsIconic: bool;
    fColorBg: COLORREF;
    fColorFg: COLORREF;
    fColorCursor: COLORREF;
    fColorCursorIme: COLORREF;
    fColors: array[0..17-1] of COLORREF;
    fLineSpace: integer;
    fBorderSize: integer;
    fScrollRight: bool;
    fTransp: integer;
    fSaveLines: integer;
    fIsTopMost: bool;
    fTranspColor: COLORREF;
    fIsTranspColor: bool;
    fScrollHide: bool;
    fFontSize: Integer;
    fFont: String;
    fCmd: String;
    fTitle: string;
    fBgBmp: String;
    fCurDir: string;
  protected
  public
    constructor Create;
    destructor Destroy; override;

  	procedure loadXdefaults();
    // argc, argv를 인자로 하는 set 함수 변경.
  	function setArgs(): BOOL;

  public
	  property isWinPos	  :	bool	read fIsWinPos;
  	property getWinPosX	:	integer	read fWinPosX;
	  property getWinPosY	:	integer	read fWinPosY;
    property getWinCharW:	integer	read fWinCharW;
    property getWinCharH:	integer	read fWinCharH;
    property isIconic		: bool read fIsIconic;
    property getColorFg		    : COLORREF read	fColorFg;{ return(m_colorFg); }
    property getColorBg		    : COLORREF read	fColorBg;{ return(m_colorBg); }
    property getColorCursor	  : COLORREF read	fColorCursor;{ return(m_colorCursor); }
    property getColorCursorIme: COLORREF read	fColorCursorIme;{ return(m_colorCursorIme); }
    function getColor(i: Integer) : COLORREF;

    property isScrollHide		: bool		read fScrollHide;
    property isScrollRight	: bool		read fScrollRight;
    property getSaveLines		: integer	read fSaveLines;
    property getBorderSize	: integer	read fBorderSize;
    property getLineSpace		: integer	read fLineSpace;
    property getTransp		  : integer	read fTransp;
    property isTranspColor	: bool		read fIsTranspColor;
    property getTranspColor	: COLORREF read fTranspColor;
    property isTopMost		  : bool		read fIsTopMost;

    property getCmd: String read fCmd;
  	property getFontSize: Integer read fFontSize;
    property getFont: String read fFont;
   

    property getBgBmp: String read fBgBmp;
  	property getCurDir: string read fCurDir;
  	property getTitle: string read fTitle;
  end;

implementation

{ TDkOpt }

constructor TDkOpt.Create;
begin
	fisWinPos := false;
	fwinPosX := 0;
	fwinPosY := 0;
	fwinCharW := 80;
	fwinCharH := 24;
	fisIconic := false;
	ffontSize := 14;
	fcolors[0]  := RGB($00, $00, $01);
	fcolors[1]  := RGB($00, $00, $80);
	fcolors[2]  := RGB($00, $80, $00);
	fcolors[3]  := RGB($00, $80, $80);
	fcolors[4]  := RGB($80, $00, $00);
	fcolors[5]  := RGB($80, $00, $80);
	fcolors[6]  := RGB($80, $80, $00);
	fcolors[7]  := RGB($C0, $C0, $C0);
	fcolors[8]  := RGB($80, $80, $80);
	fcolors[9]  := RGB($00, $00, $FF);
	fcolors[10] := RGB($00, $FF, $00);
	fcolors[11] := RGB($00, $FF, $FF);
	fcolors[12] := RGB($FF, $00, $00);
	fcolors[13] := RGB($FF, $00, $FF);
	fcolors[14] := RGB($FF, $FF, $00);
	fcolors[15] := RGB($FF, $FF, $FF);
	fcolorFg    := RGB($C0, $C0, $C0); // cursor fg
	fcolorBg    := RGB($00, $00, $01); // cursor bg
	fcolorCursor    := RGB($C0, $C0, $80);
	fcolorCursorIme := RGB($C0, $00, $00);
	fscrollHide := false;
	fscrollRight := true;
	fsaveLines := 500;
	fborderSize := 1;
	flineSpace := 0;
	ftransp := 255;
	fisTranspColor := false;
	ftranspColor := 0;
	fisTopMost := false;
end;

destructor TDkOpt.Destroy;
begin

  inherited;
end;


procedure TDkOpt.loadXdefaults;
begin

end;

function TDkOpt.setArgs: BOOL;
begin
  Result := true;
end;

function TDkOpt.getColor(i: Integer): COLORREF;
begin
  if (0 <= i) and (i <= 15) then
    Result := fColors[i]
  else
    Result := fColors[0];
end;



initialization

finalization

end.


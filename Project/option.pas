{-----------------------------------------------------------------------------
 Unit Name: option
 Author:    oranke_f
 Date:      2010-07-02
 Purpose:
    option.h, option.cpp 파일의 번역.
    ckOpt 클래스를 번역한 TDkOpt 객체 구현.

    실행파일명과 동일하면서 확장자가 opt 인 파일에서 정보 읽음.
    --> 그냥 INI를 사용하는 게 낫겠다. 

 History:
  2010-07-02
    제작 시작. 몸통에서 쓰일 수 있도록 기본 리턴값만 구현.

  2010-07-07
    Yes, No 값을 Boolean형으로 입출력하는 TMyINIFile 구현.   

  2010-07-08
    모든 INI설정값을 파라미터로 받을 수 있도록 함.   

  2010-07-12
    투명도 조절관련 옵션 추가.
      
-----------------------------------------------------------------------------}

{.$DEFINE WriteINI}

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
    fScrollHide: bool;
    fTransp: integer;
    fSaveLines: integer;
    fIsTopMost: bool;
    fTranspColor: COLORREF;
    fTranspCtrl: bool;
    fTranspCtrlStep: Integer;
    fIsTranspColor: bool;
    fFontSize: Integer;
    fFont: String;
    fCmd: String;
    fTitle: string;
    fBgBmp: String;
    fCurDir: string;
    fFontSizeCtrl: BOOL;
    fFontSizeCtrlStep: Integer;
    fUseCtrl_C_Copy: BOOL;
    fUseCtrl_V_Paste: BOOL;
    fSelectOverScreen: BOOL;
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;

  	procedure loadXdefaults();
{$IFDEF WriteINI}
    procedure saveXdefaults();
{$ENDIF}
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
    property getTranspCtrl  : bool read fTranspCtrl;
    property getTranspCtrlStep: Integer read fTranspCtrlStep;

    property isTopMost		  : bool		read fIsTopMost;

    property getCmd: String read fCmd;
  	property getFontSize: Integer read fFontSize;
    property getFont: String read fFont;


    property getBgBmp: String read fBgBmp;
  	property getCurDir: string read fCurDir;
  	property getTitle: string read fTitle;

    property getFontSizeCtrl: bool read fFontSizeCtrl;
    property getFontSizeCtrlStep: Integer read fFontSizeCtrlStep;
    property getUseCtrl_C_Copy: bool read fUseCtrl_C_Copy;
    property getUseCtrl_V_Paste: bool read fUseCtrl_V_Paste;
    property getSelectOverScreen: bool read fSelectOverScreen;

  end;

procedure WriteFontSize(const aFontSize: Integer);

implementation

uses
  {Dialogs, }INIFiles;

  
// http://www.cs.yorku.ca/~oz/hash.html 의 sdbm 방식.
// 가장 효율이 좋다고 알려짐. 실제 테스트 해 본 결과는 썩 괜찮군..
function MemHash3(const aMem: Pointer; aSize: Integer): LongWord;
var
  i: Integer;
  p: pByteArray;
begin
  Result := 0;
  p := aMem;
  for i:= 0 to aSize-1 do
    Result := p[i] + (Result shl 6) + (Result shl 16) - Result;
end;

function ExtractJustName(const FileName: String): String;
begin
  Result := ExtractFileName(FileName);
  SetLength(Result, Length(Result) - Length(ExtractFileExt(FileName)));
end;

function SwapColor(aColor: COLORREF): COLORREF; assembler;
asm
  MOV     ECX,EAX
  SHR     EAX,16
  XCHG    AL,CL
  SHL     EAX,16
  MOV     AX, CX
end;

function LookupColor(const aColorName: String; var aColor: COLORREF): BOOL;
var
  NameLen: Integer;
  NameHash: LongWord;
  //R, G, B: Byte;
begin
  Result := false;

  NameLen := Length(aColorName);
  if (NameLen = 7) and (aColorName[1] = '#') then
  begin
    {
    R := StrToIntDef('$' + Copy(aColorName, 2, 2), 0);
    G := StrToIntDef('$' + Copy(aColorName, 4, 2), 0);
    B := StrToIntDef('$' + Copy(aColorName, 6, 2), 0);
    aColor := RGB(R, G, B);
    }
    aColor := SwapColor(StrToIntDef('$' + Copy(aColorName, 2, 6), 0));
    Result := true;
  end else
  begin
    // 소문자로 바꾼 이름캐시값 구하기.
    NameHash := MemHash3(PChar(LowerCase(aColorName)), Length(aColorName));
    // 각 해시에 해당하는 컬러값 리턴. 
    case NameHash of
      $37E5B43A: aColor := $FFF8F0; // "alice blue"
      $047E755A: aColor := $FFF8F0; // "AliceBlue"
      $02FBA128: aColor := $D7EBFA; // "antique white"
      $47B81ECA: aColor := $D7EBFA; // "AntiqueWhite"
      $C51993E7: aColor := $DBEFFF; // "AntiqueWhite1"
      $C51993E8: aColor := $CCDFEE; // "AntiqueWhite2"
      $C51993E9: aColor := $B0C0CD; // "AntiqueWhite3"
      $C51993EA: aColor := $78838B; // "AntiqueWhite4"
      $2737E71E: aColor := $D4FF7F; // "aquamarine"
      $8DDFE093: aColor := $D4FF7F; // "aquamarine1"
      $8DDFE094: aColor := $C6EE76; // "aquamarine2"
      $8DDFE095: aColor := $AACD66; // "aquamarine3"
      $8DDFE096: aColor := $748B45; // "aquamarine4"
      $22DEACCF: aColor := $FFFFF0; // "azure"
      $419B8722: aColor := $FFFFF0; // "azure1"
      $419B8723: aColor := $EEEEE0; // "azure2"
      $419B8724: aColor := $CDCDC1; // "azure3"
      $419B8725: aColor := $8B8B83; // "azure4"
      $8FC82F64: aColor := $DCF5F5; // "beige"
      $7799EC75: aColor := $C4E4FF; // "bisque"
      $5B5630FC: aColor := $C4E4FF; // "bisque1"
      $5B5630FD: aColor := $B7D5EE; // "bisque2"
      $5B5630FE: aColor := $9EB7CD; // "bisque3"
      $5B5630FF: aColor := $6B7D8B; // "bisque4"
      $D183679F: aColor := $010000; // "black"
      $EAB72E08: aColor := $CDEBFF; // "blanched almond"
      $965DDEDE: aColor := $CDEBFF; // "BlanchedAlmond"
      $053F90BA: aColor := $FF0000; // "blue"
      $23A555E5: aColor := $E22B8A; // "blue violet"
      $DB5E9DF7: aColor := $FF0000; // "blue1"
      $DB5E9DF8: aColor := $EE0000; // "blue2"
      $DB5E9DF9: aColor := $CD0000; // "blue3"
      $DB5E9DFA: aColor := $8B0000; // "blue4"
      $6E88CAB9: aColor := $E22B8A; // "BlueViolet"
      $EFA52A16: aColor := $2A2AA5; // "brown"
      $23BB5B9B: aColor := $4040FF; // "brown1"  
      $23BB5B9C: aColor := $3B3BEE; // "brown2"  
      $23BB5B9D: aColor := $3333CD; // "brown3"  
      $23BB5B9E: aColor := $23238B; // "brown4"  
      $0343F999: aColor := $87B8DE; // "burlywood"  
      $C7536CD8: aColor := $9BD3FF; // "burlywood1"  
      $C7536CD9: aColor := $91C5EE; // "burlywood2"  
      $C7536CDA: aColor := $7DAACD; // "burlywood3"  
      $C7536CDB: aColor := $55738B; // "burlywood4"  
      $520F25A5: aColor := $A09E5F; // "cadet blue"  
      $A14AF92F: aColor := $A09E5F; // "CadetBlue"
      $AAA252C2: aColor := $FFF598; // "CadetBlue1"  
      $AAA252C3: aColor := $EEE58E; // "CadetBlue2"  
      $AAA252C4: aColor := $CDC57A; // "CadetBlue3"  
      $AAA252C5: aColor := $8B8653; // "CadetBlue4"
      $6073CA96: aColor := $00FF7F; // "chartreuse"  
      $8714DB1B: aColor := $00FF7F; // "chartreuse1"
      $8714DB1C: aColor := $00EE76; // "chartreuse2"  
      $8714DB1D: aColor := $00CD66; // "chartreuse3"  
      $8714DB1E: aColor := $008B45; // "chartreuse4"  
      $9680F41C: aColor := $1E69D2; // "chocolate"  
      $FDD81315: aColor := $247FFF; // "chocolate1"  
      $FDD81316: aColor := $2176EE; // "chocolate2"  
      $FDD81317: aColor := $1D66CD; // "chocolate3"
      $FDD81318: aColor := $13458B; // "chocolate4"  
      $A9613FF1: aColor := $507FFF; // "coral"  
      $EEDFBC80: aColor := $5672FF; // "coral1"  
      $EEDFBC81: aColor := $506AEE; // "coral2"  
      $EEDFBC82: aColor := $455BCD; // "coral3"  
      $EEDFBC83: aColor := $2F3E8B; // "coral4"  
      $819AA4B7: aColor := $ED9564; // "cornflower blue"
      $E094959D: aColor := $ED9564; // "CornflowerBlue"
      $167983FD: aColor := $DCF8FF; // "cornsilk"  
      $0BE47B74: aColor := $DCF8FF; // "cornsilk1"  
      $0BE47B75: aColor := $CDE8EE; // "cornsilk2"  
      $0BE47B76: aColor := $B1C8CD; // "cornsilk3"  
      $0BE47B77: aColor := $78888B; // "cornsilk4"
      $3A192623: aColor := $FFFF00; // "cyan"  
      $725362CE: aColor := $FFFF00; // "cyan1"  
      $725362CF: aColor := $EEEE00; // "cyan2"  
      $725362D0: aColor := $CDCD00; // "cyan3"  
      $725362D1: aColor := $8B8B00; // "cyan4"  
      $C6D997B4: aColor := $0B86B8; // "dark goldenrod"  
      $68484379: aColor := $006400; // "dark green"
      $A4C35B78: aColor := $6BB7BD; // "dark khaki"  
      $9C9E4D34: aColor := $2F6B55; // "dark olive green"  
      $E8F0EBF8: aColor := $008CFF; // "dark orange"  
      $430C30ED: aColor := $CC3299; // "dark orchid"  
      $BE450C98: aColor := $7A96E9; // "dark salmon"  
      $713EAF08: aColor := $8FBC8F; // "dark sea green"  
      $231552EB: aColor := $8B3D48; // "dark slate blue"  
      $0E97BED4: aColor := $4F4F2F; // "dark slate gray"  
      $0E9BBFD0: aColor := $4F4F2F; // "dark slate grey"  
      $DFB725B7: aColor := $D1CE00; // "dark turquoise"  
      $AB3988A9: aColor := $D30094; // "dark violet"
      $5627F268: aColor := $0B86B8; // "DarkGoldenrod"  
      $263CA7C9: aColor := $0FB9FF; // "DarkGoldenrod1"  
      $263CA7CA: aColor := $0EADEE; // "DarkGoldenrod2"  
      $263CA7CB: aColor := $0C95CD; // "DarkGoldenrod3"  
      $263CA7CC: aColor := $08658B; // "DarkGoldenrod4"  
      $21B1D22D: aColor := $006400; // "DarkGreen"  
      $5E2CEA2C: aColor := $6BB7BD; // "DarkKhaki"  
      $7D12FBFE: aColor := $2F6B55; // "DarkOliveGreen"
      $C3AA03B3: aColor := $70FFCA; // "DarkOliveGreen1"  
      $C3AA03B4: aColor := $68EEBC; // "DarkOliveGreen2"
      $C3AA03B5: aColor := $5ACDA2; // "DarkOliveGreen3"  
      $C3AA03B6: aColor := $3D8B6E; // "DarkOliveGreen4"  
      $189F0A44: aColor := $008CFF; // "DarkOrange"  
      $196786ED: aColor := $007FFF; // "DarkOrange1"  
      $196786EE: aColor := $0076EE; // "DarkOrange2"  
      $196786EF: aColor := $0066CD; // "DarkOrange3"  
      $196786F0: aColor := $00458B; // "DarkOrange4"
      $72BA4F39: aColor := $CC3299; // "DarkOrchid"  
      $8B127F38: aColor := $FF3EBF; // "DarkOrchid1"  
      $8B127F39: aColor := $EE3AB2; // "DarkOrchid2"  
      $8B127F3A: aColor := $CD329A; // "DarkOrchid3"  
      $8B127F3B: aColor := $8B2268; // "DarkOrchid4"  
      $EDF32AE4: aColor := $7A96E9; // "DarkSalmon"  
      $98084B2A: aColor := $8FBC8F; // "DarkSeaGreen"
      $B5347F87: aColor := $C1FFC1; // "DarkSeaGreen1"
      $B5347F88: aColor := $B4EEB4; // "DarkSeaGreen2"  
      $B5347F89: aColor := $9BCD9B; // "DarkSeaGreen3"  
      $B5347F8A: aColor := $698B69; // "DarkSeaGreen4"  
      $4FD5751D: aColor := $8B3D48; // "DarkSlateBlue"  
      $3B57E106: aColor := $4F4F2F; // "DarkSlateGray"  
      $7BA660AB: aColor := $FFFF97; // "DarkSlateGray1"  
      $7BA660AC: aColor := $EEEE8D; // "DarkSlateGray2"  
      $7BA660AD: aColor := $CDCD79; // "DarkSlateGray3"
      $7BA660AE: aColor := $8B8B52; // "DarkSlateGray4"  
      $3B5BE202: aColor := $4F4F2F; // "DarkSlateGrey"  
      $6F05806B: aColor := $D1CE00; // "DarkTurquoise"
      $DAE7A6F5: aColor := $D30094; // "DarkViolet"  
      $0B0DA84A: aColor := $9314FF; // "deep pink"  
      $472BF9AD: aColor := $FFBF00; // "deep sky blue"  
      $37E340C2: aColor := $9314FF; // "DeepPink"  
      $01AEEFEF: aColor := $9314FF; // "DeepPink1"  
      $01AEEFF0: aColor := $8912EE; // "DeepPink2"  
      $01AEEFF1: aColor := $7610CD; // "DeepPink3"  
      $01AEEFF2: aColor := $500A8B; // "DeepPink4"
      $459EE4AF: aColor := $FFBF00; // "DeepSkyBlue"  
      $06C94742: aColor := $FFBF00; // "DeepSkyBlue1"  
      $06C94743: aColor := $EEB200; // "DeepSkyBlue2"  
      $06C94744: aColor := $CD9A00; // "DeepSkyBlue3"  
      $06C94745: aColor := $8B6800; // "DeepSkyBlue4"  
      $69B8861B: aColor := $696969; // "dim gray"  
      $69BC8717: aColor := $696969; // "dim grey"  
      $9D5F7D4B: aColor := $696969; // "DimGray"  
      $9D637E47: aColor := $696969; // "DimGrey"  
      $EBFA13BF: aColor := $FF901E; // "dodger blue"  
      $B040E495: aColor := $FF901E; // "DodgerBlue"  
      $448D40DC: aColor := $FF901E; // "DodgerBlue1"
      $448D40DD: aColor := $EE861C; // "DodgerBlue2"  
      $448D40DE: aColor := $CD7418; // "DodgerBlue3"
      $448D40DF: aColor := $8B4E10; // "DodgerBlue4"  
      $09C0F72B: aColor := $2222B2; // "firebrick"  
      $5DA7D3C6: aColor := $3030FF; // "firebrick1"  
      $5DA7D3C7: aColor := $2C2CEE; // "firebrick2"  
      $5DA7D3C8: aColor := $2626CD; // "firebrick3"  
      $5DA7D3C9: aColor := $1A1A8B; // "firebrick4"  
      $FEB28FDD: aColor := $F0FAFF; // "floral white"
      $065132D5: aColor := $F0FAFF; // "FloralWhite"  
      $473F8240: aColor := $228B22; // "forest green"  
      $9C9B71A6: aColor := $228B22; // "ForestGreen"  
      $A1A2D3BE: aColor := $DCDCDC; // "gainsboro"  
      $9AA37DF8: aColor := $FFF8F8; // "ghost white"  
      $C03C0DFA: aColor := $FFF8F8; // "GhostWhite"  
      $EF52D0C0: aColor := $00D7FF; // "gold"
      $B6215F71: aColor := $00D7FF; // "gold1"
      $B6215F72: aColor := $00C9EE; // "gold2"  
      $B6215F73: aColor := $00ADCD; // "gold3"  
      $B6215F74: aColor := $00758B; // "gold4"  
      $AA3B4FDE: aColor := $20A5DA; // "goldenrod"
      $3476A7D3: aColor := $25C1FF; // "goldenrod1"  
      $3476A7D4: aColor := $22B4EE; // "goldenrod2"  
      $3476A7D5: aColor := $1D9BCD; // "goldenrod3"  
      $3476A7D6: aColor := $14698B; // "goldenrod4"  
      $F0C1FCA3: aColor := $BEBEBE; // "gray"  
      $3C602C4D: aColor := $010000; // "gray0"  
      $3C602C4E: aColor := $030303; // "gray1"  
      $07F8E762: aColor := $1A1A1A; // "gray10"  
      $DDA2F14E: aColor := $FFFFFF; // "gray100"  
      $07F8E763: aColor := $1C1C1C; // "gray11"  
      $07F8E764: aColor := $1F1F1F; // "gray12"
      $07F8E765: aColor := $212121; // "gray13"  
      $07F8E766: aColor := $242424; // "gray14"
      $07F8E767: aColor := $262626; // "gray15"  
      $07F8E768: aColor := $292929; // "gray16"  
      $07F8E769: aColor := $2B2B2B; // "gray17"  
      $07F8E76A: aColor := $2E2E2E; // "gray18"  
      $07F8E76B: aColor := $303030; // "gray19"  
      $3C602C4F: aColor := $050505; // "gray2"  
      $07F9E7A1: aColor := $333333; // "gray20"  
      $07F9E7A2: aColor := $363636; // "gray21"  
      $07F9E7A3: aColor := $383838; // "gray22"  
      $07F9E7A4: aColor := $3B3B3B; // "gray23"  
      $07F9E7A5: aColor := $3D3D3D; // "gray24"  
      $07F9E7A6: aColor := $404040; // "gray25"  
      $07F9E7A7: aColor := $424242; // "gray26"  
      $07F9E7A8: aColor := $454545; // "gray27"
      $07F9E7A9: aColor := $474747; // "gray28"  
      $07F9E7AA: aColor := $4A4A4A; // "gray29"
      $3C602C50: aColor := $080808; // "gray3"  
      $07FAE7E0: aColor := $4D4D4D; // "gray30"  
      $07FAE7E1: aColor := $4F4F4F; // "gray31"  
      $07FAE7E2: aColor := $525252; // "gray32"  
      $07FAE7E3: aColor := $545454; // "gray33"  
      $07FAE7E4: aColor := $575757; // "gray34"  
      $07FAE7E5: aColor := $595959; // "gray35"
      $07FAE7E6: aColor := $5C5C5C; // "gray36"  
      $07FAE7E7: aColor := $5E5E5E; // "gray37"  
      $07FAE7E8: aColor := $616161; // "gray38"  
      $07FAE7E9: aColor := $636363; // "gray39"  
      $3C602C51: aColor := $0A0A0A; // "gray4"  
      $07FBE81F: aColor := $666666; // "gray40"  
      $07FBE820: aColor := $696969; // "gray41"
      $07FBE821: aColor := $6B6B6B; // "gray42"
      $07FBE822: aColor := $6E6E6E; // "gray43"  
      $07FBE823: aColor := $707070; // "gray44"  
      $07FBE824: aColor := $737373; // "gray45"  
      $07FBE825: aColor := $757575; // "gray46"  
      $07FBE826: aColor := $787878; // "gray47"  
      $07FBE827: aColor := $7A7A7A; // "gray48"  
      $07FBE828: aColor := $7D7D7D; // "gray49"  
      $3C602C52: aColor := $0D0D0D; // "gray5"  
      $07FCE85E: aColor := $7F7F7F; // "gray50"  
      $07FCE85F: aColor := $828282; // "gray51"  
      $07FCE860: aColor := $858585; // "gray52"
      $07FCE861: aColor := $878787; // "gray53"  
      $07FCE862: aColor := $8A8A8A; // "gray54"  
      $07FCE863: aColor := $8C8C8C; // "gray55"  
      $07FCE864: aColor := $8F8F8F; // "gray56"  
      $07FCE865: aColor := $919191; // "gray57"  
      $07FCE866: aColor := $949494; // "gray58"  
      $07FCE867: aColor := $969696; // "gray59"  
      $3C602C53: aColor := $0F0F0F; // "gray6"  
      $07FDE89D: aColor := $999999; // "gray60"  
      $07FDE89E: aColor := $9C9C9C; // "gray61"  
      $07FDE89F: aColor := $9E9E9E; // "gray62"  
      $07FDE8A0: aColor := $A1A1A1; // "gray63"
      $07FDE8A1: aColor := $A3A3A3; // "gray64"  
      $07FDE8A2: aColor := $A6A6A6; // "gray65"  
      $07FDE8A3: aColor := $A8A8A8; // "gray66"  
      $07FDE8A4: aColor := $ABABAB; // "gray67"  
      $07FDE8A5: aColor := $ADADAD; // "gray68"  
      $07FDE8A6: aColor := $B0B0B0; // "gray69"  
      $3C602C54: aColor := $121212; // "gray7"  
      $07FEE8DC: aColor := $B3B3B3; // "gray70"
      $07FEE8DD: aColor := $B5B5B5; // "gray71"  
      $07FEE8DE: aColor := $B8B8B8; // "gray72"
      $07FEE8DF: aColor := $BABABA; // "gray73"  
      $07FEE8E0: aColor := $BDBDBD; // "gray74"  
      $07FEE8E1: aColor := $BFBFBF; // "gray75"  
      $07FEE8E2: aColor := $C2C2C2; // "gray76"  
      $07FEE8E3: aColor := $C4C4C4; // "gray77"  
      $07FEE8E4: aColor := $C7C7C7; // "gray78"  
      $07FEE8E5: aColor := $C9C9C9; // "gray79"
      $3C602C55: aColor := $141414; // "gray8"  
      $07FFE91B: aColor := $CCCCCC; // "gray80"  
      $07FFE91C: aColor := $CFCFCF; // "gray81"  
      $07FFE91D: aColor := $D1D1D1; // "gray82"
      $07FFE91E: aColor := $D4D4D4; // "gray83"  
      $07FFE91F: aColor := $D6D6D6; // "gray84"  
      $07FFE920: aColor := $D9D9D9; // "gray85"
      $07FFE921: aColor := $DBDBDB; // "gray86"
      $07FFE922: aColor := $DEDEDE; // "gray87"  
      $07FFE923: aColor := $E0E0E0; // "gray88"  
      $07FFE924: aColor := $E3E3E3; // "gray89"  
      $3C602C56: aColor := $171717; // "gray9"  
      $0800E95A: aColor := $E5E5E5; // "gray90"  
      $0800E95B: aColor := $E8E8E8; // "gray91"  
      $0800E95C: aColor := $EBEBEB; // "gray92"
      $0800E95D: aColor := $EDEDED; // "gray93"  
      $0800E95E: aColor := $F0F0F0; // "gray94"  
      $0800E95F: aColor := $F2F2F2; // "gray95"  
      $0800E960: aColor := $F5F5F5; // "gray96"  
      $0800E961: aColor := $F7F7F7; // "gray97"  
      $0800E962: aColor := $FAFAFA; // "gray98"  
      $0800E963: aColor := $FCFCFC; // "gray99"  
      $3E4465A3: aColor := $00FF00; // "green"  
      $2B3780B1: aColor := $2FFFAD; // "green yellow"  
      $B878034E: aColor := $00FF00; // "green1"  
      $B878034F: aColor := $00EE00; // "green2"  
      $B8780350: aColor := $00CD00; // "green3"  
      $B8780351: aColor := $008B00; // "green4"  
      $036BEA17: aColor := $2FFFAD; // "GreenYellow"  
      $F0C5FD9F: aColor := $BEBEBE; // "grey"  
      $3E586A51: aColor := $010000; // "grey0"  
      $3E586A52: aColor := $030303; // "grey1"  
      $C2142A5E: aColor := $1A1A1A; // "grey10"  
      $ED546D52: aColor := $FFFFFF; // "grey100"  
      $C2142A5F: aColor := $1C1C1C; // "grey11"  
      $C2142A60: aColor := $1F1F1F; // "grey12"  
      $C2142A61: aColor := $212121; // "grey13"
      $C2142A62: aColor := $242424; // "grey14"  
      $C2142A63: aColor := $262626; // "grey15"
      $C2142A64: aColor := $292929; // "grey16"  
      $C2142A65: aColor := $2B2B2B; // "grey17"
      $C2142A66: aColor := $2E2E2E; // "grey18"  
      $C2142A67: aColor := $303030; // "grey19"  
      $3E586A53: aColor := $050505; // "grey2"  
      $C2152A9D: aColor := $333333; // "grey20"  
      $C2152A9E: aColor := $363636; // "grey21"  
      $C2152A9F: aColor := $383838; // "grey22"  
      $C2152AA0: aColor := $3B3B3B; // "grey23"
      $C2152AA1: aColor := $3D3D3D; // "grey24"  
      $C2152AA2: aColor := $404040; // "grey25"  
      $C2152AA3: aColor := $424242; // "grey26"  
      $C2152AA4: aColor := $454545; // "grey27"  
      $C2152AA5: aColor := $474747; // "grey28"  
      $C2152AA6: aColor := $4A4A4A; // "grey29"  
      $3E586A54: aColor := $080808; // "grey3"
      $C2162ADC: aColor := $4D4D4D; // "grey30"
      $C2162ADD: aColor := $4F4F4F; // "grey31"  
      $C2162ADE: aColor := $525252; // "grey32"  
      $C2162ADF: aColor := $545454; // "grey33"
      $C2162AE0: aColor := $575757; // "grey34"  
      $C2162AE1: aColor := $595959; // "grey35"  
      $C2162AE2: aColor := $5C5C5C; // "grey36"  
      $C2162AE3: aColor := $5E5E5E; // "grey37"  
      $C2162AE4: aColor := $616161; // "grey38"  
      $C2162AE5: aColor := $636363; // "grey39"  
      $3E586A55: aColor := $0A0A0A; // "grey4"  
      $C2172B1B: aColor := $666666; // "grey40"  
      $C2172B1C: aColor := $696969; // "grey41"  
      $C2172B1D: aColor := $6B6B6B; // "grey42"  
      $C2172B1E: aColor := $6E6E6E; // "grey43"  
      $C2172B1F: aColor := $707070; // "grey44"  
      $C2172B20: aColor := $737373; // "grey45"  
      $C2172B21: aColor := $757575; // "grey46"  
      $C2172B22: aColor := $787878; // "grey47"  
      $C2172B23: aColor := $7A7A7A; // "grey48"  
      $C2172B24: aColor := $7D7D7D; // "grey49"  
      $3E586A56: aColor := $0D0D0D; // "grey5"  
      $C2182B5A: aColor := $7F7F7F; // "grey50"  
      $C2182B5B: aColor := $828282; // "grey51"  
      $C2182B5C: aColor := $858585; // "grey52"  
      $C2182B5D: aColor := $878787; // "grey53"  
      $C2182B5E: aColor := $8A8A8A; // "grey54"  
      $C2182B5F: aColor := $8C8C8C; // "grey55"  
      $C2182B60: aColor := $8F8F8F; // "grey56"  
      $C2182B61: aColor := $919191; // "grey57"  
      $C2182B62: aColor := $949494; // "grey58"  
      $C2182B63: aColor := $969696; // "grey59"
      $3E586A57: aColor := $0F0F0F; // "grey6"  
      $C2192B99: aColor := $999999; // "grey60"
      $C2192B9A: aColor := $9C9C9C; // "grey61"  
      $C2192B9B: aColor := $9E9E9E; // "grey62"
      $C2192B9C: aColor := $A1A1A1; // "grey63"
      $C2192B9D: aColor := $A3A3A3; // "grey64"  
      $C2192B9E: aColor := $A6A6A6; // "grey65"  
      $C2192B9F: aColor := $A8A8A8; // "grey66"  
      $C2192BA0: aColor := $ABABAB; // "grey67"
      $C2192BA1: aColor := $ADADAD; // "grey68"  
      $C2192BA2: aColor := $B0B0B0; // "grey69"  
      $3E586A58: aColor := $121212; // "grey7"  
      $C21A2BD8: aColor := $B3B3B3; // "grey70"  
      $C21A2BD9: aColor := $B5B5B5; // "grey71"  
      $C21A2BDA: aColor := $B8B8B8; // "grey72"  
      $C21A2BDB: aColor := $BABABA; // "grey73"
      $C21A2BDC: aColor := $BDBDBD; // "grey74"
      $C21A2BDD: aColor := $BFBFBF; // "grey75"  
      $C21A2BDE: aColor := $C2C2C2; // "grey76"  
      $C21A2BDF: aColor := $C4C4C4; // "grey77"  
      $C21A2BE0: aColor := $C7C7C7; // "grey78"  
      $C21A2BE1: aColor := $C9C9C9; // "grey79"  
      $3E586A59: aColor := $141414; // "grey8"  
      $C21B2C17: aColor := $CCCCCC; // "grey80"  
      $C21B2C18: aColor := $CFCFCF; // "grey81"  
      $C21B2C19: aColor := $D1D1D1; // "grey82"  
      $C21B2C1A: aColor := $D4D4D4; // "grey83"  
      $C21B2C1B: aColor := $D6D6D6; // "grey84"  
      $C21B2C1C: aColor := $D9D9D9; // "grey85"  
      $C21B2C1D: aColor := $DBDBDB; // "grey86"  
      $C21B2C1E: aColor := $DEDEDE; // "grey87"  
      $C21B2C1F: aColor := $E0E0E0; // "grey88"  
      $C21B2C20: aColor := $E3E3E3; // "grey89"  
      $3E586A5A: aColor := $171717; // "grey9"  
      $C21C2C56: aColor := $E5E5E5; // "grey90"  
      $C21C2C57: aColor := $E8E8E8; // "grey91"  
      $C21C2C58: aColor := $EBEBEB; // "grey92"  
      $C21C2C59: aColor := $EDEDED; // "grey93"  
      $C21C2C5A: aColor := $F0F0F0; // "grey94"  
      $C21C2C5B: aColor := $F2F2F2; // "grey95"  
      $C21C2C5C: aColor := $F5F5F5; // "grey96"  
      $C21C2C5D: aColor := $F7F7F7; // "grey97"  
      $C21C2C5E: aColor := $FAFAFA; // "grey98"  
      $C21C2C5F: aColor := $FCFCFC; // "grey99"  
      $9E7CB5FB: aColor := $F0FFF0; // "honeydew"  
      $B6ABC8F6: aColor := $F0FFF0; // "honeydew1"  
      $B6ABC8F7: aColor := $E0EEE0; // "honeydew2"  
      $B6ABC8F8: aColor := $C1CDC1; // "honeydew3"
      $B6ABC8F9: aColor := $838B83; // "honeydew4"
      $34935B29: aColor := $B469FF; // "hot pink"
      $F4E7E623: aColor := $B469FF; // "HotPink"  
      $2B34A2CE: aColor := $B46EFF; // "HotPink1"  
      $2B34A2CF: aColor := $A76AEE; // "HotPink2"  
      $2B34A2D0: aColor := $9060CD; // "HotPink3"  
      $2B34A2D1: aColor := $623A8B; // "HotPink4"  
      $9BEE55A8: aColor := $5C5CCD; // "indian red"  
      $5B280DDA: aColor := $5C5CCD; // "IndianRed"
      $7CB568D7: aColor := $6A6AFF; // "IndianRed1"
      $7CB568D8: aColor := $6363EE; // "IndianRed2"  
      $7CB568D9: aColor := $5555CD; // "IndianRed3"
      $7CB568DA: aColor := $3A3A8B; // "IndianRed4"  
      $853204E9: aColor := $F0FFFF; // "ivory"  
      $CC383588: aColor := $F0FFFF; // "ivory1"  
      $CC383589: aColor := $E0EEEE; // "ivory2"
      $CC38358A: aColor := $C1CDCD; // "ivory3"
      $CC38358B: aColor := $838B8B; // "ivory4"  
      $7ABF7DA2: aColor := $8CE6F0; // "khaki"  
      $B2C1EB0F: aColor := $8FF6FF; // "khaki1"  
      $B2C1EB10: aColor := $85E6EE; // "khaki2"  
      $B2C1EB11: aColor := $73C6CD; // "khaki3"  
      $B2C1EB12: aColor := $4E868B; // "khaki4"  
      $260283A7: aColor := $FAE6E6; // "lavender"  
      $520862A7: aColor := $F5F0FF; // "lavender blush"  
      $24A14EB9: aColor := $F5F0FF; // "LavenderBlush"  
      $526B5FB8: aColor := $F5F0FF; // "LavenderBlush1"  
      $526B5FB9: aColor := $E5E0EE; // "LavenderBlush2"  
      $526B5FBA: aColor := $C5C1CD; // "LavenderBlush3"  
      $526B5FBB: aColor := $86838B; // "LavenderBlush4"  
      $A4D799AF: aColor := $00FC7C; // "lawn green"  
      $2BDB8E77: aColor := $00FC7C; // "LawnGreen"  
      $8D35E176: aColor := $CDFAFF; // "lemon chiffon"  
      $8A949DF0: aColor := $CDFAFF; // "LemonChiffon"  
      $B882DE41: aColor := $CDFAFF; // "LemonChiffon1"  
      $B882DE42: aColor := $BFE9EE; // "LemonChiffon2"  
      $B882DE43: aColor := $A5C9CD; // "LemonChiffon3"  
      $B882DE44: aColor := $70898B; // "LemonChiffon4"  
      $929FCAA4: aColor := $E6D8AD; // "light blue"  
      $ADF98087: aColor := $8080F0; // "light coral"  
      $C779600D: aColor := $FFFFE0; // "light cyan"  
      $FB133A74: aColor := $82DDEE; // "light goldenrod"  
      $3F8DC9A0: aColor := $D2FAFA; // "light goldenrod yellow"  
      $7E22368D: aColor := $D3D3D3; // "light gray"  
      $7E263789: aColor := $D3D3D3; // "light grey"
      $1C7E04E0: aColor := $C1B6FF; // "light pink"  
      $EB8959D8: aColor := $7AA0FF; // "light salmon"  
      $A57851C8: aColor := $AAB220; // "light sea green"
      $A7400043: aColor := $FACE87; // "light sky blue"  
      $A004602B: aColor := $FF7084; // "light slate blue"
      $8B86CC14: aColor := $998877; // "light slate gray"  
      $8B8ACD10: aColor := $998877; // "light slate grey"  
      $D3CBC659: aColor := $DEC4B0; // "light steel blue"  
      $7FC70DDE: aColor := $E0FFFF; // "light yellow"  
      $EAC62470: aColor := $E6D8AD; // "LightBlue"  
      $EB32F7C1: aColor := $FFEFBF; // "LightBlue1"
      $EB32F7C2: aColor := $EEDFB2; // "LightBlue2"
      $EB32F7C3: aColor := $CDC09A; // "LightBlue3"  
      $EB32F7C4: aColor := $8B8368; // "LightBlue4"  
      $B93599BB: aColor := $8080F0; // "LightCoral"  
      $1F9FB9D9: aColor := $FFFFE0; // "LightCyan"  
      $8227BC98: aColor := $FFFFE0; // "LightCyan1"  
      $8227BC99: aColor := $EEEED1; // "LightCyan2"  
      $8227BC9A: aColor := $CDCDB4; // "LightCyan3"
      $8227BC9B: aColor := $8B8B7A; // "LightCyan4"
      $2B999FA8: aColor := $82DDEE; // "LightGoldenrod"  
      $5A764A89: aColor := $8BECFF; // "LightGoldenrod1"  
      $5A764A8A: aColor := $82DCEE; // "LightGoldenrod2"  
      $5A764A8B: aColor := $70BECD; // "LightGoldenrod3"  
      $5A764A8C: aColor := $4C818B; // "LightGoldenrod4"  
      $8A20CC9C: aColor := $D2FAFA; // "LightGoldenrodYellow"  
      $D6489059: aColor := $D3D3D3; // "LightGray"  
      $D64C9155: aColor := $D3D3D3; // "LightGrey"  
      $74A45EAC: aColor := $C1B6FF; // "LightPink"  
      $131F4C85: aColor := $B9AEFF; // "LightPink1"  
      $131F4C86: aColor := $ADA2EE; // "LightPink2"  
      $131F4C87: aColor := $958CCD; // "LightPink3"  
      $131F4C88: aColor := $655F8B; // "LightPink4"  
      $C8878DA4: aColor := $7AA0FF; // "LightSalmon"  
      $E6FFDB8D: aColor := $7AA0FF; // "LightSalmon1"  
      $E6FFDB8E: aColor := $7295EE; // "LightSalmon2"  
      $E6FFDB8F: aColor := $6281CD; // "LightSalmon3"  
      $E6FFDB90: aColor := $42578B; // "LightSalmon4"  
      $09174DEA: aColor := $AAB220; // "LightSeaGreen"  
      $367E5645: aColor := $FACE87; // "LightSkyBlue"  
      $BF5C3B2C: aColor := $FFE2B0; // "LightSkyBlue1"  
      $BF5C3B2D: aColor := $EED3A4; // "LightSkyBlue2"  
      $BF5C3B2E: aColor := $CDB68D; // "LightSkyBlue3"  
      $BF5C3B2F: aColor := $8B7B60; // "LightSkyBlue4"
      $2547225D: aColor := $FF7084; // "LightSlateBlue"  
      $10C98E46: aColor := $998877; // "LightSlateGray"  
      $10CD8F42: aColor := $998877; // "LightSlateGrey"  
      $255D50AF: aColor := $DEC4B0; // "LightSteelBlue"  
      $82A5DB42: aColor := $FFE1CA; // "LightSteelBlue1"  
      $82A5DB43: aColor := $EED2BC; // "LightSteelBlue2"  
      $82A5DB44: aColor := $CDB5A2; // "LightSteelBlue3"
      $82A5DB45: aColor := $8B7B6E; // "LightSteelBlue4"  
      $5CC541AA: aColor := $E0FFFF; // "LightYellow"
      $16352907: aColor := $E0FFFF; // "LightYellow1"  
      $16352908: aColor := $D1EEEE; // "LightYellow2"
      $16352909: aColor := $B4CDCD; // "LightYellow3"  
      $1635290A: aColor := $7A8B8B; // "LightYellow4"  
      $577235B8: aColor := $32CD32; // "lime green"  
      $1F2C602E: aColor := $32CD32; // "LimeGreen"  
      $F393757A: aColor := $E6F0FA; // "linen"
      $7D01C0E9: aColor := $FF00FF; // "magenta"  
      $84577988: aColor := $FF00FF; // "magenta1"  
      $84577989: aColor := $EE00EE; // "magenta2"  
      $8457798A: aColor := $CD00CD; // "magenta3"  
      $8457798B: aColor := $8B008B; // "magenta4"  
      $1C9630F0: aColor := $6030B0; // "maroon"  
      $39E60B41: aColor := $B334FF; // "maroon1"
      $39E60B42: aColor := $A730EE; // "maroon2"
      $39E60B43: aColor := $9029CD; // "maroon3"  
      $39E60B44: aColor := $621C8B; // "maroon4"  
      $2A53A209: aColor := $AACD66; // "medium aquamarine"  
      $D3EA1C25: aColor := $CD0000; // "medium blue"  
      $CF081F2E: aColor := $D355BA; // "medium orchid"
      $6ED35567: aColor := $DB7093; // "medium purple"  
      $F7D04187: aColor := $71B33C; // "medium sea green"  
      $D367602C: aColor := $EE687B; // "medium slate blue"  
      $C5B61B9B: aColor := $9AFA00; // "medium spring green"  
      $6648B836: aColor := $CCD148; // "medium turquoise"  
      $F570657B: aColor := $8515C7; // "medium violet red"  
      $9F8F7993: aColor := $AACD66; // "MediumAquamarine"  
      $68CA62AF: aColor := $CD0000; // "MediumBlue"  
      $B019C0B8: aColor := $D355BA; // "MediumOrchid"  
      $170E6D79: aColor := $FF66E0; // "MediumOrchid1"  
      $170E6D7A: aColor := $EE5FD1; // "MediumOrchid2"  
      $170E6D7B: aColor := $CD52B4; // "MediumOrchid3"  
      $170E6D7C: aColor := $8B377A; // "MediumOrchid4"  
      $4FE4F6F1: aColor := $DB7093; // "MediumPurple"  
      $A049C580: aColor := $FF82AB; // "MediumPurple1"
      $A049C581: aColor := $EE799F; // "MediumPurple2"  
      $A049C582: aColor := $CD6889; // "MediumPurple3"  
      $A049C583: aColor := $8B475D; // "MediumPurple4"  
      $F948ED29: aColor := $71B33C; // "MediumSeaGreen"  
      $E0BC52DE: aColor := $EE687B; // "MediumSlateBlue"  
      $F0E5DC81: aColor := $9AFA00; // "MediumSpringGreen"  
      $FFEC5E2C: aColor := $CCD148; // "MediumTurquoise"  
      $BE297F3D: aColor := $8515C7; // "MediumVioletRed"  
      $7F411F4A: aColor := $701919; // "midnight blue"  
      $8047464A: aColor := $701919; // "MidnightBlue"  
      $28A92644: aColor := $FAFFF5; // "mint cream"
      $05554260: aColor := $FAFFF5; // "MintCream"  
      $4B913013: aColor := $E1E4FF; // "misty rose"
      $0F7AB46B: aColor := $E1E4FF; // "MistyRose"  
      $839D6686: aColor := $E1E4FF; // "MistyRose1"  
      $839D6687: aColor := $D2D5EE; // "MistyRose2"  
      $839D6688: aColor := $B5B7CD; // "MistyRose3"  
      $839D6689: aColor := $7B7D8B; // "MistyRose4"  
      $FCD79A99: aColor := $B5E4FF; // "moccasin"  
      $11C80E2C: aColor := $ADDEFF; // "navajo white"
      $57EC30C6: aColor := $ADDEFF; // "NavajoWhite"  
      $D3E600EB: aColor := $ADDEFF; // "NavajoWhite1"  
      $D3E600EC: aColor := $A1CFEE; // "NavajoWhite2"  
      $D3E600ED: aColor := $8BB3CD; // "NavajoWhite3"  
      $D3E600EE: aColor := $5E798B; // "NavajoWhite4"  
      $2E27AF76: aColor := $800000; // "navy"  
      $345354E4: aColor := $800000; // "navy blue"
      $904C0A30: aColor := $800000; // "NavyBlue"
      $06B19630: aColor := $E6F5FD; // "old lace"  
      $90B2961E: aColor := $E6F5FD; // "OldLace"  
      $2FC200D4: aColor := $238E6B; // "olive drab"  
      $95200AAA: aColor := $238E6B; // "OliveDrab"  
      $BD8CA007: aColor := $3EFFC0; // "OliveDrab1"  
      $BD8CA008: aColor := $3AEEB3; // "OliveDrab2"  
      $BD8CA009: aColor := $32CD9A; // "OliveDrab3"  
      $BD8CA00A: aColor := $228B69; // "OliveDrab4"  
      $B427544E: aColor := $00A5FF; // "orange"  
      $77BA5EDF: aColor := $0045FF; // "orange red"  
      $A9FBBF63: aColor := $00A5FF; // "orange1"
      $A9FBBF64: aColor := $009AEE; // "orange2"  
      $A9FBBF65: aColor := $0085CD; // "orange3"  
      $A9FBBF66: aColor := $005A8B; // "orange4"  
      $BB3346E3: aColor := $0045FF; // "OrangeRed"  
      $5881720E: aColor := $0045FF; // "OrangeRed1"
      $5881720F: aColor := $0040EE; // "OrangeRed2"  
      $58817210: aColor := $0037CD; // "OrangeRed3"  
      $58817211: aColor := $00258B; // "OrangeRed4"  
      $0E429943: aColor := $D670DA; // "orchid"  
      $1BA6B7AE: aColor := $FA83FF; // "orchid1"  
      $1BA6B7AF: aColor := $E97AEE; // "orchid2"  
      $1BA6B7B0: aColor := $C969CD; // "orchid3"  
      $1BA6B7B1: aColor := $89478B; // "orchid4"  
      $6EA0FD28: aColor := $AAE8EE; // "pale goldenrod"  
      $E0ED9CED: aColor := $98FB98; // "pale green"  
      $877E8B2B: aColor := $EEEEAF; // "pale turquoise"
      $F4A44FC6: aColor := $9370DB; // "pale violet red"  
      $5CEBEFF4: aColor := $AAE8EE; // "PaleGoldenrod"  
      $7CDADBB9: aColor := $98FB98; // "PaleGreen"  
      $959512B8: aColor := $9AFF9A; // "PaleGreen1"
      $959512B9: aColor := $90EE90; // "PaleGreen2"  
      $959512BA: aColor := $7CCD7C; // "PaleGreen3"
      $959512BB: aColor := $548B54; // "PaleGreen4"  
      $75C97DF7: aColor := $EEEEAF; // "PaleTurquoise"  
      $7A8CFFFA: aColor := $FFFFBB; // "PaleTurquoise1"  
      $7A8CFFFB: aColor := $EEEEAE; // "PaleTurquoise2"  
      $7A8CFFFC: aColor := $CDCD96; // "PaleTurquoise3"  
      $7A8CFFFD: aColor := $8B8B66; // "PaleTurquoise4"  
      $34069F08: aColor := $9370DB; // "PaleVioletRed"
      $6CA92329: aColor := $AB82FF; // "PaleVioletRed1"  
      $6CA9232A: aColor := $9F79EE; // "PaleVioletRed2"  
      $6CA9232B: aColor := $8968CD; // "PaleVioletRed3"  
      $6CA9232C: aColor := $5D478B; // "PaleVioletRed4"  
      $2CA2DF8E: aColor := $D5EFFF; // "papaya whip"  
      $1C8C8202: aColor := $D5EFFF; // "PapayaWhip"  
      $8FFD6BF4: aColor := $B9DAFF; // "peach puff"
      $97B15676: aColor := $B9DAFF; // "PeachPuff"
      $AB1A473B: aColor := $B9DAFF; // "PeachPuff1"  
      $AB1A473C: aColor := $ADCBEE; // "PeachPuff2"  
      $AB1A473D: aColor := $95AFCD; // "PeachPuff3"  
      $AB1A473E: aColor := $65778B; // "PeachPuff4"  
      $8D298DF8: aColor := $3F85CD; // "peru"  
      $8F1DCAF6: aColor := $CBC0FF; // "pink"  
      $034AF2BB: aColor := $C5B5FF; // "pink1"  
      $034AF2BC: aColor := $B8A9EE; // "pink2"  
      $034AF2BD: aColor := $9E91CD; // "pink3"  
      $034AF2BE: aColor := $6C638B; // "pink4"  
      $909EFB34: aColor := $DDA0DD; // "plum"  
      $9253D1FD: aColor := $FFBBFF; // "plum1"
      $9253D1FE: aColor := $EEAEEE; // "plum2"  
      $9253D1FF: aColor := $CD96CD; // "plum3"  
      $9253D200: aColor := $8B668B; // "plum4"  
      $7CD7BAA1: aColor := $E6E0B0; // "powder blue"  
      $98216533: aColor := $E6E0B0; // "PowderBlue"
      $AE0DCF7C: aColor := $F020A0; // "purple"  
      $A4E20FB5: aColor := $FF309B; // "purple1"  
      $A4E20FB6: aColor := $EE2C91; // "purple2"  
      $A4E20FB7: aColor := $CD267D; // "purple3"  
      $A4E20FB8: aColor := $8B1A55; // "purple4"  
      $388800B1: aColor := $0000FF; // "red"
      $EA292BC0: aColor := $0000FF; // "red1"  
      $EA292BC1: aColor := $0000EE; // "red2"  
      $EA292BC2: aColor := $0000CD; // "red3"  
      $EA292BC3: aColor := $00008B; // "red4"  
      $B13AC839: aColor := $8F8FBC; // "rosy brown"  
      $560AF313: aColor := $8F8FBC; // "RosyBrown"  
      $1FC4D1DE: aColor := $C1C1FF; // "RosyBrown1"  
      $1FC4D1DF: aColor := $B4B4EE; // "RosyBrown2"
      $1FC4D1E0: aColor := $9B9BCD; // "RosyBrown3"  
      $1FC4D1E1: aColor := $69698B; // "RosyBrown4"
      $97F1BC13: aColor := $E16941; // "royal blue"  
      $0435E741: aColor := $E16941; // "RoyalBlue"  
      $F084E930: aColor := $FF7648; // "RoyalBlue1"  
      $F084E931: aColor := $EE6E43; // "RoyalBlue2"  
      $F084E932: aColor := $CD5F3A; // "RoyalBlue3"  
      $F084E933: aColor := $8B4027; // "RoyalBlue4"  
      $7ED34A1D: aColor := $13458B; // "saddle brown"
      $7ABFB82F: aColor := $13458B; // "SaddleBrown"  
      $897B74EE: aColor := $7280FA; // "salmon"  
      $4A4FC6C3: aColor := $698CFF; // "salmon1"  
      $4A4FC6C4: aColor := $6282EE; // "salmon2"  
      $4A4FC6C5: aColor := $5470CD; // "salmon3"  
      $4A4FC6C6: aColor := $394C8B; // "salmon4"  
      $9FF90ACB: aColor := $60A4F4; // "sandy brown"
      $CBB8EC01: aColor := $60A4F4; // "SandyBrown"
      $54A06732: aColor := $578B2E; // "sea green"  
      $64F83034: aColor := $578B2E; // "SeaGreen"  
      $0947DCFD: aColor := $9FFF54; // "SeaGreen1"  
      $0947DCFE: aColor := $94EE4E; // "SeaGreen2"  
      $0947DCFF: aColor := $80CD43; // "SeaGreen3"  
      $0947DD00: aColor := $578B2E; // "SeaGreen4"  
      $C2CF7E81: aColor := $EEF5FF; // "seashell"  
      $6F9121F0: aColor := $EEF5FF; // "seashell1"
      $6F9121F1: aColor := $DEE5EE; // "seashell2"  
      $6F9121F2: aColor := $BFC5CD; // "seashell3"  
      $6F9121F3: aColor := $82868B; // "seashell4"  
      $63ABC6F2: aColor := $2D52A0; // "sienna"  
      $4E37F5BF: aColor := $4782FF; // "sienna1"  
      $4E37F5C0: aColor := $4279EE; // "sienna2"  
      $4E37F5C1: aColor := $3968CD; // "sienna3"  
      $4E37F5C2: aColor := $26478B; // "sienna4"  
      $C2A9F059: aColor := $EBCE87; // "sky blue"  
      $99CE417B: aColor := $EBCE87; // "SkyBlue"  
      $1B3D1D76: aColor := $FFCE87; // "SkyBlue1"
      $1B3D1D77: aColor := $EEC07E; // "SkyBlue2"  
      $1B3D1D78: aColor := $CDA66C; // "SkyBlue3"  
      $1B3D1D79: aColor := $8B704A; // "SkyBlue4"  
      $D04BA541: aColor := $CD5A6A; // "slate blue"
      $BBCE112A: aColor := $908070; // "slate gray"  
      $BBD21226: aColor := $908070; // "slate grey"  
      $A3E8D293: aColor := $CD5A6A; // "SlateBlue"  
      $28DED25E: aColor := $FF6F83; // "SlateBlue1"  
      $28DED25F: aColor := $EE677A; // "SlateBlue2"  
      $28DED260: aColor := $CD5969; // "SlateBlue3"  
      $28DED261: aColor := $8B3C47; // "SlateBlue4"  
      $8F6B3E7C: aColor := $908070; // "SlateGray"
      $89E060B5: aColor := $FFE2C6; // "SlateGray1"  
      $89E060B6: aColor := $EED3B9; // "SlateGray2"
      $89E060B7: aColor := $CDB69F; // "SlateGray3"  
      $89E060B8: aColor := $8B7B6C; // "SlateGray4"  
      $8F6F3F78: aColor := $908070; // "SlateGrey"  
      $1D298B03: aColor := $FAFAFF; // "snow"  
      $B83C35EE: aColor := $FAFAFF; // "snow1"  
      $B83C35EF: aColor := $E9E9EE; // "snow2"  
      $B83C35F0: aColor := $C9C9CD; // "snow3"
      $B83C35F1: aColor := $89898B; // "snow4"  
      $951F2630: aColor := $7FFF00; // "spring green"  
      $C0E3D1B6: aColor := $7FFF00; // "SpringGreen"  
      $49C69BFB: aColor := $7FFF00; // "SpringGreen1"  
      $49C69BFC: aColor := $76EE00; // "SpringGreen2"  
      $49C69BFD: aColor := $66CD00; // "SpringGreen3"  
      $49C69BFE: aColor := $458B00; // "SpringGreen4"
      $04130B6F: aColor := $B48246; // "steel blue"
      $A3FF00E5: aColor := $B48246; // "SteelBlue"  
      $5CA6388C: aColor := $FFB863; // "SteelBlue1"  
      $5CA6388D: aColor := $EEAC5C; // "SteelBlue2"  
      $5CA6388E: aColor := $CD944F; // "SteelBlue3"
      $5CA6388F: aColor := $8B6436; // "SteelBlue4"  
      $39801EC1: aColor := $8CB4D2; // "tan"  
      $454891B0: aColor := $4FA5FF; // "tan1"  
      $454891B1: aColor := $499AEE; // "tan2"  
      $454891B2: aColor := $3F85CD; // "tan3"  
      $454891B3: aColor := $2B5A8B; // "tan4"  
      $098972AF: aColor := $D8BFD8; // "thistle"  
      $CB823942: aColor := $FFE1FF; // "thistle1"  
      $CB823943: aColor := $EED2EE; // "thistle2"  
      $CB823944: aColor := $CDB5CD; // "thistle3"  
      $CB823945: aColor := $8B7B8B; // "thistle4"
      $7F38202A: aColor := $4763FF; // "tomato"
      $6EF9EA87: aColor := $4763FF; // "tomato1"
      $6EF9EA88: aColor := $425CEE; // "tomato2"
      $6EF9EA89: aColor := $394FCD; // "tomato3"
      $6EF9EA8A: aColor := $26368B; // "tomato4"
      $C318DDE1: aColor := $D0E040; // "turquoise"
      $E0FF9A90: aColor := $FFF500; // "turquoise1"
      $E0FF9A91: aColor := $EEE500; // "turquoise2"
      $E0FF9A92: aColor := $CDC500; // "turquoise3"
      $E0FF9A93: aColor := $8B8600; // "turquoise4"
      $766FF0FF: aColor := $EE82EE; // "violet"
      $F254AA90: aColor := $9020D0; // "violet red"
      $8155FEF2: aColor := $9020D0; // "VioletRed"
      $D31BBDBF: aColor := $963EFF; // "VioletRed1"
      $D31BBDC0: aColor := $8C3AEE; // "VioletRed2"
      $D31BBDC1: aColor := $7832CD; // "VioletRed3"
      $D31BBDC2: aColor := $52228B; // "VioletRed4"
      $ABC22D47: aColor := $B3DEF5; // "wheat"
      $721024AA: aColor := $BAE7FF; // "wheat1"
      $721024AB: aColor := $AED8EE; // "wheat2"
      $721024AC: aColor := $96BACD; // "wheat3"
      $721024AD: aColor := $667E8B; // "wheat4"
      $ADCD6FE9: aColor := $FFFFFF; // "white"
      $03FF8CB8: aColor := $F5F5F5; // "white smoke"
      $68AB35C6: aColor := $F5F5F5; // "WhiteSmoke"
      $1DB928F4: aColor := $00FFFF; // "yellow"
      $D2390877: aColor := $32CD9A; // "yellow green"
      $7985143D: aColor := $00FFFF; // "yellow1"
      $7985143E: aColor := $00EEEE; // "yellow2"
      $7985143F: aColor := $00CDCD; // "yellow3"
      $79851440: aColor := $008B8B; // "yellow4"
      $B949EDAF: aColor := $32CD9A; // "YellowGreen"
    else
      Exit;
    end;

    Result := True;
  end;
end;

type
  TMyINIFile = class(TINIFile)
  private
  protected
  public
    procedure WriteYesNo(const Section, Ident: String; Value: Boolean);
    procedure WriteColor(const Section, Ident: String; Value: COLORREF);
    procedure WriteGeometry(const Section, Ident: String; cW, cH: Integer);
    function ReadYesNo(const Section, Ident: String; Default: Boolean): Boolean;
    function ReadColor(const Section, Ident: String; Default: COLORREF): COLORREF;
    function ReadGeometry(const Section, Ident: String; var cW, cH: Integer): Boolean;
  end;

{ TMyINIFile }

procedure TMyINIFile.WriteYesNo(const Section, Ident: String; Value: Boolean);
begin
  if Value then
    WriteString(Section, Ident, 'yes')
  else
    WriteString(Section, Ident, 'no');
end;

procedure TMyINIFile.WriteColor(const Section, Ident: String; Value: COLORREF);
var
  ColorStr: String;
begin
  ColorStr := '#' + IntToHex(SwapColor(Value), 6);
  WriteString(Section, Ident, ColorStr);
end;

procedure TMyINIFile.WriteGeometry(const Section, Ident: String; cW, cH: Integer);
var
  GeometryStr: String;
begin
  GeometryStr := IntToStr(cW) + 'X' + IntToStr(cH);
  WriteString(Section, Ident, GeometryStr);
end;

function ReadYesNoStr(aYesNoStr: String; Default: Boolean): Boolean;
begin
  aYesNoStr:= LowerCase(aYesNoStr);
  if aYesNoStr = 'yes' then
    Result := true
  else
  if aYesNoStr = 'no' then
    Result := false
  else
    Result := Default;
end;

function TMyINIFile.ReadYesNo(const Section, Ident: String; Default: Boolean): Boolean;
begin
  Result := ReadYesNoStr(ReadString(Section, Ident, ''), Default);
end;

function TMyINIFile.ReadColor(const Section, Ident: String;
  Default: COLORREF): COLORREF;
var
  ColorStr: String;
begin
  ColorStr := ReadString(Section, Ident, '');

  if not LookupColor(ColorStr, Result) then
    Result := Default;
end;

function ReadGeometryStr(aGeometryStr: String; var cW, cH: Integer): Boolean;
var
  SaveCW, SaveCH, i: Integer;
begin
  Result := false;
  aGeometryStr := UpperCase(aGeometryStr);
  i := Pos('X', aGeometryStr);
  if i <= 0 then Exit;

  SaveCW := cW;
  SaveCH := cH;

  try
    cW := StrToInt(Copy(aGeometryStr, 1, i-1));
    cH := StrToInt(Copy(aGeometryStr, i+1, Length(aGeometryStr) -i));
  except
    cW := SaveCW;
    cH := SaveCH;
    Exit;
  end;
  
  Result := true;
end;

function TMyINIFile.ReadGeometry(const Section, Ident: String; var cW, cH: Integer): Boolean;
begin
  Result := ReadGeometryStr(ReadString(Section, Ident, ''), cW, cH);
end;

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
	fcolors[0]  := $000000;
	fcolors[1]  := $800000;
	fcolors[2]  := $008000;
	fcolors[3]  := $808000;
	fcolors[4]  := $000080;
	fcolors[5]  := $800080;
	fcolors[6]  := $008080;
	fcolors[7]  := $C0C0C0;
	fcolors[8]  := $808080;
	fcolors[9]  := $FF0000;
	fcolors[10] := $00FF00;
	fcolors[11] := $FFFF00;
	fcolors[12] := $0000FF;
	fcolors[13] := $FF00FF;
	fcolors[14] := $00FFFF;
	fcolors[15] := $FFFFFF;
	fcolorFg    := $C0C0C0; // cursor fg
	fcolorBg    := $010000; // cursor bg
	fcolorCursor    := $80C0C0;
	fcolorCursorIme := $0000C0;
	fscrollHide := false;
	fscrollRight := true;
	fsaveLines := 500;
	fborderSize := 1;
	flineSpace := 0;
	ftransp := 255;
  fTranspCtrl:= true;
  fTranspCtrlStep:= 10;

	fisTranspColor := false;
	ftranspColor := 0;
	fisTopMost := false;

  fFontSizeCtrl := true;
  fFontSizeCtrlStep := 2;
  fUseCtrl_C_Copy  := false;
  fUseCtrl_V_Paste := false;
  fSelectOverScreen := false;
end;

destructor TDkOpt.Destroy;
begin
{$IFDEF WriteINI}
  saveXdefaults();
{$ENDIF}

  inherited;
end;


const
  INISection = 'DKW';

procedure TDkOpt.loadXdefaults;
var
  ININame: String;
  INIFile: TMyINIFile;
  i: Integer;
begin
  ININame := ExtractFilePath(ParamStr(0)) + ExtractJustName(ParamStr(0)) + '.ini';
  //ShowMessage(ININame);

  INIFile:= TMyINIFile.Create(ININame);
  try
    fColorFg        := INIFile.ReadColor(INISection, 'foreground', fColorFg);
    fColorBg        := INIFile.ReadColor(INISection, 'background', fColorBg);
    fColorCursor    := INIFile.ReadColor(INISection, 'cursorColor', fColorCursor);
    fColorCursorIme := INIFile.ReadColor(INISection, 'cursorImeColor', fColorCursorIme);
    fBgBmp          := INIFile.ReadString(INISection, 'backgroundBitmap', fBgBmp);

    fTitle := INIFile.ReadString(INISection, 'title', fTitle);
    fCmd := INIFile.ReadString(INISection, 'exec', fCmd);
    fCurDir := INIFile.ReadString(INISection, 'chdir', fCurDir);

    fScrollHide  := INIFile.ReadYesNo(INISection, 'scrollHide', fScrollHide);
    fScrollRight := INIFile.ReadYesNo(INISection, 'scrollRight', fScrollRight);
    fBorderSize  := INIFile.ReadInteger(INISection, 'internalBorder', fBorderSize);
    fLineSpace   := INIFile.ReadInteger(INISection, 'lineSpace', fLineSpace);
    fIsTopMost   := INIFile.ReadYesNo(INISection, 'topmost', fIsTopMost);
    fTransp      := INIFile.ReadInteger(INISection, 'transp', fTransp);
    fTranspColor := INIFile.ReadColor(INISection, 'transpColor', fTranspColor);

    fFont        := INIFile.ReadString(INISection, 'font', fFont);
    fFontSize    := INIFile.ReadInteger(INISection, 'fontSize', fFontSize);
    INIFile.ReadGeometry(INISection, 'geometry', fWinCharW, fWinCharH);
    fSaveLines   := INIFile.ReadInteger(INISection, 'saveLines', fSaveLines);

    fFontSizeCtrl     := INIFile.ReadYesNo(INISection, 'fontSizeCtrl', fFontSizeCtrl);
    fFontSizeCtrlStep := INIFile.ReadInteger(INISection, 'fontSizeCtrlStep', fFontSizeCtrlStep);
    fUseCtrl_C_Copy   := INIFile.ReadYesNo(INISection, 'UseCtrl_C_Copy', fUseCtrl_C_Copy);
    fUseCtrl_V_Paste  := INIFile.ReadYesNo(INISection, 'UseCtrl_V_Paste', fUseCtrl_V_Paste);
    fSelectOverScreen := INIFile.ReadYesNo(INISection, 'SelectOverScreen', fSelectOverScreen);


    for i := Low(fColors) to High(fColors) do
      fColors[i] := INIFile.ReadColor(INISection, 'color' + IntToStr(i), fColors[i]);
  finally
    INIFile.Free;
  end;

end;

{$IFDEF WriteINI}
procedure TDkOpt.SaveXdefaults;
var
  ININame: String;
  INIFile: TMyINIFile;
  i: Integer;
begin
  ININame := ExtractFilePath(ParamStr(0)) + ExtractJustName(ParamStr(0)) + '.ini';

  INIFile:= TMyINIFile.Create(ININame);
  try
    INIFile.WriteColor(INISection, 'foreground', fColorFg);
    INIFile.WriteColor(INISection, 'background', fColorBg);
    INIFile.WriteColor(INISection, 'cursorColor', fColorCursor);
    INIFile.WriteColor(INISection, 'cursorImeColor', fColorCursorIme);
    INIFile.WriteString(INISection, 'backgroundBitmap', fBgBmp);

    INIFile.WriteString(INISection, 'title', fTitle);
    INIFile.WriteString(INISection, 'exec', fCmd);
    INIFile.WriteString(INISection, 'chdir', fCurDir);

    INIFile.WriteYesNo(INISection, 'scrollHide', fScrollHide);
    INIFile.WriteYesNo(INISection, 'scrollRight', fScrollRight);
    INIFile.WriteInteger(INISection, 'internalBorder', fBorderSize);
    INIFile.WriteInteger(INISection, 'lineSpace', fLineSpace);
    INIFile.WriteYesNo(INISection, 'topmost', fIsTopMost);
    INIFile.WriteInteger(INISection, 'transp', fTransp);
    INIFile.WriteColor(INISection, 'transpColor', fTranspColor);

    INIFile.WriteString(INISection, 'font', fFont);
    INIFile.WriteInteger(INISection, 'fontSize', fFontSize);
    INIFile.WriteGeometry(INISection, 'geometry', fWinCharW, fWinCharH);
    INIFile.WriteInteger(INISection, 'saveLines', fSaveLines);

    INIFile.WriteYesNo(INISection, 'fontSizeCtrl', fFontSizeCtrl);
    INIFile.WriteInteger(INISection, 'fontSizeCtrlStep', fFontSizeCtrlStep);

    INIFile.WriteYesNo(INISection, 'UseCtrl_C_Copy', fUseCtrl_C_Copy);
    INIFile.WriteYesNo(INISection, 'UseCtrl_V_Paste', fUseCtrl_V_Paste);
    INIFile.WriteYesNo(INISection, 'SelectOverScreen', fSelectOverScreen);

    for i := Low(fColors) to High(fColors) do
      INIFile.WriteColor(INISection, 'color' + IntToStr(i), fColors[i]);
  finally
    INIFile.Free;
  end;
end;
{$ENDIF}


function TDkOpt.setArgs: BOOL;
var
  i, j: Integer;
  str, name, value: AnsiString;

  NameHash: LongWord;
begin
  //ShowMessage(IntToStr(ParamCount()));
  //ShowMessage(ParamStr(0));

  for i := 1 to ParamCount() do
  begin
    str := ParamStr(i);
    j := Pos('=', str);
    if j <= 0 then Continue;

    name  := Copy(str, 1, j-1);
    value := Copy(str, j+1, Length(str) -j);

    NameHash := MemHash3(PChar(LowerCase(name)), Length(name));

    case NameHash of
      $DF292A83: LookupColor(value, fColorFg);        // foreground
      $055DEA4E: LookupColor(value, fColorBg);        // background
      $5F03534D: LookupColor(value, fColorCursor);    // cursorColor
      $0D4A1B18: LookupColor(value, fColorCursorIme); // cursorImeColor
      $4D080F9D: fBgBmp := value;                     // backgroundBitmap

      $15F1CC38: fTitle := value; // title
      $96ACB911: fCmd   := trim(value); // exec
      $5CD4B3A8: fCurDir:= trim(value) ; // chdir

      $E4BAAB2F: fScrollHide  := ReadYesNoStr(value, fScrollHide);  // scrollHide
      $9DD8064F: fScrollRight := ReadYesNoStr(value, fScrollRight); // scrollRight
      $EF7C2249: fBorderSize  := StrToIntDef(value, fBorderSize);   // internalBorder
      $C563E2B2: fLineSpace   := StrToIntDef(value, fLineSpace);    // lineSpace
      $669E8438: fIsTopMost   := ReadYesNoStr(value, fIsTopMost);   // topmost
      $23113D88: fTransp      := StrToIntDef(value, fTransp);       // transp
      $136992DB: LookupColor(value, fTranspColor); // transpColor
      
      $C0CE008F: fFont      := value; // font
      $3E9551B0: fFontSize  := StrToIntDef(value, fFontSize);   // fontSize
      $38B2B8D2: ReadGeometryStr(value, fWinCharW, fWinCharH);  // geometry
      $092476A2: fSaveLines := StrToIntDef(value, fSaveLines);  // saveLines

      $3DD97E7B: fFontSizeCtrl     := ReadYesNoStr(value, fFontSizeCtrl);     // fontSizeCtrl
      $DE410907: fFontSizeCtrlStep := StrToIntDef(value, fFontSizeCtrlStep);  // fontSizeCtrlStep
      $ADF60C7E: fUseCtrl_C_Copy   := ReadYesNoStr(value, fUseCtrl_C_Copy);   // UseCtrl_C_Copy
      $AA515CDD: fUseCtrl_V_Paste  := ReadYesNoStr(value, fUseCtrl_V_Paste);  // UseCtrl_V_Paste
      $1A67F87C: fSelectOverScreen := ReadYesNoStr(value, fSelectOverScreen); // SelectOverScreen

      $DEA1B28D: LookupColor(value, fcolors[0]); // color0
      $DEA1B28E: LookupColor(value, fcolors[1]); // color1
      $DEA1B28F: LookupColor(value, fcolors[2]); // color2
      $DEA1B290: LookupColor(value, fcolors[3]); // color3
      $DEA1B291: LookupColor(value, fcolors[4]); // color4
      $DEA1B292: LookupColor(value, fcolors[5]); // color5
      $DEA1B293: LookupColor(value, fcolors[6]); // color6
      $DEA1B294: LookupColor(value, fcolors[7]); // color7

      $DEA1B295: LookupColor(value, fcolors[8]); // color8
      $DEA1B296: LookupColor(value, fcolors[9]); // color9
      $7C58F122: LookupColor(value, fcolors[10]); // color10
      $7C58F123: LookupColor(value, fcolors[11]); // color11
      $7C58F124: LookupColor(value, fcolors[12]); // color12
      $7C58F125: LookupColor(value, fcolors[13]); // color13
      $7C58F126: LookupColor(value, fcolors[14]); // color14
      $7C58F127: LookupColor(value, fcolors[15]); // color15
    end;
    //ShowMessage(str + ', ' + name + ': ' + value);
  end;




  Result := true;

  
end;

function TDkOpt.getColor(i: Integer): COLORREF;
begin
  if (0 <= i) and (i <= 15) then
    Result := fColors[i]
  else
    Result := fColors[0];
end;


procedure WriteFontSize(const aFontSize: Integer);
var
  ININame: String;
  INIFile: TMyINIFile;
begin
  ININame := ExtractFilePath(ParamStr(0)) + ExtractJustName(ParamStr(0)) + '.ini';

  INIFile:= TMyINIFile.Create(ININame);
  try
    INIFile.WriteInteger(INISection, 'fontSize', aFontSize);
  finally
    INIFile.Free;
  end;
end;

initialization

finalization

end.


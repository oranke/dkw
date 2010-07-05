{-----------------------------------------------------------------------------
 Project Name: "DKW. Delphi porting of CKW."
 Author:    oranke_f
 Date:      2010-7-01
 Purpose:
 History:
  2010-07-05
    일차적인 포팅 마무리. 일단 입출력 제대로 되는 부분까지만 확인.  

-----------------------------------------------------------------------------}

{.$APPTYPE CONSOLE}

program dkw;

uses
  //FastMM4,
  SysUtils,
  selection in 'selection.pas',
  misc in 'misc.pas',
  main in 'main.pas';

{.$R *.res}
{$R 'Resource.res' 'Resource.rc'}

begin
  WinMain;
  //Application.Initialize;
  //Application.Run;
end.

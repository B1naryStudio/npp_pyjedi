{
    Delphi Foundation for creating plugins for Notepad++
    (Short: DFPN++)

    Copyright (C) 2009 Bastian Blumentritt

    This file is part of DFPN++.

    DFPN++ is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    DFPN++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with DFPN++.  If not, see <http://www.gnu.org/licenses/>.
}

unit nppp_scifuncs;

interface

uses
  Windows,
  Vcl.Imaging.pngimage,
  nppp_types;

type
  TNPPSciFunctions = class(TObject)
  private
    FNPPHandle: HWND;
    FSci1Handle: HWND;
    FSci2Handle: HWND;
    FSciCurrentHandle: HWND;
    FNestedUpdateCalls: Cardinal;
    function CallScintilla(msg: Cardinal; const wpar: NativeUInt;
      const lpar: NativeInt): NativeInt;
  protected
  public
    constructor Create;
    procedure beginUpdate;
    procedure endUpdate;
    procedure setNPPHandle(pHandle: HWND);
    procedure setSci1Handle(pHandle: HWND);
    procedure setSci2Handle(pHandle: HWND);
    function getCurrentSciHandle: HWND;
    //position ans selection
    function getCurrentPosition: NativeInt;
    function getWordStartPosition(cursorPosition: NativeInt;
      onlyWordChars: boolean): NativeInt;
    function getWordEndPosition(cursorPosition: NativeInt;
      onlyWordChars: boolean): NativeInt;
    function getLineFromPosition(position: NativeInt): NativeInt;
    function getLineStartPosition(line: NativeInt): NativeInt;
    function getLineEndPosition(line: NativeInt): NativeInt;
    function getCurrentLine: Integer;
    function getSelectedText: nppString;
    function getSelectionEnd: Integer;
    function getSelectionStart: Integer;
    function getText(length: NativeInt = 0): nppString;
    function getTextRange(startP, endP: NativeInt): string;
    procedure goToLine(pLineNo: NativeInt);
    procedure goToPosition(position: NativeInt);
    procedure replaceSelection(const pWith: nppString);
    procedure setSelectionEnd(pPos: Integer);
    procedure setSelectionStart(pPos: Integer);
    //autocomplete stuff
    procedure autoSetListSeparator(separatorChar: byte);
    function autoGetListSeparator: byte;
    function autoIsListShowing: boolean;
    procedure autoClearImages;
    procedure autoRegisterRGBAImage(image: TPngImage; imageIndex: integer);
    procedure autoSetSortOrder(order: TSciAutoListSortOrder);
    function autoGetSortOrder: TSciAutoListSortOrder;
    procedure autoSetDropRestOfWord(drop: boolean);
    function autoGetDropRestOfWord: Boolean;
    procedure autoShowList(position: NativeInt; list: string);
    //calltip
    procedure callTipSetAutoTimeout(milliseconds: NativeInt);
    procedure callTipShow(position: NativeInt; const text: string);
    procedure callTipCancel;
    function callTipIsShowing: Boolean;
    procedure callTipHighLight(startPos, endPos: NativeInt);
    procedure callTipSetHighLightFG(color: NativeInt);
    procedure callTipSetPosition(above: Boolean);
    //misc
    procedure ScintillaSetFocus();
    //cursor pointer
    procedure setCursor(cursor: TSciCursorPointer);
  end;

implementation

uses
  System.SysUtils, nppp_consts, Winapi.Messages;

{ TNPPBaseFunctions }

function TNPPSciFunctions.CallScintilla(msg: Cardinal; const wpar: NativeUInt;
  const lpar: NativeInt): NativeInt;
begin
  Result := SendMessage(getCurrentSciHandle, msg, wpar, lpar);
end;

procedure TNPPSciFunctions.callTipCancel;
begin
  CallScintilla(SCI_CALLTIPCANCEL, 0, 0);
end;

procedure TNPPSciFunctions.callTipHighLight(startPos, endPos: NativeInt);
begin
  CallScintilla(SCI_CALLTIPSETHLT, startPos, endPos);
end;

function TNPPSciFunctions.callTipIsShowing: Boolean;
begin
  Result := CallScintilla(SCI_CALLTIPACTIVE, 0, 0) = 1;
end;

procedure TNPPSciFunctions.callTipSetAutoTimeout(milliseconds: NativeInt);
begin
  CallScintilla(SCI_SETMOUSEDWELLTIME, milliseconds, 0);
end;

procedure TNPPSciFunctions.callTipSetHighLightFG(color: NativeInt);
begin
  CallScintilla(SCI_CALLTIPSETFOREHLT, color, 0);
end;

procedure TNPPSciFunctions.callTipSetPosition(above: Boolean);
begin
  CallScintilla(SCI_CALLTIPSETPOSITION, NativeInt(above), 0);
end;

procedure TNPPSciFunctions.callTipShow(position: NativeInt; const text: string);
begin
  CallScintilla(SCI_CALLTIPSHOW, position, NativeInt(PAnsiChar(UTF8Encode(text))));
end;

constructor TNPPSciFunctions.Create;
begin
  inherited;
  FNPPHandle := 0;
  FSci1Handle := 0;
  FSci2Handle := 0;
  FSciCurrentHandle := 0;
  FNestedUpdateCalls := 0;
end;

procedure TNPPSciFunctions.setCursor(cursor: TSciCursorPointer);
begin
  CallScintilla(SCI_SETCURSOR, NativeInt(cursor), 0);
end;

procedure TNPPSciFunctions.setNPPHandle(pHandle: HWND);
begin
  FNPPHandle := pHandle;
end;

procedure TNPPSciFunctions.setSci1Handle(pHandle: HWND);
begin
  FSci1Handle := pHandle;
end;

procedure TNPPSciFunctions.setSci2Handle(pHandle: HWND);
begin
  FSci2Handle := pHandle;
end;

function TNPPSciFunctions.getCurrentSciHandle: HWND;
var
  tP: PINT;
begin
  if FSciCurrentHandle > 0 then
    Result := FSciCurrentHandle
  else
  begin
    tP := new(PINT);

    SendMessage(FNPPHandle, NPPM_GETCURRENTSCINTILLA, 0, LPARAM(tP));
    if tP^ = 0 then
      Result := FSci1Handle
    else
      Result := FSci2Handle;

    Dispose(tP);
  end;
end;

function TNPPSciFunctions.getLineEndPosition(line: NativeInt): NativeInt;
begin
  Result := CallScintilla(SCI_GETLINEENDPOSITION, line, 0);
end;

function TNPPSciFunctions.getLineFromPosition(position: NativeInt): NativeInt;
begin
  Result := CallScintilla(SCI_LINEFROMPOSITION, position, 0)
end;

function TNPPSciFunctions.getLineStartPosition(line: NativeInt): NativeInt;
begin
  Result := CallScintilla(SCI_POSITIONFROMLINE, line, 0);
end;

procedure TNPPSciFunctions.autoClearImages;
begin
  CallScintilla(SCI_CLEARREGISTEREDIMAGES, 0, 0);
end;

function TNPPSciFunctions.autoGetDropRestOfWord: Boolean;
begin
  Result := Boolean(CallScintilla(SCI_AUTOCGETDROPRESTOFWORD, 0, 0));
end;

function TNPPSciFunctions.autoGetListSeparator: byte;
begin
  Result := CallScintilla(SCI_AUTOCGETSEPARATOR, 0, 0);
end;

function TNPPSciFunctions.autoGetSortOrder: TSciAutoListSortOrder;
begin
  Result := TSciAutoListSortOrder(CallScintilla(SCI_AUTOCGETORDER, 0, 0));
end;

function TNPPSciFunctions.autoIsListShowing: boolean;
begin
  Result := CallScintilla(SCI_AUTOCACTIVE, 0, 0) <> 0;
end;

procedure GetImageAsRGBA(png: TPngImage; out rgbaArray: TBytes);
var
  i, j: Integer;
  lineRGB: pRGBLine;
  lineAlpha: Vcl.Imaging.pngimage.pByteArray;
  pCursor: PByte;
begin
  SetLength(rgbaArray, png.Width * png.Height * 4);
  pCursor := pByte(rgbaArray);
  for i := 0 to png.Height - 1 do
  begin
    lineRGB := png.Scanline[i];
    lineAlpha := png.AlphaScanline[i];
    for j := 0 to png.Width - 1 do
    begin
      pCursor^ := lineRGB[j].rgbtRed;
      Inc(pCursor);
      pCursor^ := lineRGB[j].rgbtGreen;
      Inc(pCursor);
      pCursor^ := lineRGB[j].rgbtBlue;
      Inc(pCursor);
      pCursor^ := lineAlpha[j];
      Inc(pCursor);
    end;
  end;
end;

procedure TNPPSciFunctions.autoRegisterRGBAImage(image: TPngImage;
  imageIndex: integer);
var
  rgbaArray: TBytes;
begin
  GetImageAsRGBA(image, rgbaArray);
  CallScintilla(SCI_RGBAIMAGESETWIDTH, image.Width, 0);
  CallScintilla(SCI_RGBAIMAGESETHEIGHT, image.Height, 0);
  CallScintilla(SCI_REGISTERRGBAIMAGE, imageIndex, NativeInt(pByte(rgbaArray)));
end;

procedure TNPPSciFunctions.autoSetDropRestOfWord(drop: boolean);
begin
  CallScintilla(SCI_AUTOCSETDROPRESTOFWORD, NativeInt(drop), 0);
end;

procedure TNPPSciFunctions.autoSetListSeparator(separatorChar: byte);
begin
  CallScintilla(SCI_AUTOCSETSEPARATOR, NativeInt(separatorChar), 0);
end;

procedure TNPPSciFunctions.autoSetSortOrder(order: TSciAutoListSortOrder);
begin
  CallScintilla(SCI_AUTOCSETORDER, NativeInt(order), 0);
end;

procedure TNPPSciFunctions.autoShowList(position: NativeInt; list: string);
begin
  CallScintilla(SCI_AUTOCSHOW, position,
    NativeInt(PAnsiChar(UTF8Encode(list))));
end;

procedure TNPPSciFunctions.beginUpdate;
begin
  Inc(FNestedUpdateCalls);
  if (FSciCurrentHandle = 0) then
    FSciCurrentHandle := getCurrentSciHandle;
end;

procedure TNPPSciFunctions.endUpdate;
begin
  if FNestedUpdateCalls <= 1 then
    FSciCurrentHandle := 0;
  if FNestedUpdateCalls > 0 then
    Dec(FNestedUpdateCalls);
end;

function TNPPSciFunctions.getCurrentLine: Integer;
var
  r: Integer;
begin
  beginUpdate;
  r := SendMessage(getCurrentSciHandle, SCI_GETCURRENTPOS, 0, 0);
  Result := SendMessage(getCurrentSciHandle, SCI_LINEFROMPOSITION, r, 0);
  endUpdate;
end;

function TNPPSciFunctions.getCurrentPosition: NativeInt;
begin
  Result := CallScintilla(SCI_GETCURRENTPOS, 0, 0);
end;

function TNPPSciFunctions.getSelectedText: nppString;
var
  tR: Integer;
  tS: String;
begin
  beginUpdate;
  // determine selection length first
  // -> correctly returns selection length+1
  tR := SendMessage(getCurrentSciHandle, SCI_GETSELTEXT, 0, 0);
  SetLength(tS, tR);
  SendMessage(getCurrentSciHandle, SCI_GETSELTEXT, 0, LPARAM(PChar(tS)));

  Result := PChar(tS);

  endUpdate;
end;

function TNPPSciFunctions.getSelectionEnd: Integer;
begin
  Result := SendMessage(getCurrentSciHandle, SCI_GETSELECTIONEND, 0, 0);
end;

function TNPPSciFunctions.getSelectionStart: Integer;
begin
  Result := SendMessage(getCurrentSciHandle, SCI_GETSELECTIONSTART, 0, 0);
end;

/// This function returns the complete document content as string.
function TNPPSciFunctions.getText(length: NativeInt = 0): nppString;
var
  tR: NativeInt;
  tS: RawByteString;
begin
  beginUpdate;
  if length <= 0 then
    tR := SendMessage(getCurrentSciHandle, SCI_GETTEXTLENGTH, 0, 0)
  else
    tR := length;
  Inc(tR);
  SetLength(tS, tR);
  SendMessage(getCurrentSciHandle, SCI_GETTEXT, tR, LPARAM(PAnsiChar(tS)));
  SetCodePage(tS, 65001, false);
  Result := string(PAnsiChar(tS));

  endUpdate;
end;

function TNPPSciFunctions.getTextRange(startP, endP: NativeInt): string;
var
  tr: TTextRange;
  buffer: RawByteString;
begin
  tr.chrg.cpMin := startP;
  tr.chrg.cpMax := endP;
  SetLength(buffer, endP - startP);
  tr.lpstrText := PAnsiChar(buffer);
  CallScintilla(SCI_GETTEXTRANGE, 0, NativeInt(@tr));
  SetCodePage(buffer, 65001, False);
  Result := string(PAnsiChar(buffer));
end;

function TNPPSciFunctions.getWordEndPosition(cursorPosition: NativeInt;
  onlyWordChars: boolean): NativeInt;
begin
  Result := CallScintilla(SCI_WORDENDPOSITION, cursorPosition, NativeInt(onlyWordChars));
end;

function TNPPSciFunctions.getWordStartPosition(cursorPosition: NativeInt;
  onlyWordChars: boolean): NativeInt;
begin
  Result := CallScintilla(SCI_WORDSTARTPOSITION, cursorPosition, NativeInt(onlyWordChars));
end;

/// This removes any selection and sets the caret at the start of line
/// number /pLineNo/ and scrolls the view (if needed) to make it visible.
/// The anchor position is set the same as the current position. If /pLineNo/
/// is outside the lines in the document (first line is 0), the line set is
/// the first or last.
procedure TNPPSciFunctions.goToLine(pLineNo: NativeInt);
begin
  SendMessage(getCurrentSciHandle, SCI_GOTOLINE, pLineNo, 0);
end;

procedure TNPPSciFunctions.goToPosition(position: NativeInt);
begin
  CallScintilla(SCI_GOTOPOS, position, 0);
end;

procedure TNPPSciFunctions.replaceSelection(const pWith: nppString);
begin
  SendMessage(getCurrentSciHandle, SCI_REPLACESEL, 0, LPARAM(PChar(String(pWith))));
end;

procedure TNPPSciFunctions.ScintillaSetFocus;
begin
  SendMessage(getCurrentSciHandle, WM_SETFOCUS, 0, 0);
end;

procedure TNPPSciFunctions.setSelectionEnd(pPos: Integer);
begin
  SendMessage(getCurrentSciHandle, SCI_SETSELECTIONEND, pPos, 0);
end;

procedure TNPPSciFunctions.setSelectionStart(pPos: Integer);
begin
  SendMessage(getCurrentSciHandle, SCI_SETSELECTIONSTART, pPos, 0);
end;

end.

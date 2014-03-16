///<summary> This is an extract from u_dzVclUtils which is part of dzlib available from sourceforge
///          under the MPL 1.1 </summary>
unit u_dzVclUtils;

interface

uses
  Windows,
  Messages,
  Controls,
  ComCtrls,
  Forms,
  ExtCtrls, Classes;

///<summary> Enables longer SimpleText (longer than 127 characters)
///          Call once to enable. Works, by adding a single panel with owner drawing and
///          setting the StatusBar's OnDrawPanel to a custom drawing method.
///          To make it work, you must use TStatusBar_SetLongSimpleText to set
///          the text, or use TLongSimpleTextStatusBar as an interposer class. </summary>
procedure TStatusBar_EnableLongSimpleText(_StatusBar: TStatusBar);

///<summary> Set the SimpleText of the StatusBar and invalidate it to enforce a redraw </summary>
procedure TStatusBar_SetLongSimpleText(_StatusBar: TStatusBar; const _Text: string);

///<summary> returns the full path of the executable (without the filename but including
///          a backslash) </summary>
function TApplication_GetExePath: string;

type
  TFormPlacementEnum = (fpePositionOnly, fpeSizeOnly, fpePosAndSize);

///<summary> Stores the form's current position and size to the registry
///          @param frm is the form whose placement is to be stored
///          @param Which determines whether the Position and/or the size is to be stored
///          @param RegistryPath gives the full path, including the value name to write to,
///                              not yet implemented: defaults to '<Company_name>\<executable>\<frm.Classname>\NormPos'
///                              where <Company_name> is read from the version resources
///                              and <frm.Classname is the form's ClassName without the T-Prefix
///          @param HKEY is the root key, defaults to HKEY_CURRENT_USER
///          @returns false, if anything goes wrong, including any exceptions that might
///                          occur, true if it worked.
function TForm_StorePlacement(_frm: TForm; _Which: TFormPlacementEnum; const _RegistryPath: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean; overload;

///<summary> Stores the form's current position and size to the registry
///          under Software\<executable>\<frm.name>\NormPos'
///          @param frm is the form whose placement is to be stored
///          @param Which determines whether the Position and/or the size is to be stored
///          @param HKEY is the root key, defaults to HKEY_CURRENT_USER
///          @returns false, if anything goes wrong, including any exceptions that might
///                          occur, true if it worked.
function TForm_StorePlacement(_frm: TForm; _Which: TFormPlacementEnum;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean; overload;

///<summary> Reads the form's position and size from the registry
///          @param frm is the form whose placement is to be read
///          @param Which determines whether the Position and/or the size is to be read
///          @param RegistryPath gives the full path, including the value name to write to,
///                              not yet implemented: defaults to '<Company_name>\<executable>\<frm.Classname>\NormPos'
///                              where <Company_name> is read from the version resources
///                              and <frm.Classname is the form's ClassName without the T-Prefix
///          @param HKEY is the root key, defaults to HKEY_CURRENT_USER
///          @returns false, if anything goes wrong, including any exceptions that might
///                          occur, true if it worked.
function TForm_ReadPlacement(_frm: TForm; _Which: TFormPlacementEnum; const _RegistryPath: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean; overload;

///<summary> Reads the form's position and size from the registry
///          under Software\<executable>\<frm.name>\NormPos'
///          @param frm is the form whose placement is to be stored
///          @param Which determines whether the Position and/or the size is to be stored
///          @param HKEY is the root key, defaults to HKEY_CURRENT_USER
///          @returns false, if anything goes wrong, including any exceptions that might
///                          occur, true if it worked.
function TForm_ReadPlacement(_frm: TForm; _Which: TFormPlacementEnum;
  const _HKEY: HKEY = HKEY_CURRENT_USER): Boolean; overload;

///<summary>
/// In Windows Vista and later it uses TFileOpenDialog, in XP it calls
/// SelectDirectory.
/// @param Directory on input is the default directory to use, on output
///                  it is the new directory selected by the user, only if
///                  Result = true.
/// @returns true, if the user selected a directory
function dzSelectDirectory(var _Directory: string; _Owner: TWinControl = nil): boolean;

type
  TdzButtonedEdit = class(TButtonedEdit)
  protected
    procedure KeyDown(var _Key: Word; _Shift: TShiftState); override;
  public
    procedure Loaded; override;
  end;

implementation

uses
  SysUtils,
  FileCtrl,
  Types,
  Graphics,
  Dialogs,
  u_dzClassUtils,
  gnugettext, u_dzStringUtils;

type
  // Note: This class is never instantiated, only the DrawPanel method will be used
  //       without ever referencing the self pointer (which is NIL), so it should work
  TStatusBarPainter = class
  public
    procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
  end;

procedure TStatusBarPainter.DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
var
  cnv: TCanvas;
  s: string;
  h: Integer;
begin
  s := StatusBar.SimpleText;
  cnv := StatusBar.Canvas;
  h := cnv.TextHeight(s);
  cnv.TextRect(Rect, 2, (StatusBar.ClientHeight - h) div 2, StatusBar.SimpleText);
end;

procedure TStatusBar_EnableLongSimpleText(_StatusBar: TStatusBar);
var
  Painter: TStatusBarPainter;
  pnl: TStatusPanel;
begin
  _StatusBar.SimplePanel := false;
  _StatusBar.Panels.Clear;
  pnl := _StatusBar.Panels.Add;
  pnl.Style := psOwnerDraw;
  Painter := nil;
  _StatusBar.OnDrawPanel := Painter.DrawPanel;
end;

procedure TStatusBar_SetLongSimpleText(_StatusBar: TStatusBar; const _Text: string);
begin
  _StatusBar.SimpleText := _Text;
  _StatusBar.Invalidate;
end;

{ TdzButtonedEdit }

procedure TdzButtonedEdit.KeyDown(var _Key: Word; _Shift: TShiftState);
begin
  inherited;
  if (_Key = VK_RETURN) and (ssCtrl in _Shift) then
    OnRightButtonClick(Self);
end;

procedure TdzButtonedEdit.Loaded;
begin
  inherited;
  if RightButton.Visible and (RightButton.Hint = '') then begin
    RightButton.Hint := _('Ctrl+Return to ''click'' right button.');
    ShowHint := true;
  end;
end;

function TApplication_GetExePath: string;
begin
  Result := ExtractFilePath(Application.Exename);
end;

function TForm_StorePlacement(_frm: TForm; _Which: TFormPlacementEnum; const _RegistryPath: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;
var
  L, t, w, h: Integer;
begin
  try
    L := _frm.Left;
    t := _frm.Top;
    w := _frm.Width;
    h := _frm.Height;
    case _Which of
      fpePositionOnly: begin
          w := -1;
          h := -1;
        end;
      fpeSizeOnly: begin
          L := -1;
          t := -1;
        end;
      // fpePosAndSize: ;
    end;
    TRegistry_WriteString(_RegistryPath, Format('%d,%d,%d,%d', [L, t, w, h]));
    Result := True;
  except
    Result := False;
  end;
end;

function TForm_StorePlacement(_frm: TForm; _Which: TFormPlacementEnum;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;
begin
  Result := TForm_StorePlacement(_frm, _Which,
    'Software\' + ChangeFileExt(ExtractFileName(Application.ExeName), '') + '\' + _frm.Name
    + '\NormPos', _HKEY);
end;

function TForm_ReadPlacement(_frm: TForm; _Which: TFormPlacementEnum; const _RegistryPath: string;
  _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;
var
  s: string;
  PosStr: string;
  L, t, w, h: Integer;
begin
  try
    Result := TRegistry_TryReadString(_RegistryPath, PosStr, _HKEY);
    if Result then begin
      s := ExtractStr(PosStr, ',');
      if not TryStrToInt(s, L) then
        Exit;
      s := ExtractStr(PosStr, ',');
      if not TryStrToInt(s, t) then
        Exit;
      s := ExtractStr(PosStr, ',');
      if not TryStrToInt(s, w) then
        Exit;
      s := PosStr;
      if not TryStrToInt(s, h) then
        Exit;

      case _Which of
        fpePositionOnly: begin
            if L <> -1 then
              _frm.Left := L;
            if t <> -1 then
              _frm.Top := t;
          end;
        fpeSizeOnly: begin
            if w <> -1 then
              _frm.Width := w;
            if h <> -1 then
              _frm.Height := h;
          end;
        fpePosAndSize: begin
            if L <> -1 then
              _frm.Left := L;
            if t <> -1 then
              _frm.Top := t;
            if w <> -1 then
              _frm.Width := w;
            if h <> -1 then
              _frm.Height := h;
          end;
      else
        Exit;
      end;
      _frm.Position := poDesigned;
      _frm.MakeFullyVisible(nil);
    end;
  except
    Result := False;
  end;
end;

function TForm_ReadPlacement(_frm: TForm; _Which: TFormPlacementEnum;
  const _HKEY: HKEY = HKEY_CURRENT_USER): Boolean;
begin
  Result := TForm_ReadPlacement(_frm, _Which,
    'Software\' + ChangeFileExt(ExtractFileName(Application.ExeName), '') + '\' + _frm.Name
    + '\NormPos', _HKEY);
end;

function dzSelectDirectory(var _Directory: string; _Owner: TWinControl = nil): boolean;
var
  fod: TFileOpenDialog;
begin
  if Win32MajorVersion >= 6 then begin
    // only available on Vista and later
    fod := TFileOpenDialog.Create(_Owner);
    try
      fod.Title := _('Select Directory');
      fod.Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
      fod.OkButtonLabel := _('Select');
      fod.DefaultFolder := _Directory;
      fod.FileName := _Directory;
      Result := fod.Execute;
      if Result then
        _Directory := fod.FileName;
    finally
      fod.Free;
    end
  end else begin
    Result := FileCtrl.SelectDirectory('Select Directory', ExtractFileDrive(_Directory),
      _Directory, [sdNewUI, sdNewFolder], _Owner);
  end;
end;

end.


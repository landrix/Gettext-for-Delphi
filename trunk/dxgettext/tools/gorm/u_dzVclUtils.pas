///<summary> This is an extract from u_dzVclUtils which is part of dzlib available from sourceforge
///          under the MPL 1.1 </summary>
unit u_dzVclUtils;

interface

uses
  Windows,
  Classes,
  Messages,
  Controls,
  ComCtrls,
  ComObj,
  StdCtrls,
  Forms,
  ExtCtrls;

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

type
  TOnFilesDropped = procedure(_Sender: TObject; _Files: TStrings) of object;

function TForm_ActivateDropFiles(_WinCtrl: TWinControl; _Callback: TOnFilesDropped): TObject;

///<summary>
/// In Windows Vista and later it uses TFileOpenDialog, in XP it calls
/// SelectDirectory.
/// @param Directory on input is the default directory to use, on output
///                  it is the new directory selected by the user, only if
///                  Result = true.
/// @returns true, if the user selected a directory
function dzSelectDirectory(var _Directory: string; _Owner: TWinControl = nil): Boolean;

type
  TErrorHandlingEnum = (ehReturnFalse, ehRaiseException);
type
  TAutoCompleteSourceEnum = (acsFileSystem, acsUrlHistory, acsUrlMru);
  TAutoCompleteSourceEnumSet = set of TAutoCompleteSourceEnum;
type
  TAutoCompleteTypeEnum = (actSuggest, actAppend);
  TAutoCompleteTypeEnumSet = set of TAutoCompleteTypeEnum;

///<summary>
/// Enables autocompletion for an edit control using a call to SHAutoComplete
/// @param ed is a TCustomEdit control for which autocompletion should be enabled
/// @param Source is a set of TAutoCompleteSourceEnum that determines from which source to
///               autocomplate, any combination of acsFileSystem, acsUrlHistory, acsUrlMru
///               is allowed.
/// @param Type is a set fo TAutoCompleteEnumSet that determines the type of autocompletion
///             actAppend means that the first match will be appended to the existing text and
///                       selected, so that typing anything will automatically overwrite it.
///             actSuggest means that a list of matches will be displayed as a dropdown list
///                        from which the user can then select using the arrow keys or the mouse.
///             is is possible to pass an empty set, in which case the registry setting.
///             (Unfortunately MSDN doesn't say where in the registry this setting is located.)
/// @param ErrorHandling determines whether a failure in the SHAutoComplete call should raise
///                      an EOLEException or return false. </summary>
function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = [actSuggest, actAppend];
  _ErrorHandling: TErrorHandlingEnum = ehReturnFalse): Boolean;

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
  ShellApi,
  gnugettext,
  u_dzClassUtils,
  u_dzStringUtils;

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
  _StatusBar.SimplePanel := False;
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
    ShowHint := True;
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
    'Software\' + ChangeFileExt(ExtractFileName(Application.Exename), '') + '\' + _frm.Name
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
    'Software\' + ChangeFileExt(ExtractFileName(Application.Exename), '') + '\' + _frm.Name
    + '\NormPos', _HKEY);
end;

type
  TDropFilesActivator = class(TObject)
  private
    FCtrl: TWinControl;
    FCallback: TOnFilesDropped;
    FOldWindowProc: TWndMethod;
    procedure NewWindowProc(var _Msg: TMessage);
    procedure WmDropFiles(var _Msg: TMessage);
    procedure doCallback(_st: TStrings);
  public
    constructor Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
    destructor Destroy; override;
  end;

{ TDropFilesActivator }

constructor TDropFilesActivator.Create(_WinControl: TWinControl; _Callback: TOnFilesDropped);
begin
  inherited Create;
  FCtrl := _WinControl;

  FCallback := _Callback;
  FOldWindowProc := FCtrl.WindowProc;
  FCtrl.WindowProc := NewWindowProc;
  DragAcceptFiles(FCtrl.Handle, True);
end;

destructor TDropFilesActivator.Destroy;
begin
  if Assigned(FCtrl) and Assigned(FOldWindowProc) then begin
    FCtrl.WindowProc := FOldWindowProc;
    DragAcceptFiles(FCtrl.Handle, False);
  end;
  inherited;
end;

procedure TDropFilesActivator.doCallback(_st: TStrings);
begin
  if Assigned(FCallback) then
    FCallback(FCtrl, _st);
end;

procedure TDropFilesActivator.NewWindowProc(var _Msg: TMessage);
begin
  if _Msg.Msg = WM_NCCREATE then
    DragAcceptFiles(FCtrl.Handle, True);

  if _Msg.Msg = WM_NCDESTROY then
    DragAcceptFiles(FCtrl.Handle, False);

  if _Msg.Msg = WM_DROPFILES then
    WmDropFiles(_Msg);

  // call original WindowProc method to handle all other messages
  FOldWindowProc(_Msg);
end;

procedure TDropFilesActivator.WmDropFiles(var _Msg: TMessage);
var
  arr: array[0..255] of Char;
  fn: string;
  i: Integer;
  sl: TStringList;
  cnt: Cardinal;
begin
  sl := TStringList.Create;
  try
    cnt := DragQueryFile(_Msg.wParam, $FFFFFFFF, nil, 255);
    for i := 0 to cnt - 1 do begin
      DragQueryFile(_Msg.wParam, i, @arr, SizeOf(arr));
      fn := PChar(@arr);
      sl.Add(fn);
    end;
    DragFinish(_Msg.wParam);
    doCallback(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TForm_ActivateDropFiles(_WinCtrl: Controls.TWinControl; _Callback: TOnFilesDropped): TObject;
begin
  Result := TDropFilesActivator.Create(_WinCtrl as TWinControl, _Callback);
end;

function dzSelectDirectory(var _Directory: string; _Owner: TWinControl = nil): Boolean;
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

const
  // constants and descriptions from MSDN
  // http://msdn.microsoft.com/en-us/library/windows/desktop/bb759862(v=vs.85).aspx

  // Ignore the registry default and force the AutoAppend feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_OFF = $80000000;

  // Ignore the registry value and force the AutoAppend feature on. The completed string will be
  // displayed in the edit box with the added characters highlighted.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOAPPEND_FORCE_ON = $40000000;

  // Ignore the registry default and force the AutoSuggest feature off.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_OFF = $20000000;

  // Ignore the registry value and force the AutoSuggest feature on.
  // A selection of possible completed strings will be displayed as a
  // drop-down list, below the edit box. This flag must be used in
  // combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL* flags.
  SHACF_AUTOSUGGEST_FORCE_ON = $10000000;

  // The default setting, equivalent to
  // SHACF_FILESYSTEM | SHACF_URLALL.
  // SHACF_DEFAULT cannot be combined with any other flags.
  SHACF_DEFAULT = $00000000;

  // Include the file system only.
  SHACF_FILESYS_ONLY = $00000010;

  // Include the file system and directories, UNC servers, and UNC server shares.
  SHACF_FILESYS_DIRS = $00000020;

  // Include the file system and the rest of the Shell (Desktop, Computer, and Control Panel, for example).
  SHACF_FILESYSTEM = $00000001;

  // Include the URLs in the user's History list.
  SHACF_URLHISTORY = $00000002;

  // Include the URLs in the user's Recently Used list.
  SHACF_URLMRU = $00000004;

  // Include the URLs in the users History and Recently Used lists. Equivalent to
  // SHACF_URLHISTORY | SHACF_URLMRU.
  SHACF_URLALL = SHACF_URLHISTORY or SHACF_URLMRU;

  // Allow the user to select from the autosuggest list by pressing the TAB key.
  // If this flag is not set, pressing the TAB key will shift focus to the next
  // control and close the autosuggest list.
  // If SHACF_USETAB is set, pressing the TAB key will select the first item
  // in the list. Pressing TAB again will select the next item in the list,
  // and so on. When the user reaches the end of the list, the next TAB key
  // press will cycle the focus back to the edit control.
  // This flag must be used in combination with one or more of the
  // SHACF_FILESYS* or SHACF_URL*
  // flags
  SHACF_USETAB = $00000008;

  SHACF_VIRTUAL_NAMESPACE = $00000040;

function SHAutoComplete(hwndEdit: HWnd; dwFlags: DWORD): HResult; stdcall; external 'Shlwapi.dll';

function TEdit_SetAutocomplete(_ed: TCustomEdit; _Source: TAutoCompleteSourceEnumSet = [acsFileSystem];
  _Type: TAutoCompleteTypeEnumSet = [actSuggest, actAppend]; _ErrorHandling: TErrorHandlingEnum = ehReturnFalse): Boolean;
var
  Options: DWORD;
  Res: HResult;
begin
  Options := 0;
  if acsFileSystem in _Source then
    Options := Options or SHACF_FILESYSTEM;
  if acsUrlHistory in _Source then
    Options := Options or SHACF_URLHISTORY;
  if acsUrlMru in _Source then
    Options := Options or SHACF_URLMRU;
  if actSuggest in _Type then
    Options := Options or SHACF_AUTOSUGGEST_FORCE_ON;
  if actAppend in _Type then
    Options := Options or SHACF_AUTOAPPEND_FORCE_ON;

  Res := SHAutoComplete(_ed.Handle, Options);
  Result := (Res = S_OK);
  if not Result and (_ErrorHandling = ehRaiseException) then
    raise EOleException.Create(_('Call to SHAutoComplete failed.'), Res, 'Shlwapi.dll', '', 0);
end;

end.


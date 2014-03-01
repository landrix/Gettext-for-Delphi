///<summary> This is an extract from u_dzVclUtils which is part of dzlib available from sourceforge
///          under the MPL 1.1 </summary>
unit u_dzVclUtils;

interface

uses
  Classes,
  Messages,
  Controls,
  ComCtrls,
  ExtCtrls;

///<summary> Enables longer SimpleText (longer than 127 characters)
///          Call once to enable. Works, by adding a single panel with owner drawing and
///          setting the StatusBar's OnDrawPanel to a custom drawing method.
///          To make it work, you must use TStatusBar_SetLongSimpleText to set
///          the text, or use TLongSimpleTextStatusBar as an interposer class. </summary>
procedure TStatusBar_EnableLongSimpleText(_StatusBar: TStatusBar);

///<summary> Set the SimpleText of the StatusBar and invalidate it to enforce a redraw </summary>
procedure TStatusBar_SetLongSimpleText(_StatusBar: TStatusBar; const _Text: string);

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
  Windows,
  SysUtils,
  FileCtrl,
  Types,
  Graphics,
  Dialogs,
  gnugettext;

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


{.GXFormatter.config=twm}
///<summary> generic progress form
///          This unit implements a generic progress form.
///          Create it, set FormCaption, optionally set ProgressMax, show the form.
///          Every now and then call Progress.
///          Free after you are done.
///          Note: You can use up to two %d and %% in FormCaption, examples:
///            FormCaption := 'progress %d of %d';
///            FormCaption := '%d%% done'; // display percentage
///            FormCaption := 'Progress - Line %d';
///          @author twm
///          This is an extract from w_dzProgress which is part of dzlib available from berlios
///          under the MPL 1.1 </summary>
unit w_dzProgress;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  gnugettext,
  System.UITypes;

type
  EdzProgress = class(Exception);

type
  Tf_dzProgress = class(TForm)
    l_Action: TLabel;
    pb_Progress: TProgressBar;
    b_Cancel: TButton;
    procedure b_CancelClick(Sender: TObject);
  private
    FIsCancelVisible: boolean;
    FCancelPressed: boolean;
    FProgressPos: integer;
    FMax: integer;
    FAction: string;
    FIsActionVisible: boolean;
    FPercent: integer;
    FFormCaption: string;
    FFormCaptionParams: integer;
    FFormCaptionPercent: boolean;
    FLastTickCount: Cardinal;
    FProgressTimeInterval: Cardinal;
    procedure SetFormCaption(const _FormCaption: string);
    procedure InternalSetCaption;
    procedure SetProgressPos(_ProgressPos: integer);
    procedure SetMax(_Max: integer);
    procedure SetAction(const _Action: string);
    procedure SetIsCancelVisible(_Visible: boolean);
    procedure SetIsActionVisible(_Visible: boolean);
    function AnalyseCaption(const _Caption: string;
      var _ParamCount: integer; var _Percent: boolean;
      var _Error: string): boolean;
  public
    constructor Create(_Owner: TComponent); override;
    property FormCaption: string read FFormCaption write SetFormCaption;
    property ProgressPos: integer read FProgressPos write SetProgressPos;
    property ProgressMax: integer read FMax write SetMax;
    property Action: string read FAction write SetAction;
    property IsCancelVisible: boolean read FIsCancelVisible write SetIsCancelVisible;
    property IsActionVisible: boolean read FIsActionVisible write SetIsActionVisible;
    procedure Progress(_Position: integer; const _Action: string; var _Abort: boolean); overload;
    procedure Progress(_Position: integer; var _Abort: boolean); overload;
    property ProgressTimeInterval: Cardinal read FProgressTimeInterval write FProgressTimeInterval;
  end;

implementation

{$R *.DFM}

uses
  StrUtils;

constructor Tf_dzProgress.Create(_Owner: tComponent);
begin
  inherited;
  FProgressTimeInterval := 200;
  FLastTickCount := 0;
  pb_Progress.Position := 0;
  FMax := 100;
  l_Action.Caption := '';
  FormCaption := _('Progress (%d%%)');

  PopupMode := pmExplicit;
  if _Owner is TCustomForm then
    PopupParent := _Owner as TCustomForm;

  TranslateComponent(Self, 'dzlib');
end;

procedure Tf_dzProgress.b_CancelClick(Sender: TObject);
begin
  FCancelPressed := (mrYes = MessageDlg(_('Do you really want to abort?'), mtConfirmation, [mbYes, mbNo], 0));
end;

procedure Tf_dzProgress.InternalSetCaption;
var
  OldPercent: integer;
begin
  if FFormCaptionPercent then begin
    OldPercent := FPercent;
    FPercent := (FProgressPos * 100) div FMax;
    if OldPercent = FPercent then
      exit;
  end;
  case FFormCaptionParams of
    0: Caption := FFormCaption;
    1:
      if FFormCaptionPercent then
        Caption := Format(FFormCaption, [FPercent])
      else
        Caption := Format(FFormCaption, [FProgressPos]);
    2: Caption := Format(FFormCaption, [FProgressPos, FMax]);
  end;
end;

procedure Tf_dzProgress.Progress(_Position: integer; var _Abort: boolean);
var
  NextTickCount: Int64;
begin
  // Durch ProcessMessages dauert die Anzeige meistens laenger als die eigentliche Berechnung
  // innerhalb der Schleife bei der die Progress-Funktion aufgerufen wird.
  // Deshalb nur alle 200msec updaten -> SUPER GESCHWINDIGKEITSOPTIMIERUNG
  // z.b. in Rd2Ea und Ea2Er (wurde sonst über AuswerteOdometer gemacht)
  // aber Vorsicht: GetTickCount laeuft nach ca. 25 Tagen ueber

  NextTickCount := (Int64(FLastTickCount) + Int64(FProgressTimeInterval)) and $FFFFFFFF;
  if GetTickCount > NextTickCount then begin
    FProgressPos := _Position;
    pb_Progress.Position := _Position;
    InternalSetCaption;
    Application.ProcessMessages;
    _Abort := FCancelPressed;
    FLastTickCount := GetTickCount;
  end;
end;

procedure Tf_dzProgress.Progress(_Position: integer; const _Action: string; var _Abort: boolean);
begin
  FAction := _Action;
  l_Action.Caption := _Action;
  Progress(_Position, _Abort);
end;

function Tf_dzProgress.AnalyseCaption(const _Caption: string; var _ParamCount: integer;
  var _Percent: boolean; var _Error: string): boolean;
var
  p: integer;
begin
  Result := false;
  _Percent := false;
  _ParamCount := 0;
  p := PosEx('%', _Caption, 1);
  while p <> 0 do begin
    if (p >= Length(_Caption)) then begin
      _Error := _('must not contain a % at the end');
      exit;
    end;
    if _Caption[p + 1] = 'd' then
      Inc(_ParamCount)
    else if _Caption[p + 1] = '%' then begin
      _Percent := true;
      Inc(p)
    end else begin
      _Error := _('"%" must be followed by "d" or "%".');
      exit;
    end;
    p := PosEx('%', _Caption, p + 1);
  end;
  if _ParamCount > 2 then
    _Error := _('too many parameters')
  else
    Result := true;
end;

procedure Tf_dzProgress.SetFormCaption(const _FormCaption: string);
var
  Error: string;
  Percent: boolean;
  Params: integer;
begin
  if not AnalyseCaption(_FormCaption, Params, Percent, Error) then
    raise EdzProgress.CreateFmt(_('Invalid FormCaption, %s'), [Error]);
  FFormCaption := _FormCaption;
  FFormCaptionParams := Params;
  FFormCaptionPercent := Percent;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetProgressPos(_ProgressPos: integer);
begin
  FProgressPos := _ProgressPos;
  pb_Progress.Position := _ProgressPos;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetMax(_Max: integer);
begin
  FMax := _Max;
  pb_Progress.Max := _Max;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetAction(const _Action: string);
begin
  FAction := _Action;
  l_Action.Caption := _Action;
  l_Action.Layout := tlCenter;
end;

procedure Tf_dzProgress.SetIsCancelVisible(_Visible: boolean);
begin
  FIsCancelVisible := _Visible;
  b_Cancel.Visible := FIsCancelVisible;
  if not FIsCancelVisible then
    pb_Progress.Width := ClientWidth - 2 * pb_Progress.Left
  else
    pb_Progress.Width := b_Cancel.Left - 2 * pb_Progress.Left;
end;

procedure Tf_dzProgress.SetIsActionVisible(_Visible: boolean);
begin
  FIsActionVisible := _Visible;
  l_Action.Visible := FIsActionVisible;
  if not FIsActionVisible then begin
    ClientHeight := 2 * 5 + b_Cancel.Height;
    b_Cancel.Top := (ClientHeight - b_Cancel.Height) div 2;
    pb_Progress.Top := (ClientHeight - pb_Progress.Height) div 2;
  end else begin
    // as designed
    b_Cancel.Top := 20;
    pb_Progress.Top := 24;
    Height := 76;
  end;
end;

end.


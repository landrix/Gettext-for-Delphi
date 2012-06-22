unit ClxSampleForm;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls;

type
  TFormMain = class(TForm)
    ButtonTestGettext: TButton;
    ButtonTestResourceString: TButton;
    procedure ButtonTestGettextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonTestResourceStringClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.xfm}

uses
  gnugettext;

// This example does not include a resourcestring example,
// because resourcestring doesn't work with this translation system
// on Linux.

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TranslateComponent (self);
end;

procedure TFormMain.ButtonTestGettextClick(Sender: TObject);
begin
  // Message to the user when the test button is clicked
  MessageDlg (_('Thank you for clicking this button'),mtWarning,[mbOK],0);
end;

resourcestring
  MessageToUser='Thank you for clicking this button';

procedure TFormMain.ButtonTestResourceStringClick(Sender: TObject);
begin
  MessageDlg (MessageToUser,mtWarning,[mbOK],0);
end;

end.

unit uMain;

interface

uses 
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, TypInfo,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Tabs, Vcl.ComCtrls, MyLangsRT,
  Vcl.Menus;

type
  TfrmDilDemo = class(TForm)
    Button1: TButton;
    RadioButton1: TRadioButton;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Ayarlar: TTabSheet;
    RadioGroup1: TRadioGroup;
    ListView1: TListView;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    Label2: TLabel;
    ListBox2: TListBox;
    StatusBar1: TStatusBar;
    MyLangs1: TMyLangs;
    MyLangs2: TMyLangs;
    MyLangs3: TMyLangs;
    ComboBox2: TComboBox;

    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDilDemo: TfrmDilDemo;

implementation
{$R *.dfm}

procedure TfrmDilDemo.Button1Click(Sender: TObject);
begin
//showmessage(MyLangs1.ActiveLanguage);
MyLangs1.ChangeToDefaultLanguage;
end;


procedure TfrmDilDemo.ComboBox2Change(Sender: TObject);
begin
MyLangs1.ChangeLanguage(ComboBox2.Items[ComboBox2.ItemIndex]);
StatusBar1.Panels[0].Text:= 'Selected Language: ' +
ComboBox2.Items[ComboBox2.ItemIndex];
end;

procedure TfrmDilDemo.FormCreate(Sender: TObject);
begin
{
 }
ComboBox2.Items.Assign(MyLangs1.Languages);
ComboBox2.Items.Insert(0, 'DefaultValue');
ListBox1.Items.Assign(MyLangs1.Languages);
ListBox1.Items.Insert(0, 'DefaultValue');

StatusBar1.Panels[0].Text:=
MyLangs1.DefaultLanguage + ' active language.';

end;

procedure TfrmDilDemo.ListBox1Click(Sender: TObject);
begin
{ }

MyLangs1.ChangeLanguage(ListBox1.Items[ListBox1.ItemIndex]);
StatusBar1.Panels[0].Text:= 'Selected Language: ' +
ListBox1.Items[ListBox1.ItemIndex];
end;

end.

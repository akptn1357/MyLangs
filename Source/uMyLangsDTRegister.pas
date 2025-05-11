unit uMyLangsDTRegister;

interface

uses
  System.Classes, System.SysUtils, VCL.Graphics,
  VCL.Controls, Vcl.Dialogs,
  ToolsAPI, DesignIntf, DesignEditors,
  MyLangsRT, uSettings, uLanguages;

{$R MySplash.res}

type
  TMyLangComponentEditor = class(TComponentEditor)
  private
    FMyLangs: TMyLangs;

  public
    procedure Edit; override;
    procedure SettingsChanged(Sender: TObject);
    constructor Create(AComponent: TComponent;
    ADesigner: IDesigner); override;

  end;

var
  MyLangCompEditor: TMyLangComponentEditor = nil;
  FDesignNotification: IDesignNotification;


  type
  TLanguagesPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    destructor Destroy; override;
  end;


  type
  TDefaultLanguagePropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  type
  TDataPropertyEditor = class(TStringProperty)
  public
    procedure SetValue(const Value: string); override;
  end;

procedure Register;


implementation

destructor TLanguagesPropertyEditor.Destroy;
begin

  inherited;
end;

procedure TMyLangComponentEditor.SettingsChanged(Sender: TObject);
begin
  Designer.Modified;

end;

procedure Register;
begin

  RegisterComponents('MyComponents', [TMyLangs]);

  RegisterCustomModule(TfrmSettings, TCustomModule);

  RegisterComponentEditor(TMyLangs, TMyLangComponentEditor);


  RegisterPropertyEditor(TypeInfo(string), TMyLangs,
                          'DefaultLanguage', TDefaultLanguagePropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TMyLangs,
                          'Data', TDataPropertyEditor);


  RegisterPropertyEditor(TypeInfo(TStringList), TMyLangs,
                          'Languages', TLanguagesPropertyEditor);
end;

constructor TMyLangComponentEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);

    if AComponent is TMyLangs then
    FMyLangs := TMyLangs(AComponent)
  else
    FMyLangs := nil;

end;


procedure TMyLangComponentEditor.Edit;
var
  SettingsForm: TfrmSettings;
begin

if not Assigned(FMyLangs) then
  begin
    ShowMessage('FMyLangs is nil!');
    Exit;
  end;



  SettingsForm := TfrmSettings.Create(nil, FMyLangs, Self.Designer);

  try
    SettingsForm.ShowModal;
  finally
    SettingsForm.Free;
  end;
end;


procedure TLanguagesPropertyEditor.Edit;
var
  LanguagesForm: TfrmLanguages;
  LangComponent: TMyLangs;
begin
  LangComponent := GetComponent(0) as TMyLangs;

  LanguagesForm :=
  TfrmLanguages.Create(nil, LangComponent as TMyLangs, Self.Designer);

  try

   LanguagesForm.ShowModal;

  finally
    LanguagesForm.Free;

  end;


end;



{ TLanguagePropertyEditor }

function TDefaultLanguagePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TDefaultLanguagePropertyEditor.GetValues(Proc: TGetStrProc);
var
  LangComponent: TMyLangs;
  I: Integer;
begin
  LangComponent := GetComponent(0) as TMyLangs;

  if Assigned(LangComponent) and Assigned(LangComponent.Languages) then
  begin
    for I := 0 to LangComponent.Languages.Count - 1 do
      Proc(LangComponent.Languages[I]);
  end;
end;


procedure TDataPropertyEditor.SetValue(const Value: string);
var
  LangComponent: TMyLangs;
begin
  LangComponent := GetComponent(0) as TMyLangs;

  if Assigned(LangComponent) then
  begin

      Modified;
  end;
end;


procedure TDefaultLanguagePropertyEditor.SetValue(const Value: string);
var
  LangComponent: TMyLangs;
begin
  LangComponent := GetComponent(0) as TMyLangs;

  if Assigned(LangComponent) then
  begin

    if LangComponent.Languages.IndexOf(Value) <> -1 then
    begin
      LangComponent.DefaultLanguage := Value;
      Modified;
    end
    else
      raise Exception.CreateFmt(
      'Invalid value "%s". Select a valid language.', [Value]);
  end;
end;


function TLanguagesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


procedure AddSplashInfo;
  var
    SplashIcon: VCL.Graphics.TBitmap;
  begin
    SplashIcon := VCL.Graphics.TBitmap.Create;

    SplashIcon.LoadFromResourceName(HInstance, 'MY_SPLASH_ICON');
    SplashIcon.Transparent := True;
    SplashIcon.TransparentColor := clWhite;


    SplashScreenServices.AddPluginBitmap(
    'MyLangs Localization Package © 2025 by BG',
    SplashIcon.Handle, False, 'Open Source!','v1.0');

    SplashIcon.Free;
  end;






initialization
AddSplashInfo;

end.


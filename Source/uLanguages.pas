unit uLanguages;

interface

uses
    Winapi.Windows,System.Classes, System.SysUtils,
    Vcl.Forms, Vcl.Controls, Vcl.Graphics, System.IOUtils,
    Vcl.StdCtrls, Vcl.Dialogs, MyLangsRT,
    ToolsAPI, DesignIntf, DesignEditors,
    uSettings, uDataManipulation,
    FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Param, FireDAC.Stan.Error,
    FireDAC.DatS, FireDAC.Phys.Intf, Data.DB,
    FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
    FireDAC.Comp.Client;


type
  TfrmLanguages = class(TForm)
    lstLanguages: TListBox;
    btnAddLang: TButton;
    edtNewLang: TEdit;
    FDMemTable1: TFDMemTable;
    procedure btnAddLangClick(Sender: TObject);
    procedure lstLanguagesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstLanguagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteLanguage(DLanguage: string; AIndex: Byte);
    procedure EditLanguage(ELanguage: string; AIndex: Byte);
    procedure edtNewLangKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure AddBatchLangs(SL: TStringList);


  private
    FComponent: TMyLangs;
    FDesigner: IDesigner;

  public
    property Designer: IDesigner read FDesigner write FDesigner;
    procedure NotifyChange;
    constructor Create(AOwner: TComponent; AComponent: TMyLangs;
    ADesigner: IDesigner); reintroduce;
  end;

  procedure ListMylFiles(const AFolder: string; AList: TStrings);
  function GetCurrentUnitPath: String;

implementation

{$R *.dfm}

var
  DataManip: TDataManipulation;




procedure TfrmLanguages.NotifyChange;
begin
  if Assigned(FDesigner) then
  FDesigner.Modified;
end;

{ TfrmLanguages }


constructor TfrmLanguages.Create(AOwner: TComponent;
            AComponent: TMyLangs; ADesigner: IDesigner);
begin
  inherited Create(AOwner);

  FComponent := AComponent;
  FDesigner := ADesigner;


  lstLanguages.Items.Assign(FComponent.Languages);


  lstLanguages.ItemHeight := 30;




end;


procedure ListMylFiles(const AFolder: string; AList: TStrings);
var
  Files: TArray<string>;
  FileName: string;
begin
  Files := TDirectory.GetFiles(AFolder, '*.myl',
  TSearchOption.soAllDirectories);
  for FileName in Files do
    AList.Add(FileName);
end;


procedure TfrmLanguages.FormShow(Sender: TObject);
var SL1,SL2: TStringList; I, A: byte;
begin
if FComponent.Languages.Count > 0 then Exit;

SL1:= TStringList.Create;
SL2:= TStringList.Create;

ListMylFiles(GetCurrentUnitPath, SL1);

 try

  if  SL1.Count > 0 then
  begin
    for I := 0 to SL1.Count-1 do
    begin
     FDMemTable1.LoadFromFile(SL1[I], sfJSON);

     SL2.Assign(FDMemTable1.FieldDefList);

     if SL2.Count = 0 then Continue;

     for A := 0 to 6 do SL2.Delete(0);

     if MessageDlg(
     'A language file has been found. '+  SLineBreak +
     'Would you like to use the languages in this file as a template? ' +
     SLineBreak + SLineBreak + SL2.Text,
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
     begin
     Fcomponent.Languages.Assign(SL2);
     lstLanguages.Items.Assign(SL2);
     AddBatchLangs(SL2);
     Break;
     end;
    end;
  end;

 finally
    SL1.Free;
    SL2.Free;
 end;

end;

function GetCurrentUnitPath: String;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  idx: integer;

begin
  Result := '';
  SourceEditor := nil;

  if System.SysUtils.Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    Module := ModuleServices.CurrentModule;

    if System.Assigned(Module) then
    begin
      idx := Module.GetModuleFileCount - 1;


      while (idx >= 0) and not
      System.SysUtils.Supports(Module.GetModuleFileEditor(idx),
      IOTASourceEditor, SourceEditor) do
        System.Dec(idx);


      if idx >= 0 then
        Result := ExtractFilePath(SourceEditor.FileName);

    end;

  end;

end;

procedure TfrmLanguages.AddBatchLangs(SL: TStringList);
var DataManip: TDataManipulation;
begin
  DataManip:= TDataManipulation.Create;

      try
          FComponent.MemTableData:=
          DataManip.AddNewLanguage('',
          FComponent.MemTableData, SL);
      finally
          DataManip.Free;
      end;


Designer.Modified;
end;


procedure TfrmLanguages.btnAddLangClick(Sender: TObject);
var
  NewLanguage: string;
begin

    if edtNewLang.Visible= True then
    begin

        NewLanguage := Trim(edtNewLang.Text);
        if NewLanguage = '' then
        begin
          ShowMessage('Please enter a language name.');
          Exit;
        end;


        if Assigned(FComponent) then
        begin
          if FComponent.Languages.IndexOf(NewLanguage) = -1 then

          begin
            FComponent.Languages.Add(NewLanguage);

            ShowMessage(Format('"%s" added to Languages.', [NewLanguage]));
          end
          else
          begin
            ShowMessage(Format
            ('"%s" is already in the Languages list.', [NewLanguage]));
            Exit;
          end;
        end;


          lstLanguages.Items.Assign(FComponent.Languages);

          edtNewLang.Clear;
          edtNewLang.SetFocus;
    end
  else
    begin

    edtNewLang.Visible:= True;
    edtNewLang.SetFocus;
    btnAddLang.Caption:= 'ADD & SAVE';
    Exit;
    end;

      DataManip:= TDataManipulation.Create;
      try
          if FComponent.MemTableData <> '' then
          begin
              //Showmessage('Önce: '+Length(FComponent.MemTableData).ToString);
              FComponent.MemTableData:=
              DataManip.AddNewLanguage(NewLanguage,
              FComponent.MemTableData);
              FComponent.Changed := True;
          end;
      finally
          DataManip.Free;
      end;

  Designer.Modified;
end;


procedure TfrmLanguages.lstLanguagesDrawItem(
Control: TWinControl; Index: Integer;
Rect: TRect; State: TOwnerDrawState);
var
  EditRect, DeleteRect: TRect;
  LBox: TListBox;
begin
  LBox := Control as TListBox;


  if odSelected in State then
    LBox.Canvas.Brush.Color := clHighlight
  else
    LBox.Canvas.Brush.Color := clWindow;
  LBox.Canvas.FillRect(Rect);


  LBox.Canvas.TextOut(Rect.Left + 5, Rect.Top, LBox.Items[Index]);


  EditRect := Rect;
  EditRect.Left := Rect.Right - 120;
  EditRect.Right := Rect.Right - 70;
  EditRect.Top := Rect.Top + 4;
  EditRect.Bottom := Rect.Bottom - 4;


  LBox.Canvas.Brush.Color := clLime;
  LBox.Canvas.FillRect(EditRect);
  LBox.Canvas.Pen.Color := clBlack;
  LBox.Canvas.Rectangle(EditRect);
  LBox.Canvas.TextOut(EditRect.Left + 10, EditRect.Top + 2, 'Edit');


  DeleteRect := Rect;
  DeleteRect.Left := Rect.Right - 60;
  DeleteRect.Right := Rect.Right - 10;
  DeleteRect.Top := Rect.Top + 4;
  DeleteRect.Bottom := Rect.Bottom - 4;


  LBox.Canvas.Brush.Color := clRed;
  LBox.Canvas.FillRect(DeleteRect);
  LBox.Canvas.Pen.Color := clBlack;
  LBox.Canvas.Rectangle(DeleteRect);
  LBox.Canvas.TextOut(DeleteRect.Left + 10, DeleteRect.Top + 2, 'Delete');

end;

procedure TfrmLanguages.lstLanguagesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  ItemRect, EditRect, DeleteRect: TRect;
  LBox: TListBox;
begin
  LBox := Sender as TListBox;

  Index := LBox.ItemAtPos(Point(X, Y), True);
  if Index < 0 then Exit;


  ItemRect := LBox.ItemRect(Index);


  EditRect := ItemRect;
  EditRect.Left := ItemRect.Right - 120;
  EditRect.Right := ItemRect.Right - 70;
  EditRect.Top := ItemRect.Top + 4;
  EditRect.Bottom := ItemRect.Bottom - 4;


  DeleteRect := ItemRect;
  DeleteRect.Left := ItemRect.Right - 60;
  DeleteRect.Right := ItemRect.Right - 10;
  DeleteRect.Top := ItemRect.Top + 4;
  DeleteRect.Bottom := ItemRect.Bottom - 4;


  if PtInRect(EditRect, Point(X, Y)) then
  begin

    EditLanguage(LBox.Items[LBox.ItemIndex], LBox.ItemIndex);

  end

  else if PtInRect(DeleteRect, Point(X, Y)) then
  begin

    DeleteLanguage(LBox.Items[LBox.ItemIndex], LBox.ItemIndex);
  end;
end;


procedure TfrmLanguages.DeleteLanguage(DLanguage: string; AIndex: Byte);
var DataManip: TDataManipulation;
begin
  var UserResponse := MessageDlg('Are you sure want to delete "' +
                      lstLanguages.Items[AIndex] + '" Language?',
                      mtConfirmation, [mbYes, mbNo], 0);

      if UserResponse = mrYes then
      begin

        FComponent.Languages.Delete(
        FComponent.Languages.IndexOf(DLanguage));

        if FComponent.DefaultLanguage =
        lstLanguages.Items[AIndex] then
        FComponent.DefaultLanguage:= '';

        lstLanguages.Items.Delete(AIndex);

        DataManip:= TDataManipulation.Create;

          try

            try
            if FComponent.MemTableData <> '' then
            FComponent.MemTableData:=
            DataManip.DeleteLanguage(
            DLanguage, FComponent.MemTableData);
            FComponent.Changed := True;


            Designer.Modified;

            except
            on E: Exception do
            end;


          finally
          DataManip.Free;
          end;
        ShowMessage('Item deleted.');
      end
      else
      ShowMessage('Deletion cancelled.');


end;


procedure TfrmLanguages.EditLanguage(ELanguage: string; AIndex: Byte);
var DataManip: TDataManipulation;
begin

var NewLanguage := InputBox('Edit ' + QuotedStr(ELanguage) +
    ' Language', 'Set New Language Name:', '');

if Trim(NewLanguage) = '' then Exit;



lstLanguages.Items[AIndex] := NewLanguage;


FComponent.Languages
[FComponent.Languages.IndexOf(ELanguage)] := NewLanguage;

if FComponent.DefaultLanguage = ELanguage then
FComponent.DefaultLanguage:= NewLanguage;


DataManip:= TDataManipulation.Create;

  try

    try
    if FComponent.MemTableData <> '' then
    FComponent.MemTableData:=
    DataManip.ChangeLanguageName(
    ELanguage, NewLanguage, FComponent.MemTableData);
    FComponent.Changed := True;
    Designer.Modified;
    except
    on E: Exception do
    showmessage(E.Message);
    end;

  finally
  DataManip.Free;
  end;

end;


procedure TfrmLanguages.edtNewLangKeyPress(Sender: TObject; var Key: Char);
begin
if Key = #13 then btnAddLang.Click;
end;





end.


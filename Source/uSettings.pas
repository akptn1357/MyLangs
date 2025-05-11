unit uSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.RTTI,
  System.IOUtils,
  System.Classes, System.SysUtils, System.ZLib,
  System.TypInfo, System.Generics.Collections,
  System.ImageList, System.UITypes, StrUtils,
  ToolsAPI, DesignIntf, DesignEditors,

  VCl.GraphUtil, Vcl.Clipbrd, Vcl.ImgList,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Graphics,
  Vcl.Buttons, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.DApt.Intf, FireDAC.Stan.StorageBin, FireDAC.UI.Intf,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.VCLUI.Wait, FireDAC.DApt, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.SQLiteVDataSet,
  FireDAC.Comp.DataSet, MyLangsRT, udataManipulation;

type  {Winapi.Messages}
  TDBGrid = class(Vcl.DBGrids.TDBGrid)
  private
    procedure WmVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WmHScroll(var Message: TWMHScroll); message WM_HSCROLL;
  protected
    procedure WndProc(var Message: TMessage); override;
  end;


type
  TfrmSettings = class(TForm)
    tvCompCategories: TTreeView;
    Panel1: TPanel;
    btnReadMYLFile: TButton;
    btnSave: TButton;
    btnLanguages: TButton;
    trvCompProps: TTreeView;
    memTblLang: TFDMemTable;
    memTblControl: TFDMemTable;
    conLang: TFDConnection;
    qryLang: TFDQuery;
    locSQL: TFDLocalSQL;
    dsComps: TDataSource;
    btnSetValues: TButton;
    PhysSQLiteDrv: TFDPhysSQLiteDriverLink;
    pnlGrid: TPanel;
    DBNavigator1: TDBNavigator;

    grdCompProps: TDBGrid;
    ImageList: TImageList;
    ImageList1: TImageList;

    procedure btnSaveClick(Sender: TObject);
    procedure btnLanguagesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddNewField(NewField: string);
    procedure FormShow(Sender: TObject);
    procedure ReloadAllComponentsProperties;
    procedure GetComponentsDatasFirstTime;
    procedure LoadFromFile;
    procedure ListComponents;
    procedure HideColumns;

    procedure AllPropertiesToMemTable(
              AComponent: TObject; AStringList: TStringList;
              AVisited: TList<TObject>; MaxDepth: Integer = 5;
              CurrentDepth: Integer = 0; AParentName: string = '');
    procedure PropertiesToTreeView(
              AComponent: TObject; ATreeView: TTreeView;
              AVisited: TList<TObject>; MaxDepth: Integer = 5;
              CurrentDepth: Integer = 0;
              AParentNode: TTreeNode = nil);
    procedure tvCompCategoriesCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultValueDraw: Boolean);
    procedure tvCompCategoriesClick(Sender: TObject);
    procedure GetComponentAllDetails(AComponent: TComponent);
    procedure QueryAComponentsDetails(AProperty: string);
    procedure ListLanguages;
    procedure grdCompPropsTitleClick(Column: TColumn);
    procedure btnSetValuesClick(Sender: TObject);
    procedure DeleteFieldAndRefresh(MemTable: TFDMemTable;
              const FieldName: string);
    procedure DeleteColumnFromDBGrid(DBGrid: TDBGrid;
              const ColumnName: string);
    procedure ChangeColumnNameofDBGrid(DBGrid: TDBGrid;
              const ColumnName, NewColumnName: string);
    procedure FormDestroy(Sender: TObject);
    procedure memTblLangNewRecord(DataSet: TDataSet);
    procedure grdCompPropsDrawColumnCell(Sender: TObject; const Rect: TRect;
              DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure btnReadMYLFileClick(Sender: TObject);
    procedure grdCompPropsCellClick(Column: TColumn);
    procedure ChangeColumnName(OldName, NewName: string);
    procedure  ExportMemTableToStringList(
              AMemTable: TDataSet);
    function  CompressStringListToHex(const SL: TStringList): string;
    procedure qryLangAfterDelete(DataSet: TDataSet);
    procedure qryLangAfterPost(DataSet: TDataSet);
    procedure memTblLangAfterOpen(DataSet: TDataSet);
    procedure tvCompCategoriesMouseDown(Sender: TObject; Button: TMouseButton;
              Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);


  private
    FComponent: TMyLangs;
    FDesigner: IDesigner;
    DataManip: TDataManipulation;

  public
    property Designer: IDesigner read FDesigner write FDesigner;

    procedure NotifyChange;
    constructor Create(AOwner: TComponent; AComponent: TMyLangs;
    ADesigner: IDesigner); reintroduce;

  end;

  function String2Hex(const Buffer: AnsiString): string;
  function Hex2String(const Buffer: string): AnsiString;

  function HexToBytes(const HexStr: string): TBytes;
  function BytesToHex(const Buffer: TBytes): string;

  procedure PasteClipboardToGrid(AGrid: TDBGrid; AMemTable: TFDQuery);
  procedure SaveDBGridToCSV(DBGrid: TDBGrid; const FileName: string);

  function GetCurrentUnitPath(out UnitName: String): String;
  function GetCurrentUnitPath2: String;

  procedure UpdateFieldsFromControl(
  SourceTable, TargetTable: TFDMemTable; StartIndex: Integer);
var
  x: integer;
  frmSettings: TfrmSettings;
  F,UN: string;
implementation

{$R *.dfm}


uses
  uLanguages, uTDBGrid_Helper,
  uMyLangsDTRegister;


procedure TfrmSettings.NotifyChange;
begin
  if Assigned(FDesigner) then
  FDesigner.Modified;
end;

{ TfrmSettings }

procedure TDBGrid.WndProc(var Message: TMessage);
var
  X, Y: Integer;
  Coord: TGridCoord;
  MouseDownEvent: TMouseEvent;
begin
  case Message.Msg of
    WM_LBUTTONDOWN:
      begin

        X := LOWORD(Message.LParam);
        Y := HIWORD(Message.LParam);
        Coord := MouseCoord(X, Y);

        if (Coord.X > 0) and (Coord.Y > 0) then
        begin

          if Assigned(Self.OnMouseDown) then
          begin
            MouseDownEvent := Self.OnMouseDown;
            MouseDownEvent(Self, mbLeft, [], X, Y);
          end;
        end;
      end;
  end;

  inherited WndProc(Message);
end;



procedure TDBGrid.WmVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollCode = SB_THUMBTRACK then
     Message.ScrollCode := SB_THUMBPOSITION;
  inherited;
end;

procedure TDBGrid.WmHScroll(var Message: TWMHScroll);
begin
  if Message.ScrollCode = SB_THUMBTRACK then
     Message.ScrollCode := SB_THUMBPOSITION;
  inherited;
end;



procedure DFMToQryLang(const DfmString: string; MemTable: TFDMemTable);
var
  MemStream: TMemoryStream;
  DecompressedStream: TMemoryStream;
  DecompressionStream: TDecompressionStream;
  Bytes: TBytes;
begin


  Bytes := HexToBytes(DfmString);


  MemStream := TMemoryStream.Create;
  DecompressedStream := TMemoryStream.Create;

  try

    MemStream.WriteBuffer(Bytes[0], Length(Bytes));
    MemStream.Position := 0;


    DecompressionStream := TDecompressionStream.Create(MemStream);
    try
      DecompressedStream.CopyFrom(
      DecompressionStream, DecompressionStream.Size);
    finally
      DecompressionStream.Free;
    end;


    DecompressedStream.Position := 0;

    MemTable.LoadFromStream(DecompressedStream, sfJSon);

  finally
    MemStream.Free;
    DecompressedStream.Free;
  end;
end;

procedure SaveDBGridToCSV(
          DBGrid: TDBGrid; const FileName: string);
var
  SL: TStringList;
  i: Integer;
  sLine: string;
  DS: TDataSet;
  Field: TField;
begin
  SL := TStringList.Create;
  try
    DS := DBGrid.DataSource.DataSet;
    DS.DisableControls;
    try
      // Baþlýk satýrý
      sLine := '';
      for i := 0 to DS.FieldCount - 1 do
      begin
        Field := DS.Fields[i];
        sLine := sLine + '"' + Field.DisplayLabel + '"';
        if i < DS.FieldCount - 1 then
          sLine := sLine + ';';
      end;
      SL.Add(sLine);

      // Veri satýrlarý
      DS.First;
      while not DS.Eof do
      begin
        sLine := '';
        for i := 0 to DS.FieldCount - 1 do
        begin
          Field := DS.Fields[i];
          sLine := sLine + '"' + Field.AsString + '"';
          if i < DS.FieldCount - 1 then
            sLine := sLine + ';';
        end;
        SL.Add(sLine);
        DS.Next;
      end;

      // UTF-8 olarak kaydet
      SL.SaveToFile(FileName, TEncoding.UTF8);
    finally
      DS.EnableControls;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrmSettings.ExportMemTableToStringList(
          AMemTable: TDataSet);
var
  SL: TStringList;
  i: Integer;
  RowData: string;
  Delimiter: string;
begin

  SL := TStringList.Create;
  Delimiter:= FComponent.Delimiter;



  if FComponent.Languages.Count = 0  then
  qryLang.Open('Select Property, DefaultValue ' +
                'From memTblLang Where Active = 1')
  else
  qryLang.Open('Select Property, DefaultValue, ' +
                FComponent.Languages.CommaText +
                ' From memTblLang Where Active = 1');


  try

     RowData := '';
     for i := 0 to qryLang.FieldCount - 1 do
     begin
       RowData := RowData + qryLang.Fields[i].FieldName;
       if i < qryLang.FieldCount - 1 then
         RowData := RowData + Delimiter;
     end;
     SL.Add(RowData);

    qryLang.First;
    while not qryLang.Eof do
    begin
      RowData := '';
      for i := 0 to qryLang.FieldCount - 1 do
      begin
        RowData := RowData + qryLang.Fields[i].AsString;
        if i < qryLang.FieldCount - 1 then
          RowData := RowData + Delimiter;
      end;
      SL.Add(RowData);
      qryLang.Next;
    end;

    FComponent.CompressedData:= SL.Text;

  finally
    SL.Free;
  end;



end;


procedure TfrmSettings.LoadFromFile;
var SL : TStringList;
I: Integer;
begin

      qryLang.Close;
      grdCompProps.DataSource := nil;
      conLang.Connected := True;

      if FileExists(F) then
       begin

          memTblLang.LoadFromFile(F, sfJSON);
          FComponent.MemTableData:=
          DataManip.MemTableToStr(memTblLang);

          SL := TStringList.Create;
          try
            SL.Assign(memTblLang.FieldDefList);

            for I := 6 downto 0 do
              SL.Delete(I);

          finally
            FComponent.Languages.Assign(SL);
            SL.Free;
          end;

       end;

      memTblLang.Name:= 'memTblLang';
      locSQL.Connection := conLang;
      locSQL.DataSets.Clear;
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;

      qryLang.Open('Select * From memTblLang');
      dsComps.DataSet:= qryLang;
      grdCompProps.DataSource := dsComps;


end;

procedure TfrmSettings.GetComponentsDatasFirstTime;
var Column: TColumn;
i, a: integer;
PropertiesList: TStringList;
Visited: TList<TObject>;
begin

      memTblLang.Close;
      qryLang.Close;

      memTblLang.Fields.Clear;

      memTblLang.FieldDefs.Add('rID', ftAutoInc);
      memTblLang.FieldDefs.Add('Active', ftByte);
      memTblLang.FieldDefs.Add('Property', ftWideString, 300);
      memTblLang.FieldDefs.Add('DefaultValue', ftWideString, 300);
      memTblLang.FieldDefs.Add('PropName', ftWideString, 100);
      memTblLang.FieldDefs.Add('PropType', ftWideString, 100);
      memTblLang.FieldDefs.Add('PropTypeKind', ftWideString, 100);

      for a := 0 to FComponent.Languages.Count -1 do
      memTblLang.FieldDefs.Add(FComponent.Languages[a], ftWideString, 100);
      memTblLang.CreateDataSet;
      memTblLang.Open;
      grdCompProps.Columns.Clear;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'rID';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 50;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'Active';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 15;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'Property';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 300;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'DefaultValue';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 300;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'PropName';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 100;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'PropType';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 100;

      Column := grdcompprops.Columns.Add;
      Column.FieldName := 'PropTypeKind';
      Column.Title.Caption := Column.FieldName;
      Column.Width := 100;

      for a := 0 to FComponent.Languages.Count -1 do
      begin
      Column := grdCompProps.Columns.Add;

      Column.FieldName := FComponent.Languages[a];
      Column.Title.Caption := Column.FieldName;
      Column.Width := 100;
      end;

      conLang.DriverName := 'SQLite';
      conLang.Connected := True;

      locSQL.Connection := conLang;
      locSQL.DataSets.Clear;
      memTblLang.Name:= 'memTblLang';
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;

      dsComps.DataSet := qryLang;
      qryLang.SQL.Text := 'Select * From memTblLang';
      qryLang.Active := True;

      PropertiesList := TStringList.Create;
      Visited := TList<TObject>.Create;

      AllPropertiesToMemtable(
        FComponent.Owner as TForm, PropertiesList, Visited);

      for i := 0 to FComponent.Owner.ComponentCount -1 do
      begin
          AllPropertiesToMemtable(
          FComponent.Owner.Components[i], PropertiesList, Visited);
      end;

      PropertiesList.Free;
      Visited.Free;

       try

        qryLang.SQL.Text:=
        'DELETE FROM memTblLang ' +
        'WHERE rID NOT IN (' +
        'SELECT MIN(rID) FROM memTblLang GROUP BY Property)';

        qryLang.ExecSQL;
       except
       on E: Exception do
        showmessage(E.Message);
       end;

      qryLang.SQL.Text:= 'SELECT * FROM memTblLang Order By rID';
      qryLang.Open;

      qryLang.SQL.Text:=
      'Select * From memTblLang Where ' +
      '((PropTypeKind = ''tkClass'' AND PropName = ''Items'') ' +
      'OR  '  +
      '(PropTypeKind = ''tkUString''))  ' +
      'AND ' +
      '(DefaultValue <> ''(empty)''  '  +
      'AND DefaultValue <> '''' '  +
      'AND PropName <> ''Name'')';

      qryLang.Open();

      memTblLang.EmptyDataSet;
      memTblLang.CopyDataSet(qryLang);

      for  a := 0 to 6 do
      grdCompProps.Fields[a].ReadOnly := True;

      HideColumns;
      grdCompProps.ReSizeAllColumnsToFit;
      ListComponents;
      FComponent.MemTableData:= DataManip.MemTableToStr(memTblLang);

end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
if Assigned(DataManip) then FreeAndNil(DataManip);
end;

constructor TfrmSettings.Create(AOwner: TComponent;
            AComponent: TMyLangs; ADesigner: IDesigner);
begin

  inherited Create(AOwner);

  FComponent := AComponent;
  FDesigner := ADesigner;

  if Assigned(FComponent) then
  begin
  F := GetCurrentUnitPath(UN) +
  FComponent.Owner.UnitName + '_' +  FComponent.Name + '.MYL';

  end
  else
  raise Exception.Create('FComponent or its Owner is not assigned!');


  if FileExists(F) then
  LoadFromFile;
end;


procedure TfrmSettings.FormCloseQuery(Sender: TObject;
var CanClose: Boolean);
begin
if FComponent.Changed = True then
 if MessageDlg(
 'If you don''t save your changes, your data may be lost. ' +
 #13#10 +  'And may crash your app at Runtime!!!' +
 #13#10 + 'DO YOU STILL WANT TO EXIT WITHOUT SAVING?',
 mtConfirmation, [mbYes, mbNo], 0) = mrNo then
 CanClose:= False;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
if not Assigned(DataManip) then
DataManip := TDataManipulation.Create;
//if  FComponent.MemTableData <> '' then Exit;

end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
   if FComponent.MemTableData = '' then
   GetComponentsDatasFirstTime
   else
   ReloadAllComponentsProperties;
end;




procedure TfrmSettings.ReloadAllComponentsProperties;
  var PropertiesList: TStringList;
  var Visited: TList<TObject>;
begin
  qryLang.Close;
  qryLang.Fields.Clear;
  grdCompProps.DataSource := nil;
  conLang.Connected := True;

  DataManip.StrToMemTable(FComponent.MemTableData, memTblLang);


  try
    memTblControl.Close;
    memTblControl.FieldDefs.Clear;
    memTblControl.FieldDefs.Assign(memTblLang.FieldDefs);
    memTblControl.CreateDataSet;
    memTblControl.Open;
    memTblControl.CopyDataSet(memTblLang,
    [coStructure, coRestart, coAppend]);

  except
    raise;
  end;

  memTblLang.Close;

  qryLang.Close;

  locSQL.Connection := conLang;
  locSQL.DataSets.Clear;
  memTblLang.Name:= 'memTblLang';
  locSQL.DataSets.Add(memTblLang);
  locSQL.Active := True;


  PropertiesList := nil;
  Visited := nil;
  try
  PropertiesList := TStringList.Create;
  Visited := TList<TObject>.Create;


  AllPropertiesToMemtable(
  FComponent.Owner as TForm, PropertiesList, Visited);

    for var i := 0 to FComponent.Owner.ComponentCount -1 do
    begin

        AllPropertiesToMemtable(
        FComponent.Owner.Components[i], PropertiesList, Visited);
    end;

  finally
  PropertiesList.Free;
  Visited.Free;

  end;

  qryLang.Open;

  {}

 qryLang.SQL.Text:=
 'DELETE FROM memTblLang  ' +
 'WHERE rID NOT IN ( ' +
 'SELECT MIN(rID) FROM memTblLang GROUP BY Property)';
  qryLang.ExecSQL;

  qryLang.SQL.Text:= 'SELECT * FROM memTblLang Order By rID';
  qryLang.Open;

  qryLang.SQL.Text:=
  'Select * From memTblLang Where ' +
  '((PropTypeKind = "tkClass" AND PropName = "Items") ' +
  'OR '  +
  '(PropTypeKind = "tkUString") ) ' +
  'AND ' +
  '(DefaultValue <> "(empty)" '  +
  'AND DefaultValue <> "" '  +
  'AND PropName <> "Name")';
  qryLang.Open;

  memTblLang.EmptyDataSet;


  memTblLang.CopyDataSet(qryLang, [coAppend]);

  memTblControl.DisableControls;
  memTblControl.First;



  while not memTblControl.EOF do
  begin

     if memTblLang.Locate('Property',
     memTblControl.FieldByName('Property').AsString, []) then
     begin

        if memTblControl.FieldByName('Property').AsString =
        memTblLang.FieldByName('Property').AsString then
        begin
        UpdateFieldsFromControl(memTblControl, memTblLang, 4);
        end;

     end;
    memTblControl.Next;
    end;


 FComponent.MemTableData:=
 DataManip.MemTableToStr(memTblLang);


 qryLang.Refresh;

 qryLang.Open;
 grdCompProps.DataSource:= dsComps;

for var a := 0 to 6 do
grdCompProps.Fields[a].ReadOnly := True;

HideColumns;
grdCompProps.ReSizeAllColumnsToFit;
ListComponents;

end;

procedure UpdateFieldsFromControl(SourceTable, TargetTable: TFDMemTable;
                                  StartIndex: Integer);
var
  i: Integer;
begin

  TargetTable.Edit;
  try
    for i := StartIndex to SourceTable.FieldCount - 1 do
    begin

      TargetTable.FieldByName(SourceTable.Fields[i].FieldName).Value :=
        SourceTable.Fields[i].Value;


      TargetTable.Fields[1].Value := SourceTable.Fields[1].Value;
    end;
    TargetTable.Post;
  except
    TargetTable.Cancel;
    raise;
  end;
end;

procedure TfrmSettings.ListComponents;
var
  Node: TTreeNode;

begin

  tvCompCategories.Items.Clear;

  Node := tvCompCategories.Items.Add(nil, 'GENERAL PROPERTIES');
  tvCompCategories.Items.AddChild(Node, 'Caption');
  tvCompCategories.Items.AddChild(Node, 'Hint');
  tvCompCategories.Items.AddChild(Node, 'Items');
  tvCompCategories.Items.AddChild(Node, 'Lines');
  tvCompCategories.Items.AddChild(Node, 'Text');
  tvCompCategories.Items.AddChild(Node, 'Others');


  Node := tvCompCategories.Items.Add(nil, 'ALL COMPONENTS');
  for var i := 0 to FComponent.Owner.ComponentCount -1 do
  begin
    tvCompCategories.Items.AddChild(Node,
    FComponent.Owner.Components[i].Name);
  end;

  tvCompCategories.Items[0].Expand(True);
  tvCompCategories.TopItem := tvCompCategories.Items[0];
  qryLang.Open;
end;


procedure TfrmSettings.btnSaveClick(Sender: TObject);
var
F, UnitName: string;
SQLString: string;

begin


    if FComponent.Delimiter = '' then
    if MessageDlg('The "Delimiter" property has not been set.'+
    'This property cannot be left empty.' +
    'Would you like to use the default value: "^@~"?',
    mtConfirmation, [mbYes, mbNo],  0) = mrYes then
    begin
    FComponent.Delimiter := '^@~';
    if Assigned(FDesigner) then FDesigner.Modified;
    end
    else
    Exit;


    F:=GetCurrentUnitPath(UnitName) +
    FComponent.Owner.UnitName + '_' + FComponent.Name + '.MYL' ;
    if Not FileExists(F) then
    if MessageDlg(
    'The file that will store all your translation data will '+
    'be saved at the following location:' + #13#10 + #13#10 +
    GetCurrentUnitPath(UnitName) +
    FComponent.Owner.UnitName + '_' + FComponent.Name + '.MYL' +
    #13#10 +  #13#10 +
    'This file must be located in the same directory as the ' +
    'unit file of the component''s owner form. ' +
    'And you must NOT change the name or location of it. ' +
    'If you change the unit name, you must also change  ' +
    'the beginning part of this file''s name. ' +  #13#10 +
    'i.e: if you change  "Unit1.pas" -> "uMain.pas"' + #13#10 +
    ' you must also change ' + #13#10 +
    '"Unit1_Maylangs1.MYL" -> "uMain_Mylangs1.MYL" ' + #13#10 +
    'Otherwise, the component will not be able to locate this file.' +
    #13#10 +  #13#10 +
    'If you delete this component, you should also delete '+
    'the associated language file!'  +  #13#10 +
    'If you haven''t saved the project to its final directory yet, ' +
    'it''s recommended to do that first!'  + #13#10 +
    'Would you like to continue with the saving at this location? ',
    mtConfirmation, [mbYes, mbNo],  0) = mrNo then Exit;

    SQLString:=(
    'Select * From memTblLang Where ' +
    '((PropTypeKind = "tkClass" AND PropName = "Items") ' +
    'OR '  +
    '(PropTypeKind = "tkUString")) ' +
    'AND ' +
    '(DefaultValue <> "(empty)" '  +
    'AND DefaultValue <> "" '  +
    'AND PropName <> "Name")');


    grdCompProps.DataSource.DataSet.DisableControls;
    qryLang.Open(SQLString);


    qryLang.SaveToFile( GetCurrentUnitPath(UnitName) +
                        FComponent.Owner.UnitName + '_' +
                        FComponent.Name + '.MYL', sfJSON);


    SaveDBGridToCSV(
                    grdCompProps,
                    GetCurrentUnitPath(UnitName) +
                    FComponent.Owner.UnitName + '_' +
                    FComponent.Name + '.CSV');



    {Geçerli memTBlLang tablosunu SL olarak al ve stream
    olarak HEX formatýnda FComponent.Data özelline ata}
    ExportMemTableToStringList(memtblLang);


    qryLang.Open(SQLString);
    grdCompProps.DataSource.DataSet.EnableControls;


    FComponent.MemTableData:=
    DataManip.MemTableToStr(memTblLang);


    Designer.Modified;
    FComponent.Changed := False;

    grdCompProps.ReSizeAllColumnsToFit;
    HideColumns;

end;

procedure TfrmSettings.btnSetValuesClick(Sender: TObject);
begin
qryLang.Edit;
PasteClipboardToGrid(grdCompProps, qryLang);
qryLang.Post;
qryLang.Refresh;
end;


procedure TfrmSettings.HideColumns;
begin
  grdCompProps.Columns[0].Visible:= False;
  grdCompProps.Columns[4].Visible:= False;
  grdCompProps.Columns[5].Visible:= False;
  grdCompProps.Columns[6].Visible:= False;
end;




procedure TfrmSettings.AllPropertiesToMemTable(
  AComponent: TObject; AStringList: TStringList;
  AVisited: TList<TObject>; MaxDepth: Integer = 5;
  CurrentDepth: Integer = 0; AParentName: string = '');
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
  PropTyp: TRttiType;
  Value: TValue;
  J: integer;
  SetValue: Integer;
  SetStr: string;
  FullName: string;
  PropTypKind: string;

begin
  if AComponent is TMyLangs then Exit;

  if CurrentDepth > MaxDepth then Exit;


  AVisited.Add(AComponent);

  Ctx := TRttiContext.Create;
  try
    PropTyp := Ctx.GetType(AComponent.ClassType);

    for Prop in PropTyp.GetProperties do
    begin

      if Prop.PropertyType.TypeKind = tkMethod then continue;
      if not (Prop.Visibility = mvPublished) then continue;

      {  }
      Value := Prop.GetValue(AComponent);
       if not Value.IsObject then
      begin
        if not (Prop.Name = 'Caption') and
           not (Prop.Name = 'Hint') and
           not (RightStr(Prop.Name, 4) = 'Text') then
           Continue;
      end;


      PropTypKind:= GetEnumName(TypeInfo(TTypeKind),
      Integer(Prop.PropertyType.TypeKind));

      try

        Value := Prop.GetValue(AComponent);

        if AParentName = '' then
          FullName := TComponent(AComponent).Name + '.' + Prop.Name
         else
          FullName := AParentName + '.' + Prop.Name;


        if Prop.IsReadable then
        begin

          if (not Value.IsObject) or (Value.AsObject = nil) then
          begin

            if Prop.PropertyType.TypeKind = tkSet then
            begin

              var SetType := TRttiSetType(Prop.PropertyType);
              var BaseType := SetType.ElementType.Handle;
              SetStr := '[';
              var First := True;

              SetValue := 0;
              Move(Value.GetReferenceToRawData^, SetValue, SizeOf(Integer));

              for var i := 0 to GetTypeData(BaseType)^.MaxValue do
              begin

                if (SetValue and (1 shl i)) <> 0 then
                begin
                var EnumName := GetEnumName(BaseType, i);
                  if EnumName <> '' then
                  begin

                   memTblLang.Append;
                   memTblLang.FieldByName('Property').AsWideString :=
                   FullName + '.' + EnumName;
                   memTblLang.FieldByName
                   ('DefaultValue').AsWideString :='True';
                   memTblLang.FieldByName
                   ('PropName').AsWideString := Prop.Name;
                   memTblLang.FieldByName
                   ('PropType').AsWideString := Prop.PropertyType.Name;
                   memTblLang.FieldByName
                   ('PropTypeKind').AsWideString := PropTypKind;
                   memTblLang.post;
                  end;

                    if not First then SetStr := SetStr + ', ';

                    SetStr := SetStr + EnumName + '=True';
                    First := False;
                end
                else
                begin
                  var EnumName := GetEnumName(BaseType, i);
                  if EnumName <> '' then
                  begin

                   if not (Prop.Name = 'Caption') and
                   not (Prop.Name = 'Hint') and

                   not (RightStr(Prop.Name, 4) = 'Text') then
                   Continue;

                   memTblLang.Append;
                   memTblLang.FieldByName
                   ('Property').AsWideString :=
                   FullName + '.' + EnumName;
                   memTblLang.FieldByName
                   ('DefaultValue').AsWideString := 'False';
                   memTblLang.FieldByName
                   ('PropName').AsWideString := Prop.Name;
                   memTblLang.FieldByName
                   ('PropType').AsWideString := Prop.PropertyType.Name;
                   memTblLang.FieldByName
                   ('PropTypeKind').AsWideString := PropTypKind;
                   memTblLang.post;
                  end;

                if not First then SetStr := SetStr + ', ';

                SetStr := SetStr + EnumName + '=False';
                First := False;
                end;

              end;


              SetStr := SetStr + ']';

            end
            else
            begin

             if not (Prop.Name = 'Caption') and
             not (Prop.Name = 'Hint') and
             not (RightStr(Prop.Name, 4) = 'Text') then
             Continue;


             memTblLang.Append;
             memTblLang.FieldByName('Property').AsWideString := FullName;
             memTblLang.FieldByName('DefaultValue').AsWideString := Value.ToString;
             memTblLang.FieldByName('PropName').AsWideString := Prop.Name;
             memTblLang.FieldByName('PropType').AsWideString := Prop.PropertyType.Name;
             memTblLang.FieldByName('PropTypeKind').AsWideString := PropTypKind;
             memTblLang.post;
            end;

          end


          else
          begin

          if  (Value.AsObject is TStrings) then
          begin
            var Items := TStrings(Value.AsObject);

            for  j := 0 to Items.Count - 1 do
            begin

             memTblLang.Append;
             memTblLang.FieldByName('Property').AsWideString :=
             FullName + '[' + j.ToString + ']';
             memTblLang.FieldByName('DefaultValue').AsWideString :=Items[j];
             memTblLang.FieldByName('PropName').AsWideString := Prop.Name;;
             memTblLang.FieldByName('PropType').AsWideString := Prop.PropertyType.Name;
             memTblLang.FieldByName('PropTypeKind').AsWideString := PropTypKind;
             memTblLang.Post;
            end;

          end;

          if Value.IsObject and (Value.AsObject is TCollection) then
          begin
            var Collection := TCollection(Value.AsObject);
            for var i := 0 to Collection.Count - 1 do
            begin
            var Item := Collection.Items[i];

            AllPropertiesToMemTable(Item, AStringList, AVisited,
            MaxDepth, CurrentDepth + 1, FullName +
            '[' + IntToStr(i) + ']');
            end;

          end

          else
            AllPropertiesToMemTable(Value.AsObject, AStringList,
            AVisited, MaxDepth, CurrentDepth + 1, FullName);
          end;


        end;
      except
        on E: Exception do
        begin
          showmessage(FullName +'-'+Value.ToString +'-'+
          Prop.Name +'-'+
          GetEnumName(TypeInfo(TTypeKind),
          Integer(Prop.PropertyType.TypeKind)));
          showmessage('HATA: ' + E.Message);
          Break;
        end;
      end;
    end;

  finally
    Ctx.Free;

  end;

end;

procedure TfrmSettings.btnReadMYLFileClick(Sender: TObject);
var F: string;
ColumnIndex: Byte;
OpenDialog: TOpenDialog;
begin

        OpenDialog := TOpenDialog.Create(nil);
        try
          OpenDialog.Title := 'Select a "MYL" file';
          OpenDialog.Filter :=
          'MYL Files (*.myl)|*.myl|All Files (*.*)|*.*';
          OpenDialog.InitialDir := GetCurrentDir;

          if OpenDialog.Execute then F:= OpenDialog.FileName;
        finally
          OpenDialog.Free;
        end;


      if Not FileExists(F) then
       begin
        Showmessage(F + 'The file is missing.' );
        Exit;
       end;


      qryLang.Close;

      qryLang.Fields.Clear;
      grdCompProps.DataSource := nil;
      grdCompProps.Columns.Clear;
      FComponent.Languages.Clear;
      conLang.Connected := True;

      memTblLang.LoadFromFile(F, sfJSON);

      for ColumnIndex := 7 to memTblLang.Fields.Count - 1 do
       FComponent.Languages.Add(memTblLang.Fields[ColumnIndex].FieldName);

      dsComps.DataSet:= qryLang;
      grdCompProps.DataSource := dsComps;

      locSQL.Connection := conLang;
      locSQL.DataSets.Clear;
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;


      qryLang.Open('Select * From memTblLang');




      try
        memTblControl.Close;
        memTblControl.FieldDefs.Clear;

        memTblControl.FieldDefs.Assign(memTblLang.FieldDefs);
        memTblControl.CreateDataSet;
        memTblControl.CopyDataSet(memTblLang, [
        coStructure, coRestart, coAppend]);
      except
        raise;
      end;


      memTblLang.Close;
      qryLang.Close;


      locSQL.Connection := conLang;
      locSQL.DataSets.Clear;
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;

      var PropertiesList: TStringList;
      var Visited: TList<TObject>;
      PropertiesList := TStringList.Create;
      Visited := TList<TObject>.Create;


      AllPropertiesToMemtable(
      FComponent.Owner as TForm, PropertiesList, Visited);

      for var i := 0 to FComponent.Owner.ComponentCount -1 do
      begin

          AllPropertiesToMemtable(
          FComponent.Owner.Components[i], PropertiesList, Visited);
      end;

      PropertiesList.Free;
      Visited.Free;


     qryLang.SQL.Text:=
     'DELETE FROM memTblLang  ' +
     'WHERE rID NOT IN ( ' +
     'SELECT MIN(rID) FROM memTblLang GROUP BY Property)';
      qryLang.ExecSQL;
      qryLang.ExecSQL;


      qryLang.SQL.Text:= 'SELECT * FROM memTblLang Order By rID';
      qryLang.Open;


      qryLang.SQL.Text:=
      'Select * From memTblLang Where ' +
      '((PropTypeKind = "tkClass" AND PropName = "Items") ' +
      'OR '  +
      '(PropTypeKind = "tkUString")) ' +
      'AND ' +
      '(DefaultValue <> "(empty)" '  +
      'AND DefaultValue <> "" '  +
      'AND PropName <> "Name")';
      qryLang.Open;


      memTblLang.EmptyDataSet;
      memTblLang.CopyDataSet(qryLang, [coAppend]);

      memTblControl.DisableControls;
      memTblControl.First;
      while not memTblControl.EOF do
      begin


       if memTblLang.Locate('Property',
       memTblControl.FieldByName('Property').AsString, []) then
       begin

       if memTblControl.FieldByName('Property').AsString =
          memTblLang.FieldByName('Property').AsString then
          begin
          UpdateFieldsFromControl(memTblControl, memTblLang, 3);
          end;
       end;
      memTblControl.Next;
      end;


      qryLang.Refresh;


    grdCompProps.ReSizeAllColumnsToFit;
    HideColumns;
    ListComponents;
end;

procedure TfrmSettings.btnLanguagesClick(Sender: TObject);
  var
  LanguagesForm: TfrmLanguages;
begin

  LanguagesForm :=
  TfrmLanguages.Create(nil, FComponent as TMyLangs, Self.Designer);


  try

      LanguagesForm.ShowModal;
   finally
      LanguagesForm.Free;

 end;




  if FComponent.MemTableData <> '' then
  begin
        if Assigned(DataManip) Then
        DataManip.StrToMemTable(FComponent.memTableData, memTblLang);

        grdCompProps.DataSource:=nil;
        qryLang.Close;
        grdCompProps.Columns.Clear;
        locSQL.Connection := conLang;
        locSQL.DataSets.Clear;
        memTblLang.Name:= 'memTblLang';
        locSQL.DataSets.Add(memTblLang);
        locSQL.Active := True;

        dsComps.DataSet := qryLang;

        qryLang.SQL.Text := 'Select * From memTblLang';
        qryLang.Active := True;
        grdCompProps.DataSource:= dsComps;
        HideColumns;
        grdCompProps.ReSizeAllColumnsToFit;
        ListComponents;
  end;



end;


procedure TfrmSettings.AddNewField(NewField: string);
var Column:TColumn;
  MemTable: TFDMemTable;
begin
exit;

MemTable := TFDMemTable.Create(nil);

try

 MemTable.CopyDataSet(memTblLang, [coStructure, coRestart, coAppend]);

  memTblLang.Close;
  memTblLang.FieldDefs.Clear;

  memTblLang.Fields.Clear;



  for var i := 0 to MemTable.FieldDefs.Count - 1 do
    memTblLang.FieldDefs.Add(MemTable.FieldDefs[i].Name,
    MemTable.FieldDefs[i].DataType, MemTable.FieldDefs[i].Size);

    memTblLang.FieldDefs.Add(NewField, ftWideString, 100);

    memTblLang.CreateDataSet;

    grdCompProps.Columns.Clear;


    for var i := 0 to memTblLang.FieldCount - 1 do
    begin
      Column := grdCompProps.Columns.Add;
      Column.FieldName := memTblLang.Fields[i].FieldName;
      Column.Title.Caption := memTblLang.Fields[i].DisplayName;
      Column.Width := memTblLang.Fields[i].DisplayWidth;
      Column.ReadOnly := False;
    end;


      memTblLang.open;

      MemTable.open;

      MemTable.First;


      while not MemTable.Eof do
      begin
        memTblLang.Append;
        for var t: Integer := 0 to MemTable.FieldCount - 1 do
          memTblLang.Fields[t].Value := MemTable.Fields[t].Value;
        memTblLang.Post;
        MemTable.Next;
      end;

      memTblLang.Refresh;
      locSQL.DataSets.Clear;
      memTblLang.Name := 'memTblLang';
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;

      qryLang.SQL.Text := 'Select * From memTblLang';
      qryLang.Active := True;
      finally
        FreeAndNil(MemTable);
      end;



      FreeAndNil(MemTable);

      qryLang.Close;


      locSQL.DataSets.Clear;
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;

      qryLang.SQL.Text := 'Select * From memTblLang';
      qryLang.Active := True;



end;


function String2Hex(const Buffer: AnsiString): string;
begin
  SetLength(Result, Length(Buffer) * 2);
  BinToHex(PAnsiChar(Buffer), PChar(Result), Length(Buffer));
end;

function Hex2String(const Buffer: string): AnsiString;
begin
  SetLength(Result, Length(Buffer) div 2);
  HexToBin(PChar(Buffer), PAnsiChar(Result), Length(Result));
end;



function BytesToHex(const Buffer: TBytes): string;
begin
  if Length(Buffer) = 0 then Exit('');

  SetLength(Result, Length(Buffer) * 2);
  BinToHex(@Buffer[0], PChar(Result), Length(Buffer));
end;


function HexToBytes(const HexStr: string): TBytes;
var
  Len: Integer;
begin
  Len := Length(HexStr);


  if (Len = 0) or (Len mod 2 <> 0) then
    raise Exception.Create(
    'Invalid string length, it must be even number');


  SetLength(Result, Len div 2);


  HexToBin(PChar(HexStr), @Result[0], Len div 2);
end;


procedure TfrmSettings.PropertiesToTreeView(
                        AComponent: TObject; ATreeView: TTreeView;
                        AVisited: TList<TObject>; MaxDepth: Integer = 5;
                        CurrentDepth: Integer = 0;
                        AParentNode: TTreeNode = nil);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Value: TValue;
  PropNode: TTreeNode;
begin



  if CurrentDepth > MaxDepth then Exit;


  if AVisited.Contains(AComponent) then Exit;

  if not AVisited.Contains(AComponent) then
  AVisited.Add(AComponent);

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AComponent.ClassType);

    for Prop in Typ.GetProperties do
    begin

      if Prop.PropertyType.TypeKind = tkMethod then continue;
      if Prop.Visibility <> mvPublished then continue;

      try

        Value := Prop.GetValue(AComponent);


        if Prop.IsReadable then
        begin

          if Assigned(AParentNode) then
          begin
            PropNode := ATreeView.Items.AddChild(
            AParentNode, Prop.Name + '=>' + Value.ToString);
          end
          else
          begin
            PropNode := ATreeView.Items.Add(
            nil, Prop.Name + '=>>' + Value.ToString);
          end;


          if Value.IsObject and (Value.AsObject <> nil) then
          begin

            if Value.AsObject is TStrings then
            begin
              var Items := TStrings(Value.AsObject);
              for var j := 0 to Items.Count - 1 do
              begin
                ATreeView.Items.AddChild(PropNode,
                Format('Items[%d] =>>> %s', [j, Items[j]]));
              end;
            end
            else
            begin

              PropertiesToTreeView(
              Value.AsObject, ATreeView, AVisited,
              MaxDepth, CurrentDepth + 1, PropNode);
            end;
          end;


          if Prop.PropertyType is TRttiSetType then
          begin
            var SetType := TRttiSetType(Prop.PropertyType);
            var BaseType := SetType.ElementType.Handle;
            var SetValue := Prop.GetValue(AComponent);


            for var i := 0 to GetTypeData(BaseType)^.MaxValue do
            begin
              var SetEnumName := GetEnumName(BaseType, i);

              ATreeView.Items.AddChild(PropNode, SetEnumName);
            end;
          end;
        end;
      except
        on E: Exception do
        begin
          ATreeView.Items.AddChild(
          AParentNode, 'ERROR: ' + Prop.Name + ': ' + E.Message);
        end;
      end;
    end;
  finally
    Ctx.Free;

  end;
end;

procedure TfrmSettings.qryLangAfterDelete(DataSet: TDataSet);
begin
  FComponent.MemTableData:= DataManip.MemTableToStr(memTblLang);
  FComponent.Changed := True;
end;

procedure TfrmSettings.qryLangAfterPost(DataSet: TDataSet);
begin
  FComponent.MemTableData:= DataManip.MemTableToStr(memTblLang);
  FComponent.Changed := True;
end;

procedure TfrmSettings.tvCompCategoriesClick(Sender: TObject);
 var VisitedList: TList<TObject>;
begin


  if (TTreeNode(tvCompCategories.Selected).Level = 0)
   AND (tvCompCategories.Selected.Text =  'GENERAL PROPERTIES') then
   begin
   qryLang.Open('SELECT * FROM memTblLang');
   HideColumns;
   grdCompProps.ReSizeAllColumnsToFit;

   Exit;
   end;

  if TTreeNode(tvCompCategories.Selected).Level = 0 then Exit;

  if tvCompCategories.Selected.Parent.Text = 'GENERAL PROPERTIES' then
  begin

  trvCompProps.Visible:= False;


  qryLang.SQL.Text:=
  'SELECT * FROM memTblLang Where PropName=' +
  QuotedStr(tvCompCategories.Selected.Text);


  if tvCompCategories.Selected.Text = 'Others' then
  qryLang.SQL.Text:=
  'SELECT * FROM memTblLang Where '+
  'PropTypeKind= ''tkUString'' AND '+
  'PropName NOT IN (''Caption'', ''Hint'', '+
  '''Items'', ''Text'', ''Name'')'
  ;
  qryLang.Open;

  HideColumns;
  grdCompProps.ReSizeAllColumnsToFit;

  Exit;
  end;


    if tvCompCategories.Selected.Parent.Text = 'ALL COMPONENTS' then
    begin
     VisitedList := nil;
    trvCompProps.Visible:= True;
    try

    VisitedList := TList<TObject>.Create;
    trvCompProps.Items.Clear;
    trvCompProps.Items.BeginUpdate;

    PropertiesToTreeView(
    FComponent.Owner.FindComponent(tvCompCategories.Selected.Text),
    trvCompProps, VisitedList);
    finally
      trvCompProps.Items.EndUpdate;
      VisitedList.Free;
    end;

  end;
end;

procedure TfrmSettings.tvCompCategoriesCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultValueDraw: Boolean);

begin

  if Node.Level = 0 then
    tvCompCategories.Canvas.Font.Style := [fsBold]
  else if Node.Level = 1 then
    tvCompCategories.Canvas.Font.Color := clBlue
  else
    tvCompCategories.Canvas.Font.Color := clGray;

    if (cdsSelected in State) and
        not tvCompCategories.Focused then
    begin

    tvCompCategories.Canvas.Brush.Color := clYellow;
    tvCompCategories.Canvas.Font.Color := clBlack;


    tvCompCategories.Canvas.FillRect(Node.DisplayRect(False));
    DefaultValueDraw := False;
    end;



  DefaultValueDraw := True;

end;

procedure TfrmSettings.tvCompCategoriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  Node := tvCompCategories.GetNodeAt(X, Y);
  if Node = nil then
    Exit;
end;

procedure TfrmSettings.ListLanguages;
var Column: TColumn; a: Byte;
begin

    showmessage(FComponent.Languages.Count.ToString);
    showmessage(FComponent.Languages[0]);
  for a := 0 to FComponent.Languages.Count -1 do
  begin
  memTblLang.FieldDefs.Add(FComponent.Languages[a], ftWideString, 100);
  showmessage('------------------');

  Column := grdCompProps.Columns.Add;

  Column.FieldName := FComponent.Languages[a];
  Column.Title.Caption := Column.FieldName;
  Column.Width := 100;
  end;

memTblLang.Open;
showmessage(memTblLang.FieldDefList.Text);


grdCompProps.ReSizeAllColumnsToFit;
HideColumns;
end;



procedure TfrmSettings.memTblLangAfterOpen(DataSet: TDataSet);
begin
FComponent.MemTableData:= DataManip.MemTableToStr(memTblLang);
end;

procedure TfrmSettings.memTblLangNewRecord(DataSet: TDataSet);
begin
DataSet.FieldByName('Active').AsInteger := 1;
end;

procedure TfrmSettings.GetComponentAllDetails(AComponent: TComponent);
var
  j: Integer;
  Tip: TRttiType;
  SubRttiType: TRttiType;
  Context: TRttiContext;
  Prop: TRttiProperty;
  SubProp: TRttiProperty;

  PropValue: TValue;
  SubValue: TValue;

begin
       showmessage('GetComponentAllDetails');
    try
        Context := TRttiContext.Create;

        Tip := Context.GetType(AComponent.ClassType);

        memTblLang.Active:= True;
        memTblLang.DisableControls;
        memTblLang.EmptyDataSet;
        for Prop in Tip.GetProperties do
        begin
            if not Prop.IsReadable then
            begin


            end;


          try

            PropValue := Prop.GetValue(AComponent);

            if (PropValue.IsObject) and (PropValue.AsObject <> nil) then
            showmessage(Prop.Name + ' is an object');



            memTblLang.Append;
            memTblLang.FieldByName('Property').AsString :=
                AComponent.Name + '.' + Prop.Name;
            memTblLang.FieldByName('VALUE').ASString :=
                PropValue.AsString;
            memTblLang.Post;
            except

            on E: Exception do
            begin
            Application.ProcessMessages;

            memTblLang.Append;
            memTblLang.FieldByName('Property').AsString :=
                AComponent.Name + '.' + Prop.Name +
                ' Error while retrieving PropValue: ' + E.Message;
            memTblLang.Post;
            end;
          end;



          if (PropValue.IsObject) and (PropValue.AsObject <> nil)
          then
          begin
          memTblLang.open;



            SubRttiType := Context.GetType(PropValue.AsObject.ClassType);


            for SubProp in SubRttiType.GetProperties do
            begin
              SubValue := SubProp.GetValue(PropValue.AsObject);

              memTblLang.Append;
              memTblLang.FieldByName('PROP').AsString :=
              AComponent.Name + '.' + SubProp.Name ;
              memTblLang.FieldByName('VALUE').ASString :=
              SubValue.ToString;
              memTblLang.Post;
            end;
          end;



          if (Prop.Name = 'Items') and (PropValue.IsObject) and
              (PropValue.AsObject is TStrings) then
          begin

            PropValue := Prop.GetValue(AComponent);
            if PropValue.IsObject and (PropValue.AsObject is TStrings) then
            begin
              for j := 0 to TStrings(PropValue.AsObject).Count - 1 do
              begin
              showmessage(Prop.PropertyType.ToString);
              memTblLang.Append;
              memTblLang.FieldByName('PROP').AsString :=
              AComponent.Name + '.' + Prop.Name + '[' + j.ToString + ']';
              memTblLang.FieldByName('VALUE').ASString :=
              TStrings(PropValue.AsObject)[j];
              memTblLang.Post;
              end;

            end;
            Continue;
          end;
          { }
        end;

    finally
      Context.Free;
      memTblLang.EnableControls;
    end;



qryLang.SQL.Text:=('Select Property, VALUE From memTblLang');
qryLang.Open;

end;



procedure TfrmSettings.grdCompPropsCellClick(Column: TColumn);
begin
    if grdCompProps.SelectedField.FieldNo = 2  then
    begin
    qryLang.Edit;
    grdCompProps.Fields[1].ReadOnly := False;
        if grdCompProps.SelectedField.Value = 0 then
        grdCompProps.SelectedField.Value := 1 else
        grdCompProps.SelectedField.Value := 0;

    qryLang.Post;

    for var a := 0 to 6 do
    grdCompProps.Fields[a].ReadOnly := True;
    grdCompProps.Fields[2].ReadOnly := True;

    end
    else if grdCompProps.SelectedField.FieldNo > 7  then
    grdCompProps.Options := grdCompProps.Options + [dgEditing];


end;

procedure TfrmSettings.grdCompPropsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
begin

     if grdCompProps.DataSource.DataSet.RecordCount = 0 then exit;

      if (Column.Field.DataSet.RecNo mod 2 = 0) then
      grdCompProps.Canvas.Brush.Color := $D2F5FF
    else
      grdCompProps.Canvas.Brush.Color := $F5F5EA;


     grdCompProps.DefaultDrawColumnCell(Rect, DataCol, Column, State);


  if (Column.FieldName = 'Active') then
  begin

      grdCompProps.Canvas.FillRect(Rect);
      ImageList.Draw(
        grdCompProps.Canvas,
        Rect.Left + (Rect.Width - 16) div 2,
        Rect.Top + (Rect.Height - 16) div 2,
        Column.Field.AsInteger);
  end;

end;


procedure TfrmSettings.grdCompPropsTitleClick(Column: TColumn);
var
  SortField: string;
begin
  if Assigned(grdCompProps.DataSource) and
      Assigned(grdCompProps.DataSource.DataSet) then
  begin
    SortField := Column.FieldName;

    with TFDQuery(grdCompProps.DataSource.DataSet) do
    begin

      SQL.Text := Format(
      'SELECT * FROM memTblLang ORDER BY %s COLLATE NOCASE ASC',
      [SortField]);
      Open;
    end;

    grdCompProps.ReSizeAllColumnsToFit;
    HideColumns;
  end;

end;

procedure TfrmSettings.QueryAComponentsDetails(AProperty: string);
begin
showmessage(qryLang.SQL.Text + ': '  + qryLang.RecordCount.ToString);
qryLang.SQL.Text:=
'Select * From memTblLang Where PropName = ' + QuotedStr(AProperty);
qryLang.Open;

showmessage(qryLang.SQL.Text + ': '  + qryLang.RecordCount.ToString);
end;




function GetCurrentUnitPath(out UnitName: String): String;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  idx: integer;

begin
  Result := '';
  SourceEditor := nil;

  if System.SysUtils.Supports(
  BorlandIDEServices, IOTAModuleServices, ModuleServices) then
  begin
    Module := ModuleServices.CurrentModule;
    UnitName:= Module.FileName;
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


procedure PasteClipboardToGrid(AGrid: TDBGrid; AMemTable: TFDQuery);
var
  ClipboardData: string;
  Rows: TArray<string>;
  Cols: TArray<string>;
  StartRow, StartCol, CurrentRow, I, J: Integer;
begin

  ClipboardData := Clipboard.AsText;


  if Trim(ClipboardData) = '' then
    Exit;


  Rows := ClipboardData.Split([sLineBreak], TStringSplitOptions.ExcludeEmpty);


  StartRow := AMemTable.RecNo;
  StartCol := AGrid.SelectedIndex;


  if not AMemTable.Active then
    AMemTable.Open;


  CurrentRow := StartRow;
  for I := 0 to High(Rows) do
  begin

    Cols := Rows[I].Split([#9], TStringSplitOptions.None);


    if CurrentRow > AMemTable.RecordCount then
      AMemTable.Append
    else
      AMemTable.RecNo := CurrentRow;

     AMemTable.edit;




    for J := 0 to High(Cols) do
    begin
      if (StartCol + J) < AMemTable.FieldCount then
        AMemTable.Fields[StartCol + J].AsString := Trim(Cols[J]);
    end;

    Inc(CurrentRow);
  end;
end;



procedure TfrmSettings.DeleteColumnFromDBGrid(DBGrid: TDBGrid;
          const ColumnName: string);
var
  I: Integer;
begin

  for I := DBGrid.Columns.Count - 1 downto 0 do
  begin

    if SameText(DBGrid.Columns[I].FieldName, ColumnName) then
    begin

      DBGrid.Columns[I].Free;

      Exit;
    end;
  end;

end;



procedure TfrmSettings.ChangeColumnNameofDBGrid(DBGrid: TDBGrid;
          const ColumnName, NewColumnName: string);
var
  I: Integer;
begin

  for I := DBGrid.Columns.Count - 1 downto 0 do
  begin

    if SameText(DBGrid.Columns[I].FieldName, ColumnName) then
    begin

      DBGrid.Columns[I].Title.Caption:= NewColumnName ;

      Exit;
    end;
  end;

end;



procedure TfrmSettings.DeleteFieldAndRefresh(MemTable: TFDMemTable;
          const FieldName: string);
var
  Field: TField;
  TemFL: TFieldDefs;
  s: TStrings;
begin
  TemFL:= TFieldDefs.Create(nil);
  TemFL.Assign(MemTable.FieldDefs);

  Field := MemTable.FieldByName(FieldName);
  TemFL.Delete(Field.Index);


  s:= TStringList.Create;
  TemFL.GetItemNames(s);



  qryLang.SQL.Text:=
  'Select ' + s.CommaText + ' From memTblLang';

  qryLang.Open;


  MemTable.Close;
  MemTable.FieldDefs.Assign(TemFL);
  MemTable.CreateDataSet;
  MemTable.CopyDataSet(qryLang);

end;


function GetCurrentUnitPath2: String;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  idx: integer;

begin
  Result := '';
  SourceEditor := nil;

  if Supports(BorlandIDEServices, IOTAModuleServices,
    ModuleServices) then
  begin
    Module := ModuleServices.CurrentModule;

    if System.Assigned(Module) then
    begin
      idx := Module.GetModuleFileCount - 1;


      while (idx >= 0) and not Supports(Module.GetModuleFileEditor(idx), IOTASourceEditor, SourceEditor) do
        System.Dec(idx);


      if idx >= 0 then
        Result := ExtractFilePath(SourceEditor.FileName);
    end;

  end;

end;



procedure TfrmSettings.ChangeColumnName(OldName, NewName: string);
var Field: TField;
SL: string;
begin

      grdCompProps.DataSource:=nil;
      qryLang.EmptyDataSet;
      qryLang.SQL.Clear;
      qryLang.FieldDefs.Clear;
      qryLang.Fields.Clear;
      qryLang.Close;


      SL:= '';
      for Field in MemTblLang.Fields do
      begin
      if Field.FieldName = OldName then
      SL:= SL + ', ' + (Field.FieldName) + ' AS ' + NewName
      else
      SL:= SL + ', ' + (Field.FieldName);
      end;
      Delete(SL, 1, 1);


      qryLang.Open('Select ' + SL + ' From memTblLang');


      memTblLang.EmptyDataSet;
      memTblLang.FieldDefs.Clear;
      memTblLang.Close;



      memTblLang.FieldDefs.Assign(qryLang.FieldDefs);
      memTblLang.FieldDefs.Update;
      memTblLang.CreateDataSet;
      memTblLang.Open;



      locSQL.Connection := conLang;
      locSQL.DataSets.Clear;
      locSQL.DataSets.Add(memTblLang);
      locSQL.Active := True;



      memTblLang.CopyDataSet(qryLang,
      [coStructure, coRestart, coAppend]);


      qryLang.SQL.Text := 'SELECT Property FROM memTblLang';
      qryLang.Open;

      qryLang.Close;

      qryLang.Open('Select * From memTblLang');
      grdCompProps.DataSource:= dsComps;

      grdCompProps.ReSizeAllColumnsToFit;
      HideColumns;

end;


function TfrmSettings.CompressStringListToHex(
          const SL: TStringList): string;
var
  StringStream: TStringStream;
  InputStream, OutputStream: TMemoryStream;
  CompressionStream: TCompressionStream;
  Buffer: TBytes;
  i: Integer;
begin

  StringStream := TStringStream.Create(SL.Text, TEncoding.UTF8);
  try

    InputStream := TMemoryStream.Create;
    OutputStream := TMemoryStream.Create;
    try
      InputStream.CopyFrom(StringStream, StringStream.Size);
      InputStream.Position := 0;


      CompressionStream :=
      TCompressionStream.Create(clMax, OutputStream);
      try
        CompressionStream.CopyFrom(InputStream, InputStream.Size);
      finally
        CompressionStream.Free;
      end;


      OutputStream.Position := 0;
      SetLength(Buffer, OutputStream.Size);
      OutputStream.ReadBuffer(Buffer[0], OutputStream.Size);
      Result := '';
      for i := 0 to Length(Buffer) - 1 do
        Result := Result + IntToHex(Buffer[i], 2);
    finally
      InputStream.Free;
      OutputStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;


end.



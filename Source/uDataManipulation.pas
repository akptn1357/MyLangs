unit uDataManipulation;

interface
uses
  System.Classes, System.SysUtils, System.RTTI,System.ZLib,
  System.TypInfo, System.Generics.Collections,
  ToolsAPI,
  VCl.GraphUtil, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  Vcl.Dialogs,  Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Graphics, Vcl.Buttons,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Stan.Def,FireDAC.DApt.Intf,
  FireDAC.Stan.StorageBin, FireDAC.UI.Intf,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.VCLUI.Wait, FireDAC.DApt, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.SQLiteVDataSet,
  FireDAC.Comp.DataSet;

type
  TDataManipulation = class
  private
    conLang: TFDConnection;
    memTblLang: TFDMemTable;
    qryLang: TFDQuery;
    locSQL: TFDLocalSQL;

    PhysSQLiteDrv: TFDPhysSQLiteDriverLink;

    public
    constructor Create;
    destructor Destroy; override;
    function ChangeLanguageName(OldName, NewName, MemStr: string): string;
    function DeleteLanguage(FieldName, MemStr: string): string;
    function HexToBytes(const HexStr: string): TBytes;
    function BytesToHex(const Buffer: TBytes): string;
    function AddNewLanguage(NewField: string; MemStr: string;
                            SL: TstringList = nil): string;
    function MemTableToStr(Memtable: TFDMemTable): string;
    procedure StrToMemTable(Str: string; Memtable: TFDMemTable);
    function GetCurrentUnitPath(out UnitName: String): String;
  end;


implementation


constructor TDataManipulation.Create;
begin

  inherited Create;


conLang := TFDConnection.Create(nil);
qryLang := TFDQuery.Create(nil);
memTblLang := TFDMemTable.Create(nil);
memTblLang.Name := 'memTblLang';
locSQL := TFDLocalSQL.Create(nil);


conLang.DriverName := 'SQLite';
conLang.LoginPrompt:= False;

conLang.Params.Values['Database'] := ':memory:';

qryLang.Connection:= conLang;
qryLang.FetchOptions.RecordCountMode:=cmTotal;
conLang.Connected := True;


conLang.ExecSQL(
'CREATE TABLE CONTROL ( ' +
'rID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
'Active INTEGER(1),' +
'Property TEXT(300), ' +
'DefaultValue TEXT(300), '  +
'PropName TEXT(100), ' +
'PropType TEXT(100),  '  +
'PropTypeKind TEXT(100) );'
);

end;



destructor TDataManipulation.Destroy;
begin

  if locSQL <> nil then locSQL.Free;
  if qryLang <> nil then qryLang.Free;
  if memTblLang <> nil then memTblLang.Free;
  if PhysSQLiteDrv <> nil then PhysSQLiteDrv.Free;
  if conLang <> nil then conLang.Free;

  inherited Destroy;
end;


function TDataManipulation.GetCurrentUnitPath(out UnitName: String): String;
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



function TDataManipulation.ChangeLanguageName(
          OldName, NewName, MemStr: string): string;
var Field: TField;
SL: string;
begin

    if (qryLang.Active) then
    begin
    qryLang.EmptyDataSet;
    qryLang.SQL.Clear;
    qryLang.FieldDefs.Clear;
    qryLang.Fields.Clear;
    qryLang.Close;
    end;


    StrToMemTable(MemStr, memTblLang);

    locSQL.Connection := conLang;
    locSQL.DataSets.Clear;
    memTblLang.Name := 'memTblLang';
    locSQL.DataSets.Add(memTblLang);
    locSQL.Active := True;


    SL:= '';
    for Field in memTblLang.Fields do
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
    memTblLang.Name := 'memTblLang';
    locSQL.DataSets.Add(memTblLang);
    locSQL.Active := True;


    memTblLang.CopyDataSet(qryLang,
    [coStructure, coRestart, coAppend]);

    Result:= MemTableToStr(memTblLang);

end;


function TDataManipulation.DeleteLanguage(FieldName, MemStr: string): string;
var
  Field: TField;
  TemFL: TFieldDefs;
  s: TStrings;
begin


  StrToMemTable(MemStr, memTblLang);

  TemFL:= TFieldDefs.Create(nil);
  TemFL.Assign(memTblLang.FieldDefs);
  Field := memTblLang.FieldByName(FieldName);
  TemFL.Delete(Field.Index);

  s:= TStringList.Create;
  TemFL.GetItemNames(s);

  locSQL.Connection := conLang;
  locSQL.DataSets.Clear;
  memTblLang.Name := 'memTblLang';
  locSQL.DataSets.Add(memTblLang);
  locSQL.Active := True;

  qryLang.SQL.Text:=
  'Select ' + s.CommaText + ' From memTblLang';

  qryLang.Open;


  memTblLang.Close;
  memTblLang.FieldDefs.Assign(TemFL);
  memTblLang.CreateDataSet;
  memTblLang.CopyDataSet(qryLang);

  Result:= MemTableToStr(memTblLang);

end;

function TDataManipulation.HexToBytes(const HexStr: string): TBytes;
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


function TDataManipulation.BytesToHex(const Buffer: TBytes): string;
begin
  if Length(Buffer) = 0 then Exit('');

  SetLength(Result, Length(Buffer) * 2);
  BinToHex(@Buffer[0], PChar(Result), Length(Buffer));
end;



function TDataManipulation.MemTableToStr(Memtable: TFDMemTable): string;
var
  Reader: TStreamReader;
  MemStream: TMemoryStream;
begin
    MemStream := TMemoryStream.Create;
    try
      Reader := TStreamReader.Create(MemStream, TEncoding.UTF8);
      MemTable.SaveToStream(MemStream, sfJSON);
      MemStream.Position := 0;

      try
      Result:= Reader.ReadToEnd;
      finally
      Reader.Free;
      end;

     finally
       MemStream.Free;
    end;
end;

procedure TDataManipulation.StrToMemTable(Str: string; Memtable: TFDMemTable);
var
  MemStream: TMemoryStream;
  Writer: TStreamWriter;
begin

  MemStream := TMemoryStream.Create;
  try

    Writer := TStreamWriter.Create(MemStream, TEncoding.UTF8);
    try
      Writer.Write(Str);
      Writer.Flush;
      MemStream.Position := 0;
    finally
      Writer.Free;
    end;

    MemTable.LoadFromStream(MemStream, sfJSON);

  finally
    MemStream.Free;
  end;

end;


function TDataManipulation.AddNewLanguage(
          NewField: string; MemStr: string;
          SL: TstringList = nil): string;
var
  I: byte;
  TempMemtable: TFDMemTable;
  MemStream: TMemoryStream;
  Reader: TStreamReader;
begin

  TempMemTable := TFDMemTable.Create(nil);
  MemStream := TMemoryStream.Create;

  try
    StrToMemTable(MemStr, TempMemTable);

    TempMemTable.SaveToStream(MemStream, sfJSON);
    TempMemTable.Close;

    TempMemTable.FieldDefs.Update;

    if SL <> nil then
    begin
     for I := 0 to SL.Count-1 do
     begin
     TempMemTable.FieldDefs.Add(SL[I], ftWideString, 100);
     end;
    end
    else
    TempMemTable.FieldDefs.Add(NewField, ftWideString, 100);

    TempMemTable.CreateDataSet;
    MemStream.Position := 0;
    memTblLang.LoadFromStream(MemStream, sfJSON);

    MemStream.Size := 0;

    TempMemTable.CopyDataSet(memTblLang, [coRestart, coAppend]);

    TempMemTable.SaveToStream(MemStream, sfJSON);
    MemStream.Position := 0;

    Reader := TStreamReader.Create(MemStream, TEncoding.UTF8);
    try
    Result:= Reader.ReadToEnd;
    finally
      Reader.Free;
    end;


   finally
     TempMemTable.Free;
     MemStream.Free;
  end;

end;

end.

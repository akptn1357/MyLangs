unit MyLangsRT;

interface

uses
  System.Classes, System.SysUtils, System.RTTI, System.Zlib,
  VCL.Dialogs, VCl.Controls, System.TypInfo, Vcl.Forms,
  System.StrUtils;

{$R TMyLangs.res}
type
  TMyLangs = class(TComponent)
  private
    FLanguages: TStringList;
    FDefaultLanguage: string;
    FData: string;
    FDelimiter: string;
    FMemTableData: string;
    FChanged: Boolean;
    procedure SetProperty(const PropPath: string; const Value: Variant);
    procedure SetCompressedData(const Value: string);
    function GetCompressedData: string;
    procedure SetLanguages(const Value: TStringList);
    procedure SetDelimiter(const Value: string);
    function GetLanguages: TStringList;
    procedure SetDefaultLanguage(const Value: string);
    function DecompressHexToString(const HexData: string): string;
    function CompressStringToHex(const s: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CompressedData: string read GetCompressedData write SetCompressedData;
    property MemTableData: string read FMemTableData write FMemTableData;
    property Changed: Boolean read FChanged write FChanged default False;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure Loaded; override;
  published
    property Languages: TStringList read GetLanguages write SetLanguages;
    property DefaultLanguage: string read FDefaultLanguage write SetDefaultLanguage;
    property Delimiter: string read FDelimiter write SetDelimiter;
    function ChangeLanguage(NewLanguage: String): Boolean;
    procedure ChangeToDefaultLanguage;
  end;


implementation

{ TMyLangs}
procedure TMyLangs.ChangeToDefaultLanguage;
begin
if FDefaultLanguage <> '' then
ChangeLanguage(FDefaultLanguage)
else
ShowMessage('The active language hasn''t been set');
end;

procedure TMyLangs.Loaded;

begin
  inherited;
  if  not (csDesigning in ComponentState) then
  if FDefaultLanguage <> '' then ChangeLanguage(FDefaultLanguage);
end;

procedure TMyLangs.SetDelimiter(const Value: string);
begin
  if FDelimiter <> Value then
  begin
    FDelimiter := Value;
  end;
end;

function TMyLangs.ChangeLanguage(NewLanguage: string): Boolean;
var SL: TStringList; LangIndex: SmallInt;
I: SmallInt; Parts: TArray<string>;
begin
Result := False;

  if FData = '' then
  begin
  ShowMessage('No Translation Data found!!');
  Exit;
  end;
  if FDelimiter = '' then
  begin
  ShowMessage('No Delimiter found!!');
  Exit;
  end;
  if not Assigned(FLanguages) then
  begin
  ShowMessage('No Languages List found!!');
  Exit;
  end;


SL:= TStringList.Create;
SL.Text:= DecompressHexToString(FData);

try
if FLanguages.IndexOf(NewLanguage) = -1 then
LangIndex:= -1
else
LangIndex:= FLanguages.IndexOf(NewLanguage);

    //showmessage(LangIndex.ToString + slinebreak +NewLanguage);

    for I := 1 to SL.Count -1 do
    begin
      Parts := SL[I].Split(FDelimiter);
      if Parts[LangIndex+2] <> '' then
      SetProperty(Parts[0], Parts[LangIndex+2]);
    end;
    finally
    SL.Free;
    end;

 Result:= True;
end;

constructor TMyLangs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not Assigned(FLanguages) then
  begin
    FLanguages := TStringList.Create;
  end;

  FDefaultLanguage := '';
  FData := '';
  FDelimiter := '^@~';

end;

destructor TMyLangs.Destroy;
begin

  if (csDesigning in ComponentState) then
  begin
  //..
  end;
  FLanguages.Free;
  inherited Destroy;

end;


procedure TMyLangs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TMyLangs.SetLanguages(const Value: TStringList);
var
  I: Integer;
begin
  if not Assigned(Value) then Exit;

  if not Assigned(FLanguages) then
    FLanguages := TStringList.Create;

  FLanguages.Clear;
  for I := 0 to Value.Count - 1 do
    FLanguages.Add(Trim(Value[I]));
end;


function TMyLangs.GetLanguages: TStringList;
begin
  Result := FLanguages;
end;


procedure TMyLangs.SetDefaultLanguage(const Value: string);
begin
  if (FLanguages.IndexOf(Value) <> -1) Or (Value = '') then
  begin
    FDefaultLanguage := Value;
  end
  else if Value <> '' then
  begin
    raise Exception.CreateFmt(
    'Language "%s" is not defined in the list.', [Value]);
  end;
end;



procedure TMyLangs.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, FData <> '');
end;


procedure TMyLangs.ReadData(Stream: TStream);
var
  L: Integer;
  Bytes: TBytes;
  I: Integer;
  HexStr: string;
begin
  L := Stream.Size - Stream.Position;
  SetLength(Bytes, L);
  Stream.ReadBuffer(Bytes[0], L);

  HexStr := '';
  for I := 0 to L - 1 do
    HexStr := HexStr + IntToHex(Bytes[I], 2);

  FData := DecompressHexToString(HexStr);
end;


procedure TMyLangs.WriteData(Stream: TStream);
var
  HexStr: string;
  L, I: Integer;
  Bytes: TBytes;
begin

  HexStr := CompressStringToHex(FData);
  L := Length(HexStr) div 2;
  SetLength(Bytes, L);
  for I := 0 to L - 1 do
    Bytes[I] := StrToInt('$' + Copy(HexStr, I * 2 + 1, 2));

  if L > 0 then
    Stream.WriteBuffer(Bytes[0], L);
end;



function TMyLangs.CompressStringToHex(const s: string): string;
var
  InStream, OutStream: TMemoryStream;
  CompressionStream: TCompressionStream;
  CompressedBytes: TBytes;
  I: Integer;
  HexStr: string;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try

    InStream.WriteBuffer(TEncoding.UTF8.GetBytes(s)[0], TEncoding.UTF8.GetByteCount(s));
    InStream.Position := 0;

    CompressionStream := TCompressionStream.Create(clDefault, OutStream);
    try
      CompressionStream.CopyFrom(InStream, InStream.Size);
    finally
      CompressionStream.Free;
    end;

    OutStream.Position := 0;
    SetLength(CompressedBytes, OutStream.Size);
    OutStream.ReadBuffer(CompressedBytes[0], OutStream.Size);

    HexStr := '';
    for I := 0 to Length(CompressedBytes) - 1 do
      HexStr := HexStr + IntToHex(CompressedBytes[I], 2);
    Result := HexStr;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;



function TMyLangs.DecompressHexToString(const HexData: string): string;
var
  i, ByteCount: Integer;
  Bytes: TBytes;
  InputStream, OutputStream: TMemoryStream;
  DecompressionStream: TDecompressionStream;
  Buffer: array[0..4095] of Byte;
  Count: Integer;
  DecompressedStr: string;
  StringStream: TStringStream;
begin

  ByteCount := Length(HexData) div 2;
  SetLength(Bytes, ByteCount);
  for i := 0 to ByteCount - 1 do
    Bytes[i] := StrToInt('$' + Copy(HexData, i * 2 + 1, 2));


  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.WriteBuffer(Bytes[0], ByteCount);
    InputStream.Position := 0;


    DecompressionStream := TDecompressionStream.Create(InputStream);
    try

      repeat
        Count := DecompressionStream.Read(Buffer, SizeOf(Buffer));
        if Count > 0 then
          OutputStream.WriteBuffer(Buffer, Count);
      until Count = 0;
    finally
      DecompressionStream.Free;
    end;


    OutputStream.Position := 0;
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      StringStream.CopyFrom(OutputStream, OutputStream.Size);
      DecompressedStr := StringStream.DataString;
    finally
      StringStream.Free;
    end;

    Result := DecompressedStr;

  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;


procedure TMyLangs.SetProperty(
  const PropPath: string; const Value: Variant);
var
  Context: TRttiContext;
  CurrentObj: TObject;
  RootObj: TObject;
  InstanceType: TRttiType;
  Prop: TRttiProperty;
  PropName: string;
  SubPath: string;
  BracketPos1, BracketPos2: Integer;
  Index: Integer;
begin
  Context := TRttiContext.Create;
  RootObj := nil;
  try

    PropName := Copy(PropPath, 1, Pos('.', PropPath + '.') - 1);
    SubPath := Copy(PropPath, Pos('.', PropPath + '.') + 1,
    Length(PropPath));

    if Application.FindComponent(PropName) is TForm then
    CurrentObj:= Application.FindComponent(PropName)
    else
    CurrentObj := Owner.FindComponent(PropName);
    if not Assigned(CurrentObj) then Exit;

    RootObj := CurrentObj;


    while SubPath <> '' do
    begin

      if Pos('.', SubPath) > 0 then
      begin
        PropName := Copy(SubPath, 1, Pos('.', SubPath) - 1);
        SubPath := Copy(SubPath, Pos('.', SubPath) + 1, Length(SubPath));
      end
      else
      begin
        PropName := SubPath;
        SubPath := '';
      end;


      BracketPos1 := Pos('[', PropName);
      BracketPos2 := Pos(']', PropName);

      if (BracketPos1 > 0) and (BracketPos2 > BracketPos1) then
      begin
        Index := StrToInt(Copy(PropName, BracketPos1 + 1, BracketPos2 - BracketPos1 - 1));
        PropName := Copy(PropName, 1, BracketPos1 - 1);

        InstanceType := Context.GetType(CurrentObj.ClassType);
        Prop := InstanceType.GetProperty(PropName);

        if not Assigned(Prop) then
          raise Exception.CreateFmt('Property not found: %s', [PropName]);

        if Prop.PropertyType.TypeKind = tkClass then
        begin
        CurrentObj := Prop.GetValue(CurrentObj).AsObject;

        if CurrentObj is TCollection then
        begin
          if (Index < 0) or (Index >= TCollection(CurrentObj).Count) then
            raise Exception.CreateFmt('Index %d is out of bounds.', [Index]);

          CurrentObj := (CurrentObj as TCollection).Items[Index];
        end

        else if CurrentObj is TStrings then
        begin
          if (Index < 0) or (Index >= TStrings(CurrentObj).Count) then
            raise
            Exception.CreateFmt('Index %d is out of bounds.', [Index]);


          TStrings(CurrentObj)[Index] := Value;
          Exit;
        end
        else
        raise Exception.CreateFmt(
        'Unexpected object type: %s. '+
        'Only TCollection and TStrings are supported.',
        [CurrentObj.ClassName]);
        end;
      end
      else
      begin

        InstanceType := Context.GetType(CurrentObj.ClassType);
        Prop := InstanceType.GetProperty(PropName);

        if not Assigned(Prop) then
          raise Exception.CreateFmt('Property not found: %s', [PropName]);


        if SubPath = '' then
        begin
          if Prop.IsWritable then
          begin
            Prop.SetValue(CurrentObj, TValue.From<Variant>(Value));
            Exit;
          end
          else
            raise Exception.CreateFmt('Property is read-only: %s', [PropName]);
        end;


        if Prop.PropertyType.TypeKind = tkClass then
        begin
          CurrentObj := Prop.GetValue(CurrentObj).AsObject;

          if not Assigned(CurrentObj) then
            raise Exception.CreateFmt('Sub-object is nil: %s', [PropName]);
        end
        else
          raise Exception.CreateFmt('Property is not an object: %s', [PropName]);
      end;
    end;
  finally
    if RootObj is TWinControl then
      TWinControl(RootObj).Invalidate;
    Context.Free;
  end;
end;


procedure TMyLangs.SetCompressedData(const Value: string);
begin
  FData := CompressStringToHex(Value);
end;

function TMyLangs.GetCompressedData: string;
begin
  if FData <> '' then
    Result := DecompressHexToString(FData)
  else
    Result := '';
end;



end.


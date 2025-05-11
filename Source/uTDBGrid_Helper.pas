unit uTDBGrid_Helper;

interface

uses
  Vcl.DBGrids, Vcl.Graphics, Data.DB, System.Math;

type
  TDBGridHelper = class helper for TDBGrid
  public
    procedure ReSizeAllColumnsToFit;
  end;

implementation

procedure TDBGridHelper.ReSizeAllColumnsToFit;
var
  I, ContentWidth, TitleWidth: Integer;
  DataSet: TDataSet;
  BM: TBookmark;
begin
  if not Assigned(DataSource) or not Assigned(DataSource.DataSet) then Exit;
  DataSet := DataSource.DataSet;
  for I := 0 to Columns.Count - 1 do
  begin
    TitleWidth := Canvas.TextWidth(Columns[I].Title.Caption) + 20;
    ContentWidth := TitleWidth;
    if not DataSet.IsEmpty then
    begin
      BM := DataSet.GetBookmark;
      try
        DataSet.DisableControls;
        DataSet.First;
        while not DataSet.Eof do
        begin
          ContentWidth := Max(ContentWidth,
            Canvas.TextWidth(
            DataSet.FieldByName(Columns[I].FieldName).AsString) + 20);
          DataSet.Next;
        end;
      finally
        DataSet.GotoBookmark(BM);
        DataSet.FreeBookmark(BM);
        DataSet.EnableControls;
      end;
    end;
    Columns[I].Width := Max(TitleWidth, ContentWidth);
  end;

end;

end.

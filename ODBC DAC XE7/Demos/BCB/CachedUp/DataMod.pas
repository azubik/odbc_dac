unit DataMod;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB,
  ODBCCustomDataset, ODBCUpdateSQL, ODBCQuery, ODBCConnection;

type
  TCacheData = class(TDataModule)
    DataSource: TDataSource;
    ODBCConnection: TODBCConnection;
    ODBCQuery: TODBCQuery;
    ODBCUpdateSQL: TODBCUpdateSQL;
    ODBCQuerycustomer_num: TAutoIncField;
    ODBCQueryfname: TStringField;
    ODBCQuerylname: TStringField;
    ODBCQueryUpdateStatus: TStringField;
    procedure UpdateErrorHandler(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure ODBCQueryCalcFields(DataSet: TDataSet);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CacheData: TCacheData;

implementation

uses CachedUp, ErrForm;

{$R *.DFM}

{ This event is triggered when an error occurs during the update process
  (such as a key violation).  Here we use another form to show the user
  the error and allow them to decide what to do about it.  See ErrForm.pas
  for more information }

procedure TCacheData.UpdateErrorHandler(DataSet: TDataSet; E: EDatabaseError; UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  UpdateAction := UpdateErrorForm.HandleError(DataSet, E, UpdateKind);
end;

{ This event displays the current update status in a calculated field }

procedure TCacheData.ODBCQueryCalcFields(DataSet: TDataSet);
const
  UpdateStatusStr: array[TUpdateStatus] of string = ('Unmodified', 'Modified', 'Inserted', 'Deleted');
begin
  if ODBCQuery.CachedUpdates then
    ODBCQueryUpdateStatus.Value := UpdateStatusStr[ODBCQuery.UpdateStatus];
end;

end.

